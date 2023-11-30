library(here)
library(tidyverse)
library(labelled)
library(ggrepel)
library(tidymodels)
library(modelr)
library(lme4)
library(gghighlight)
library(patchwork)
library(modelsummary)
library(brms)
library(tidybayes)
options(mc.cores = 4,
        brms.backend = "cmdstanr")

# read in the cleaned GATS data
gd <- read_rds(here("data-clean", "gats-clean.rds")) %>%
  select(-hwarn) %>%
  remove_var_label() %>%
  drop_na() %>%
  mutate(poly = ifelse(numtob > 1, 1, 0))

# Now take a sample to develop the model
sample_data <- gd %>% 
  slice_sample(n = 5000) %>%
  mutate(wbincg = factor(wbincg))

bm <- 
  brm(poly ~ male + wbincg + mpower +
        (1 | country), 
        data = sample_data, 
        family = bernoulli(),
      prior = c(prior(normal(0, 3), class = Intercept),
        prior(normal(0, 0.5), class = b), 
        prior(exponential(1), class = sd)),
      chains = 4, cores = 4, iter = 2000, seed = 298,
      sample_prior = "yes")

bm2 <-
  brm(poly ~ male + wbincg + mpower + 
        (1 + wealth | country) + (1 | agegp) + 
        (1 | educ3) + (1 | wealth) + 
        (1 | male:educ3) + (1 | educ3:agegp) + 
        (1 | male:wealth),
      data = sample_data, 
        family = bernoulli(),
      prior = c(prior(normal(0, 3), class = Intercept),
        prior(normal(0, 0.5), class = b), 
        prior(exponential(1), class = sd),
        prior(lkj(2), class = cor)),
      chains = 4, cores = 4, iter = 2000, seed = 24,
      adapt_delta = 0.99, sample_prior = "yes",
      file = "code/fits/bm2")

bm3l <- 
  brm(poly ~ 1 + wealth + 
        (1 + wealth | country), 
        data = sample_data, 
        family = bernoulli(),
      prior = c(prior(normal(0, 3), class = Intercept),
        prior(normal(0, 0.5), class = b), 
        prior(exponential(1), class = sd),
        prior(lkj(2), class = cor)),
      chains = 4, cores = 4, iter = 2000, seed = 24,
      adapt_delta = 0.99, sample_prior = "yes")

# create poststratification frame
# get country-level predictors
clp <- gd %>% 
  select(country, wbincg, mpower) %>%
  group_by(country) %>%
  summarise_all(list(mean))

psframe <- gd %>%
  select(country, male, educ3, agegp, 
         wealth, weight) %>%
  drop_na() %>%
  count(country, male, educ3, agegp, wealth,
        wt = weight) %>%
  left_join(clp, by = join_by(country))

# add model predictions to poststratification frame
poststratified_estimates <- psframe %>%
  group_by(country) %>%

# generate weights for each country
  mutate(wt = n / sum(n)) %>%
  
# add predicted draws from posterior 
  add_epred_draws(bm2, ndraws = 1000,
    allow_new_levels = TRUE) %>%
  group_by(country, .draw) %>%
  
# generate weighted predictions
  mutate(west = .epred * wt) %>%
  summarise(wsum = sum(west)) %>%
  
# post stratified estimates
  summarise(mrp = mean(wsum) * 100, 
            mrp_se = sd(wsum) * 100)

# combine truth and sampled prevalence and plot
comp2 <- true_prev %>%
  left_join(poststratified_estimates) %>%
  rename(est_1 = truth, est_2 = mrp,
         se_1 = true_se, se_2 = mrp_se) %>%
  pivot_longer(!country,
    names_to = c(".value", "sample"),
    names_sep="_") %>%
  mutate(sample = recode(sample, `1` = "Truth",
    `2` = "MRP"))

comp2 %>%
  ggplot(aes(x=est, y=country, 
    color = sample, group = sample)) + 
  geom_point(alpha = 0.7) +
  geom_errorbar(aes(xmin = est - 2 * se,
    xmax = est + 2 * se, width = 0),
    alpha = 0.7) +
  labs(x ='Percentage of dual or poly-tobacco use') +
  scale_colour_manual(values = c("#e41a1c", "#377eb8")) +
  theme_minimal()


# poststratification by sex
# add model predictions to poststratification frame
pe_sex <- psframe %>%
  group_by(country, male) %>%

# generate weights for each country
  mutate(wt = n / sum(n)) %>%
  
# add predicted draws from posterior 
  add_epred_draws(bm, ndraws = 1000,
    allow_new_levels = TRUE) %>%
  group_by(country, male, .draw) %>%
  
# generate weighted predictions
  mutate(west = .epred * wt) %>%
  summarise(wsum = sum(west)) %>%
  
# post stratified estimates by strata
  summarise(mrp = mean(wsum), 
            mrp_se = sd(wsum),
            mrp_95l = mrp - 1.96 * mrp_se,
            mrp_95h = mrp + 1.96 * mrp_se)

pe_sex %>%
  ggplot(aes(x = mrp, y = country)) + 
  geom_point() +
  geom_errorbar(aes(xmin = mrp - 2 * mrp_se,
    xmax = mrp + 2 * mrp_se, width = 0)) +
  facet_grid(cols = vars(male))


# poststratification by sex and wealth
# add model predictions to poststratification frame
pe_sex_wealth <- psframe %>%
  group_by(country, male, wealth) %>%

# generate weights for each country
  mutate(wt = n / sum(n)) %>%
  
# add predicted draws from posterior 
  add_epred_draws(bm2, ndraws = 1000,
    allow_new_levels = TRUE) %>%
  group_by(country, male, wealth, .draw) %>%
  
# generate weighted predictions
  mutate(west = .epred * wt) %>%
  summarise(wsum = sum(west)) %>%
  
# post stratified estimates by strata
  summarise(mrp = mean(wsum) * 100, 
            mrp_se = sd(wsum) * 100)

pe_sex_wealth %>%
  ggplot(aes(x = mrp, y = country)) + 
  geom_point() +
  geom_errorbar(aes(xmin = mrp - 2 * mrp_se,
    xmax = mrp + 2 * mrp_se, width = 0)) +
  facet_grid(cols = vars(male),
             rows = vars(wealth)) +
  theme_minimal()






m <- pe_sex_wealth %>%
  filter(male==1) %>%
  ggplot(aes(x=wealth, y=mrp, 
             colour = country)) + 
  geom_line() + 
  scale_x_continuous(breaks = c(1,2,3)) +
  gghighlight(country %in% 
    c("Bangladesh", "Senegal", "Romania")) + 
  labs(x ='Wealth group', 
       y ='Percentage of dual or poly-tobacco use' ) +
  ggtitle("Men") +
  theme_minimal()

w <- pe_sex_wealth %>%
  filter(male==0) %>%
  ggplot(aes(x=wealth, y=mrp, 
             colour = country)) + 
  geom_line() + 
  scale_x_continuous(breaks = c(1,2,3)) +
  gghighlight(country %in% 
    c("Romania", "Botswana", "Costa Rica")) + 
  labs(x ='Wealth group', 
       y ='Percentage of dual or poly-tobacco use' ) +
  ggtitle("Women") +
  theme_minimal()

pr <- m + w 
pr

m_c <-
  glmer(poly ~ male + wealth + (1 | country), 
        data = gd, 
        family ='binomial')
summary(m_c)

m_cv5 <-
  glmer(poly ~ male*factor(wealth) + (1 | wealth) +  
          (1 + wealth | country) +
          (1 + male | country), 
        data = gd, 
        family ='binomial')
summary(m_cv)

m_c_psf <- gd %>%
  select(country, male, wealth, weight) %>%
  drop_na() %>%
  count(country, male, wealth,
        wt = weight)

ps_m_c <- m_c_psf %>%
  add_predictions(m_cv4, type = 'response') %>%
  group_by(country, male, wealth) %>% 
  mutate(estimate = weighted.mean(pred, n) * 100)

m_v <- ps_m_c %>%
  filter(male==1) %>%
  ggplot(aes(x=wealth, y=estimate, 
             colour = country)) + 
  geom_line() + 
  scale_x_continuous(breaks = c(1,2,3)) +
  gghighlight(country %in% 
    c("Bangladesh", "Senegal", "Romania")) + 
  labs(x ='Wealth group', 
       y ='Percentage of dual or poly-tobacco use' ) +
  ggtitle("Men") +
  theme_minimal()

f_v <- ps_m_c %>%
  filter(male==0) %>%
  ggplot(aes(x=wealth, y=estimate, 
             colour = country)) + 
  geom_line() + 
  scale_x_continuous(breaks = c(1,2,3)) +
  gghighlight(country %in% 
    # c("Bangladesh", "Senegal", "Romania")) + 
      c("Bangladesh", "Senegal", "Romania")) + 
  labs(x ='Wealth group', 
       y ='Percentage of dual or poly-tobacco use' ) +
  ggtitle("Women") +
  theme_minimal()

m_v + f_v

ds <- gd %>%
  select(country, wealth, male, poly) %>%
  mutate(wealth_mid = ifelse(wealth==2, 1, 0),
         wealth_hi = ifelse(wealth==3, 1, 0))

m_cv5 <-
  glmer(poly ~ male*wealth_mid + male*wealth_hi +
          (1 | wealth_mid) + (1 | wealth_hi) +
          (1 + male | country), 
        data = ds, 
        family ='binomial')



state_df <- data.frame(
  State = 1:50,
  model_state_sd = rep(-1, 50),
  model_state_pref = rep(-1, 50),
  sample_state_pref = rep(-1, 50),
  true_state_pref = rep(-1, 50),
  N = rep(-1, 50)
)


fit <- stan_glmer(
  poly ~ male + factor(wbincg) + mpower +
        (1 | country),
  family = binomial(link = "logit"),
  data = sample_data,
  prior = normal(0, 1, autoscale = TRUE),
  prior_covariance = decov(scale = 0.50),
  adapt_delta = 0.99,
  refresh = 0,
  seed = 1010)

epred_mat <- posterior_epred(fit, newdata = psframe, draws = 10)
mrp_estimates_vector <- epred_mat %*% psframe$n / sum(psframe$n)
mrp_estimate <- c(mean = mean(mrp_estimates_vector) * 100, sd = sd(mrp_estimates_vector) * 100)
cat("MRP estimate mean, sd: ", round(mrp_estimate, 3))



filtering_condition <- which(psframe$country == "China")
state_epred_mat <- epred_mat[ ,filtering_condition]
k_filtered <- psframe[filtering_condition, ]$n
mrp_estimates_vector_sub <- state_epred_mat %*% k_filtered / sum(k_filtered)
mrp_estimate_sub <- c(mean = mean(mrp_estimates_vector_sub) * 100, sd = sd(mrp_estimates_vector_sub) * 100)
cat("MRP sub-estimate mean, sd: ", round(mrp_estimate_sub, 3))



data(chimpanzees, package = "rethinking")
d <- chimpanzees
rm(chimpanzees)

# wrangle
d <-
  d %>% 
  mutate(actor     = factor(actor),
         block     = factor(block),
         treatment = factor(1 + prosoc_left + 2 * condition),
         # this will come in handy, later
         labels    = factor(treatment,
                            levels = 1:4,
                            labels = c("r/n", "l/n", "r/p", "l/p")))

glimpse(d)

b14.3 <- 
  brm(data = d, 
      family = bernoulli(),
      pulled_left  ~ 0 + treatment + (0 + treatment | actor) + (0 + treatment | block),
      prior = c(prior(normal(0, 1), class = b),
                prior(exponential(1), class = sd, group = actor),
                prior(exponential(1), class = sd, group = block),
                prior(lkj(2), class = cor, group = actor),
                prior(lkj(2), class = cor, group = block)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,  
      seed = 4387510)


a       <-  3.5  # average morning wait time
b       <- -1    # average difference afternoon wait time
sigma_a <-  1    # std dev in intercepts
sigma_b <-  0.5  # std dev in slopes
rho     <- -.7   # correlation between intercepts and slopes

# the next three lines of code simply combine the terms, above
mu     <- c(a, b)

cov_ab <- sigma_a * sigma_b * rho
sigma  <- matrix(c(sigma_a^2, cov_ab, 
                   cov_ab, sigma_b^2), ncol = 2)

sigmas <- c(sigma_a, sigma_b)          # standard deviations
rho    <- matrix(c(1, rho,             # correlation matrix
                   rho, 1), nrow = 2)

# now matrix multiply to get covariance matrix
sigma <- diag(sigmas) %*% rho %*% diag(sigmas)

# how many cafes would you like?
n_cafes <- 20

set.seed(5)  # used to replicate example

vary_effects <- 
  MASS::mvrnorm(n_cafes, mu, sigma) %>% 
  data.frame() %>% 
  set_names("a_cafe", "b_cafe")

head(vary_effects)

n_visits <- 10
sigma    <-  0.5  # std dev within cafes

set.seed(22)  # used to replicate example

d <-
  vary_effects %>% 
  mutate(cafe = 1:n_cafes) %>% 
  expand_grid(visit = 1:n_visits) %>% 
  mutate(afternoon = rep(0:1, times = n() / 2)) %>% 
  mutate(mu = a_cafe + b_cafe * afternoon) %>% 
  mutate(wait = rnorm(n = n(), mean = mu, sd = sigma)) %>% 
  select(cafe, everything())


b14.1 <- 
  brm(data = d, 
      family = bernoulli(),
      waitd ~ 1 + afternoon + (1 + afternoon | cafe),
      prior = c(prior(normal(5, 2), class = Intercept),
                prior(normal(-1, 0.5), class = b),
                prior(exponential(1), class = sd)),
                #prior(lkj(2), class = cor)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 867530)


p <- avg_predictions(              # Compute predictions,
    model = bm2,               # using the multilevel regression model `mod`, 
    newdata = psframe,  # for each row of the `stratification` table.
    by = c("country", "male", "wealth"),               # Then, take the weighted average of predictions by city,
    wts = "n")  # using demographic weights.

p |>
  mutate(mrp = estimate * 100,
         mrpll = conf.low * 100,
         mrpul = conf.high * 100) |>
  ggplot(aes(x = mrp, y = country)) + 
  geom_point() +
  geom_errorbar(aes(xmin = mrpll,
    xmax = mrpul, width = 0)) +
  facet_grid(cols = vars(male),
             rows = vars(wealth)) +
  theme_minimal()

p |> 
    # extract draws from the posterior distribution
    posterior_draws() |>
    mutate(mrp = draw * 100) |>
    # sort cities by interest in meat substitutes
    arrange(estimate) |>
    mutate(country = factor(country, 
      levels = rev(unique(country)))) |>
    # plot the results
    ggplot(aes(y = country, colour = male, 
      group = interaction(country, male), fill = male)) +
    geom_density_ridges() +
    # facet_wrap(~male) +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    labs(
        x = "Average proportion dual/poly tobacco use",
        y = NULL,
        title = "Estimated polytobacco use",
        subtitle = "Multilevel Regression and Poststratification",
        caption = "Source: GATS Surveys")

  
  