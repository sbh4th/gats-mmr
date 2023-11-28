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
  slice_sample(n = 5000) 

bm <- 
  brm(poly ~ male + factor(wbincg) + mpower +
        (1 | country), 
        data = sample_data, 
        family = bernoulli(),
      prior = c(prior(normal(0, 3), class = Intercept),
        prior(normal(0, 0.5), class = b), 
        prior(exponential(1), class = sd)),
      chains = 4, cores = 4, iter = 2000, seed = 298,
      sample_prior = "yes")

bm2 <- 
  brm(poly ~ male + factor(wbincg) + mpower +
        (1 | wealth:country) + (1 | agegp) + 
        (1 | educ3) + 
        (1 | wealth) +  (1 | male:educ3) + 
        (1 | educ3:agegp) + 
        (1 | male:wealth), 
        data = sample_data, 
        family = bernoulli(),
      prior = c(prior(normal(0, 3), class = Intercept),
        prior(normal(0, 0.5), class = b), 
        prior(exponential(1), class = sd)),
      chains = 4, cores = 4, iter = 2000, seed = 3478,
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

poststratified_estimates <- psframe %>%
  group_by(country) %>%
  mutate(wt = n / sum(n)) %>%
  add_epred_draws(bm, ndraws = 10,
    allow_new_levels = TRUE) %>%
  group_by(country, .draw) %>%
  mutate(west = .epred * wt) %>%
  summarise(wsum = sum(west)) %>%
  summarise(mrp = mean(wsum) * 100, 
            mrp_se = sd(wsum) * 100)
%>%
%>%
  group_by(country) %>%
   mutate(sumwt = sum(n),
         wt = n / sum(n),
         nobs = n(),
         est = (.epred * n * 100),
         mrp = sum(est) / sumwt,
         wsqd = n * (.epred * 100 - mrp)^2,
         sqds = sum(wsqd),
         test = (.epred * wt * 100)) %>%
  filter(country=="India") %>%
  group_by(.draw) %>%
  mutate(west = .epred * wt) %>%
  summarise(wsum = sum(west))


, wsum = sum(west)) %>%
  ungroup() 
%>%
  summarise(mean = mean(wsum) * 100, std = sd(wsum) * 100)
  
%>%
  summarise_at(c("mrp", "sqds", 
                 "nobs", "sumwt"), max) %>%
  mutate(mrpsd = sqrt(sqds / sumwt * (nobs - 1) / nobs)) %>%
  select(country, mrp, mrpsd)


epred_mat <- add_epred_draws(
  bm, newdata = psframe, draws = 1000)
mrp_estimates_vector <- epred_mat %*% psframe$n / sum(psframe$n)

ggplot(data = true_prev, aes(x = truth, y = country)) +
  geom_point(color = "#e41a1c") + 
  geom_errorbar(aes(xmin = truth - 2 * true_se,
    xmax = truth + 2 * true_se, width = 0)) +
  geom_point(data = poststratified_estimates,
    aes(x = estimate, y = country))

                geom_errorbar(
    xmin = 
  )

compare_to_truth(poststratified_estimates, truth)

model2 <- 
  glmer(poly ~ male + as.factor(educ3) + agegp + 
        as.factor(wealth) + (1 | country) + factor(wbincg), 
      data = sample_data, 
      family ='binomial' )

# create poststratification frame
psframe2 <- gd %>%
  select(country, male, educ3, agegp, wealth, wbincg, weight) %>%
  drop_na() %>%
  count(country, male, educ3, agegp, wealth, wbincg,
        wt = weight)

poststratified_estimates2 <- psframe2 %>%
  add_predictions(model2, type = 'response') %>%
  group_by(country) %>% 
  summarize(estimate = weighted.mean(pred, n) * 100)

compare_to_truth(poststratified_estimates2, truth)

model3 <- 
  glmer(poly ~ (1 | male) + (1 | educ3) + (1 | agegp) + 
          (1 | wealth) + (1 | country) + factor(wbincg), 
        data = sample_data, 
        family ='binomial' )

poststratified_estimates3 <- psframe2 %>%
  add_predictions(model3, type = 'response') %>%
  group_by(country) %>% 
  summarize(estimate = weighted.mean(pred, n) * 100)

compare_to_truth(poststratified_estimates3, truth)

model4 <- 
  brm(poly ~ (1 | male) + (1 | educ3) + (1 | agegp) + 
          (1 | wealth) + (1 | country) + factor(wbincg), 
        data = sample_data, 
        family ='bernoulli',
      chains = 4, cores = 4, iter = 2000, seed = 298)

poststratified_estimates4 <- psframe2 %>%
  add_epred_draws(model4, ndraws = 1000) %>%
  group_by(country) %>% 
  summarize(estimate = weighted.mean(.epred, n) * 100)

compare_to_truth(poststratified_estimates4, truth)

model5 <- 
  brm(poly ~ (1 | male) + (1 | educ3) + (1 | agegp) + 
          (1 | wealth) + (1 | country) + factor(wbincg), 
        data = gd, 
        family ='bernoulli',
      chains = 4, cores = 4, iter = 2000, seed = 298)



cinc <- gd %>% 
  group_by(country, wealth, male) %>%
  summarise(tru_prev = weighted.mean(poly, weight) * 100) 


m <- cinc %>%
  filter(male==1) %>%
  ggplot(aes(x=wealth, y=tru_prev, 
             colour = country)) + 
  geom_line() + 
  scale_x_continuous(breaks = c(1,2,3)) +
  gghighlight(country %in% 
    c("Bangladesh", "Senegal", "Romania")) + 
  labs(x ='Wealth group', 
       y ='Percentage of dual or poly-tobacco use' ) +
  ggtitle("Men") +
  theme_minimal()

w <- cinc %>%
  filter(male==0) %>%
  ggplot(aes(x=wealth, y=tru_prev, 
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

  
  