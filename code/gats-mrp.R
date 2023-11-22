library(here)
library(tidyverse)
library(ggrepel)
library(tidymodels)
library(modelr)
library(lme4)
library(gghighlight)
library(patchwork)

# read in the cleaned GATS data
gd <- read_rds(here("data-clean", "gats-clean.rds")) %>%
  select(-hwarn) %>%
  drop_na() %>%
  mutate(poly = ifelse(numtob > 1, 1, 0))

# Observed proportions of polytobacco use by country
truth <- gd %>% 
  group_by(country) %>%
  summarise(tru_prev = weighted.mean(poly, weight) * 100) 
  
truth %>%
  mutate(country = fct_reorder(country, tru_prev)) %>% 
  ggplot(mapping =aes(x=tru_prev, y=country)) + 
  geom_point(alpha = 0.7) + 
  labs(x ='Percentage of dual or poly-tobacco use', y ='Country' ) +
  theme_minimal()

# take a 10% sample
sample_data <- gd %>% 
  slice_sample(n = 1000) 

sample_summary <- sample_data %>%
  mutate(poly = ifelse(numtob > 1, 1, 0)) %>%
  group_by(country) %>%
  summarise(estimate = mean(poly) * 100) 


# a function to plot the state-level estimates against the truth
compare_to_truth <- function(estimates, truth){ 
  
  d <-left_join(estimates, truth, by ='country' ) 
  
  ggplot(data = d, 
         mapping = aes(x=estimate, y=tru_prev, label=country)) + 
    geom_point(alpha = 0.5) + geom_text_repel() + 
    theme_minimal() + 
    geom_abline(intercept = 0, slope =1, linetype ='dashed' ) + 
    labs(x ='Estimate' , y ='Truth' , 
         caption =paste0( 'Correlation = ', 
         round(cor(d$estimate, d$tru_prev), 2), 
         ', Mean Absolute Error = ', 
         round(mean(abs(d$estimate -d$tru_prev)), 3))) } 

compare_to_truth(sample_summary, truth)

# fit a basic model
model1 <- 
  glm(poly ~ male + as.factor(educ3) + agegp + 
         as.factor(wealth), 
      data = sample_data, family ='binomial' )
tidy(model1)


# create poststratification frame
psframe <- gd %>%
  select(country, male, educ3, agegp, wealth, weight) %>%
  drop_na() %>%
  count(country, male, educ3, agegp, wealth,
        wt = weight)

poststratified_estimates <- psframe %>%
  add_predictions(model1, type = 'response') %>%
  group_by(country) %>% 
  summarize(estimate = weighted.mean(pred, n) * 100)

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



  
  