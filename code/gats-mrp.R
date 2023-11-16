library(here)
library(tidyverse)
library(ggrepel)
library(modelr)
library(lme4)

# read in the cleaned GATS data
gd <- read_rds(here("data-clean", "gats-clean.rds"))

# Observed proportions of polytobacco use by country
truth <- gd %>% drop_na() %>% 
  mutate(poly = ifelse(numtob > 1, 1, 0)) %>%
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
  
  ggplot(data =d, 
         mapping =aes(x=estimate, y=tru_prev, label=country)) + 
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
gd$poly <- if_else(gd$numtob > 1, 1, 0)
sample_data$poly <- if_else(sample_data$numtob > 1, 1, 0)

model <- 
  glm(poly ~ male + as.factor(educ3) + agegp + 
         as.factor(wealth), 
             data = sample_data, 
             family ='binomial' )

# create poststratification frame
psframe <- gd %>%
  select(country, male, educ3, agegp, wealth, weight) %>%
  drop_na() %>%
  count(country, male, educ3, agegp, wealth,
        wt = weight)

poststratified_estimates <- psframe %>%
  add_predictions(model, type = 'response') %>%
  group_by(country) %>% 
  summarize(estimate = weighted.mean(pred, n) * 100)

compare_to_truth(poststratified_estimates, truth)

model2 <- 
  glmer(poly ~ male + as.factor(educ3) + agegp + 
        as.factor(wealth) + (1 | country) + factor(wbincg), 
      data = sample_data, 
      family ='binomial' )

# create poststratification frame
psframe <- gd %>%
  select(country, male, educ3, agegp, wealth, wbincg, weight) %>%
  drop_na() %>%
  count(country, male, educ3, agegp, wealth, wbincg,
        wt = weight)

poststratified_estimates2 <- psframe %>%
  add_predictions(model2, type = 'response') %>%
  group_by(country) %>% 
  summarize(estimate = weighted.mean(pred, n) * 100)

compare_to_truth(poststratified_estimates2, truth)

model3 <- 
  glmer(poly ~ (1 | male) + (1 | educ3) + (1 | agegp) + 
          (1 | wealth) + (1 | country) + factor(wbincg), 
        data = sample_data, 
        family ='binomial' )

poststratified_estimates3 <- psframe %>%
  add_predictions(model3, type = 'response') %>%
  group_by(country) %>% 
  summarize(estimate = weighted.mean(pred, n) * 100)

compare_to_truth(poststratified_estimates3, truth)

model4 <- 
  brm(poly ~ (1 | male) + (1 | educ3) + (1 | agegp) + 
          (1 | wealth) + (1 | country) + factor(wbincg), 
        data = sample_data, 
        family ='binomial',
      chains = 4, cores = 4, iter = 2000, seed = 298)

poststratified_estimates4 <- psframe %>%
  add_epred_draws(model4, ndraws = 1000) %>%
  group_by(country) %>% 
  summarize(estimate = weighted.mean(.epred, n) * 100)

compare_to_truth(poststratified_estimates4, truth)

  
  