library(here)
library(tidyverse)
library(haven)
library(modelsummary)
library(brms)
library(tidybayes)
# Use the cmdstanr backend for Stan
# You need to install the cmdstanr package first
# (https://mc-stan.org/cmdstanr/) and then run cmdstanr::install_cmdstan() to
# install cmdstan on your computer.
options(mc.cores = 4,
        brms.backend = "cmdstanr")

d <- read_sav(here("data-clean",
  "FOR MULTINOMIAL MULTILEVEL REGRESSION.sav"))

des <- svydesign(ids = ~gatscluster, strata = ~gatsstrata, 
  weights = ~gatsweight, 
  data = d, nest = TRUE)

#  data = brfss_11[is.na(brfss_11$cntywt)==F,], nest=T )


bm <- brm(
  bf(polytob_recode ~  (1 | country_n)),
  data = d,
  family = poisson(),
  prior = c(
    prior(normal(0, 3), class = Intercept),
    # prior(normal(0, 3), class = b),
    prior(exponential(1), class = sd)
  ),
  chains = 4, cores = 4, iter = 2000, seed = 1234)

dsmall <- d %>% select(polytob_recode, country_n)

bm2 <-
  brm(data = list(polytob_recode = polytob_recode), 
      family = categorical(link = logit),
      polytob_recode ~ 1,
      prior = c(prior(normal(0, 5), class = Intercept, dpar = mu2),
                prior(normal(0, 5), class = Intercept, dpar = mu3)),
      iter = 2000, cores = 4, chains = 4, seed = 10,
      file = "fits/bm2")

# nonlinear syntax
bm_nl <-
  brm(data = dsmall, 
      family = categorical(link = logit, refcat = 0),
      bf(polytob_recode ~ 1,
         nlf(mu1 ~ a1),
         nlf(mu2 ~ a2),
         nlf(mu3 ~ a3),
         a1 + a2 + a3 ~ 1),
      prior = c(prior(normal(0, 1), class = b, nlpar = a1),
                prior(normal(0, 1), class = b, nlpar = a2),
                prior(normal(0, 1), class = b, nlpar = a3)),
      iter = 2000, warmup = 1000, cores = 4, chains = 4,
      seed = 11,
      file = "code/fits/bm_nl")

t <- as_draws(bm_nl)

as_draws_df(bm_nl) %>% 
  mutate(b_a4_Intercept = 0) %>% 
  mutate(p1 = exp(b_a1_Intercept) / 
    (exp(b_a1_Intercept) + exp(b_a2_Intercept) + 
    exp(b_a3_Intercept) + exp(b_a4_Intercept)),
    p2 = exp(b_a2_Intercept) / 
    (exp(b_a1_Intercept) + exp(b_a2_Intercept) + 
    exp(b_a3_Intercept) + exp(b_a4_Intercept)),
    p3 = exp(b_a3_Intercept) / 
    (exp(b_a1_Intercept) + exp(b_a2_Intercept) + 
     exp(b_a3_Intercept) + exp(b_a4_Intercept)),
    p4 = exp(b_a4_Intercept) / 
    (exp(b_a1_Intercept) + exp(b_a2_Intercept) + 
     exp(b_a3_Intercept) + exp(b_a4_Intercept))) %>% 
  pivot_longer(p1:p4) %>% 
  group_by(name) %>% 
  mean_qi(value) %>% 
  mutate_if(is.double, round, digits = 2)

# nonlinear syntax with country-RE
bm_nl_ml <-
  brm(data = dsmall, 
      family = categorical(link = logit, refcat = 0),
      bf(polytob_recode ~ (1 | country_n),
         nlf(mu1 ~ a1),
         nlf(mu2 ~ a2),
         nlf(mu3 ~ a3),
         a1 + a2 + a3 ~ 1),
      prior = c(prior(normal(0, 1), class = b, nlpar = a1),
                prior(normal(0, 1), class = b, nlpar = a2),
                prior(normal(0, 1), class = b, nlpar = a3)),
                # prior(exponential(1), class = sd)),
      iter = 2000, warmup = 1000, cores = 4, chains = 4,
      seed = 294,
      file = "code/fits/bm_nl_ml")
  
