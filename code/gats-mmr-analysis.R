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

# read in the cleaned GATS data
gd <- read_rds(here("data-clean", "gats-clean.rds"))

# Table 1

# Observed proportions of outcome
gd %>% group_by(numtob) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n) * 100)

# Simple Bayesian model

# take a 5% sample to get initial models
gds <- gd %>% slice_sample(prop = 0.05)

# observed proportions of outcome
gds %>% group_by(numtob) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n) * 100)

m1 <- 
  brm(numtob ~ 1 + (1 | country), 
    data = gds, 
    family = categorical(link = "logit"),
    #prior = c(
     # prior(normal(0, 3), class = Intercept),
      # prior(normal(0, 3), class = b),
      #prior(exponential(1), class = sd)),
    chains = 4, cores = 4, iter = 2000, seed = 1234,
    file = here("code/fits", "m1"))

fitted(m1)[1, , ] %>% 
  round(digits = 2) %>% 
  t()

# load model results (if already run)
m1 <- readRDS(here("code/fits", "m1.rds")) 


bm <- brm(
  bf(polytob_recode ~  (1 | country_n)),
  data = d,
  family = poisson(),
  prior = c(
    prior(normal(0, 3), class = Intercept),
    # prior(normal(0, 3), class = b),
    prior(exponential(1), class = sd)
  ),
  chains = 4, cores = 4, iter = 2000, seed = 1234,
  file = "fits/bm2")

dsmall <- gd %>% select(numtob, id, country)

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
  brm(data = gds, 
      family = categorical(link = logit, refcat = 0),
      bf(numtob ~ 1,
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

bm_nl <- readRDS(here("code/fits", "bm_nl.rds")) 

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
  brm(data = gds, 
    family = categorical(link = logit, refcat = 0),
    bf(numtob ~ 1,
      nlf(mu1 ~ a1),
      nlf(mu2 ~ a2),
      nlf(mu3 ~ a3),
      a1 + a2 + a3 ~ 1 + (1 | country) ),
    prior = c(prior(normal(0, 1), class = b, nlpar = a1),
      prior(normal(0, 1), class = b, nlpar = a2),
      prior(normal(0, 1), class = b, nlpar = a3)),
      # prior(exponential(1), class = sd, dpar = mu1)),
    iter = 2000, warmup = 1000, cores = 4, chains = 4,
    seed = 294,
    file = "code/fits/bm_nl_ml")

bm_nl_ml <- readRDS(here("code/fits", "bm_nl_ml.rds")) 

dm <- d %>% filter(sex==1) %>% mutate(poly = ifelse(polytob_recode==3,1,0))

bm_m <-
  brm(poly ~ (1 | country_n),
      data = dm, 
      family = bernoulli(),
      prior = c(prior(exp(1), class = sd)))
         


load_data <- function(N = 1000000, n = 10000) {
  softmax <- function(x) exp(x) / sum(exp(x))
  
  # Age
  age_values <- c('18–25', '26–35', '36–45', '46–55', '56–65', '66+')
  age_probabilities <- softmax(c(2, 3, 3, 2, 2, 1))
  
  # Seniority
  seniority_values <- c('6M', '1Y', '2Y', '3Y', '4Y', '5Y', '6Y+')
  seniority_probabilities <- softmax(c(3, 2, 2, 2, 1, 1, 1))
  
  # Score
  score_values <- seq(0, 10)
  score_probabilities <- softmax(c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4))
  
  # Generate a population
  population <- tibble(age = sample(age_values, N,
                                    prob = age_probabilities,
                                    replace = TRUE),
                       seniority = sample(seniority_values, N,
                                          prob = seniority_probabilities,
                                          replace = TRUE))
  
  # Take a sample from the population
  sample <- population %>%
    sample_n(n) %>%
    mutate(score = sample(score_values, n,
                          prob = score_probabilities,
                          replace = TRUE)) %>%
    mutate(category = case_when(score < 7 ~ 'detractor',
                                score > 8 ~ 'promoter',
                                TRUE ~ 'neutral'))
  
  # Summarize the population
  population <- population %>%
    group_by(age, seniority) %>%
    count(name = 'cell_size')
  
  # Summarize the sample
  sample <- sample %>%
    group_by(age, seniority) %>%
    summarize(detractors = sum(category == 'detractor'),
              neutrals = sum(category == 'neutral'),
              promoters = sum(category == 'promoter')) %>%
    mutate(cell_size = detractors + neutrals + promoters)
  
  # Bind counts of neutrals, detractors, and promoters (needed for brms)
  sample$cell_counts <- with(sample, cbind(detractors, neutrals, promoters))
  colnames(sample$cell_counts) <- c('detractor', 'neutral', 'promoter')
  
  # Remove unused columns
  sample <- sample %>% select(-detractors, -neutrals, -promoters)
  
  list(population = population, sample = sample)
}
data <- load_data()

N <- 15
dat <- data.frame(
  y1 = rbinom(N, 10, 0.3), y2 = rbinom(N, 10, 0.5), 
  y3 = rbinom(N, 10, 0.7), x = rnorm(N)
)
dat$size <- with(dat, y1 + y2 + y3)
dat$y <- with(dat, cbind(y1, y2, y3))
  
