library(here)
library(tidyverse)
library(haven)
library(modelsummary)

d <- read_sav(here("data-clean",
  "FOR MULTINOMIAL MULTILEVEL REGRESSION.sav"))
