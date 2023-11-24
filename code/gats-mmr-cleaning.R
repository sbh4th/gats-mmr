library(here)
library(tidyverse)
library(haven)
library(labelled)
library(modelsummary)

d <- read_sav(here("data-clean",
  "FOR MULTINOMIAL MULTILEVEL REGRESSION.sav"))

datasummary_skim(d)

# create age groups, collapsed education, income and
# knowledge of tobacco score

d1 <- d %>%
  mutate(
    # recode age group
    agegp = cut(AGE, breaks = c(15,30,40,50,120),
      right = FALSE, labels = c("15-29", "30-39", 
      "40-49", "50+")),
         
   # recode education
   educ3 = ifelse(education<=1, 1,
    ifelse(education == 2 , 2,  
      ifelse(education > 2, 3, NA))),
   
   # recode tobacco knowledge
   knows = ifelse(knowscore<=3, 1, 
    ifelse(knowscore>3 & knowscore<8, 2, 
      ifelse(knowscore>=8, 3, NA))),
   
   # recode gender
   male = ifelse(sex == 1, 1, 0),
   
   # recode danger
   danger = ifelse(danger_recode == 2, 1, 0),
   
   # recode advertising
   advert = ifelse(advert_recode == 2, 1, 0),
   
   # recode rules about smoking in home
   hrules = ifelse(homerules_recode == 0, 1,
     ifelse(homerules_recode>0 & homerules_recode<3, 2, 
       ifelse(homerules_recode == 3, 3, NA))),
   
   # recode health warnings
   hwarn = ifelse(healthwarn_recode == 1, 1,
     ifelse(healthwarn_recode==0 | healthwarn_recode==3, 0, NA)),
   
   # recode income quintiles
   wealth = ifelse(wealthquintile <= 2, 1, 
     ifelse(wealthquintile == 3, 2, 
       ifelse(wealthquintile >= 4, 3, NA))),
   
   # fix country labels
   country = factor(country_n, levels=1:18, 
   labels=c("Uruguay", "China", "Indonesia", "Kazakhstan",
     "Vietnam", "Bangladesh", "Botswana", "Costa Rica",
     "Ethiopia", "Philippines", "Mexico", "India",
     "Romania", "Russia", "Senegal", "Tanzania",
     "Turkey", "Ukraine"))
  ) %>%

    # rename a few variables
  rename(numtob = polytob_recode,
         wbincg = incomegrp,
         strata = gatsstrata,
         cluster = gatscluster,
         weight = gatsweight,
         id = ID) %>%
  
  # limit to recoded
  select(id, strata, cluster, weight, country, wbincg,
         numtob, wealth, hwarn, hrules, advert, danger,
         knows, male, educ3, agegp, mpower)

# add some labels
d2 <- d1 %>%
  remove_var_label() %>%
  set_variable_labels(
  id = "Individual ID",
  strata = "GATS survey strata",
  cluster = "GATS survey cluster",
  weight = "GATS survey weight",
  country = "Country",
  wbincg = "World Bank Income Group",
  numtob = "Number of tobacco types",
  wealth = "Household wealth group",
  hwarn = "Seen health warnings on cig packs",
  hrules = "Household rules for tobacco",
  advert = "Exposed to pro-tobacco media",
  danger = "Sources of smoking dangers",
  knows =  "Knowledge of smoking harms",
  male = "Male gender",
  educ3 = "Education category",
  agegp = "Age group",
  mpower = "MPOWER policy score")

datasummary_skim(d2)

# write to file
write_rds(d2, here("data-clean", "gats-clean.rds"))

         

  
