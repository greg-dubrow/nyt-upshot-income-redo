library(ipumsr)
library(srvyr)
library(tidyverse)
library(tidylog)
library(janitor)

source("~/Data/r/basic functions.R")
options(scipen=10000)


# metadata
nytddi2 <- read_ipums_ddi("data/usa_00006.xml")
names(nytddi)

censuscheck1 <- read_ipums_micro(nytddi2)

glimpse(censuscheck1)

censuscheck1 <- censuscheck1 %>%
  filter(LABFORCE == 2) %>%
  filter(between(AGE, 18, 67)) %>%
  filter(PERWT >0) %>%
  filter(YEAR == 1980)

censuscheck1 %>%
  filter(WKSWORK2 > 3) %>%
  count(UHRSWORK) %>%
  view()

censuscheck1 %>%
  filter(WKSWORK2 > 3) %>%
  filter(UHRSWORK >= 30) %>%
  mutate(INCWAGE = ifelse(INCWAGE == 999999, NA, INCWAGE)) %>%
  mutate(INCWAGE = ifelse(INCFARM == 999999, NA, INCFARM)) %>%
  mutate(INCWAGE = ifelse(INCBUS == 999999, NA, INCBUS)) %>%
  mutate(incearn1 = INCWAGE + INCBUS + INCFARM) %>%
  filter(incearn1 > 0) %>%
  as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT) %>%
  srvyr::summarise(poptot = survey_total(),
                   inc_med = survey_median(incearn1, na.rm = TRUE))
# n = 319899 inc_med 16090 se 102

# from 1%
# nytdata1 <- read_ipums_micro(nytddi) %>%
#   filter(LABFORCE == 2) %>%
#   filter(between(AGE, 18, 67)) %>%
#   filter(PERWT >0)
#
# nytdata1 %>%
#   filter(YEAR == 1980) %>%
#   mutate(incwage = as.numeric(INCWAGE)) %>%
#   #  summarise(inc_med = median(incwage))
#   as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT) %>%
#   srvyr::summarise(inc_med = survey_median(incwage, na.rm = TRUE))
# # inc_med = 9665 se = 25.5

# 1990
censuscheck90 <- censuscheck1 %>%
  filter(LABFORCE == 2) %>%
  filter(between(AGE, 18, 67)) %>%
  filter(PERWT >0) %>%
  filter(YEAR == 1990)

glimpse(censuscheck1)

censuscheck1 %>%
  filter(LABFORCE == 2) %>%
  filter(between(AGE, 18, 67)) %>%
  filter(PERWT >0) %>%
  filter(YEAR == 1990) %>%
  count(INCEARN) %>%
  view()
