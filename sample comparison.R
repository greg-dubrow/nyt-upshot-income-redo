library(ipumsr)
library(srvyr)
library(tidyverse)
library(tidylog)
library(janitor)

source("~/Data/r/basic functions.R")
options(scipen=10000)

# removes STRATA = 1 that stops sample operations!
options(survey.lonely.psu="remove")

# metadata
nytddi6 <- read_ipums_ddi("data/usa_00006.xml")
names(nytddi6)

# data - 5% samples in 1980, 1990 & 2000, 1% ACS 2010, 5-year 5% 2022 ACS (2018-2022)
censuscheck1 <- read_ipums_micro(nytddi6)

glimpse(censuscheck1)

censuscheck1 %>%
  filter(YEAR == 2010) %>%
  summarise(poptot = sum(PERWT))

censuscheck1 %>%
  filter(YEAR == 2022) %>%
  count(MULTYEAR)


censuscheck1 %>%
  filter(YEAR == 2000) %>%
  as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT) %>%
  srvyr::summarise(poptot = survey_total())

# population estimates
# 1980 226,862,400 se 0
# 1990 248,107,628 se 33751
# 2000 281,421,906
# 2010 309,349,689

censuscheck1 %>%
  count(YEAR)

censuscheck1 <- censuscheck1 %>%
  filter(LABFORCE == 2) %>%
  filter(between(AGE, 18, 67)) %>%
  filter(WKSWORK2 > 3) %>%
  filter(UHRSWORK >= 30) %>%
  filter(PERWT >0)


censuscheck1 %>%
  filter(WKSWORK2 > 3) %>%
  count(UHRSWORK) %>%
  view()

censuscheck1 %>%
  count(YEAR)

censuscheck1 %>%
  filter(YEAR == 2010) %>%
  mutate(sex = ifelse(SEX == 1, "Male", "Female")) %>%
  mutate(eth_cat = case_when((RACED %in% c(100,110) & HISPAND == 0) ~ "White",
                             (RACED == 200 & HISPAN == 0) ~ "African American",
                             (between(RACED, 300, 399) & HISPAND == 0) ~ "Native American",
                             (between(RACED, 400, 600) & HISPAND == 0) ~ "Asian/PI",
                             (between(RACED, 861, 893) & HISPAND == 0) ~ "Asian/PI",
                             (RACED %in% c(943, 944) & HISPAND == 0) ~ "Asian/PI",
                             (RACED == 976 & HISPAND == 0) ~ "Asian/PI",
                             HISPAND >=100 ~ "Hispanic/Latino",
                             TRUE ~ "Other/Multi")) %>%
#  select(YEAR, CLUSTER, STRATA, PERWT, GQ, PERNUM, INCEARN) %>%
  # count(INCEARN) %>%
  # view()
    # for 1980 only use derived vqr, after that use INCEARN
  # mutate(incwage = as.numeric(ifelse(INCWAGE == 999999, NA, INCWAGE))) %>%
  # mutate(incfarm = as.numeric(ifelse(INCFARM == 999999, NA, INCFARM))) %>%
  # mutate(incbus = as.numeric(ifelse(INCBUS == 999999, NA, INCBUS))) %>%
  # mutate(incearn1 = incwage + incfarm + incbus) %>%
  # after 1980
  mutate(incearn1 = as.numeric(INCEARN)) %>%
  select(-INCEARN) %>%
  filter(sex == "Female") %>%
  group_by(eth_cat) %>%
#  group_by(SEX) %>%
  as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT) %>%
  srvyr::summarise(poptot = survey_total(),
                   inc_med = survey_median(incearn1, na.rm = TRUE))

gc()

library(data.table)
censuscheck1dt <- data.table(censuscheck1) %>%
  filter(YEAR == 1990) %>%
  mutate(ctid = 1) %>%
  # mutate(incwage = as.numeric(ifelse(INCWAGE == 999999, NA, INCWAGE))) %>%
  # mutate(incfarm = as.numeric(ifelse(INCFARM == 999999, NA, INCFARM))) %>%
  # mutate(incbus = as.numeric(ifelse(INCBUS == 999999, NA, INCBUS))) %>%
  # mutate(incearn1 = incwage + incfarm + incbus)
  mutate(sex = ifelse(SEX == 1, "Male", "Female")) %>%
  mutate(incearn1 = as.numeric(INCEARN))

censuscheck1dt %>%
  count(ctid)

dt_design <- survey::svydesign(
  #formula = ~1,
  ids = ~CLUSTER,
  data = censuscheck1dt,
  strata = ~STRATA,
  weights = ~PERWT
)

census90_means
survey::svyquantile(~incearn1, dt_design, quantile=c(0.5), ci=TRUE, se=ci)
survey::svyquantile(~incearn1+sex, dt_design, quantile=c(0.5), ci=TRUE, se=ci)
survey::svytotal(~ctid, dt_design)

# 1980
# rows = 3,621,069 pop = 72,421,280  pop se 0  inc_med 13005 se 7.65

# 1990
# rows = 4,322,255 pop = 87,005,511, pop se 19179, incmed 22000 se 23.7

# 2000
# rows = 4,892,458 pop = 99,740,697, pop se 22343, incmed 30000 se 25.5

# 2010
# rows = 4,892,458 pop = 99,740,697, pop se 22343, incmed 30000 se 25.5


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

glimpse(censuscheck90)

censuscheck90 %>%
  mutate(incearn1 = as.numeric(INCEARN)) %>%
  as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT) %>%
  srvyr::summarise(poptot = survey_total(),
                   inc_med = survey_median(incearn1, na.rm = TRUE))
