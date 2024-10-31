# reproduce this chart:
# https://www.nytimes.com/interactive/2024/10/26/upshot/census-relative-income.html
# but add white men w/ degrees and asian-americans

# response to these threads:
# https://bsky.app/profile/swordsjew.bsky.social/post/3l7hvtq6ars2j

# To calculate the relative economic standing of various demographic groups and workers over time,
# The Upshot analyzed income reported in microdata from the decennial census and American Community Survey
# for the years 1980, 1990, 2000, and the 2010 and the 2022 five-year sample.
#
# The average full-time worker benchmark shown is the average personal income of full-time workers in
# America who worked at least 50 weeks and 35 hours per week in the previous year.
# Relative incomes for each demographic group or occupation are calculated by comparing the
# average income of full-time workers in that group to the national benchmark.
# A result of 1 represents a group with an average income comparable to the average full-time worker nationwide.
# A higher figure represents a group earning more on average than the typical full-time American worker;
# a lower number is a group earning less.

# maybe instead of comparing to the average, start all groups at 0 in 1980 and do relative percent + or -
# interactive chart? rollover to highlight a group? or facet? both?

# use ACS via ipums?

library(ipumsr)
library(srvyr)
library(tidyverse)
library(tidylog)
library(janitor)

# metadata
nytddi <- read_ipums_ddi("data/usa_00004.xml")
names(nytddi)

# display variable information in console
nytddi$var_info$code_instr[7] %>%
  strwrap(60)

# opens up object in viewer to explore variables, values and labels
ipums_view(nytddi)


# display values and labels for variables
nytddi$var_info[[4]][[14]]


# actual data
nytdata <- read_ipums_micro(nytddi)
glimpse(nytdata)

nytdata %>%
  count(YEAR)

nytdata %>%
  count(SEX)

nytdata %>%
  filter(YEAR == 1980) %>%
  count(EDUC, EDUCD) %>%
  print(n = 49)

nytdata %>%
  filter(YEAR == 1980) %>%
  count(EDUCD) %>%
  print(n = 49)


nytdata %>%
  count(YEAR, RACE) %>%
  print(n = 49)

summary(nytdata$INCTOT)
summary(nytdata$INCWAGE)

## create dfs for each year, then do summary stats by category. create perm dfs for each year to join
  ## into longtidunal df
## as each year frame being made, might need to delete full file and reload
## analysis - use survey and/or srvry packages
# https://federicovegetti.github.io/teaching/heidelberg_2018/lab/sst_lab_day2.html
## Use PERWT for person-specific variables, HHWT for household variables
# also good explainer at:
  # https://coreysparks.github.io/appdem_Book/survey.html#replicates-and-jack-knifes-and-expansions-oh-my

## 1980 - note, ed attainment does not distinguish beyond bac degree. that starts in 1990
nytdata_1980 <- nytdata %>%
  filter(YEAR == 1980) %>%
  select(-MULTYEAR, -SERIAL, - CBSERIAL, -HISPAND, -GQ, -RACED, -HISPAND, -SAMPLE) %>%
  # set incomes to NA if 9999999
  mutate(INCTOT = ifelse(INCTOT == 9999999, NA, INCTOT)) %>%
  mutate(INCWAGE = ifelse(INCWAGE == 9999999, NA, INCWAGE)) %>%
  mutate(inctot2 = ifelse(INCTOT == 9999999, NA, as.numeric(INCTOT))) %>%
  mutate(incwage2 = ifelse(INCWAGE == 999999, NA, as.numeric(INCWAGE))) %>%
  ## recode sex to character value
  mutate(sex = ifelse(SEX == 1, "Male", "Female")) %>%
  # recode race/ethnicity. note two or more does not come until 2000
  mutate(eth_cat = case_when((RACE == 1 & HISPAN == 0) ~ "White",
                             (RACE == 2 & HISPAN == 0) ~ "African American",
                             (RACE == 3 & HISPAN == 0) ~ "Native American",
                             (RACE %in% c(4, 5, 6) & HISPAN == 0) ~ "Asian/PI",
                             (RACE = 7 & HISPAN == 0) ~ "Other (not defined)",
                             (RACE %in% c(8, 9) & HISPAN == 0) ~ "Two or more",
                             HISPAN >=1 ~ "Hispanic/Latino")) %>%
  mutate(eth_cat = factor(eth_cat, levels = c("Native American", "African American",
                                              "Hispanic/Latino", "Asian/PI",
                                              "White", "Two or more", "Other (not defined)"))) %>%
  # recode education - note, no coding for grad/professional degs in 1980
  mutate(ed_cat = case_when(EDUCD == 0 ~ "NA or no schooling",
                            EDUCD == 2 ~ "No schooling",
                            between(EDUCD, 11, 50) ~ "Less than HS",
                            EDUCD == 60 ~ "HS Diploma",
                            between(EDUCD, 65, 90) ~ "Some College",
                            #between(EDUCD, 82, 83) ~ "Associate deg",
                            EDUCD >= 100 ~ "BA/BS+")) %>%
  mutate(ed_cat = factor(ed_cat,
                         levels = c("NA or no schooling","No schooling", "Less than HS",
                                    "HS Diploma", "Some College", "BA/BS+")))

"Associate deg",

glimpse(nytdata_1980)

nytdata_1980 %>%
  count(eth_cat)

nytdata_1980 %>%
  count(ed_cat, EDUCD) %>%
  print(n = 40)

nytdata_1980 %>%
  count(SEX, ed_cat)

nytdata_1980 %>%
  summarise(tot_pop = sum(PERWT), n_resp = n())

nytdata_1980 %>%
  filter(SEX == 1) %>%
  filter(ed_cat == "Less than HS") %>%
  count(incwage2) %>%
  view()

nytdata_1980 %>%
  filter(SEX == 1) %>%
  filter(ed_cat == "Less than HS") %>%
  count(INCTOT) %>%
  view()


# run ed attain by sex with std error
nytdata_1980_edsex <-
nytdata_1980 %>%
  as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT) %>%
  group_by(sex, ed_cat) %>%
  srvyr::summarise(sex_ed = survey_total()) %>%
  ungroup() %>%
  group_by(sex) %>%
  mutate(sex_ed_pct = sex_ed /sum(sex_ed)) %>%
  ungroup() %>%
  select(sex, ed_cat, sex_ed_n = sex_ed, sex_ed_pct)


nytdata_1980_edsex_inc <-
  nytdata_1980 %>%
  as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT) %>%
  group_by(sex, ed_cat) %>%
  srvyr::summarise(inctot_med = survey_median(inctot2, na.rm = TRUE),
            incwage_med = survey_median(incwage2, na.rm = T),
            incwage = survey_quantile(incwage2, c(0.25, 0.75), na.rm = TRUE),
            inctot = survey_quantile(inctot2, c(0.25, 0.75), na.rm = TRUE)) %>%
  ungroup() %>%
  select(sex, ed_cat, incwage_q25, incwage_med, incwage_q75,
         inctot_q25, inctot_med, inctot_q75)

glimpse(nytdata_1980_edsex_inc)

nytdata_1980_edsex <- nytdata_1980_edsex %>%
  merge(nytdata_1980_edsex_inc, by = c("sex", "ed_cat")) %>%
  as_tibble()

saveRDS(nytdata_1980_edsex, file = "~/Data/r/nyt-upshot-income-redo/data/nytdata_1980_edsex.rds")


## 1990

nytdata %>%
  filter(YEAR == 1990) %>%
  count(EDUCD)

nytdata_1990 <- nytdata %>%
  filter(YEAR == 1990) %>%
  select(-MULTYEAR, -SERIAL, - CBSERIAL, -HISPAND, -GQ, -RACED, -HISPAND, -SAMPLE) %>%
  # set incomes to NA if 9999999
  mutate(INCTOT = ifelse(INCTOT == 9999999, NA, INCTOT)) %>%
  mutate(INCWAGE = ifelse(INCWAGE == 9999999, NA, INCWAGE)) %>%
  mutate(inctot2 = ifelse(INCTOT == 9999999, NA, as.numeric(INCTOT))) %>%
  mutate(incwage2 = ifelse(INCWAGE == 999999, NA, as.numeric(INCWAGE))) %>%
  ## recode sex to character value
  mutate(sex = ifelse(SEX == 1, "Male", "Female")) %>%
  # recode race/ethnicity. note two or more does not come until 2000
  mutate(eth_cat = case_when((RACE == 1 & HISPAN == 0) ~ "White",
                             (RACE == 2 & HISPAN == 0) ~ "African American",
                             (RACE == 3 & HISPAN == 0) ~ "Native American",
                             (RACE %in% c(4, 5, 6) & HISPAN == 0) ~ "Asian/PI",
                             (RACE = 7 & HISPAN == 0) ~ "Other (not defined)",
                             (RACE %in% c(8, 9) & HISPAN == 0) ~ "Two or more",
                             HISPAN >=1 ~ "Hispanic/Latino")) %>%
  mutate(eth_cat = factor(eth_cat, levels = c("Native American", "African American",
                                              "Hispanic/Latino", "Asian/PI",
                                              "White", "Two or more", "Other (not defined)"))) %>%
  # recode education - note, no coding for grad/professional degs in 1980
  mutate(ed_cat = case_when(EDUCD == 1 ~ "NA or no schooling",
                            EDUCD == 2 ~ "No schooling",
                            between(EDUCD, 11, 61) ~ "Less than HS",
                            EDUCD == 62 ~ "HS Diploma",
                            EDUCD == 71 ~ "Some College",
                            between(EDUCD, 82, 83) ~ "Associate deg",
                            EDUCD >= 100 ~ "BA/BS+")) %>%
  mutate(ed_cat = factor(ed_cat,
                         levels = c("NA or no schooling","No schooling", "Less than HS",
                                    "HS Diploma", "Some College", "Associate deg", "BA/BS+"))) %>%
  # ed_cat2 includes post-bacc degs
  mutate(ed_cat2 = case_when(EDUCD == 1 ~ "NA or no schooling",
                            EDUCD == 2 ~ "No schooling",
                            between(EDUCD, 11, 61) ~ "Less than HS",
                            EDUCD == 62 ~ "HS Diploma",
                            EDUCD == 71 ~ "Some College",
                            between(EDUCD, 82, 83) ~ "Associate deg",
                            EDUCD == 101 ~ "BA/BS",
                            EDUCD == 114 ~ "Masters",
                            EDUCD == 115 ~ "Professional",
                            EDUCD == 116 ~ "Doctoral")) %>%
  mutate(ed_cat2 = factor(ed_cat2,
                         levels = c("NA or no schooling","No schooling", "Less than HS",
                                    "HS Diploma", "Some College", "Associate deg", "BA/BS",
                                    "Masters", "Professional", "Doctoral")))

glimpse(nytdata_1990)

nytdata_1990 %>%
  count(ed_cat, EDUCD)

nytdata_1990 %>%
#  filter(is.na(inctot2)) %>%
  filter(ed_cat == "NA or no schooling") %>%
  count(SEX, INCWAGE, INCTOT)
%>%
  view()


nytdata_1990 %>%
#  filter(EDUCD == 1) %>%
  mutate(EDUCD2 = as.numeric(EDUCD)) %>%
  count(EDUC, EDUCD2)


nytdata_1990 %>%
  count(ed_cat, sex)

nytdata_1990 %>%
  is.na(inctot2)

nytdata_1990_edsex <-
  nytdata_1990 %>%
  as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT) %>%
  group_by(sex, ed_cat) %>%
  srvyr::summarise(sex_ed = survey_total()) %>%
  ungroup() %>%
  group_by(sex) %>%
  mutate(sex_ed_pct = sex_ed /sum(sex_ed)) %>%
  ungroup() %>%
  select(sex, ed_cat, sex_ed_n = sex_ed, sex_ed_pct)

saveRDS(nytdata_1990_edsex, file = "~/Data/r/nyt-upshot-income-redo/data/nytdata_1990_edsex_pcts.rds")

nytdata_1990_sd <-
nytdata_1990 %>%
  select(YEAR, CLUSTER, STRATA, PERNUM, PERWT, sex, ed_cat, ed_cat2, inctot2, incwage2,
         EDUC, EDUCD, INCTOT, INCWAGE) %>%
#  filter(!is.na(inctot2)) %>%
#  filter(!is.na(incwage2)) %>%
  as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT)
%>%
  group_by(sex, ed_cat) %>%
  summarise(incwage_med = survey_median(incwage2, na.rm = T))

nytdata_1990_sd %>%
  mutate(EDUCD2 = as.numeric(EDUCD)) %>%
  group_by(EDUCD2) %>%
  summarise(incwage_med = survey_quantile(incwage2, c(0.25, 0.75), na.rm=TRUE))

  srvyr::summarise(inctot_med = survey_median(inctot2, na.rm = TRUE))
