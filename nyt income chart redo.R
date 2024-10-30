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
library(DataExplorer)

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
  filter(YEAR == 1980) %>%
  count(EDUC) %>%
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
  select(-MULTYEAR, -SERIAL, - CBSERIAL, -EDUCD, -HISPAND, -GQ, -RACED, -HISPAND, -SAMPLE) %>%
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
  mutate(ed_cat = case_when(EDUC <6 ~ "Less than HS",
                            EDUC == 6 ~ "HS Diploma",
                            between(EDUC, 7, 9) ~ "Some College",
                            EDUC >= 10 ~ "BA+")) %>%
  mutate(ed_cat = factor(ed_cat, levels = c("Less than HS", "HS Diploma", "Some College", "BA+")))

glimpse(nytdata_1980)

nytdata_1980 %>%
  count(eth_cat)

nytdata_1980 %>%
  count(ed_cat, EDUC)

nytdata_1980 %>%
  count(SEX, ed_cat)

nytdata_1980 %>%
  summarise(tot_pop = sum(PERWT), n_resp = n())

# run ed attain by sex with std error
nytdata_1980_edsex <-
nytdata_1980 %>%
  as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT) %>%
  group_by(SEX, ed_cat) %>%
  summarise(sex_ed = survey_total(vartype = c("se", "cv"))) %>%
  ungroup() %>%
  group_by(SEX) %>%
  mutate(sex_ed_pct = sex_ed /sum(sex_ed)) %>%
  ungroup()


#nytdata_1980_edsex_inc <-
  nytdata_1980 %>%
  mutate(inctot2 = as.numeric(INCTOT)) %>%
  as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT) %>%
  group_by(SEX, ed_cat) %>%
  summarise(sex_ed_inctot = survey_median(inctot2, na.rm = T))



nytdata_1980_sd <-  nytdata_1980 %>%
  as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT)


nytdata_1980 %>%
  count(RACE)
