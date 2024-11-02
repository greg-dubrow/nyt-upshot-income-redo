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

# from https://osf.io/7gft4
# *I used 1980 1% metro, 1990 1% metro, 2000 1%, 2010 ACS (1 year), and 2022 ACS (5 yr)
# *The ACS files report data collected in the previous year. So, 2022 5-yr data were collected
# *in 2017-2021. I'll excluded the pandemic-era ACS years below, but for simplicity
# *just call this "2022".
#
# *To be consistent with census timing, I should have grabbed the 2021 ACS but I didn't.
#
# *Also, note the NYT article uses 5-year ACS 2010, meaning their "2010" data were
# *collected in years 2005-2009.
#
# *Filters I imposed at download to shrink the files: age 18-67, labforce==2 (Yes)

# bls for inflation adjustment
# df <- bls_api(c("LAUCN040010000000005", "LAUCN040010000000006"),
#               startyear = "2010", endyear = "2012",
#               registrationKey = "BLS_KEY",
#               calculations = TRUE, annualaverage = TRUE, catalog = TRUE)


library(ipumsr)
library(srvyr)
library(tidyverse)
library(tidylog)
library(janitor)

source("~/Data/r/basic functions.R")
options(scipen=10000)

# removes STRATA = 1 that stops sample operations!
options(survey.lonely.psu="remove")

# get cpi data
cpivals <- readRDS("~/Data/r/nyt-upshot-income-redo/data/cpivals.rds")

# cpivals1 <- blscrapeR::inflation_adjust("1980-01-01") %>%
#   arrange(desc(date))
#
# cpivals <- cpivals1 %>%
#   group_by(year) %>%
#   mutate(avg_cpi = round(mean(value), 1)) %>%
#   ungroup() %>%
#   distinct(year, avg_cpi) %>%
#   filter(year %in% c(1980, 1990, 2000, 2010, 2021, 2022)) %>%
#   pivot_wider(names_from = year,
#               names_glue = "avg_cpi_{year}",
#               values_from = avg_cpi)
#
# saveRDS(cpivals, file = "~/Data/r/nyt-upshot-income-redo/data/cpivals.rds")


# census metadata
nytddi5 <- read_ipums_ddi("data/usa_00005.xml")
names(nytddi5)

# census data raw file
# again, the 1% 1980, 1990 & 2000 samples and single year ACS in 2010, 2021, 2022
nytdata1 <- read_ipums_micro(nytddi5) %>%
  filter(LABFORCE == 2) %>%
  filter(between(AGE, 18, 67)) %>%
  filter(PERWT >0)

glimpse(nytdata1)

nytdata1 %>%
  count(YEAR)

# test unweighted & weighted means
nytdata1 %>%
  filter(between(INCWAGE, 1, 50000)) %>%
  group_by(SEX, RACE) %>%
  summarize(
    total_pop = n(),
    total_pop_wt = sum(PERWT),
    inc_avg = mean(INCWAGE),
    inc_avg_w = weighted.mean(INCWAGE, PERWT))


nytdata1 %>%
  filter(YEAR == 1980) %>%
  count(INCWAGE) %>%
  view()

nytdata1 %>%
  group_by(YEAR, STRATA) %>%
  summarise(CNT = n()) %>%
  view()

nytdata1 %>%
  filter(YEAR == 2000) %>%
  as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT) %>%
  srvyr::summarise(poptot = survey_total())


nytdata1 %>%
  filter(YEAR == 1980) %>%
  filter(WKSWORK2 > 3) %>%
  filter(UHRSWORK >= 30) %>%
  mutate(INCWAGE = ifelse(INCWAGE == 999999, NA, INCWAGE)) %>%
  mutate(INCWAGE = ifelse(INCFARM == 999999, NA, INCFARM)) %>%
  mutate(INCWAGE = ifelse(INCBUS == 999999, NA, INCBUS)) %>%
  mutate(incearn1 = INCWAGE + INCBUS + INCFARM) %>%
 filter(incearn1 > 0) %>%
  # mutate(incwage = as.numeric(INCWAGE)) %>%
#  summarise(inc_med = median(incwage))
  as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT) %>%
  srvyr::summarise(poptot = survey_total(),
                   inc_med = survey_median(incearn1, na.rm = TRUE))

# all filters 6,357,200 16510 148

nytdata1 %>%
  count(RACED) %>%
  print(n = 400)

nytdata1 %>%
  count(HISPAND) %>%
  print(n = 400)

nytdata1 %>%
  filter(YEAR == 1980) %>%
  count(EDUC) %>%
  print(n = 400)

nytdata1 %>%
#  filter(YEAR == 1980) %>%
  count(EDUCD) %>%
  print(n = 400)

nytdata1 %>%
  count(UHRSWORK)


# *incearn is only available for 1990-2022, but can approximated by adding incwage,
# *income from business and farm
# clonevar incearn2=incearn
# replace incearn2=incwage+incbus+incfarm if year<1990
nytdata <- nytdata1 %>%
  as_tibble() %>%
  select(-LABFORCE) %>%
  # remove strata < 2 to elimintate sampling error
  # group_by(YEAR, STRATA) %>%
  # mutate(CNT = n()) %>%
  # filter(CNT > 1) %>%
  # ungroup() %>%
  # add cpi data
  cbind(cpivals) %>%
  # construct income variable to measure
  mutate(INCWAGE = ifelse(INCWAGE == 999999, NA, INCWAGE)) %>%
  mutate(INCFARM = ifelse(INCFARM == 999999, NA, INCFARM)) %>%
  mutate(INCBUS = ifelse(INCBUS == 999999, NA, INCBUS)) %>%
  mutate(incearn1 = INCWAGE + INCBUS + INCFARM) %>%
  mutate(incearn = ifelse(YEAR == 1980, incearn1, INCEARN)) %>%
  select(-incearn1) %>%
  # create cpi adjusted income
  mutate(incearn_2022 = case_when(
    YEAR == 1980 ~ (avg_cpi_2022/avg_cpi_1980) * incearn,
    YEAR == 1990 ~ (avg_cpi_2022/avg_cpi_1990) * incearn,
    YEAR == 2000 ~ (avg_cpi_2022/avg_cpi_2000) * incearn,
    YEAR == 2010 ~ (avg_cpi_2022/avg_cpi_2010) * incearn,
    YEAR == 2021 ~ (avg_cpi_2022/avg_cpi_2021) * incearn,
    TRUE ~ incearn)) %>%
  # create ethnicity vars
  mutate(eth_cat = case_when((RACED %in% c(100,110) & HISPAND == 0) ~ "White",
                             (RACED == 200 & HISPAN == 0) ~ "African American",
                             (between(RACED, 300, 399) & HISPAND == 0) ~ "Native American",
                             (between(RACED, 400, 600) & HISPAND == 0) ~ "Asian/PI",
                             (between(RACED, 861, 893) & HISPAND == 0) ~ "Asian/PI",
                             (RACED %in% c(943, 944) & HISPAND == 0) ~ "Asian/PI",
                             (RACED == 976 & HISPAND == 0) ~ "Asian/PI",
                             HISPAND >=100 ~ "Hispanic/Latino",
                             TRUE ~ "Other/Multi")) %>%
  mutate(eth_cat = factor(eth_cat, levels = c("Native American", "African American",
                                              "Hispanic/Latino", "Asian/PI",
                                              "White", "Other/Multi"))) %>%
  # create education variables
  mutate(ed_cat1 = case_when(EDUCD == 2 ~ "No schooling",
                             (YEAR == 1980 & between(EDUCD, 11, 50)) ~ "No HS Diploma",
                             (YEAR > 1980 & between(EDUCD, 10, 61)) ~ "No HS Diploma",
                             (YEAR == 1980 & EDUCD  == 60) ~ "HS Diploma",
                             (YEAR > 1980 & EDUCD %in% c(62, 63, 64)) ~ "HS Diploma",
                            between(EDUCD, 65, 90) ~ "Some College",
                            EDUCD >= 100 ~ "BA/BS+")) %>%
  mutate(ed_cat1 = factor(ed_cat1, levels = c("NA","No schooling", "No HS Diploma",
                                              "HS Diploma", "Some College", "BA/BS+"))) %>%
  mutate(ed_cat2 = case_when((YEAR > 1980 & EDUCD == 2) ~ "No schooling",
                             (YEAR > 1980 & between(EDUCD, 10, 61)) ~ "No HS Diploma",
                             (YEAR > 1980 & EDUCD %in% c(62, 63, 64)) ~ "HS Diploma",
                             (YEAR > 1980 & EDUCD %in% c(65, 71)) ~ "Some college no deg",
                             (YEAR > 1980 & between(EDUCD, 81, 83)) ~ "Associate deg",
                             (YEAR > 1980 & EDUCD == 101) ~ "BA/BS",
                             (YEAR > 1980 & EDUCD == 114) ~ "Masters",
                             (YEAR > 1980 & EDUCD == 115) ~ "Professional",
                             (YEAR > 1980 & EDUCD == 116) ~ "Doctoral",
                             TRUE ~ "NA")) %>%
  mutate(ed_cat2 = factor(ed_cat2,
                          levels = c("No schooling", "No HS Diploma",
                                     "HS Diploma", "Some college no deg", "Associate deg", "BA/BS",
                                     "Masters", "Professional", "Doctoral", "NA"))) %>%
  mutate(bachelor_deg = ifelse(ed_cat1 == "BA/BS+", "Yes", "No")) %>%
  select(YEAR:UHRSWORK, incearn:bachelor_deg)


glimpse(nytdata)

nytdata %>%
  count(bachelor_deg, ed_cat1)

nytdata %>%
  filter(YEAR == 1990) %>%
  as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT) %>%
  srvyr::summarise(poptot = survey_total(),
                   inc_med = survey_median(incearn, na.rm = TRUE))

nytdata %>%
  filter(YEAR == 1990) %>%
  filter(WKSWORK2 > 3) %>%
  filter(UHRSWORK >= 30) %>%
  filter(incearn > 0) %>%
  group_by(SEX, eth_cat) %>%
  summarize(
    total_pop = n(),
    total_pop_wt = sum(PERWT),
    inc_avg = mean(incearn),
    inc_avg_w = weighted.mean(incearn, PERWT),
    inc_med = median(incearn))


nytdata %>%
  filter(YEAR == 1980) %>%
  filter(WKSWORK2 > 3) %>%
  filter(UHRSWORK >= 30) %>%
  filter(incearn > 0) %>%
  group_by(SEX, eth_cat, ed_cat1) %>%
  mutate(CNT = n()) %>%
  # removes any cells < 5
  filter(CNT > 4) %>%
   # summarise(n = n()) %>%
   # view()
  # summarise(poptot = n(),
  #           inc_med = median(incearn))
  as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT) %>%
  srvyr::summarise(poptot = survey_total(),
                   inc_med = survey_median(incearn, na.rm = TRUE),) %>%
  select(-poptot_se, -inc_med_se) %>%
  view()

nytdata %>%
  filter(YEAR == 1990) %>%
  filter(WKSWORK2 > 3) %>%
  filter(UHRSWORK >= 30) %>%
  filter(incearn > 0) %>%
  group_by(SEX, eth_cat) %>%
#  filter(!STRATA %in% c(35, 119)) %>%
  # summarise(poptot = n(),
  #           inc_med = median(incearn))
  as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT) %>%
  srvyr::summarise(poptot = survey_total(),
                   inc_med = survey_median(incearn, na.rm = TRUE,
                                           vartype = c("se", "ci", "var", "cv"))) %>%
  view()


nytdata %>%
  filter(YEAR == 1980) %>%
  count(incearn) %>%
  view()

  nytdata %>%
  filter(YEAR == 1980) %>%
  filter(!is.na(incearn)) %>%
  group_by(ed_cat1) %>%
  as_survey_design(ids = CLUSTER, strata = STRATA, weights = PERWT, nest = TRUE) %>%
  srvyr::summarise(inc_med = survey_median(incearn, na.rm = TRUE))





nytdata %>%
  group_by(YEAR, ed_cat1) %>%
  summarise(incmed = median(incearn),
            incear_adj = median(incearn_2022))
