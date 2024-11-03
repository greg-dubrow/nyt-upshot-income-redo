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

# metadata
nytddi6 <- read_ipums_ddi("data/usa_00006.xml")
names(nytddi6)

# data - 5% samples in 1980, 1990 & 2000, 1% ACS 2010, 5-year 5% 2022 ACS (2018-2022)
# read in and limit to adult workforce population
nytredo1 <- read_ipums_micro(nytddi6) %>%
  filter(LABFORCE == 2) %>%
  filter(between(AGE, 18, 67)) %>%
  filter(WKSWORK2 > 3) %>%
  filter(UHRSWORK >= 30) %>%
  filter(PERWT >0) %>%
  select(-LABFORCE)

nytredo <- nytredo1 %>%
  cbind(cpivals) %>%
  # sex as character
  mutate(sex = ifelse(SEX == 1, "Male", "Female")) %>%
  mutate(age = as.numeric(AGE)) %>%
  # construct income variable to measure
  mutate(incwage = as.numeric(ifelse(INCWAGE == 999999, NA, INCWAGE))) %>%
  mutate(incfarm = as.numeric(ifelse(INCFARM == 999999, NA, INCFARM))) %>%
  mutate(incbus = as.numeric(ifelse(INCBUS == 999999, NA, INCBUS))) %>%
  mutate(incearn1 = as.numeric(incwage + incbus + incfarm)) %>%
  mutate(incearn = as.numeric(ifelse(YEAR == 1980, incearn1, INCEARN))) %>%
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
  mutate(weeks_worked = case_when(AGE <= 16 & WKSWORK2 == 0 ~ "NA",
                                  AGE > 16 & WKSWORK2 == 0 ~ "0 weeks",
                                  WKSWORK2 == 1 ~ "1-13 wks",
                                  WKSWORK2 == 2 ~ "14-26 wks",
                                  WKSWORK2 == 3 ~ "27-39 wks",
                                  WKSWORK2 == 4 ~ "40-47 wks",
                                  WKSWORK2 == 5 ~ "48-49 wks",
                                  WKSWORK2 == 6 ~ "50-52 wks")) %>%
  mutate(hours_worked = as.numeric(UHRSWORK)) %>%
  select(YEAR:PERWT, sex, age, weeks_worked, hours_worked, incearn:bachelor_deg) %>%
  tibble()

glimpse(nytredo)

nytredo %>%
  count(YEAR)

# free memory
gc()

# delete original files to save space
rm(nytredo1)
rm(nytredo)

# create sample year dfs
nytredo_1980 <- nytredo %>%
  filter(YEAR == 1980)
saveRDS(nytredo_1980, file = "~/Data/r/nyt-upshot-income-redo/data/nytredo_1980.rds")

nytredo_1990 <- nytredo %>%
  filter(YEAR == 1990)
saveRDS(nytredo_1990, file = "~/Data/r/nyt-upshot-income-redo/data/nytredo_1990.rds")

nytredo_2000 <- nytredo %>%
  filter(YEAR == 2000)
saveRDS(nytredo_2000, file = "~/Data/r/nyt-upshot-income-redo/data/nytredo_2000.rds")

nytredo_2010 <- nytredo %>%
  filter(YEAR == 2010)
saveRDS(nytredo_2010, file = "~/Data/r/nyt-upshot-income-redo/data/nytredo_2010.rds")

# 2022 keep only 2021 & 2022
nytredo_2022 <- nytredo %>%
  filter(YEAR == 2022) %>%
  filter(MULTYEAR == 2021)

saveRDS(nytredo_2022, file = "~/Data/r/nyt-upshot-income-redo/data/nytredo_2022.rds")

glimpse(nytredo_2022)

nytredo_2022 %>%
  count(MULTYEAR)
