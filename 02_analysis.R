## can run either with year files in environment or reload saved dfs

glimpse(nytredo_1980)

income_1980 <-
nytredo_1980 %>%
  group_by(sex, eth_cat) %>%
  as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT) %>%
  srvyr::summarise(poptot = survey_total(vartype = c("se", "cv")),
                   inc_med = survey_quantile(incearn, c(0.25, 0.5, 0.75),
                                             na.rm = TRUE,
                                             vartype = c("se", "cv"))) %>%
  ungroup() %>%
  mutate(year = 1980) %>%
  select(year, everything())

glimpse(income_1980)

## current dollars
pop_income_curr(nytredo_1980, "1980", c(sex, eth_cat, ed_cat1))
pop_income_curr(nytredo_1990, "1990", c(sex, eth_cat, ed_cat1))
pop_income_curr(nytredo_2000, "2000", c(sex, eth_cat, ed_cat1))

# for 2010 and 2022 split m & f, group by eth & ed,
# 2010
nytredo_2010_m <- nytredo_2010 %>%
  filter(sex == "Male")

pop_income_curr(nytredo_2010_m, "2010_Male", c(eth_cat, ed_cat1))
income_2010_Male_curr <- income_2010_Male_curr %>%
  mutate(year = str_remove(year, "_Male")) %>%
  mutate(sex = "Male") %>%
  select(year, sex, everything())

nytredo_2010_f <- nytredo_2010 %>%
  filter(sex == "Female")

pop_income_curr(nytredo_2010_f, "2010_Female", c(eth_cat, ed_cat1))
income_2010_Female_curr <- income_2010_Female_curr %>%
  mutate(year = str_remove(year, "_Female")) %>%
  mutate(sex = "Female") %>%
  select(year, sex, everything())

income_2010_curr <- income_2010_Female_curr %>%
  rbind(income_2010_Male_curr)

#2022
nytredo_2022a <- nytredo_2022 %>%
  filter(MULTYEAR == 2021)

nytredo_2022_m <- nytredo_2022a %>%
  filter(sex == "Male")

pop_income_curr(nytredo_2022_m, "2022_Male", c(eth_cat, ed_cat1))
income_2022_Male_curr <- income_2022_Male_curr %>%
  mutate(year = str_remove(year, "_Male")) %>%
  mutate(sex = "Male") %>%
  select(year, sex, everything())

nytredo_2022_f <- nytredo_2022a %>%
  filter(sex == "Female")

pop_income_curr(nytredo_2022_f, "2022_Female", c(eth_cat, ed_cat1))
income_2022_Female_curr <- income_2022_Female_curr %>%
  mutate(year = str_remove(year, "_Female")) %>%
  mutate(sex = "Female") %>%
  select(year, sex, everything())

income_2022_curr <- income_2022_Female_curr %>%
  rbind(income_2022_Male_curr)

income_all_curr <-income_1980_curr %>%
  rbind(income_1990_curr) %>%
  rbind(income_2000_curr) %>%
  rbind(income_2010_curr)  %>%
  rbind(income_2022_curr)

## constant 2022 dollars
pop_income_con(nytredo_1980, "1980", c(sex, eth_cat, ed_cat1))


inc_2010_m <-
nytredo_2010_m %>%
  mutate(del_gp = ifelse(eth_cat == "Native American" & ed_cat1 == "No schooling", 1, 0)) %>%
  filter(del_gp == 0) %>%
  group_by(eth_cat, ed_cat1) %>%
  as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT) %>%
  srvyr::summarise(poptot = survey_total(vartype = c("se", "cv")),
                   inc_med = survey_quantile(incearn, c(0.25, 0.5, 0.75),
                                             na.rm = TRUE,
                                             vartype = c("se", "cv"))) %>%
  ungroup() %>%
  mutate(year = 1980) %>%
#  add_row(eth_cat == "Native American", ed_cat1 == "No Schooling")
  select(year, everything())
