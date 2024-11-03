# creates plots of income

# read in current dollar df
income_all_curr <- readRDS("~/Data/r/nyt-upshot-income-redo/data/income_all_curr.rds")

glimpse(income_all_curr)


## create index df
income_all_curr_ind <-
income_all_curr %>%
  select(year:ed_cat1, inc_med_q50) %>%
  group_by(sex, ed_cat1, eth_cat) %>%
  arrange(year) %>%
  mutate(inc_index = (inc_med_q50/inc_med_q50[which.min(year)])*100) %>%
  ungroup()

view(income_all_curr_ind)

income_all_con_ind <-
  income_all_curr %>%
  select(year:ed_cat1, inc_med_q50) %>%
  group_by(sex, ed_cat1, eth_cat) %>%
  arrange(year) %>%
  mutate(inc_index = (inc_med_q50/inc_med_q50[which.min(year)])*100) %>%
  ungroup()

view(income_all_con_ind)


# line charts x = year, y = income, group = ethcat, facet grid sex ~ edcat
income_all_curr %>%
  select(year:ed_cat1, inc_med_q50) %>%
  ggplot(aes(x = year, y = inc_med_q50, group = ed_cat1, color = ed_cat1)) +
  geom_line() +
  facet_grid(sex ~ eth_cat)

income_all_curr_ind %>%
#  select(year:ed_cat1, inc_med_q50) %>%
  ggplot(aes(x = year, y = inc_index, group = ed_cat1, color = ed_cat1)) +
  geom_line() +
  facet_grid(sex ~ eth_cat)

income_all_con2022 %>%
  select(year:ed_cat1, inc_med_q50) %>%
  ggplot(aes(x = year, y = inc_med_q50, group = ed_cat1, color = ed_cat1)) +
  geom_line() +
  facet_grid(sex ~ eth_cat)

income_all_con_ind %>%
  ggplot(aes(x = year, y = inc_index, group = ed_cat1, color = ed_cat1)) +
  geom_line() +
  facet_grid(sex ~ eth_cat)






## bar charts
# faceted
income_all_curr %>%
  filter(year == 1980) %>%
  select(year:ed_cat1, inc_med_q50) %>%
  ggplot(aes(x = inc_med_q50, y = ed_cat1)) +
  geom_bar(stat = "identity") +
  facet_grid(sex ~ eth_cat)

# butterfly charts
range(income_all_curr$inc_med_q50)

income_curr_butter <- income_all_curr %>%
  filter(year == 1980) %>%
  select(year:ed_cat1, inc_med_q50) %>%
  mutate(inc_med_q50_sp = ifelse(sex == "Male", -inc_med_q50, inc_med_q50)) %>%
  mutate(inc_med_max = max(inc_med_q50)) %>%
  mutate(inc_med_min = min(inc_med_q50))
view(income_curr_butter)

income_curr_butter %>%
  filter(eth_cat == "White") %>%
  ggplot(aes(x = inc_med_q50_sp, y = ed_cat1, fill = sex)) +
  geom_col()
  geom_bar(stat = "identity") +
  facet_grid(sex ~ eth_cat)
