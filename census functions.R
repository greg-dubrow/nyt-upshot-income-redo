pop_income_curr <- function(input_df, year = "NA", group_var = NULL) {
  dfout <- input_df %>%
    group_by(across({{ group_var }})) %>%
    as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT) %>%
    srvyr::summarise(poptot = survey_total(vartype = c("se", "cv")),
                     inc_med = survey_quantile(incearn, c(0.25, 0.5, 0.75),
                                               na.rm = TRUE,
                                               vartype = c("se", "cv"))) %>%
    ungroup() %>%
    mutate(year = year) %>%
    select(year, everything())
  assign(str_c("income_", year, "_curr"), dfout, envir=.GlobalEnv)
}

pop_income_con <- function(input_df, year = "NA", group_var = NULL) {
  dfout <- input_df %>%
    group_by(across({{ group_var }})) %>%
    as_survey_design(cluster = CLUSTER, strata = STRATA, weights = PERWT) %>%
    srvyr::summarise(poptot = survey_total(vartype = c("se", "cv")),
                     inc_med = survey_quantile(incearn_2022, c(0.25, 0.5, 0.75),
                                               na.rm = TRUE,
                                               vartype = c("se", "cv"))) %>%
    ungroup() %>%
    mutate(year = year) %>%
    select(year, everything())
  assign(str_c("income_", year, "_con2022"), dfout, envir=.GlobalEnv)
}

