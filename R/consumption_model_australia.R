#!/usr/bin/env Rscript

# Australian household consumption model in a Muellbauer-style framework.
# ------------------------------------------------------------------------------
# This script is designed to be a practical, reproducible starting point for an
# integrated household consumption system for Australia using public data.
#
# Design choices:
# - Use explicit ABS series IDs downloaded with readabs.
# - Work with quarterly data.
# - Estimate a latent credit conditions index in state-space form.
# - Estimate a long-run consumption relation and a short-run ECM.
# - Save both the processed data and the model outputs for inspection.
#
# The specification follows the broad empirical logic in the Muellbauer / Duca /
# Chauvin tradition:
# - Consumption depends on income, balance-sheet variables and credit access.
# - Housing wealth matters separately from other wealth.
# - Unemployment proxies income uncertainty / precautionary motives.
# - Credit conditions are treated as latent and time-varying.

suppressPackageStartupMessages({
  library(readabs)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(zoo)
  library(ggplot2)
  library(KFAS)
  library(urca)
  library(broom)
})

options(stringsAsFactors = FALSE, scipen = 999)

project_root <- normalizePath("C:/Users/david/Documents/ConsModelling", winslash = "/", mustWork = TRUE)
raw_dir <- file.path(project_root, "data_raw")
use_cci <- Sys.getenv("USE_CCI", unset = "1") != "0"
output_dir <- file.path(project_root, if (use_cci) "outputs" else "outputs_no_cci")

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

Sys.setenv(R_READABS_PATH = raw_dir)
source(file.path(project_root, "R", "model_helpers.R"), local = TRUE)


# ------------------------------------------------------------------------------
# Download the exact ABS series used in the model
# ------------------------------------------------------------------------------

abs_series_catalog <- tribble(
  ~object_name, ~source_group, ~series_id, ~series_name, ~series_type,
  "consumption_real", "hfce", "A2303280V", "FINAL CONSUMPTION EXPENDITURE: Chain volume measures ;", "Seasonally Adjusted",
  "consumption_nominal", "hfce", "A2302236T", "FINAL CONSUMPTION EXPENDITURE: Current prices ;", "Seasonally Adjusted",
  "gross_disposable_income", "hh_income", "A2302939L", "GROSS DISPOSABLE INCOME ;", "Seasonally Adjusted",
  "final_consumption_hh", "hh_income", "A2304037L", "Final consumption expenditure ;", "Seasonally Adjusted",
  "currency_deposits", "hh_balance", "A83722680X", "Financial assets - Currency and deposits ;", "Original",
  "equities", "hh_balance", "A83722669K", "Financial assets - Shares and other equity ;", "Original",
  "superannuation", "hh_balance", "A83728303A", "Financial assets - Insurance technical reserves - Superannuation ;", "Original",
  "household_debt", "hh_balance", "A83722644R", "Liabilities - Loans and placements ;", "Original",
  "housing_wealth", "hh_balance", "A83728305F", "Residential land and dwellings ;", "Original",
  "housing_loan_flow", "housing_credit", "A130268891F", "Households ; Housing Finance ; Total dwellings excluding refinancing ; New loan commitments ; Value ;", "Seasonally Adjusted",
  "house_price_index", "house_prices", "A83728455L", "Residential Property Price Index ; Weighted average of eight capital cities ;", "Original",
  "unemployment_rate", "labour_force", "A84423050A", "Unemployment rate ; Persons ;", "Seasonally Adjusted",
  "working_age_population", "labour_force", "A84423091W", "Civilian population aged 15 years and over ; Persons ;", "Original"
)


# ------------------------------------------------------------------------------
# Read and prepare raw time-series data
# ------------------------------------------------------------------------------

legacy_house_prices_raw <- read_legacy_house_price_series(file.path(raw_dir, "houseprice_old.csv"))

series_store <- read_abs_series_catalog(abs_series_catalog, path = raw_dir)

# Consumption volume and nominal consumption allow us to construct a consistent
# implicit household consumption deflator.
consumption_real <- series_store[["consumption_real"]] %>%
  transmute(date, consumption_real = value)

consumption_nominal <- series_store[["consumption_nominal"]] %>%
  transmute(date, consumption_nominal = value)

consumption_deflator <- consumption_real %>%
  inner_join(consumption_nominal, by = "date") %>%
  mutate(consumption_deflator = 100 * consumption_nominal / consumption_real) %>%
  select(date, consumption_deflator)

gross_disposable_income <- series_store[["gross_disposable_income"]] %>%
  transmute(date, gross_disposable_income = value)

final_consumption_hh <- series_store[["final_consumption_hh"]] %>%
  transmute(date, final_consumption_hh = value)

# Household balance sheet decomposition.
currency_deposits <- series_store[["currency_deposits"]] %>%
  transmute(date, currency_deposits = value)

equities <- series_store[["equities"]] %>%
  transmute(date, equities = value)

superannuation <- series_store[["superannuation"]] %>%
  transmute(date, superannuation = value)

household_debt <- series_store[["household_debt"]] %>%
  transmute(date, household_debt = value)

housing_wealth <- series_store[["housing_wealth"]] %>%
  transmute(date, housing_wealth = value)

# Credit conditions indicators.
housing_loan_flow <- series_store[["housing_loan_flow"]] %>%
  transmute(date, housing_loan_flow = value)

national_house_prices <- series_store[["house_price_index"]] %>%
  transmute(date, house_price_index = value)

spliced_house_prices <- splice_house_price_series(
  current_series = national_house_prices,
  legacy_series = legacy_house_prices_raw
) %>%
  rename(house_price_index = house_price_spliced)

unemployment_rate <- series_store[["unemployment_rate"]] %>%
  transmute(date, unemployment_rate = value)

working_age_population <- series_store[["working_age_population"]] %>%
  transmute(date, working_age_population = value * 1000)


# ------------------------------------------------------------------------------
# Construct the modelling dataset
# ------------------------------------------------------------------------------

model_data <- consumption_real %>%
  inner_join(consumption_nominal, by = "date") %>%
  inner_join(consumption_deflator, by = "date") %>%
  inner_join(gross_disposable_income, by = "date") %>%
  inner_join(final_consumption_hh, by = "date") %>%
  inner_join(currency_deposits, by = "date") %>%
  inner_join(equities, by = "date") %>%
  inner_join(superannuation, by = "date") %>%
  inner_join(household_debt, by = "date") %>%
  inner_join(housing_wealth, by = "date") %>%
  left_join(housing_loan_flow, by = "date") %>%
  inner_join(spliced_house_prices, by = "date") %>%
  inner_join(working_age_population, by = "date") %>%
  inner_join(unemployment_rate, by = "date") %>%
  arrange(date) %>%
  mutate(
    # ABS household income flows are reported in $ millions, while the
    # household balance-sheet stocks are reported in $ billions. Convert the
    # flow measures to $ billions before constructing wealth-income and
    # debt-income ratios.
    gross_disposable_income_billions = gross_disposable_income / 1000,
    final_consumption_hh_billions = final_consumption_hh / 1000,
    illiquid_financial_wealth = equities + superannuation,
    liquid_assets = currency_deposits,
    net_liquid_assets = currency_deposits - household_debt,
    net_financial_wealth = currency_deposits + illiquid_financial_wealth - household_debt,
    net_worth = currency_deposits + illiquid_financial_wealth + housing_wealth - household_debt,
    final_consumption_hh_sa = final_consumption_hh,
    annualised_income = gross_disposable_income_billions * 4,
    # The ABS household balance sheet is current price. Deflating by the
    # household consumption deflator is an approximation, but it keeps the
    # balance-sheet variables on a household spending price basis.
    real_gdi = gross_disposable_income / consumption_deflator * 100,
    real_consumption_per_working_age = consumption_real / working_age_population,
    real_gdi_per_working_age = real_gdi / working_age_population,
    real_housing_wealth = housing_wealth / consumption_deflator * 100,
    real_illiquid_financial_wealth = illiquid_financial_wealth / consumption_deflator * 100,
    real_household_debt = household_debt / consumption_deflator * 100,
    consumption_income_ratio = final_consumption_hh_billions / gross_disposable_income_billions,
    log_net_worth_income_ratio = safe_log(net_worth / annualised_income),
    net_worth_income_ratio = net_worth / annualised_income,
    net_financial_wealth_income_ratio = net_financial_wealth / annualised_income,
    housing_wealth_income_ratio = housing_wealth / annualised_income,
    illiquid_financial_income_ratio = illiquid_financial_wealth / annualised_income,
    liquid_assets_income_ratio = liquid_assets / annualised_income,
    net_liquid_assets_income_ratio = net_liquid_assets / annualised_income,
    debt_income_ratio = household_debt / annualised_income,
    house_price_income_log_ratio = safe_log(house_price_index) - safe_log(annualised_income / working_age_population),
    lcons = safe_log(real_consumption_per_working_age),
    lgdi = safe_log(real_gdi_per_working_age)
  )

permanent_log_income <- adaptive_permanent_income_log(model_data$lgdi, lambda = 0.97)

model_data <- model_data %>%
  mutate(
    lperm_income = permanent_log_income,
    permanent_income_per_working_age = exp(lperm_income),
    disposable_permanent_income_ratio = real_gdi_per_working_age / permanent_income_per_working_age,
    log_disposable_permanent_income_ratio = lgdi - lperm_income,
    lcons_perm_income_ratio = lcons - lperm_income
  )

gfc_reference_value <- model_data %>%
  filter(date == as.Date("2008-06-01")) %>%
  pull(log_disposable_permanent_income_ratio)

if (length(gfc_reference_value) == 0L || is.na(gfc_reference_value[1])) {
  gfc_reference_value <- model_data %>%
    filter(date < as.Date("2008-09-01")) %>%
    slice_tail(n = 1L) %>%
    pull(log_disposable_permanent_income_ratio)
}

model_data <- model_data %>%
  mutate(
    gfc_shift = if_else(date >= as.Date("2008-09-01"), 1, 0),
    gfc_income_shift_interaction = gfc_shift * (log_disposable_permanent_income_ratio - gfc_reference_value[1])
  )


# ------------------------------------------------------------------------------
# Latent credit conditions index via state-space system
# ------------------------------------------------------------------------------

if (use_cci) {
  # In the Muellbauer tradition, measured borrowing conditions are not fully
  # observed. We therefore summarise several indicators into a single latent
  # factor. Signs are chosen so that a higher index means easier credit:
  # - more loan commitments -> easier credit
  # - faster house prices -> easier credit / more collateral
  # - lower debt-income ratio -> easier credit, after leverage has built up
  credit_panel <- model_data %>%
    transmute(
      date,
      loan_flow_z = standardise(safe_log(housing_loan_flow)),
      house_price_growth_z = standardise(lead_lag_diff(safe_log(house_price_index), 4L)),
      leverage_z = standardise(-debt_income_ratio)
    )

  y_matrix <- as.matrix(credit_panel %>% select(loan_flow_z, house_price_growth_z, leverage_z))

  credit_fit <- fitSSM(
    inits = c(rep(log(0.25), 3L), log(0.10), 0.5, 0.5),
    model = build_credit_ssm(y_matrix, log_h = rep(log(0.25), 3L), log_q = log(0.10), lambda_2 = 0.5, lambda_3 = 0.5),
    updatefn = function(pars, model) {
      build_credit_ssm(
        y_matrix = y_matrix,
        log_h = pars[1:3],
        log_q = pars[4],
        lambda_2 = pars[5],
        lambda_3 = pars[6]
      )
    },
    method = "BFGS"
  )

  credit_model <- build_credit_ssm(
    y_matrix = y_matrix,
    log_h = credit_fit$optim.out$par[1:3],
    log_q = credit_fit$optim.out$par[4],
    lambda_2 = credit_fit$optim.out$par[5],
    lambda_3 = credit_fit$optim.out$par[6]
  )

  credit_panel$credit_conditions_index <- standardise(extract_state(credit_fit, credit_model))

  model_data <- model_data %>%
    left_join(credit_panel %>% select(date, credit_conditions_index), by = "date")
} else {
  credit_panel <- model_data %>%
    transmute(date, credit_conditions_index = NA_real_)

  model_data <- model_data %>%
    mutate(credit_conditions_index = NA_real_)
}


# ------------------------------------------------------------------------------
# Long-run consumption equation
# ------------------------------------------------------------------------------

# Candidate Muellbauer-style long-run specifications. The benchmark preserves
# the original net-liquid-assets treatment, while the alternatives separate
# liquid assets and debt explicitly.
if (use_cci) {
  long_run_specs <- list(
    net_liquid_assets = c(
      "log_disposable_permanent_income_ratio",
      "housing_wealth_income_ratio",
      "illiquid_financial_income_ratio",
      "net_liquid_assets_income_ratio",
      "unemployment_rate",
      "credit_conditions_index"
    ),
    separate_liquid_debt = c(
      "log_disposable_permanent_income_ratio",
      "housing_wealth_income_ratio",
      "illiquid_financial_income_ratio",
      "liquid_assets_income_ratio",
      "debt_income_ratio",
      "unemployment_rate",
      "credit_conditions_index"
    ),
    separate_liquid_debt_no_credit = c(
      "log_disposable_permanent_income_ratio",
      "housing_wealth_income_ratio",
      "illiquid_financial_income_ratio",
      "liquid_assets_income_ratio",
      "debt_income_ratio",
      "unemployment_rate"
    ),
    separate_liquid_debt_no_uncertainty = c(
      "log_disposable_permanent_income_ratio",
      "housing_wealth_income_ratio",
      "illiquid_financial_income_ratio",
      "liquid_assets_income_ratio",
      "debt_income_ratio",
      "credit_conditions_index"
    )
  )
} else {
  long_run_specs <- list(
    net_liquid_assets_no_cci = c(
      "log_disposable_permanent_income_ratio",
      "housing_wealth_income_ratio",
      "illiquid_financial_income_ratio",
      "net_liquid_assets_income_ratio",
      "unemployment_rate"
    ),
    separate_liquid_debt_no_cci = c(
      "log_disposable_permanent_income_ratio",
      "housing_wealth_income_ratio",
      "illiquid_financial_income_ratio",
      "liquid_assets_income_ratio",
      "debt_income_ratio",
      "unemployment_rate"
    ),
    liquid_debt_only_no_cci = c(
      "log_disposable_permanent_income_ratio",
      "liquid_assets_income_ratio",
      "debt_income_ratio",
      "unemployment_rate"
    )
  )
}

long_run_candidates <- lapply(names(long_run_specs), function(spec_name) {
  fit_long_run_spec(
    model_data,
    spec_name,
    long_run_specs[[spec_name]],
    response_var = "lcons_perm_income_ratio"
  )
})
names(long_run_candidates) <- names(long_run_specs)

long_run_comparison <- bind_rows(lapply(long_run_candidates, `[[`, "diagnostics")) %>%
  arrange(bic, aic)

best_long_run_name <- long_run_comparison %>%
  slice(1L) %>%
  pull(specification)

best_long_run <- long_run_candidates[[best_long_run_name]]
long_run_fit <- best_long_run$fit
long_run_rhs_vars <- best_long_run$rhs_vars
long_run_sample <- best_long_run$sample %>%
  mutate(ecm_residual = resid(long_run_fit))

ecm_adf <- ur.df(long_run_sample$ecm_residual, type = "drift", lags = 4L)


# ------------------------------------------------------------------------------
# Short-run error-correction model
# ------------------------------------------------------------------------------

ecm_data <- long_run_sample %>%
  mutate(
    d_lcons = lead_lag_diff(lcons, 1L),
    d_lgdi = lead_lag_diff(lgdi, 1L),
    d_log_y_yp = lead_lag_diff(log_disposable_permanent_income_ratio, 1L),
    d_hw_ratio = lead_lag_diff(housing_wealth_income_ratio, 1L),
    d_ifw_ratio = lead_lag_diff(illiquid_financial_income_ratio, 1L),
    d_liquid_assets_ratio = lead_lag_diff(liquid_assets_income_ratio, 1L),
    d_debt_ratio = lead_lag_diff(debt_income_ratio, 1L),
    d_nla_ratio = lead_lag_diff(net_liquid_assets_income_ratio, 1L),
    d_u = lead_lag_diff(unemployment_rate, 1L),
    d_cci = lead_lag_diff(credit_conditions_index, 1L),
    d_house_prices = lead_lag_diff(safe_log(house_price_index), 1L),
    ecm_lag = lag(ecm_residual, 1L),
    d_lcons_lag = lag(d_lcons, 1L),
    d_lgdi_lag = lag(d_lgdi, 1L)
  ) %>%
  filter(
    complete.cases(
      d_lcons, d_lgdi, d_log_y_yp, d_hw_ratio, d_ifw_ratio,
      d_liquid_assets_ratio, d_debt_ratio, d_nla_ratio,
      d_u, d_house_prices, ecm_lag, d_lcons_lag, d_lgdi_lag
    )
  )

ecm_term_map <- c(
  log_disposable_permanent_income_ratio = "d_log_y_yp",
  housing_wealth_income_ratio = "d_hw_ratio",
  illiquid_financial_income_ratio = "d_ifw_ratio",
  liquid_assets_income_ratio = "d_liquid_assets_ratio",
  debt_income_ratio = "d_debt_ratio",
  net_liquid_assets_income_ratio = "d_nla_ratio",
  unemployment_rate = "d_u",
  credit_conditions_index = "d_cci"
)

selected_ecm_terms <- unname(ecm_term_map[long_run_rhs_vars])
ecm_formula <- reformulate(
  c(
    "ecm_lag",
    "d_lgdi",
    "d_lgdi_lag",
    "d_lcons_lag",
    selected_ecm_terms,
    "d_house_prices"
  ),
  response = "d_lcons"
)

ecm_fit <- lm(ecm_formula, data = ecm_data)


# ------------------------------------------------------------------------------
# Fitted values and diagnostics
# ------------------------------------------------------------------------------

model_data <- model_data %>%
  left_join(
    long_run_sample %>%
      transmute(date, long_run_fitted_ratio = fitted(long_run_fit), ecm_residual = ecm_residual),
    by = "date"
  ) %>%
  left_join(
    ecm_data %>%
      transmute(date, ecm_fitted = fitted(ecm_fit), d_lcons_actual = d_lcons),
    by = "date"
  )

long_run_r2 <- summary(long_run_fit)$r.squared
ecm_r2 <- summary(ecm_fit)$r.squared
speed_of_adjustment <- coef(ecm_fit)[["ecm_lag"]]
income_short_run <- coef(ecm_fit)[["d_lgdi"]]
average_consumption_to_annual_income <- mean(model_data$final_consumption_hh / model_data$annualised_income, na.rm = TRUE)
housing_mpc_approx <- if ("housing_wealth_income_ratio" %in% names(coef(long_run_fit))) {
  coef(long_run_fit)[["housing_wealth_income_ratio"]] * average_consumption_to_annual_income
} else {
  NA_real_
}
illiquid_financial_mpc_approx <- if ("illiquid_financial_income_ratio" %in% names(coef(long_run_fit))) {
  coef(long_run_fit)[["illiquid_financial_income_ratio"]] * average_consumption_to_annual_income
} else {
  NA_real_
}
liquid_assets_mpc_approx <- if ("liquid_assets_income_ratio" %in% names(coef(long_run_fit))) {
  coef(long_run_fit)[["liquid_assets_income_ratio"]] * average_consumption_to_annual_income
} else {
  NA_real_
}
debt_mpc_approx <- if ("debt_income_ratio" %in% names(coef(long_run_fit))) {
  coef(long_run_fit)[["debt_income_ratio"]] * average_consumption_to_annual_income
} else {
  NA_real_
}


# ------------------------------------------------------------------------------
# Italy-paper style build-up of the specification
# ------------------------------------------------------------------------------

italy_ecm_base <- model_data %>%
  select(-any_of(c("ecm_residual", "long_run_fitted_ratio", "ecm_fitted", "d_lcons_actual"))) %>%
  mutate(
    d_lcons = lead_lag_diff(lcons, 1L),
    d_lgdi = lead_lag_diff(lgdi, 1L),
    d_log_y_yp = lead_lag_diff(log_disposable_permanent_income_ratio, 1L),
    d_log_net_worth_income_ratio = lead_lag_diff(log_net_worth_income_ratio, 1L),
    d_net_worth_income_ratio = lead_lag_diff(net_worth_income_ratio, 1L),
    d_net_financial_wealth_income_ratio = lead_lag_diff(net_financial_wealth_income_ratio, 1L),
    d_hw_ratio = lead_lag_diff(housing_wealth_income_ratio, 1L),
    d_ifw_ratio = lead_lag_diff(illiquid_financial_income_ratio, 1L),
    d_liquid_assets_ratio = lead_lag_diff(liquid_assets_income_ratio, 1L),
    d_debt_ratio = lead_lag_diff(debt_income_ratio, 1L),
    d_nla_ratio = lead_lag_diff(net_liquid_assets_income_ratio, 1L),
    d_house_price_income_log_ratio = lead_lag_diff(house_price_income_log_ratio, 1L),
    d_gfc_income_shift_interaction = lead_lag_diff(gfc_income_shift_interaction, 1L),
    d_u = lead_lag_diff(unemployment_rate, 1L),
    d_cci = lead_lag_diff(credit_conditions_index, 1L),
    d_house_prices = lead_lag_diff(safe_log(house_price_index), 1L),
    d_lcons_lag = lag(d_lcons, 1L),
    d_lgdi_lag = lag(d_lgdi, 1L)
  )

italy_term_map <- c(
  log_disposable_permanent_income_ratio = "d_log_y_yp",
  log_net_worth_income_ratio = "d_log_net_worth_income_ratio",
  net_worth_income_ratio = "d_net_worth_income_ratio",
  net_financial_wealth_income_ratio = "d_net_financial_wealth_income_ratio",
  housing_wealth_income_ratio = "d_hw_ratio",
  illiquid_financial_income_ratio = "d_ifw_ratio",
  liquid_assets_income_ratio = "d_liquid_assets_ratio",
  debt_income_ratio = "d_debt_ratio",
  net_liquid_assets_income_ratio = "d_nla_ratio",
  house_price_income_log_ratio = "d_house_price_income_log_ratio",
  gfc_income_shift_interaction = "d_gfc_income_shift_interaction",
  unemployment_rate = "d_u",
  credit_conditions_index = "d_cci"
)

fit_italy_sequence_spec <- function(data, ecm_base, spec_name, rhs_vars, extra_short_run = character()) {
  long_run <- fit_long_run_spec(
    data = data,
    spec_name = spec_name,
    rhs_vars = rhs_vars,
    response_var = "lcons_perm_income_ratio"
  )

  long_run_sample <- long_run$sample %>%
    mutate(ecm_residual = resid(long_run$fit))

  ecm_data_local <- ecm_base %>%
    inner_join(long_run_sample %>% select(date, ecm_residual), by = "date") %>%
    mutate(ecm_lag = lag(ecm_residual, 1L))

  selected_terms <- unname(italy_term_map[rhs_vars])
  ecm_terms <- unique(c(
    "ecm_lag",
    "d_lgdi",
    "d_lgdi_lag",
    "d_lcons_lag",
    selected_terms,
    extra_short_run,
    "d_u",
    "d_house_prices"
  ))

  ecm_data_local <- ecm_data_local %>%
    filter(complete.cases(across(all_of(c("d_lcons", ecm_terms)))))

  ecm_formula_local <- reformulate(ecm_terms, response = "d_lcons")
  ecm_fit_local <- lm(ecm_formula_local, data = ecm_data_local)
  ecm_summary <- summary(ecm_fit_local)

  diagnostics <- long_run$diagnostics %>%
    mutate(
      ecm_adj_r2 = ecm_summary$adj.r.squared,
      ecm_sigma = ecm_summary$sigma,
      ecm_n_obs = nobs(ecm_fit_local),
      speed_of_adjustment = if ("ecm_lag" %in% names(coef(ecm_fit_local))) coef(ecm_fit_local)[["ecm_lag"]] else NA_real_
    )

  list(
    spec_name = spec_name,
    rhs_vars = rhs_vars,
    extra_short_run = extra_short_run,
    long_run_fit = long_run$fit,
    long_run_sample = long_run_sample,
    ecm_fit = ecm_fit_local,
    ecm_data = ecm_data_local,
    diagnostics = diagnostics
  )
}

italy_sequence_specs <- list(
  step1_textbook_log_net_worth = list(
    rhs_vars = c("log_disposable_permanent_income_ratio", "log_net_worth_income_ratio"),
    extra_short_run = character()
  ),
  step2_add_short_run_cci = list(
    rhs_vars = c("log_disposable_permanent_income_ratio", "log_net_worth_income_ratio"),
    extra_short_run = if (use_cci) c("d_cci") else character()
  ),
  step3_level_net_worth = list(
    rhs_vars = c("log_disposable_permanent_income_ratio", "net_worth_income_ratio"),
    extra_short_run = if (use_cci) c("d_cci") else character()
  ),
  step4_split_housing_net_financial = list(
    rhs_vars = c(
      "log_disposable_permanent_income_ratio",
      "net_financial_wealth_income_ratio",
      "housing_wealth_income_ratio",
      "house_price_income_log_ratio"
    ),
    extra_short_run = if (use_cci) c("d_cci") else character()
  ),
  step5_split_liquid_illiquid_debt = list(
    rhs_vars = c(
      "log_disposable_permanent_income_ratio",
      "housing_wealth_income_ratio",
      "house_price_income_log_ratio",
      "liquid_assets_income_ratio",
      "illiquid_financial_income_ratio",
      "debt_income_ratio"
    ),
    extra_short_run = if (use_cci) c("d_cci") else character()
  ),
  step6_post_gfc_shift = list(
    rhs_vars = c(
      "log_disposable_permanent_income_ratio",
      "gfc_income_shift_interaction",
      "housing_wealth_income_ratio",
      "house_price_income_log_ratio",
      "liquid_assets_income_ratio",
      "illiquid_financial_income_ratio",
      "debt_income_ratio"
    ),
    extra_short_run = if (use_cci) c("d_cci") else character()
  )
)

italy_sequence_results <- lapply(names(italy_sequence_specs), function(spec_name) {
  spec <- italy_sequence_specs[[spec_name]]
  fit_italy_sequence_spec(
    data = model_data,
    ecm_base = italy_ecm_base,
    spec_name = spec_name,
    rhs_vars = spec$rhs_vars,
    extra_short_run = spec$extra_short_run
  )
})
names(italy_sequence_results) <- names(italy_sequence_specs)

italy_sequence_comparison <- bind_rows(lapply(italy_sequence_results, `[[`, "diagnostics")) %>%
  mutate(step_order = seq_len(n())) %>%
  select(
    specification, step_order, n_obs, aic, bic, adj_r2, sigma,
    adf_stat, adf_1pct, adf_5pct, adf_10pct, adf_reject_5pct,
    ecm_n_obs, ecm_adj_r2, ecm_sigma, speed_of_adjustment
  )

italy_final <- italy_sequence_results[["step6_post_gfc_shift"]]


# ------------------------------------------------------------------------------
# Save data and model tables
# ------------------------------------------------------------------------------

write.csv(model_data, file.path(output_dir, "model_dataset.csv"), row.names = FALSE)
write.csv(tidy(long_run_fit), file.path(output_dir, "long_run_coefficients.csv"), row.names = FALSE)
write.csv(tidy(ecm_fit), file.path(output_dir, "ecm_coefficients.csv"), row.names = FALSE)
write.csv(long_run_comparison, file.path(output_dir, "long_run_model_comparison.csv"), row.names = FALSE)
write.csv(italy_sequence_comparison, file.path(output_dir, "italy_sequence_model_comparison.csv"), row.names = FALSE)
write.csv(
  bind_rows(lapply(names(italy_sequence_results), function(spec_name) {
    tidy(italy_sequence_results[[spec_name]]$long_run_fit) %>%
      mutate(specification = spec_name, equation = "long_run")
  })) %>%
    bind_rows(
      bind_rows(lapply(names(italy_sequence_results), function(spec_name) {
        tidy(italy_sequence_results[[spec_name]]$ecm_fit) %>%
          mutate(specification = spec_name, equation = "ecm")
      }))
    ),
  file.path(output_dir, "italy_sequence_coefficients.csv"),
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# Charts
# ------------------------------------------------------------------------------

plot_credit <- ggplot(model_data, aes(x = date, y = credit_conditions_index)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey60") +
  geom_line(linewidth = 0.7, colour = "#0B5C8C") +
  labs(
    title = if (use_cci) "Latent credit conditions index" else "Credit conditions index disabled",
    subtitle = if (use_cci) "Higher values indicate easier household credit conditions" else "No CCI estimated in this specification",
    x = NULL,
    y = "Standardised factor"
  ) +
  theme_minimal(base_size = 11)

plot_ratio_fit <- model_data %>%
  filter(!is.na(long_run_fitted_ratio)) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = lcons_perm_income_ratio, colour = "Actual")) +
  geom_line(aes(y = long_run_fitted_ratio, colour = "Long-run fit"), linetype = "dashed") +
  scale_colour_manual(values = c("Actual" = "#111111", "Long-run fit" = "#C84C09")) +
  labs(
    title = "Long-run consumption to permanent-income ratio fit",
    x = NULL,
    y = "log(C / YP)",
    colour = NULL
  ) +
  theme_minimal(base_size = 11)

plot_consumption_growth <- ecm_data %>%
  transmute(date, actual = d_lcons, fitted = fitted(ecm_fit)) %>%
  pivot_longer(cols = c(actual, fitted), names_to = "series", values_to = "value") %>%
  ggplot(aes(x = date, y = value, colour = series)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = c("actual" = "#111111", "fitted" = "#2F8F2F")) +
  labs(
    title = "Short-run consumption growth equation",
    x = NULL,
    y = "Quarterly change in log real consumption",
    colour = NULL
  ) +
  theme_minimal(base_size = 11)

ggsave(file.path(output_dir, "credit_conditions_index.png"), plot_credit, width = 9, height = 5, dpi = 150)
ggsave(file.path(output_dir, "long_run_fit.png"), plot_ratio_fit, width = 9, height = 5, dpi = 150)
ggsave(file.path(output_dir, "ecm_fit.png"), plot_consumption_growth, width = 9, height = 5, dpi = 150)


# ------------------------------------------------------------------------------
# Human-readable summary file
# ------------------------------------------------------------------------------

summary_file <- file.path(output_dir, "model_summary.txt")
con <- file(summary_file, open = "wt")

writeLines("Australian household consumption model summary", con = con)
writeLines(paste("Run time:", Sys.time()), con = con)
writeLines(paste("Sample start:", min(model_data$date, na.rm = TRUE)), con = con)
writeLines(paste("Sample end:", max(model_data$date, na.rm = TRUE)), con = con)

write_section(con, "Specification")
writeLines(
  c(
    paste("House prices are spliced from houseprice_old.csv into the ABS series; resulting house-price sample starts:", min(spliced_house_prices$date, na.rm = TRUE)),
    "Real consumption and income are scaled by civilian population aged 15 years and over.",
    "Permanent income is constructed with a one-sided adaptive filter on log real disposable income per working-age person.",
    paste("Preferred long-run specification:", best_long_run_name),
    paste("Long-run RHS variables:", paste(long_run_rhs_vars, collapse = ", ")),
    "Short-run equation: error-correction model for quarterly log real consumption growth using the differenced terms implied by the selected long-run specification."
  ),
  con = con
)

write_section(con, "Long-run model comparison")
writeLines(capture.output(print(long_run_comparison)), con = con)

write_section(con, "Long-run fit")
writeLines(capture.output(print(summary(long_run_fit))), con = con)

write_section(con, "ADF test on error-correction residual")
writeLines(capture.output(show(ecm_adf)), con = con)

write_section(con, "Short-run ECM")
writeLines(capture.output(print(summary(ecm_fit))), con = con)

write_section(con, "Headline diagnostics")
writeLines(
  c(
    paste("Long-run R-squared:", round(long_run_r2, 4)),
    paste("Short-run R-squared:", round(ecm_r2, 4)),
    paste("Speed of adjustment (ECM coefficient):", round(speed_of_adjustment, 4)),
    paste("Short-run income elasticity term on dlog income:", round(income_short_run, 4)),
    paste("Approximate housing wealth MPC (dollars per dollar):", round(housing_mpc_approx, 4)),
    paste("Approximate illiquid financial wealth MPC (dollars per dollar):", round(illiquid_financial_mpc_approx, 4)),
    paste("Approximate liquid assets MPC (dollars per dollar):", round(liquid_assets_mpc_approx, 4)),
    paste("Approximate debt MPC (dollars per dollar):", round(debt_mpc_approx, 4))
  ),
  con = con
)

close(con)

italy_summary_file <- file.path(output_dir, "italy_sequence_summary.txt")
con_italy <- file(italy_summary_file, open = "wt")

writeLines("Italy-paper style specification build-up for Australia", con = con_italy)
writeLines(paste("Run time:", Sys.time()), con = con_italy)
writeLines(paste("Sample start:", min(model_data$date, na.rm = TRUE)), con = con_italy)
writeLines(paste("Sample end:", max(model_data$date, na.rm = TRUE)), con = con_italy)

write_section(con_italy, "Sequence")
writeLines(
  c(
    "Step 1: textbook-style model with log net worth to income.",
    "Step 2: add short-run change in credit conditions.",
    "Step 3: replace log net worth ratio with level net worth ratio.",
    "Step 4: split housing wealth from net financial wealth and add housing affordability.",
    "Step 5: split financial wealth into liquid assets, illiquid financial assets and debt.",
    "Step 6: allow a post-2008Q3 shift in the permanent-vs-current income weight."
  ),
  con = con_italy
)

write_section(con_italy, "Model Comparison")
writeLines(capture.output(print(italy_sequence_comparison)), con = con_italy)

write_section(con_italy, "Final Long-run Fit")
writeLines(capture.output(print(summary(italy_final$long_run_fit))), con = con_italy)

write_section(con_italy, "Final Short-run ECM")
writeLines(capture.output(print(summary(italy_final$ecm_fit))), con = con_italy)

close(con_italy)


# ------------------------------------------------------------------------------
# Console output
# ------------------------------------------------------------------------------

cat("\nModel completed successfully.\n")
cat("Outputs written to:\n")
cat(" - ", file.path(output_dir, "model_dataset.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "long_run_coefficients.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "ecm_coefficients.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "long_run_model_comparison.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "italy_sequence_model_comparison.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "italy_sequence_coefficients.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "italy_sequence_summary.txt"), "\n", sep = "")
cat(" - ", file.path(output_dir, "model_summary.txt"), "\n", sep = "")
cat(" - ", file.path(output_dir, "credit_conditions_index.png"), "\n", sep = "")
cat(" - ", file.path(output_dir, "long_run_fit.png"), "\n", sep = "")
cat(" - ", file.path(output_dir, "ecm_fit.png"), "\n", sep = "")
