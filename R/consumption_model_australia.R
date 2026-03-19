#!/usr/bin/env Rscript

# Australian household consumption model in a Muellbauer-style framework.
# ------------------------------------------------------------------------------
# This script is designed to be a practical, reproducible starting point for an
# integrated household consumption system for Australia using public data.
#
# Design choices:
# - Use ABS workbooks downloaded with readabs.
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

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
raw_dir <- file.path(project_root, "data_raw")
output_dir <- file.path(project_root, "outputs")

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

Sys.setenv(R_READABS_PATH = raw_dir)
source(file.path(project_root, "R", "model_helpers.R"), local = TRUE)


# ------------------------------------------------------------------------------
# Download the specific public ABS workbooks used in the system
# ------------------------------------------------------------------------------

workbooks <- tribble(
  ~catalogue, ~file_name, ~tag,
  "australian-national-accounts-national-income-expenditure-and-product", "5206008_Household_Final_Consumption_Expenditure.xlsx", "hfce",
  "australian-national-accounts-finance-and-wealth", "5232035.xlsx", "hh_balance_sheet",
  "australian-national-accounts-finance-and-wealth", "5232036.xlsx", "hh_income_wealth",
  "lending-indicators", "560101.xlsx", "housing_credit_flow",
  "residential-property-price-indexes-eight-capital-cities", "641601.xlsx", "house_prices",
  "labour-force-australia", "6202001.xlsx", "labour_force"
)

for (i in seq_len(nrow(workbooks))) {
  download_abs_workbook(raw_dir, workbooks$catalogue[[i]], workbooks$file_name[[i]])
}

hh_income_account_path <- download_file_if_missing(
  url = "https://www.abs.gov.au/statistics/economy/national-accounts/australian-national-accounts-national-income-expenditure-and-product/dec-2025/5206020_Household_Income.xlsx",
  target_path = file.path(raw_dir, "5206020_Household_Income.xlsx")
)

population_age_path <- download_file_if_missing(
  url = "https://www.abs.gov.au/statistics/people/population/national-state-and-territory-population/sep-2025/3101059.xlsx",
  target_path = file.path(raw_dir, "3101059.xlsx")
)


# ------------------------------------------------------------------------------
# Read and prepare raw time-series data
# ------------------------------------------------------------------------------

hfce_raw <- read_abs_ts_workbook(file.path(raw_dir, "5206008_Household_Final_Consumption_Expenditure.xlsx"))
hh_bs_raw <- read_abs_ts_workbook(file.path(raw_dir, "5232035.xlsx"))
hh_inc_raw <- read_abs_ts_workbook(file.path(raw_dir, "5232036.xlsx"))
credit_raw <- read_abs_ts_workbook(file.path(raw_dir, "560101.xlsx"))
house_prices_raw <- read_abs_ts_workbook(file.path(raw_dir, "641601.xlsx"))
labour_raw <- read_abs_ts_workbook(file.path(raw_dir, "6202001.xlsx"))
hh_income_account_raw <- read_abs_ts_workbook(hh_income_account_path)
population_age_raw <- read_abs_ts_workbook(population_age_path)
legacy_house_prices_raw <- read_legacy_house_price_series(file.path(raw_dir, "houseprice_old.csv"))

# Consumption volume and nominal consumption from the same workbook allow us to
# construct a consistent implicit consumption deflator.
consumption_real <- pick_preferred_series(
  hfce_raw,
  "^FINAL CONSUMPTION EXPENDITURE: Chain volume measures"
) %>%
  transmute(date, consumption_real = value)

consumption_nominal <- pick_preferred_series(
  hfce_raw,
  "^FINAL CONSUMPTION EXPENDITURE: Current prices"
) %>%
  transmute(date, consumption_nominal = value)

consumption_deflator <- consumption_real %>%
  inner_join(consumption_nominal, by = "date") %>%
  mutate(consumption_deflator = 100 * consumption_nominal / consumption_real) %>%
  select(date, consumption_deflator)

# Household income / consumption / net worth table.
# The provided series id A2302929L does not correspond to gross disposable
# income in the current Table 20 workbook. Use it if present, otherwise fall
# back to the actual seasonally adjusted GDI series in the workbook.
gdi_series_id <- if ("A2302929L" %in% hh_income_account_raw$series_id) {
  "A2302929L"
} else {
  "A2302939L"
}

gross_disposable_income <- pick_series_by_id(
  hh_income_account_raw,
  gdi_series_id
) %>%
  transmute(date, gross_disposable_income = value)

final_consumption_series_id <- if ("A2304037L" %in% hh_income_account_raw$series_id) {
  "A2304037L"
} else {
  pick_preferred_series(
    hh_income_account_raw,
    "^Final consumption expenditure ;$",
    preferred_types = c("Seasonally Adjusted", "Seasonally adjusted", "Trend", "Original")
  ) %>%
    distinct(series_id) %>%
    slice(1L) %>%
    pull(series_id)
}

final_consumption_hh <- pick_series_by_id(
  hh_income_account_raw,
  final_consumption_series_id
) %>%
  transmute(date, final_consumption_hh = value)

mortgage_interest_paid <- pick_preferred_series(
  hh_income_account_raw,
  "^Property income payable - Interest - Dwellings ;$",
  preferred_types = c("Seasonally Adjusted", "Seasonally adjusted", "Trend", "Original")
) %>%
  transmute(date, mortgage_interest_paid = value)

closing_net_worth <- pick_preferred_series(
  hh_inc_raw,
  "^Closing net worth ;$"
) %>%
  transmute(date, closing_net_worth = rescale_to_millions(value, unit))

# Household balance sheet decomposition.
currency_deposits <- pick_preferred_series(
  hh_bs_raw,
  "^Financial assets - Currency and deposits ;$"
) %>%
  transmute(date, currency_deposits = rescale_to_millions(value, unit))

equities <- pick_preferred_series(
  hh_bs_raw,
  "^Financial assets - Shares and other equity ;$"
) %>%
  transmute(date, equities = rescale_to_millions(value, unit))

superannuation <- pick_preferred_series(
  hh_bs_raw,
  "^Financial assets - Insurance technical reserves - Superannuation ;$"
) %>%
  transmute(date, superannuation = rescale_to_millions(value, unit))

household_debt <- pick_preferred_series(
  hh_bs_raw,
  "^Liabilities - Loans and placements ;$"
) %>%
  transmute(date, household_debt = rescale_to_millions(value, unit))

housing_wealth <- pick_preferred_series(
  hh_bs_raw,
  "^Residential land and dwellings ;$"
) %>%
  transmute(date, housing_wealth = rescale_to_millions(value, unit))

# Credit conditions indicators.
housing_loan_flow <- pick_preferred_series(
  credit_raw,
  "^Households ; Housing Finance ; Total dwellings excluding refinancing ; New loan commitments ; Value ;$"
) %>%
  transmute(date, housing_loan_flow = value)

first_home_buyer_loans <- pick_preferred_series(
  credit_raw,
  "^Households ; Housing Finance ; Owner occupier ; First Home Buyers ; New loan commitments ; Number ;$"
) %>%
  transmute(date, first_home_buyer_loans = value)

non_first_home_buyer_loans <- pick_preferred_series(
  credit_raw,
  "^Households ; Housing Finance ; Owner occupier ; Non-First Home Buyers ; New loan commitments ; Number ;$"
) %>%
  transmute(date, non_first_home_buyer_loans = value)

national_house_prices <- pick_preferred_series(
  house_prices_raw,
  "^Residential Property Price Index ; Weighted average of eight capital cities ;$",
  preferred_types = c("Original")
) %>%
  transmute(date, house_price_index = value)

spliced_house_prices <- splice_house_price_series(
  current_series = national_house_prices,
  legacy_series = legacy_house_prices_raw
) %>%
  rename(house_price_index = house_price_spliced)

unemployment_rate <- pick_preferred_series(
  labour_raw,
  "^Unemployment rate ; Persons ;$"
) %>%
  transmute(date, unemployment_rate = value)

working_age_population <- pick_preferred_series(
  labour_raw,
  "^Civilian population aged 15 years and over ; Persons ;$",
  preferred_types = c("Original")
) %>%
  transmute(date, working_age_population = value * 1000)

population_by_age_annual <- population_age_raw %>%
  filter(str_detect(series_name, "^Estimated Resident Population ; Persons ; ")) %>%
  mutate(
    date = to_quarter_start_date(date),
    age_label = str_match(series_name, "^Estimated Resident Population ; Persons ; ([0-9]{1,3}|100 and over) ;$")[, 2],
    age_lower = case_when(
      age_label == "100 and over" ~ 100,
      !is.na(age_label) ~ as.numeric(age_label),
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(age_lower)) %>%
  group_by(date) %>%
  summarise(
    total_population = sum(value, na.rm = TRUE),
    prime_working_age_population = sum(value[age_lower >= 25 & age_lower <= 44], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(prime_working_age_share = prime_working_age_population / total_population)

prime_working_age_share <- interpolate_annual_to_quarterly(
  annual_data = population_by_age_annual,
  quarter_dates = consumption_real$date,
  value_col = "prime_working_age_share"
)


# ------------------------------------------------------------------------------
# Construct the modelling dataset
# ------------------------------------------------------------------------------

model_data <- consumption_real %>%
  inner_join(consumption_nominal, by = "date") %>%
  inner_join(consumption_deflator, by = "date") %>%
  inner_join(gross_disposable_income, by = "date") %>%
  inner_join(final_consumption_hh, by = "date") %>%
  inner_join(mortgage_interest_paid, by = "date") %>%
  inner_join(closing_net_worth, by = "date") %>%
  inner_join(currency_deposits, by = "date") %>%
  inner_join(equities, by = "date") %>%
  inner_join(superannuation, by = "date") %>%
  inner_join(household_debt, by = "date") %>%
  inner_join(housing_wealth, by = "date") %>%
  left_join(housing_loan_flow, by = "date") %>%
  left_join(first_home_buyer_loans, by = "date") %>%
  left_join(non_first_home_buyer_loans, by = "date") %>%
  inner_join(spliced_house_prices, by = "date") %>%
  left_join(prime_working_age_share, by = "date") %>%
  inner_join(working_age_population, by = "date") %>%
  inner_join(unemployment_rate, by = "date") %>%
  arrange(date) %>%
  mutate(
    illiquid_financial_wealth = equities + superannuation,
    liquid_assets = currency_deposits,
    net_liquid_assets = currency_deposits - household_debt,
    final_consumption_hh_sa = final_consumption_hh,
    annualised_income = gross_disposable_income * 4,
    # The ABS household balance sheet is current price. Deflating by the
    # household consumption deflator is an approximation, but it keeps the
    # balance-sheet variables on a household spending price basis.
    real_gdi = gross_disposable_income / consumption_deflator * 100,
    real_consumption_per_working_age = consumption_real / working_age_population,
    real_gdi_per_working_age = real_gdi / working_age_population,
    real_housing_wealth = housing_wealth / consumption_deflator * 100,
    real_illiquid_financial_wealth = illiquid_financial_wealth / consumption_deflator * 100,
    real_household_debt = household_debt / consumption_deflator * 100,
    nominal_consumption_income_ratio = final_consumption_hh_sa / annualised_income,
    housing_wealth_income_ratio = housing_wealth / annualised_income,
    illiquid_financial_income_ratio = illiquid_financial_wealth / annualised_income,
    liquid_assets_income_ratio = liquid_assets / annualised_income,
    net_liquid_assets_income_ratio = net_liquid_assets / annualised_income,
    debt_income_ratio = household_debt / annualised_income,
    mortgage_interest_income_ratio = mortgage_interest_paid / annualised_income,
    mortgage_rate_nominal = 400 * mortgage_interest_paid / lag(household_debt, 1L),
    first_home_buyer_share = first_home_buyer_loans / (first_home_buyer_loans + non_first_home_buyer_loans),
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
    lcons_perm_income_ratio = lcons - lperm_income,
    annual_consumption_inflation = 100 * lead_lag_diff(log(consumption_deflator), 4L),
    real_mortgage_rate = mortgage_rate_nominal - annual_consumption_inflation
  )


# ------------------------------------------------------------------------------
# Latent credit conditions index via state-space system
# ------------------------------------------------------------------------------

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

spline_credit_panel <- model_data %>%
  transmute(
    date,
    loan_flow_z = standardise(safe_log(housing_loan_flow)),
    house_price_growth_z = standardise(lead_lag_diff(safe_log(house_price_index), 4L)),
    leverage_z = standardise(-debt_income_ratio),
    first_home_buyer_share_z = standardise(first_home_buyer_share),
    mortgage_rate_headwind_z = standardise(-real_mortgage_rate)
  )

spline_credit_panel$credit_conditions_spline <- build_spline_credit_index(
  indicator_data = spline_credit_panel,
  indicator_cols = c(
    "loan_flow_z",
    "house_price_growth_z",
    "leverage_z",
    "first_home_buyer_share_z",
    "mortgage_rate_headwind_z"
  ),
  spline_df = 6L
)

model_data <- model_data %>%
  left_join(credit_panel %>% select(date, credit_conditions_index), by = "date") %>%
  left_join(spline_credit_panel %>% select(date, credit_conditions_spline), by = "date") %>%
  mutate(
    cci_x_log_disposable_permanent_income_ratio = credit_conditions_index * log_disposable_permanent_income_ratio,
    cci_x_housing_wealth_income_ratio = credit_conditions_index * housing_wealth_income_ratio,
    cci_x_unemployment_rate = credit_conditions_index * unemployment_rate,
    cci_spline_x_log_disposable_permanent_income_ratio = credit_conditions_spline * log_disposable_permanent_income_ratio,
    cci_spline_x_housing_wealth_income_ratio = credit_conditions_spline * housing_wealth_income_ratio,
    cci_spline_x_unemployment_rate = credit_conditions_spline * unemployment_rate
  )


# ------------------------------------------------------------------------------
# Long-run consumption equation
# ------------------------------------------------------------------------------

# Candidate Muellbauer-style long-run specifications. The benchmark preserves
# the original net-liquid-assets treatment, while the alternatives separate
# liquid assets and debt explicitly.
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
  ),
  interactive_credit = c(
    "log_disposable_permanent_income_ratio",
    "housing_wealth_income_ratio",
    "illiquid_financial_income_ratio",
    "liquid_assets_income_ratio",
    "debt_income_ratio",
    "unemployment_rate",
    "credit_conditions_index",
    "cci_x_log_disposable_permanent_income_ratio",
    "cci_x_housing_wealth_income_ratio",
    "cci_x_unemployment_rate"
  ),
  interactive_credit_spline = c(
    "log_disposable_permanent_income_ratio",
    "housing_wealth_income_ratio",
    "illiquid_financial_income_ratio",
    "liquid_assets_income_ratio",
    "debt_income_ratio",
    "unemployment_rate",
    "credit_conditions_spline",
    "cci_spline_x_log_disposable_permanent_income_ratio",
    "cci_spline_x_housing_wealth_income_ratio",
    "cci_spline_x_unemployment_rate"
  ),
  augmented_muellbauer = c(
    "log_disposable_permanent_income_ratio",
    "housing_wealth_income_ratio",
    "illiquid_financial_income_ratio",
    "liquid_assets_income_ratio",
    "debt_income_ratio",
    "mortgage_interest_income_ratio",
    "real_mortgage_rate",
    "prime_working_age_share",
    "first_home_buyer_share",
    "unemployment_rate",
    "credit_conditions_spline"
  )
)

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

ecm_adf <- tryCatch(
  ur.df(long_run_sample$ecm_residual, type = "drift", lags = 4L),
  error = function(e) NULL
)


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
    d_mortgage_cashflow = lead_lag_diff(mortgage_interest_income_ratio, 1L),
    d_real_mortgage_rate = lead_lag_diff(real_mortgage_rate, 1L),
    d_prime_working_age_share = lead_lag_diff(prime_working_age_share, 1L),
    d_first_home_buyer_share = lead_lag_diff(first_home_buyer_share, 1L),
    d_u = lead_lag_diff(unemployment_rate, 1L),
    d_cci = lead_lag_diff(credit_conditions_index, 1L),
    d_cci_spline = lead_lag_diff(credit_conditions_spline, 1L),
    d_cci_x_log_y_yp = lead_lag_diff(cci_x_log_disposable_permanent_income_ratio, 1L),
    d_cci_x_hw_ratio = lead_lag_diff(cci_x_housing_wealth_income_ratio, 1L),
    d_cci_x_u = lead_lag_diff(cci_x_unemployment_rate, 1L),
    d_cci_spline_x_log_y_yp = lead_lag_diff(cci_spline_x_log_disposable_permanent_income_ratio, 1L),
    d_cci_spline_x_hw_ratio = lead_lag_diff(cci_spline_x_housing_wealth_income_ratio, 1L),
    d_cci_spline_x_u = lead_lag_diff(cci_spline_x_unemployment_rate, 1L),
    d_house_prices = lead_lag_diff(safe_log(house_price_index), 1L),
    ecm_lag = lag(ecm_residual, 1L),
    d_lcons_lag = lag(d_lcons, 1L),
    d_lgdi_lag = lag(d_lgdi, 1L)
  ) %>%
  filter(
    complete.cases(
      d_lcons, d_lgdi, d_log_y_yp, d_hw_ratio, d_ifw_ratio,
      d_liquid_assets_ratio, d_debt_ratio, d_nla_ratio,
      d_mortgage_cashflow, d_real_mortgage_rate, d_prime_working_age_share,
      d_first_home_buyer_share, d_u, d_cci, d_cci_spline, d_cci_x_log_y_yp,
      d_cci_x_hw_ratio, d_cci_x_u, d_cci_spline_x_log_y_yp, d_cci_spline_x_hw_ratio,
      d_cci_spline_x_u, d_house_prices,
      ecm_lag, d_lcons_lag, d_lgdi_lag
    )
  )

ecm_term_map <- c(
  log_disposable_permanent_income_ratio = "d_log_y_yp",
  housing_wealth_income_ratio = "d_hw_ratio",
  illiquid_financial_income_ratio = "d_ifw_ratio",
  liquid_assets_income_ratio = "d_liquid_assets_ratio",
  debt_income_ratio = "d_debt_ratio",
  net_liquid_assets_income_ratio = "d_nla_ratio",
  mortgage_interest_income_ratio = "d_mortgage_cashflow",
  real_mortgage_rate = "d_real_mortgage_rate",
  prime_working_age_share = "d_prime_working_age_share",
  first_home_buyer_share = "d_first_home_buyer_share",
  unemployment_rate = "d_u",
  credit_conditions_index = "d_cci",
  credit_conditions_spline = "d_cci_spline",
  cci_x_log_disposable_permanent_income_ratio = "d_cci_x_log_y_yp",
  cci_x_housing_wealth_income_ratio = "d_cci_x_hw_ratio",
  cci_x_unemployment_rate = "d_cci_x_u",
  cci_spline_x_log_disposable_permanent_income_ratio = "d_cci_spline_x_log_y_yp",
  cci_spline_x_housing_wealth_income_ratio = "d_cci_spline_x_hw_ratio",
  cci_spline_x_unemployment_rate = "d_cci_spline_x_u"
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
housing_mpc_approx <- coef(long_run_fit)[["housing_wealth_income_ratio"]] * average_consumption_to_annual_income
illiquid_financial_mpc_approx <- coef(long_run_fit)[["illiquid_financial_income_ratio"]] * average_consumption_to_annual_income
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
# Save data and model tables
# ------------------------------------------------------------------------------

write.csv(model_data, file.path(output_dir, "model_dataset.csv"), row.names = FALSE)
write.csv(tidy(long_run_fit), file.path(output_dir, "long_run_coefficients.csv"), row.names = FALSE)
write.csv(tidy(ecm_fit), file.path(output_dir, "ecm_coefficients.csv"), row.names = FALSE)
write.csv(long_run_comparison, file.path(output_dir, "long_run_model_comparison.csv"), row.names = FALSE)


# ------------------------------------------------------------------------------
# Charts
# ------------------------------------------------------------------------------

plot_credit <- ggplot(model_data, aes(x = date, y = credit_conditions_index)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey60") +
  geom_line(linewidth = 0.7, colour = "#0B5C8C") +
  labs(
    title = "State-space credit conditions index",
    subtitle = "Higher values indicate easier household credit conditions",
    x = NULL,
    y = "Standardised factor"
  ) +
  theme_minimal(base_size = 11)

plot_credit_comparison <- model_data %>%
  select(date, credit_conditions_index, credit_conditions_spline) %>%
  pivot_longer(cols = c(credit_conditions_index, credit_conditions_spline), names_to = "series", values_to = "value") %>%
  ggplot(aes(x = date, y = value, colour = series)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey70") +
  geom_line(linewidth = 0.75) +
  scale_colour_manual(
    values = c(
      credit_conditions_index = "#0B5C8C",
      credit_conditions_spline = "#C84C09"
    ),
    labels = c(
      credit_conditions_index = "State-space factor",
      credit_conditions_spline = "Spline composite"
    )
  ) +
  labs(
    title = "Credit conditions indicators",
    subtitle = "Comparing latent and spline-based measures",
    x = NULL,
    y = "Standardised index",
    colour = NULL
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
ggsave(file.path(output_dir, "credit_conditions_comparison.png"), plot_credit_comparison, width = 9, height = 5, dpi = 150)
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
    "The augmented system adds a mortgage cash-flow burden term from ABS household interest payable on dwellings and an implicit real mortgage rate constructed from household interest payments, debt stocks and annual consumption inflation.",
    "Demographic and access terms include a prime working-age population share interpolated from annual ERP ages 25 to 44 and an owner-occupier first-home-buyer loan share from ABS lending indicators.",
    "A transparent spline-based credit index is estimated alongside the latent state-space factor and is available for long-run specification selection.",
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
if (is.null(ecm_adf)) {
  writeLines("ADF test could not be computed for the selected residual series with 4 lags.", con = con)
} else {
  writeLines(capture.output(show(ecm_adf)), con = con)
}

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


# ------------------------------------------------------------------------------
# Console output
# ------------------------------------------------------------------------------

cat("\nModel completed successfully.\n")
cat("Outputs written to:\n")
cat(" - ", file.path(output_dir, "model_dataset.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "long_run_coefficients.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "ecm_coefficients.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "long_run_model_comparison.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "model_summary.txt"), "\n", sep = "")
cat(" - ", file.path(output_dir, "credit_conditions_index.png"), "\n", sep = "")
cat(" - ", file.path(output_dir, "credit_conditions_comparison.png"), "\n", sep = "")
cat(" - ", file.path(output_dir, "long_run_fit.png"), "\n", sep = "")
cat(" - ", file.path(output_dir, "ecm_fit.png"), "\n", sep = "")
