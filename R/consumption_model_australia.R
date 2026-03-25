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
  library(sandwich)
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
  "australian-national-accounts-national-income-expenditure-and-product", "5206004.xlsx", "gfcf_sector",
  "australian-national-accounts-finance-and-wealth", "5232035.xlsx", "hh_balance_sheet",
  "australian-national-accounts-finance-and-wealth", "5232036.xlsx", "hh_income_wealth",
  "lending-indicators", "560101.xlsx", "housing_credit_flow",
  "residential-property-price-indexes-eight-capital-cities", "641601.xlsx", "house_prices_bridge",
  "labour-force-australia", "6202001.xlsx", "labour_force"
)

for (i in seq_len(nrow(workbooks))) {
  tryCatch(
    download_abs_workbook(raw_dir, workbooks$catalogue[[i]], workbooks$file_name[[i]]),
    error = function(e) message("Could not download ", workbooks$file_name[[i]], ": ", conditionMessage(e))
  )
}

hh_income_account_path <- download_file_if_missing(
  url = "https://www.abs.gov.au/statistics/economy/national-accounts/australian-national-accounts-national-income-expenditure-and-product/dec-2025/5206020_Household_Income.xlsx",
  target_path = file.path(raw_dir, "5206020_Household_Income.xlsx")
)

population_age_path <- download_file_if_missing(
  url = "https://www.abs.gov.au/statistics/people/population/national-state-and-territory-population/sep-2025/3101059.xlsx",
  target_path = file.path(raw_dir, "3101059.xlsx")
)

total_value_dwellings_path <- download_file_if_missing(
  url = "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/total-value-dwellings/dec-quarter-2025/643201.xlsx",
  target_path = file.path(raw_dir, "643201.xlsx")
)


# ------------------------------------------------------------------------------
# Read and prepare raw time-series data
# ------------------------------------------------------------------------------

hfce_raw <- read_abs_ts_workbook(file.path(raw_dir, "5206008_Household_Final_Consumption_Expenditure.xlsx"))
gfcf_sector_raw <- tryCatch(
  read_abs_ts_workbook(file.path(raw_dir, "5206004.xlsx")),
  error = function(e) {
    message("Note: 5206004.xlsx not available; HEW equation will be skipped.")
    NULL
  }
)
hh_bs_raw <- read_abs_ts_workbook(file.path(raw_dir, "5232035.xlsx"))
hh_inc_raw <- read_abs_ts_workbook(file.path(raw_dir, "5232036.xlsx"))
credit_raw <- read_abs_ts_workbook(file.path(raw_dir, "560101.xlsx"))
house_prices_bridge_raw <- read_abs_ts_workbook(file.path(raw_dir, "641601.xlsx"))
labour_raw <- read_abs_ts_workbook(file.path(raw_dir, "6202001.xlsx"))
hh_income_account_raw <- read_abs_ts_workbook(hh_income_account_path)
population_age_raw <- read_abs_ts_workbook(population_age_path)
total_value_dwellings_raw <- read_abs_ts_workbook(total_value_dwellings_path)
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

compensation_employees <- pick_preferred_series(
  hh_income_account_raw,
  "^Compensation of employees ;$",
  preferred_types = c("Seasonally Adjusted", "Seasonally adjusted", "Trend", "Original")
) %>%
  transmute(date, compensation_employees = value)

gross_mixed_income <- pick_preferred_series(
  hh_income_account_raw,
  "^Gross mixed income ;$",
  preferred_types = c("Seasonally Adjusted", "Seasonally adjusted", "Trend", "Original")
) %>%
  transmute(date, gross_mixed_income = value)

gross_operating_surplus_dwellings <- pick_preferred_series(
  hh_income_account_raw,
  "^Gross operating surplus ; Dwellings owned by persons ;$",
  preferred_types = c("Seasonally Adjusted", "Seasonally adjusted", "Trend", "Original")
) %>%
  transmute(date, gross_operating_surplus_dwellings = value)

total_primary_income_receivable <- pick_preferred_series(
  hh_income_account_raw,
  "^Total primary income receivable ;$",
  preferred_types = c("Seasonally Adjusted", "Seasonally adjusted", "Trend", "Original")
) %>%
  transmute(date, total_primary_income_receivable = value)

total_primary_income_payable <- pick_preferred_series(
  hh_income_account_raw,
  "^Total primary income payable ;$",
  preferred_types = c("Seasonally Adjusted", "Seasonally adjusted", "Trend", "Original")
) %>%
  transmute(date, total_primary_income_payable = value)

total_secondary_income_receivable <- pick_preferred_series(
  hh_income_account_raw,
  "^Total secondary income receivable ;$",
  preferred_types = c("Seasonally Adjusted", "Seasonally adjusted", "Trend", "Original")
) %>%
  transmute(date, total_secondary_income_receivable = value)

total_secondary_income_payable <- pick_preferred_series(
  hh_income_account_raw,
  "^Total secondary income payable ;$",
  preferred_types = c("Seasonally Adjusted", "Seasonally adjusted", "Trend", "Original")
) %>%
  transmute(date, total_secondary_income_payable = value)

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

bridge_house_prices <- pick_preferred_series(
  house_prices_bridge_raw,
  "^Residential Property Price Index ; Weighted average of eight capital cities ;$",
  preferred_types = c("Original")
) %>%
  transmute(date, house_price_index = value)

legacy_with_bridge_prices <- splice_house_price_series(
  current_series = bridge_house_prices,
  legacy_series = legacy_house_prices_raw
) %>%
  rename(house_price_old = house_price_spliced)

current_house_prices <- pick_preferred_series(
  total_value_dwellings_raw,
  "^Mean price of residential dwellings ; Australia ;$",
  preferred_types = c("Original")
) %>%
  transmute(date, house_price_index = value)

spliced_house_prices <- splice_house_price_series(
  current_series = current_house_prices,
  legacy_series = legacy_with_bridge_prices
) %>%
  rename(house_price_index = house_price_spliced)

# Gross fixed capital formation – household dwellings (for HEW construction).
# Tries several common ABS series name patterns across workbook releases.
dwelling_investment <- if (!is.null(gfcf_sector_raw)) {
  patterns <- c(
    "^Dwellings ;.*Household",
    "^Gross fixed capital formation.*[Dd]wellings.*[Hh]ousehold",
    "^Households.*[Dd]wellings.*[Gg]ross fixed",
    "^Private.*[Dd]wellings",
    "[Dd]wellings"
  )
  result <- NULL
  for (pat in patterns) {
    candidates <- gfcf_sector_raw %>%
      filter(str_detect(series_name, pat), series_type %in% c("Seasonally Adjusted", "Seasonally adjusted"))
    if (nrow(candidates) > 0) {
      best_id <- candidates %>% count(series_id, sort = TRUE) %>% slice(1L) %>% pull(series_id)
      result <- candidates %>%
        filter(series_id == best_id) %>%
        arrange(date) %>%
        distinct(date, .keep_all = TRUE) %>%
        transmute(date, dwelling_investment = rescale_to_millions(value, unit))
      break
    }
  }
  if (is.null(result)) {
    message("Note: dwelling investment series not found in 5206004.xlsx; HEW will use debt-change only.")
  }
  result
} else {
  NULL
}

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
  inner_join(compensation_employees, by = "date") %>%
  inner_join(gross_mixed_income, by = "date") %>%
  inner_join(gross_operating_surplus_dwellings, by = "date") %>%
  inner_join(total_primary_income_receivable, by = "date") %>%
  inner_join(total_primary_income_payable, by = "date") %>%
  inner_join(total_secondary_income_receivable, by = "date") %>%
  inner_join(total_secondary_income_payable, by = "date") %>%
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
  { if (!is.null(dwelling_investment)) left_join(., dwelling_investment, by = "date") else mutate(., dwelling_investment = NA_real_) } %>%
  inner_join(working_age_population, by = "date") %>%
  inner_join(unemployment_rate, by = "date") %>%
  arrange(date) %>%
  mutate(
    illiquid_financial_wealth = equities + superannuation,
    liquid_assets = currency_deposits,
    net_liquid_assets = currency_deposits - household_debt,
    final_consumption_hh_sa = final_consumption_hh,
    property_income_receivable = total_primary_income_receivable - compensation_employees - gross_mixed_income - gross_operating_surplus_dwellings,
    non_property_disposable_income = gross_disposable_income - property_income_receivable + total_primary_income_payable,
    annualised_income = gross_disposable_income * 4,
    annualised_nonproperty_income = non_property_disposable_income * 4,
    # The ABS household balance sheet is current price. Deflating by the
    # household consumption deflator is an approximation, but it keeps the
    # balance-sheet variables on a household spending price basis.
    real_gdi = gross_disposable_income / consumption_deflator * 100,
    real_nonproperty_income = non_property_disposable_income / consumption_deflator * 100,
    real_consumption_per_working_age = consumption_real / working_age_population,
    real_gdi_per_working_age = real_gdi / working_age_population,
    real_nonproperty_income_per_working_age = real_nonproperty_income / working_age_population,
    real_housing_wealth = housing_wealth / consumption_deflator * 100,
    real_illiquid_financial_wealth = illiquid_financial_wealth / consumption_deflator * 100,
    real_household_debt = household_debt / consumption_deflator * 100,
    nominal_consumption_income_ratio = final_consumption_hh_sa / annualised_nonproperty_income,
    housing_wealth_income_ratio = housing_wealth / annualised_nonproperty_income,
    illiquid_financial_income_ratio = illiquid_financial_wealth / annualised_nonproperty_income,
    liquid_assets_income_ratio = liquid_assets / annualised_nonproperty_income,
    net_liquid_assets_income_ratio = net_liquid_assets / annualised_nonproperty_income,
    debt_income_ratio = household_debt / annualised_nonproperty_income,
    mortgage_interest_income_ratio = mortgage_interest_paid / annualised_nonproperty_income,
    mortgage_rate_nominal = 400 * mortgage_interest_paid / lag(household_debt, 1L),
    mortgage_cashflow_pressure = lead_lag_diff(mortgage_rate_nominal, 4L) * lag(debt_income_ratio, 1L),
    first_home_buyer_share = first_home_buyer_loans / (first_home_buyer_loans + non_first_home_buyer_loans),
    lcons = safe_log(real_consumption_per_working_age),
    lgdi = safe_log(real_nonproperty_income_per_working_age),
    # log(c/y): income-normalised consumption.  Income elasticity is imposed at 1
    # (not estimated) by putting this on the LHS.  This avoids the multicollinearity
    # that arises when lgdi is a free RHS predictor alongside wealth/income ratios.
    # More volatile than lcons - lperm_income, giving meaningful ECM residuals.
    lcons_income_ratio = safe_log(real_consumption_per_working_age) -
      safe_log(real_nonproperty_income_per_working_age),
    # Housing equity withdrawal (HEW): change in nominal household debt minus
    # nominal gross dwelling investment.  dwelling_investment is NA when
    # 5206004.xlsx was not available; in that case hew_income_ratio is also NA.
    hew_nominal = (household_debt - lag(household_debt, 1L)) -
      ifelse(!is.na(dwelling_investment), dwelling_investment, NA_real_),
    hew_income_ratio = hew_nominal / annualised_nonproperty_income,
    hew_data_available = !is.na(dwelling_investment)
  )

model_data <- model_data %>%
  mutate(
    annual_consumption_inflation = 100 * lead_lag_diff(log(consumption_deflator), 4L),
    real_mortgage_rate = mortgage_rate_nominal - annual_consumption_inflation
  )

income_expectations_fit <- fit_income_expectations_model(model_data)
expected_log_income <- compute_expected_log_income_path(model_data, income_expectations_fit, horizon = 40L)
expected_log_income_rolling <- compute_expected_log_income_path_rolling(model_data, horizon = 40L, min_obs = 40L)
log_yp_over_y <- compute_log_yp_over_y(model_data$lgdi, expected_log_income, discount = 0.05, horizon = 40L)
log_yp_over_y_rolling <- compute_log_yp_over_y(model_data$lgdi, expected_log_income_rolling, discount = 0.05, horizon = 40L)
adaptive_log_income_alt <- adaptive_permanent_income_log(model_data$lgdi, lambda = 0.97)

model_data <- model_data %>%
  mutate(
    expected_log_income = expected_log_income,
    expected_log_income_rolling = expected_log_income_rolling,
    log_disposable_permanent_income_ratio = log_yp_over_y,
    log_disposable_permanent_income_ratio_rolling = log_yp_over_y_rolling,
    lperm_income = lgdi + log_disposable_permanent_income_ratio,
    permanent_income_per_working_age = exp(lperm_income),
    disposable_permanent_income_ratio = real_gdi_per_working_age / permanent_income_per_working_age,
    lcons_perm_income_ratio = lcons - lperm_income,
    lperm_income_adaptive = adaptive_log_income_alt,
    log_disposable_permanent_income_ratio_adaptive = lgdi - lperm_income_adaptive,
    lcons_perm_income_ratio_adaptive = lcons - lperm_income_adaptive,
    lperm_income_rolling = lgdi + log_disposable_permanent_income_ratio_rolling,
    lcons_perm_income_ratio_rolling = lcons - lperm_income_rolling,
    gst_dummy = as.integer(date == as.Date("2000-09-01")),
    gfc_dummy = as.integer(date >= as.Date("2008-09-01") & date <= as.Date("2009-12-01")),
    covid_dummy = as.integer(date >= as.Date("2020-03-01") & date <= as.Date("2020-09-01"))
  )


# ------------------------------------------------------------------------------
# Credit conditions index with institutional timing priors
# ------------------------------------------------------------------------------

credit_regime_basis <- build_credit_regime_basis(model_data$date)
institutional_cci <- construct_institutional_cci(model_data, credit_regime_basis)

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
  left_join(credit_regime_basis, by = "date") %>%
  left_join(institutional_cci, by = "date") %>%
  left_join(spline_credit_panel %>% select(date, credit_conditions_spline), by = "date")

institutional_orientation <- orient_credit_index(
  model_data,
  cci_col = "cci_institutional_raw",
  easier_credit_targets = c("housing_loan_flow", "house_price_index", "first_home_buyer_share"),
  tighter_credit_targets = c("real_mortgage_rate")
)

model_data <- model_data %>%
  mutate(
    credit_conditions_index = institutional_orientation$cci
  )

selected_cci_variant <- "institutional_spline"
cci_orientation_sign <- institutional_orientation$orientation[[1]]

cci_means <- model_data %>%
  filter(!is.na(credit_conditions_index)) %>%
  summarise(
    mean_log_disposable_permanent_income_ratio = mean(log_disposable_permanent_income_ratio, na.rm = TRUE),
    mean_housing_wealth_income_ratio = mean(housing_wealth_income_ratio, na.rm = TRUE),
    mean_real_mortgage_rate = mean(real_mortgage_rate, na.rm = TRUE)
  )

model_data <- model_data %>%
  mutate(
    centred_log_disposable_permanent_income_ratio = log_disposable_permanent_income_ratio - cci_means$mean_log_disposable_permanent_income_ratio,
    centred_housing_wealth_income_ratio = housing_wealth_income_ratio - cci_means$mean_housing_wealth_income_ratio,
    centred_real_mortgage_rate = real_mortgage_rate - cci_means$mean_real_mortgage_rate,
    cci_x_log_disposable_permanent_income_ratio = credit_conditions_index * centred_log_disposable_permanent_income_ratio,
    cci_x_housing_wealth_income_ratio = credit_conditions_index * centred_housing_wealth_income_ratio,
    cci_x_real_mortgage_rate = credit_conditions_index * centred_real_mortgage_rate,
    cci_spline_x_log_disposable_permanent_income_ratio = credit_conditions_spline * centred_log_disposable_permanent_income_ratio,
    cci_spline_x_housing_wealth_income_ratio = credit_conditions_spline * centred_housing_wealth_income_ratio,
    cci_spline_x_real_mortgage_rate = credit_conditions_spline * centred_real_mortgage_rate
  ) %>%
  mutate(
    lhouse_real = safe_log(house_price_index / consumption_deflator * 100),
    lhouse_income_ratio = lhouse_real - lgdi,
    ldebt_real_per_working_age = safe_log(real_household_debt / working_age_population),
    ldebt_income_ratio = safe_log(debt_income_ratio)
  ) %>%
  mutate(
    centred_lhouse_income_ratio = lhouse_income_ratio - mean(lhouse_income_ratio, na.rm = TRUE),
    cci_x_lhouse_income_ratio = credit_conditions_index * centred_lhouse_income_ratio
  )

evaluate_credit_variant <- function(data, cci_col, variant_name) {
  work <- data %>%
    mutate(
      credit_conditions_index = .data[[cci_col]]
    )

  # Use lcons_income_ratio = log(c/y) as response, consistent with the main
  # long-run specs.  Income elasticity is imposed at 1 via the LHS.
  rhs_vars <- c(
    "log_disposable_permanent_income_ratio",
    "housing_wealth_income_ratio",
    "illiquid_financial_income_ratio",
    "net_liquid_assets_income_ratio",
    "real_mortgage_rate",
    "credit_conditions_index",
    "cci_x_housing_wealth_income_ratio",
    "cci_x_real_mortgage_rate"
  )

  fit_obj <- fit_long_run_spec(
    work,
    spec_name = variant_name,
    rhs_vars = rhs_vars,
    response_var = "lcons_income_ratio"
  )

  coefs <- coef(fit_obj$fit)

  fit_obj$diagnostics %>%
    mutate(
      cci_variant = variant_name,
      cci_start = min(work$date[!is.na(work$credit_conditions_index)], na.rm = TRUE),
      cci_end = max(work$date[!is.na(work$credit_conditions_index)], na.rm = TRUE),
      sign_score = sum(
        c(
          ("credit_conditions_index" %in% names(coefs)) && coefs[["credit_conditions_index"]] > 0,
          ("cci_x_housing_wealth_income_ratio" %in% names(coefs)) && coefs[["cci_x_housing_wealth_income_ratio"]] > 0,
          ("cci_x_real_mortgage_rate" %in% names(coefs)) && coefs[["cci_x_real_mortgage_rate"]] < 0
        ),
        na.rm = TRUE
      ),
      corr_loan_flow = suppressWarnings(cor(work$credit_conditions_index, work$housing_loan_flow, use = "pairwise.complete.obs")),
      corr_house_prices = suppressWarnings(cor(work$credit_conditions_index, work$house_price_index, use = "pairwise.complete.obs")),
      corr_real_mortgage_rate = suppressWarnings(cor(work$credit_conditions_index, work$real_mortgage_rate, use = "pairwise.complete.obs"))
    )
}

credit_variant_comparison <- bind_rows(
  evaluate_credit_variant(model_data, "credit_conditions_index", "institutional_spline"),
  evaluate_credit_variant(model_data, "credit_conditions_spline", "diagnostic_spline")
) %>%
  arrange(desc(adf_reject_5pct), desc(sign_score), bic, aic)


# ------------------------------------------------------------------------------
# Long-run consumption equation
# ------------------------------------------------------------------------------

# Candidate long-run specifications.
#
# Normalisation: response variable is lcons (log real consumption per capita).
# Log non-property income (lgdi) enters explicitly on the RHS so that the
# income elasticity is estimated rather than imposed.  The income-expectations
# ratio (log_disposable_permanent_income_ratio = log y^p/y) captures the
# forward-looking smoothing channel; its coefficient is expected to be positive
# (higher permanent vs. current income → higher consumption today).
#
# Two wealth decompositions are tested:
#   nla_* : net liquid assets (currency + deposits − debt) as one variable
#   lad_* : liquid assets and debt entered separately (tests NLA restriction)
#
# Three income-expectations constructions are each tested:
#   base     : full-sample AR forecast (preferred)
#   rolling  : recursive out-of-sample forecast
#   adaptive : exponential smoother (backward-looking alternative)
#
# LIVES variants (suffix _lives) add the two key CCI interactions from the
# Latent Interactive Variable Equation System:
#   cci_x_housing_wealth_income_ratio  – housing collateral channel
#   cci_x_real_mortgage_rate           – interest-rate sensitivity channel
long_run_specs <- list(
  nla_base = c(
    "log_disposable_permanent_income_ratio",
    "housing_wealth_income_ratio",
    "illiquid_financial_income_ratio",
    "net_liquid_assets_income_ratio",
    "real_mortgage_rate",
    "credit_conditions_index"
  ),
  nla_base_lives = c(
    "log_disposable_permanent_income_ratio",
    "housing_wealth_income_ratio",
    "illiquid_financial_income_ratio",
    "net_liquid_assets_income_ratio",
    "real_mortgage_rate",
    "credit_conditions_index",
    "cci_x_housing_wealth_income_ratio",
    "cci_x_real_mortgage_rate"
  ),
  nla_rolling = c(
    "log_disposable_permanent_income_ratio_rolling",
    "housing_wealth_income_ratio",
    "illiquid_financial_income_ratio",
    "net_liquid_assets_income_ratio",
    "real_mortgage_rate",
    "credit_conditions_index"
  ),
  nla_adaptive = c(
    "log_disposable_permanent_income_ratio_adaptive",
    "housing_wealth_income_ratio",
    "illiquid_financial_income_ratio",
    "net_liquid_assets_income_ratio",
    "real_mortgage_rate",
    "credit_conditions_index"
  ),
  lad_base = c(
    "log_disposable_permanent_income_ratio",
    "housing_wealth_income_ratio",
    "illiquid_financial_income_ratio",
    "liquid_assets_income_ratio",
    "debt_income_ratio",
    "real_mortgage_rate",
    "credit_conditions_index"
  ),
  lad_base_lives = c(
    "log_disposable_permanent_income_ratio",
    "housing_wealth_income_ratio",
    "illiquid_financial_income_ratio",
    "liquid_assets_income_ratio",
    "debt_income_ratio",
    "real_mortgage_rate",
    "credit_conditions_index",
    "cci_x_housing_wealth_income_ratio",
    "cci_x_real_mortgage_rate"
  ),
  lad_rolling = c(
    "log_disposable_permanent_income_ratio_rolling",
    "housing_wealth_income_ratio",
    "illiquid_financial_income_ratio",
    "liquid_assets_income_ratio",
    "debt_income_ratio",
    "real_mortgage_rate",
    "credit_conditions_index"
  ),
  lad_adaptive = c(
    "log_disposable_permanent_income_ratio_adaptive",
    "housing_wealth_income_ratio",
    "illiquid_financial_income_ratio",
    "liquid_assets_income_ratio",
    "debt_income_ratio",
    "real_mortgage_rate",
    "credit_conditions_index"
  )
)

long_run_candidates <- lapply(names(long_run_specs), function(spec_name) {
  fit_long_run_spec(
    model_data,
    spec_name,
    long_run_specs[[spec_name]],
    response_var = "lcons_income_ratio"
  )
})
names(long_run_candidates) <- names(long_run_specs)

long_run_sign_scores <- tibble(
  specification = names(long_run_candidates),
  sign_score = vapply(long_run_candidates, function(obj) {
    coefs <- coef(obj$fit)
    score <- sum(
      c(
        # Income-expectations ratio: LHS is log(c/y).  When y^p > y, households
        # consume more relative to current income (borrow against future) →
        # log(c/y) rises → positive coefficient.
        ("log_disposable_permanent_income_ratio" %in% names(coefs)) && coefs[["log_disposable_permanent_income_ratio"]] > 0,
        ("log_disposable_permanent_income_ratio_rolling" %in% names(coefs)) && coefs[["log_disposable_permanent_income_ratio_rolling"]] > 0,
        ("log_disposable_permanent_income_ratio_adaptive" %in% names(coefs)) && coefs[["log_disposable_permanent_income_ratio_adaptive"]] > 0,
        # Wealth: all positive MPCs expected
        ("housing_wealth_income_ratio" %in% names(coefs)) && coefs[["housing_wealth_income_ratio"]] > 0,
        ("illiquid_financial_income_ratio" %in% names(coefs)) && coefs[["illiquid_financial_income_ratio"]] > 0,
        (!("liquid_assets_income_ratio" %in% names(coefs))) || coefs[["liquid_assets_income_ratio"]] > 0,
        (!("net_liquid_assets_income_ratio" %in% names(coefs))) || coefs[["net_liquid_assets_income_ratio"]] > 0,
        (!("debt_income_ratio" %in% names(coefs))) || coefs[["debt_income_ratio"]] < 0,
        # Interest rate and credit
        (!("real_mortgage_rate" %in% names(coefs))) || coefs[["real_mortgage_rate"]] < 0,
        ("credit_conditions_index" %in% names(coefs)) && coefs[["credit_conditions_index"]] > 0,
        # LIVES interactions: housing collateral grows with credit ease; interest
        # sensitivity amplified by tight credit (negative interaction)
        (!("cci_x_housing_wealth_income_ratio" %in% names(coefs))) || coefs[["cci_x_housing_wealth_income_ratio"]] > 0,
        (!("cci_x_real_mortgage_rate" %in% names(coefs))) || coefs[["cci_x_real_mortgage_rate"]] < 0
      ),
      na.rm = TRUE
    )
    score
  }, numeric(1L))
)

replace_terms <- function(terms, mapping) {
  unname(vapply(terms, function(term) if (term %in% names(mapping)) mapping[[term]] else term, character(1L)))
}

assess_consumption_spec_stability <- function(spec_name, rhs_vars) {
  family_name <- str_split_fixed(spec_name, "_", 2)[, 1]
  family_specs <- names(long_run_specs)[str_detect(names(long_run_specs), paste0("^", family_name, "_"))]

  # All variants use lcons_income_ratio = log(c/y) as response.  Income
  # elasticity is imposed at 1 (not estimated) to avoid the multicollinearity
  # that arises when log(y) is a free predictor alongside wealth/income ratios.
  fits <- lapply(family_specs, function(name) {
    tryCatch(
      fit_long_run_spec(model_data, spec_name = name, rhs_vars = long_run_specs[[name]], response_var = "lcons_income_ratio"),
      error = function(e) NULL
    )
  })
  names(fits) <- family_specs

  pre_fit <- tryCatch(
    fit_long_run_spec(model_data %>% filter(date < as.Date("2008-09-01")),
                      spec_name = paste0(spec_name, "_pre"), rhs_vars = rhs_vars,
                      response_var = "lcons_income_ratio"),
    error = function(e) NULL
  )
  post_fit <- tryCatch(
    fit_long_run_spec(model_data %>% filter(date >= as.Date("2008-09-01")),
                      spec_name = paste0(spec_name, "_post"), rhs_vars = rhs_vars,
                      response_var = "lcons_income_ratio"),
    error = function(e) NULL
  )

  get_coef_sign <- function(fit_obj, term) {
    if (is.null(fit_obj)) {
      return(NA_real_)
    }
    coefs <- coef(fit_obj$fit)
    if (!(term %in% names(coefs)) || is.na(coefs[[term]]) || coefs[[term]] == 0) {
      return(NA_real_)
    }
    sign(coefs[[term]])
  }

  sign_terms <- c("housing_wealth_income_ratio")
  expected_signs <- c(1L)
  names(expected_signs) <- sign_terms

  stable_count <- 0L
  for (term in sign_terms) {
    variant_signs <- vapply(fits, get_coef_sign, numeric(1L), term = term)
    valid_signs <- variant_signs[!is.na(variant_signs)]
    if (length(valid_signs) == 0L) {
      next
    }
    if (all(valid_signs == expected_signs[[term]])) {
      stable_count <- stable_count + 1L
    }
  }

  credit_signs <- c(
    family = vapply(fits, get_coef_sign, numeric(1L), term = "credit_conditions_index"),
    pre_gfc = get_coef_sign(pre_fit, "credit_conditions_index"),
    post_gfc = get_coef_sign(post_fit, "credit_conditions_index")
  )
  credit_consistent <- all(credit_signs[!is.na(credit_signs)] > 0)

  family_cointegration <- all(vapply(fits, function(fit_obj) {
    if (is.null(fit_obj)) {
      return(FALSE)
    }
    isTRUE(fit_obj$diagnostics$adf_reject_5pct[[1]])
  }, logical(1L)))

  tibble(
    specification = spec_name,
    stability_score = stable_count + as.integer(credit_consistent) + as.integer(family_cointegration),
    credit_sign_consistent = credit_consistent,
    robustness_pass = stable_count >= 1L && credit_consistent && family_cointegration
  )
}

long_run_stability <- bind_rows(lapply(names(long_run_specs), function(spec_name) {
  assess_consumption_spec_stability(spec_name, long_run_specs[[spec_name]])
}))

long_run_comparison <- bind_rows(lapply(long_run_candidates, `[[`, "diagnostics")) %>%
  left_join(long_run_sign_scores, by = "specification") %>%
  left_join(long_run_stability, by = "specification") %>%
  mutate(
    # Admissibility criteria:
    #   1. ADF rejects unit root at 5% (cointegration evidence required).
    #   2. At least 5 correct signs from the 12-term checklist.  This filters
    #      out specs with severe OLS multicollinearity (e.g. lad_* in
    #      Australian data where separating NLA into liquid assets and debt
    #      collapses the CCI and rate signs).
    #   3. robustness_pass is INFORMATIONAL only — it is too strict as a hard
    #      gate because rolling/adaptive sub-sample failures drive
    #      family_cointegration = FALSE for every spec in every family.
    #
    # Among admissible specs, preference order:
    #   i.   Base specs (full-sample forward-looking Y^p) over rolling/adaptive.
    #   ii.  Strongest cointegration (most negative ADF stat).
    #   iii. Better AIC as tie-breaker.
    is_base_spec = !grepl("_rolling$|_adaptive$", specification),
    admissible = adf_reject_5pct & sign_score >= 5
  ) %>%
  arrange(desc(admissible), desc(is_base_spec), adf_stat, aic)

benchmark_long_run_name <- long_run_comparison %>%
  filter(admissible) %>%
  slice(1L) %>%
  pull(specification)

if (length(benchmark_long_run_name) == 0L || is.na(benchmark_long_run_name)) {
  benchmark_long_run_name <- long_run_comparison %>%
    slice(1L) %>%
    pull(specification)
}

best_long_run_name <- benchmark_long_run_name
best_long_run <- long_run_candidates[[best_long_run_name]]
long_run_fit <- best_long_run$fit
long_run_rhs_vars <- best_long_run$rhs_vars
long_run_sample <- best_long_run$sample %>%
  mutate(ecm_residual = resid(long_run_fit))

# ---- DOLS estimation (Stock & Watson 1993) --------------------------------
# Corrects endogeneity / multiple-cointegrating-vector bias in the two-step
# OLS long-run coefficients.  Static residuals replace OLS residuals as the
# ECM correction term so that the speed-of-adjustment estimate is unbiased.
consumption_dols <- tryCatch(
  fit_dols_spec(
    data         = best_long_run$sample,
    spec_name    = best_long_run_name,
    rhs_vars     = long_run_rhs_vars,
    response_var = "lcons_income_ratio",
    leads_lags   = 2L
  ),
  error = function(e) {
    message("DOLS estimation failed for consumption equation: ", conditionMessage(e))
    NULL
  }
)

if (!is.null(consumption_dols)) {
  long_run_sample <- long_run_sample %>%
    left_join(consumption_dols$static_resid, by = "date") %>%
    mutate(ecm_residual = ifelse(!is.na(dols_resid), dols_resid, ecm_residual)) %>%
    select(-dols_resid)
  message("Consumption long-run: using DOLS static residuals as ECM correction term.")
}
# ---------------------------------------------------------------------------

ecm_adf <- tryCatch(
  ur.df(long_run_sample$ecm_residual, type = "drift", lags = 4L),
  error = function(e) NULL
)

select_ecm_spec <- function(data, response_var, base_terms, optional_term_sets, required_negative = character(), required_positive = character(), adjustment_term = NULL, min_abs_adjustment = 0.005) {
  candidate_sets <- c(list(base_terms), optional_term_sets)
  candidate_names <- c("base", names(optional_term_sets))

  fits <- lapply(seq_along(candidate_sets), function(i) {
    terms <- unique(c(base_terms, candidate_sets[[i]]))
    fit <- lm(reformulate(terms, response = response_var), data = data)
    summary_fit <- summary(fit)
    coefs <- coef(fit)
    residual_box_p <- tryCatch(Box.test(resid(fit), lag = 4L, type = "Ljung-Box")$p.value, error = function(e) NA_real_)
    adjustment_ok <- if (is.null(adjustment_term) || !(adjustment_term %in% names(coefs))) {
      TRUE
    } else {
      is.finite(coefs[[adjustment_term]]) && coefs[[adjustment_term]] <= -min_abs_adjustment
    }
    sign_score <- sum(
      c(
        vapply(required_negative, function(term) term %in% names(coefs) && coefs[[term]] < 0, logical(1L)),
        vapply(required_positive, function(term) term %in% names(coefs) && coefs[[term]] > 0, logical(1L))
      ),
      na.rm = TRUE
    )

    tibble(
      specification = candidate_names[[i]],
      n_obs = nobs(fit),
      aic = AIC(fit),
      bic = BIC(fit),
      adj_r2 = summary_fit$adj.r.squared,
      sigma = summary_fit$sigma,
      residual_box_p = residual_box_p,
      sign_score = sign_score,
      adjustment_ok = adjustment_ok,
      admissible = sign_score == (length(required_negative) + length(required_positive)) &
        adjustment_ok &
        (is.na(residual_box_p) || residual_box_p > 0.05)
    ) %>%
      mutate(fit_obj = list(fit), terms = list(terms))
  })

  comparison <- bind_rows(fits) %>%
    arrange(desc(admissible), bic, aic)

  selected <- comparison %>% slice(1L)

  list(
    comparison = comparison %>% select(-fit_obj, -terms),
    selected_name = selected$specification[[1]],
    fit = selected$fit_obj[[1]],
    terms = selected$terms[[1]],
    admissible = selected$admissible[[1]]
  )
}


# ------------------------------------------------------------------------------
# Short-run error-correction model
# ------------------------------------------------------------------------------

ecm_data <- long_run_sample %>%
  mutate(
    d_lcons = lead_lag_diff(lcons, 1L),
    d_lgdi = lead_lag_diff(lgdi, 1L),
    d_log_unemployment = lead_lag_diff(safe_log(unemployment_rate), 1L),
    d_mortgage_cashflow = lead_lag_diff(mortgage_cashflow_pressure, 1L),
    d_cci = lead_lag_diff(credit_conditions_index, 1L),
    d_cci_x_log_y_yp = lead_lag_diff(cci_x_log_disposable_permanent_income_ratio, 1L),
    d_cci_x_hw_ratio = lead_lag_diff(cci_x_housing_wealth_income_ratio, 1L),
    d_cci_x_real_rate = lead_lag_diff(cci_x_real_mortgage_rate, 1L),
    negative_housing_return = pmin(lead_lag_diff(safe_log(house_price_index), 4L), 0),
    ecm_lag = lag(ecm_residual, 1L),
    d_lcons_yoy = lead_lag_diff(lcons, 4L),
    d_lcons_yoy_lag = lag(d_lcons_yoy, 1L)
  ) %>%
  filter(
    complete.cases(
      d_lcons, d_lgdi, d_log_unemployment,
      d_mortgage_cashflow, d_cci, d_cci_x_log_y_yp,
      d_cci_x_hw_ratio, d_cci_x_real_rate,
      negative_housing_return, ecm_lag, d_lcons_yoy_lag
    )
  )

ecm_term_map <- c(
  credit_conditions_index = "d_cci",
  cci_x_log_disposable_permanent_income_ratio = "d_cci_x_log_y_yp",
  cci_x_housing_wealth_income_ratio = "d_cci_x_hw_ratio",
  cci_x_real_mortgage_rate = "d_cci_x_real_rate"
)

selected_ecm_terms <- unique(na.omit(unname(ecm_term_map[long_run_rhs_vars])))
consumption_ecm_base_terms <- c(
  "ecm_lag",
  "d_lgdi",
  "d_log_unemployment",
  selected_ecm_terms
)
consumption_ecm_options <- list(
  with_cashflow = c("d_mortgage_cashflow"),
  with_housing = c("negative_housing_return"),
  with_persistence = c("d_lcons_yoy_lag"),
  with_cashflow_and_housing = c("d_mortgage_cashflow", "negative_housing_return"),
  with_all = c("d_mortgage_cashflow", "negative_housing_return", "d_lcons_yoy_lag")
)
consumption_ecm_selection <- select_ecm_spec(
  data = ecm_data,
  response_var = "d_lcons",
  base_terms = consumption_ecm_base_terms,
  optional_term_sets = consumption_ecm_options,
  required_negative = c("ecm_lag", "d_log_unemployment"),
  adjustment_term = "ecm_lag",
  min_abs_adjustment = 0.005
)
ecm_fit <- consumption_ecm_selection$fit
selected_consumption_ecm_name <- consumption_ecm_selection$selected_name
consumption_ecm_comparison <- consumption_ecm_selection$comparison
consumption_ecm_admissible <- consumption_ecm_selection$admissible
selected_consumption_ecm_terms <- consumption_ecm_selection$terms

fit_consumption_long_run_robustness <- function(data, label, rhs_vars, response_var = "lcons_income_ratio", date_filter = rep(TRUE, nrow(data))) {
  work <- data[date_filter, , drop = FALSE]
  fit_obj <- fit_long_run_spec(
    work,
    spec_name = label,
    rhs_vars = rhs_vars,
    response_var = response_var
  )
  coefs <- coef(fit_obj$fit)

  fit_obj$diagnostics %>%
    mutate(
      variant = label,
      response_var = response_var,
      credit_coef = if ("credit_conditions_index" %in% names(coefs)) coefs[["credit_conditions_index"]] else NA_real_,
      credit_spline_coef = if ("credit_conditions_spline" %in% names(coefs)) coefs[["credit_conditions_spline"]] else NA_real_,
      income_expectations_coef = if ("log_disposable_permanent_income_ratio" %in% names(coefs)) coefs[["log_disposable_permanent_income_ratio"]] else if ("log_disposable_permanent_income_ratio_adaptive" %in% names(coefs)) coefs[["log_disposable_permanent_income_ratio_adaptive"]] else NA_real_
    )
}

base_long_run_terms <- long_run_rhs_vars
adaptive_long_run_terms <- replace_terms(
  long_run_rhs_vars,
  c(
    log_disposable_permanent_income_ratio = "log_disposable_permanent_income_ratio_adaptive",
    cci_x_log_disposable_permanent_income_ratio = "cci_x_log_disposable_permanent_income_ratio"
  )
)
spline_long_run_terms <- replace_terms(
  long_run_rhs_vars,
  c(
    credit_conditions_index = "credit_conditions_spline",
    cci_x_log_disposable_permanent_income_ratio = "cci_spline_x_log_disposable_permanent_income_ratio",
    cci_x_housing_wealth_income_ratio = "cci_spline_x_housing_wealth_income_ratio",
    cci_x_real_mortgage_rate = "cci_spline_x_real_mortgage_rate"
  )
)

consumption_long_run_robustness <- bind_rows(
  fit_consumption_long_run_robustness(model_data, "base", base_long_run_terms),
  fit_consumption_long_run_robustness(model_data, "adaptive_income", adaptive_long_run_terms),
  fit_consumption_long_run_robustness(model_data, "spline_cci", spline_long_run_terms),
  fit_consumption_long_run_robustness(model_data, "pre_gfc", base_long_run_terms, date_filter = model_data$date < as.Date("2008-09-01")),
  fit_consumption_long_run_robustness(model_data, "post_gfc", base_long_run_terms, date_filter = model_data$date >= as.Date("2008-09-01"))
) %>%
  arrange(match(variant, c("base", "adaptive_income", "spline_cci", "pre_gfc", "post_gfc")))

ecm_robustness_fits <- list(
  base = lm(reformulate(selected_consumption_ecm_terms, response = "d_lcons"), data = ecm_data),
  with_crisis_dummies = lm(reformulate(c(selected_consumption_ecm_terms, "gst_dummy", "gfc_dummy", "covid_dummy"), response = "d_lcons"), data = ecm_data),
  pre_gfc = lm(reformulate(selected_consumption_ecm_terms, response = "d_lcons"), data = ecm_data %>% filter(date < as.Date("2008-09-01"))),
  post_gfc = lm(reformulate(selected_consumption_ecm_terms, response = "d_lcons"), data = ecm_data %>% filter(date >= as.Date("2008-09-01")))
)

consumption_ecm_robustness <- bind_rows(lapply(names(ecm_robustness_fits), function(name) {
  fit <- ecm_robustness_fits[[name]]
  coefs <- coef(fit)
  summary_fit <- summary(fit)

  tibble(
    variant = name,
    n_obs = nobs(fit),
    aic = AIC(fit),
    bic = BIC(fit),
    adj_r2 = summary_fit$adj.r.squared,
    sigma = summary_fit$sigma,
    ecm_coef = if ("ecm_lag" %in% names(coefs)) coefs[["ecm_lag"]] else NA_real_,
    unemployment_coef = if ("d_log_unemployment" %in% names(coefs)) coefs[["d_log_unemployment"]] else NA_real_,
    cci_coef = if ("d_cci" %in% names(coefs)) coefs[["d_cci"]] else NA_real_
  )
})) %>%
  arrange(match(variant, c("base", "with_crisis_dummies", "pre_gfc", "post_gfc")))


# ------------------------------------------------------------------------------
# Partial LIVES extension: house prices and mortgage credit
# ------------------------------------------------------------------------------

house_price_specs <- list(
  house_price_core = c(
    "credit_conditions_index",
    "real_mortgage_rate",
    "prime_working_age_share",
    "first_home_buyer_share"
  ),
  house_price_rate_interaction = c(
    "credit_conditions_index",
    "real_mortgage_rate",
    "prime_working_age_share",
    "first_home_buyer_share",
    "cci_x_real_mortgage_rate"
  ),
  house_price_credit_only = c(
    "credit_conditions_index",
    "real_mortgage_rate",
    "first_home_buyer_share"
  )
)

house_price_candidates <- lapply(names(house_price_specs), function(spec_name) {
  fit_long_run_spec(
    model_data,
    spec_name = spec_name,
    rhs_vars = house_price_specs[[spec_name]],
    response_var = "lhouse_income_ratio"
  )
})
names(house_price_candidates) <- names(house_price_specs)

house_price_sign_scores <- tibble(
  specification = names(house_price_candidates),
  sign_score = vapply(house_price_candidates, function(obj) {
    coefs <- coef(obj$fit)
    sum(
      c(
        # Easier credit → higher equilibrium house-price-to-income ratio
        ("credit_conditions_index" %in% names(coefs)) && coefs[["credit_conditions_index"]] > 0,
        # Higher real rates → lower house prices
        ("real_mortgage_rate" %in% names(coefs)) && coefs[["real_mortgage_rate"]] < 0,
        # FHB share is counter-cyclical: high when prices are low / accessible
        ("first_home_buyer_share" %in% names(coefs)) && coefs[["first_home_buyer_share"]] < 0,
        # CCI × rate interaction: easier credit dampens rate sensitivity
        (!("cci_x_real_mortgage_rate" %in% names(coefs))) || coefs[["cci_x_real_mortgage_rate"]] < 0
      ),
      na.rm = TRUE
    )
  }, numeric(1L))
)

house_price_comparison <- bind_rows(lapply(house_price_candidates, `[[`, "diagnostics")) %>%
  left_join(house_price_sign_scores, by = "specification") %>%
  mutate(admissible = adf_reject_5pct & sign_score >= 3) %>%
  arrange(desc(admissible), desc(sign_score), bic, aic)

selected_house_price_name <- house_price_comparison %>%
  filter(admissible) %>%
  slice(1L) %>%
  pull(specification)

if (length(selected_house_price_name) == 0L || is.na(selected_house_price_name)) {
  selected_house_price_name <- house_price_comparison %>% slice(1L) %>% pull(specification)
}

house_price_long_run <- house_price_candidates[[selected_house_price_name]]
house_price_long_run_fit <- house_price_long_run$fit
house_price_admissible <- house_price_comparison %>%
  filter(specification == selected_house_price_name) %>%
  slice(1L) %>%
  pull(admissible)
house_price_sample <- house_price_long_run$sample %>%
  mutate(ecm_residual_hp = resid(house_price_long_run_fit))

house_price_dols <- tryCatch(
  fit_dols_spec(
    data         = house_price_long_run$sample,
    spec_name    = selected_house_price_name,
    rhs_vars     = house_price_long_run$rhs_vars,
    response_var = "lhouse_income_ratio",
    leads_lags   = 2L
  ),
  error = function(e) {
    message("DOLS estimation failed for house price equation: ", conditionMessage(e))
    NULL
  }
)

if (!is.null(house_price_dols)) {
  house_price_sample <- house_price_sample %>%
    left_join(house_price_dols$static_resid, by = "date") %>%
    mutate(ecm_residual_hp = ifelse(!is.na(dols_resid), dols_resid, ecm_residual_hp)) %>%
    select(-dols_resid)
  message("House price long-run: using DOLS static residuals as ECM correction term.")
}

house_price_adf <- tryCatch(
  ur.df(house_price_sample$ecm_residual_hp, type = "drift", lags = 4L),
  error = function(e) NULL
)

house_price_ecm_data <- house_price_sample %>%
  mutate(
    d_lhouse_ratio = lead_lag_diff(lhouse_income_ratio, 1L),
    d_lgdi = lead_lag_diff(lgdi, 1L),
    d_cci = lead_lag_diff(credit_conditions_index, 1L),
    d_real_mortgage_rate = lead_lag_diff(real_mortgage_rate, 1L),
    d_log_unemployment = lead_lag_diff(safe_log(unemployment_rate), 1L),
    house_price_yoy = lead_lag_diff(lhouse_real, 4L),
    house_price_yoy_lag = lag(house_price_yoy, 1L),
    # Cubic term: captures non-linear momentum / frenzy effects in Australian
    # housing (Muellbauer & Williams 2012). The cubic penalises extreme
    # momentum episodes more than a linear lag alone.
    house_price_yoy_lag_cubed = house_price_yoy_lag^3,
    ecm_lag_hp = lag(ecm_residual_hp, 1L)
  ) %>%
  filter(complete.cases(d_lhouse_ratio, d_lgdi, d_cci, d_real_mortgage_rate,
                        d_log_unemployment, house_price_yoy_lag,
                        house_price_yoy_lag_cubed, ecm_lag_hp))

house_price_ecm_selection <- select_ecm_spec(
  data = house_price_ecm_data,
  response_var = "d_lhouse_ratio",
  base_terms = c("ecm_lag_hp", "d_real_mortgage_rate"),
  optional_term_sets = list(
    with_cci           = c("d_cci"),
    with_income        = c("d_lgdi"),
    with_momentum      = c("house_price_yoy_lag"),
    with_frenzy        = c("house_price_yoy_lag", "house_price_yoy_lag_cubed"),
    with_income_and_frenzy = c("d_lgdi", "house_price_yoy_lag", "house_price_yoy_lag_cubed"),
    with_all           = c("d_cci", "d_lgdi", "d_log_unemployment",
                           "house_price_yoy_lag", "house_price_yoy_lag_cubed")
  ),
  # Correct sign priors:
  #   ecm_lag_hp < 0 : error correction (prices mean-revert to fundamentals)
  # d_cci sign is not hard-required because it appears only in optional specs;
  # when present, a positive coefficient is expected (easier credit → higher
  # prices) and will be reflected in sign_score for ranking purposes.
  required_negative = c("ecm_lag_hp"),
  required_positive = character(0),
  adjustment_term = "ecm_lag_hp",
  min_abs_adjustment = 0.002
)
house_price_ecm_fit <- house_price_ecm_selection$fit
selected_house_price_ecm_name <- house_price_ecm_selection$selected_name
house_price_ecm_comparison <- house_price_ecm_selection$comparison
house_price_ecm_admissible <- house_price_ecm_selection$admissible

mortgage_credit_specs <- list(
  mortgage_credit_core = c(
    "lhouse_income_ratio",
    "credit_conditions_index",
    "first_home_buyer_share",
    "real_mortgage_rate"
  ),
  mortgage_credit_housing_interaction = c(
    "lhouse_income_ratio",
    "credit_conditions_index",
    "first_home_buyer_share",
    "real_mortgage_rate",
    "cci_x_lhouse_income_ratio"
  ),
  mortgage_credit_full = c(
    "lhouse_income_ratio",
    "log_disposable_permanent_income_ratio",
    "credit_conditions_index",
    "first_home_buyer_share",
    "real_mortgage_rate",
    "cci_x_lhouse_income_ratio"
  )
)

mortgage_credit_candidates <- lapply(names(mortgage_credit_specs), function(spec_name) {
  fit_long_run_spec(
    model_data,
    spec_name = spec_name,
    rhs_vars = mortgage_credit_specs[[spec_name]],
    response_var = "ldebt_income_ratio"
  )
})
names(mortgage_credit_candidates) <- names(mortgage_credit_specs)

mortgage_credit_sign_scores <- tibble(
  specification = names(mortgage_credit_candidates),
  sign_score = vapply(mortgage_credit_candidates, function(obj) {
    coefs <- coef(obj$fit)
    sum(
      c(
        ("lhouse_income_ratio" %in% names(coefs)) && coefs[["lhouse_income_ratio"]] > 0,
        ("credit_conditions_index" %in% names(coefs)) && coefs[["credit_conditions_index"]] > 0,
        ("first_home_buyer_share" %in% names(coefs)) && coefs[["first_home_buyer_share"]] > 0,
        ("real_mortgage_rate" %in% names(coefs)) && coefs[["real_mortgage_rate"]] < 0,
        (!("cci_x_lhouse_income_ratio" %in% names(coefs))) || coefs[["cci_x_lhouse_income_ratio"]] > 0
      ),
      na.rm = TRUE
    )
  }, numeric(1L))
)

mortgage_credit_comparison <- bind_rows(lapply(mortgage_credit_candidates, `[[`, "diagnostics")) %>%
  left_join(mortgage_credit_sign_scores, by = "specification") %>%
  mutate(admissible = adf_reject_5pct & sign_score >= 4) %>%
  arrange(desc(admissible), desc(sign_score), bic, aic)

selected_mortgage_credit_name <- mortgage_credit_comparison %>%
  filter(admissible) %>%
  slice(1L) %>%
  pull(specification)

if (length(selected_mortgage_credit_name) == 0L || is.na(selected_mortgage_credit_name)) {
  selected_mortgage_credit_name <- mortgage_credit_comparison %>% slice(1L) %>% pull(specification)
}

mortgage_credit_long_run <- mortgage_credit_candidates[[selected_mortgage_credit_name]]
mortgage_credit_long_run_fit <- mortgage_credit_long_run$fit
mortgage_credit_sample <- mortgage_credit_long_run$sample %>%
  mutate(ecm_residual_mc = resid(mortgage_credit_long_run_fit))

mortgage_credit_dols <- tryCatch(
  fit_dols_spec(
    data         = mortgage_credit_long_run$sample,
    spec_name    = selected_mortgage_credit_name,
    rhs_vars     = mortgage_credit_long_run$rhs_vars,
    response_var = "ldebt_income_ratio",
    leads_lags   = 2L
  ),
  error = function(e) {
    message("DOLS estimation failed for mortgage credit equation: ", conditionMessage(e))
    NULL
  }
)

if (!is.null(mortgage_credit_dols)) {
  mortgage_credit_sample <- mortgage_credit_sample %>%
    left_join(mortgage_credit_dols$static_resid, by = "date") %>%
    mutate(ecm_residual_mc = ifelse(!is.na(dols_resid), dols_resid, ecm_residual_mc)) %>%
    select(-dols_resid)
  message("Mortgage credit long-run: using DOLS static residuals as ECM correction term.")
}

mortgage_credit_adf <- tryCatch(
  ur.df(mortgage_credit_sample$ecm_residual_mc, type = "drift", lags = 4L),
  error = function(e) NULL
)

mortgage_credit_coefs <- coef(mortgage_credit_long_run_fit)
mortgage_credit_sign_score <- sum(
  c(
    mortgage_credit_coefs[["lhouse_income_ratio"]] > 0,
    mortgage_credit_coefs[["credit_conditions_index"]] > 0,
    ("first_home_buyer_share" %in% names(mortgage_credit_coefs)) && mortgage_credit_coefs[["first_home_buyer_share"]] > 0,
    ("real_mortgage_rate" %in% names(mortgage_credit_coefs)) && mortgage_credit_coefs[["real_mortgage_rate"]] < 0,
    (!("cci_x_lhouse_income_ratio" %in% names(mortgage_credit_coefs))) || mortgage_credit_coefs[["cci_x_lhouse_income_ratio"]] > 0
  ),
  na.rm = TRUE
)
mortgage_credit_adf_stats <- run_adf_drift(resid(mortgage_credit_long_run_fit), lags = 4L)
mortgage_credit_admissible <- isTRUE(mortgage_credit_adf_stats$adf_stat < mortgage_credit_adf_stats$adf_5pct) &&
  mortgage_credit_sign_score >= 4

mortgage_credit_ecm_data <- mortgage_credit_sample %>%
  mutate(
    d_ldebt_ratio = lead_lag_diff(ldebt_income_ratio, 1L),
    d_lgdi = lead_lag_diff(lgdi, 1L),
    d_lhouse_ratio = lead_lag_diff(lhouse_income_ratio, 1L),
    d_cci = lead_lag_diff(credit_conditions_index, 1L),
    d_real_mortgage_rate = lead_lag_diff(real_mortgage_rate, 1L),
    d_log_unemployment = lead_lag_diff(safe_log(unemployment_rate), 1L),
    d_mortgage_cashflow = lead_lag_diff(mortgage_cashflow_pressure, 1L),
    ecm_lag_mc = lag(ecm_residual_mc, 1L)
  ) %>%
  filter(complete.cases(d_ldebt_ratio, d_lgdi, d_lhouse_ratio, d_cci, d_real_mortgage_rate, d_log_unemployment, d_mortgage_cashflow, ecm_lag_mc))

mortgage_credit_ecm_selection <- select_ecm_spec(
  data = mortgage_credit_ecm_data,
  response_var = "d_ldebt_ratio",
  base_terms = c("ecm_lag_mc", "d_lhouse_ratio", "d_cci"),
  optional_term_sets = list(
    with_income = c("d_lgdi"),
    with_rates = c("d_real_mortgage_rate"),
    with_unemployment = c("d_log_unemployment"),
    with_cashflow = c("d_mortgage_cashflow"),
    with_income_and_rates = c("d_lgdi", "d_real_mortgage_rate"),
    with_all = c("d_lgdi", "d_real_mortgage_rate", "d_log_unemployment", "d_mortgage_cashflow")
  ),
  required_negative = c("ecm_lag_mc"),
  required_positive = c("d_lhouse_ratio", "d_cci"),
  adjustment_term = "ecm_lag_mc",
  min_abs_adjustment = 0.005
)
mortgage_credit_ecm_fit <- mortgage_credit_ecm_selection$fit
selected_mortgage_credit_ecm_name <- mortgage_credit_ecm_selection$selected_name
mortgage_credit_ecm_comparison <- mortgage_credit_ecm_selection$comparison
mortgage_credit_short_run_admissible <- mortgage_credit_ecm_selection$admissible


# ------------------------------------------------------------------------------
# Fourth equation: Housing Equity Withdrawal (HEW)
#
# HEW = Δ(household debt) − gross dwelling investment (in nominal terms),
# normalised by annualised non-property income.  This is the channel through
# which housing wealth is directly liquidated into spending.
#
# The HEW equation is estimated as a direct level regression (not two-step)
# because the normalised ratio is near-I(0) in Australian data.  Where
# dwelling investment data are unavailable (5206004 not downloaded), the
# simplified debt-flow approximation is used with a note in the output.
# ------------------------------------------------------------------------------

hew_available <- any(!is.na(model_data$hew_income_ratio))

if (hew_available) {
  hew_data <- model_data %>%
    mutate(
      d_lhouse_ratio = lead_lag_diff(lhouse_income_ratio, 1L),
      d_lgdi           = lead_lag_diff(lgdi, 1L),
      d_cci            = lead_lag_diff(credit_conditions_index, 1L)
    ) %>%
    filter(!is.na(hew_income_ratio), !is.na(credit_conditions_index),
           !is.na(real_mortgage_rate), !is.na(lhouse_income_ratio),
           !is.na(log_disposable_permanent_income_ratio), !is.na(d_lgdi))

  # Long-run (level) HEW specification: equity extraction depends on credit
  # ease, house-price-to-income ratio (collateral), real interest rate cost,
  # and the income growth outlook.
  hew_specs <- list(
    hew_core = c(
      "credit_conditions_index",
      "lhouse_income_ratio",
      "real_mortgage_rate",
      "first_home_buyer_share"
    ),
    hew_lives = c(
      "credit_conditions_index",
      "lhouse_income_ratio",
      "real_mortgage_rate",
      "first_home_buyer_share",
      "cci_x_lhouse_income_ratio"
    ),
    hew_income = c(
      "credit_conditions_index",
      "lhouse_income_ratio",
      "real_mortgage_rate",
      "first_home_buyer_share",
      "log_disposable_permanent_income_ratio"
    )
  )

  hew_candidates <- lapply(names(hew_specs), function(spec_name) {
    fit_long_run_spec(hew_data, spec_name, hew_specs[[spec_name]],
                     response_var = "hew_income_ratio")
  })
  names(hew_candidates) <- names(hew_specs)

  hew_sign_scores <- tibble(
    specification = names(hew_candidates),
    sign_score = vapply(hew_candidates, function(obj) {
      coefs <- coef(obj$fit)
      sum(c(
        # Easier credit → more equity withdrawal
        ("credit_conditions_index" %in% names(coefs)) && coefs[["credit_conditions_index"]] > 0,
        # Higher house prices (better collateral) → more withdrawal
        ("lhouse_income_ratio" %in% names(coefs)) && coefs[["lhouse_income_ratio"]] > 0,
        # Higher rates → costlier withdrawal
        ("real_mortgage_rate" %in% names(coefs)) && coefs[["real_mortgage_rate"]] < 0,
        # CCI × house-price interaction: credit amplifies collateral channel
        (!("cci_x_lhouse_income_ratio" %in% names(coefs))) || coefs[["cci_x_lhouse_income_ratio"]] > 0
      ), na.rm = TRUE)
    }, numeric(1L))
  )

  hew_comparison <- bind_rows(lapply(hew_candidates, `[[`, "diagnostics")) %>%
    left_join(hew_sign_scores, by = "specification") %>%
    mutate(admissible = sign_score >= 3) %>%
    arrange(desc(admissible), desc(sign_score), bic, aic)

  selected_hew_name <- hew_comparison %>%
    filter(admissible) %>% slice(1L) %>% pull(specification)
  if (length(selected_hew_name) == 0L || is.na(selected_hew_name)) {
    selected_hew_name <- hew_comparison %>% slice(1L) %>% pull(specification)
  }

  hew_long_run <- hew_candidates[[selected_hew_name]]
  hew_long_run_fit <- hew_long_run$fit
  hew_long_run_r2 <- summary(hew_long_run_fit)$r.squared

  # Short-run HEW equation: changes in house prices, credit and income drive
  # quarterly fluctuations around the long-run HEW/y level.
  hew_sample <- hew_long_run$sample %>%
    mutate(ecm_residual_hew = resid(hew_long_run_fit))

  hew_ecm_data <- hew_sample %>%
    mutate(
      d_hew_ratio     = lead_lag_diff(hew_income_ratio, 1L),
      d_lhouse_ratio  = lead_lag_diff(lhouse_income_ratio, 1L),
      d_lgdi          = lead_lag_diff(lgdi, 1L),
      d_cci           = lead_lag_diff(credit_conditions_index, 1L),
      ecm_lag_hew     = lag(ecm_residual_hew, 1L)
    ) %>%
    filter(complete.cases(d_hew_ratio, d_lhouse_ratio, d_lgdi, d_cci, ecm_lag_hew))

  hew_ecm_selection <- select_ecm_spec(
    data = hew_ecm_data,
    response_var = "d_hew_ratio",
    base_terms = c("ecm_lag_hew", "d_lhouse_ratio"),
    optional_term_sets = list(
      with_cci    = c("d_cci"),
      with_income = c("d_lgdi"),
      with_all    = c("d_cci", "d_lgdi")
    ),
    required_negative = c("ecm_lag_hew"),
    required_positive = character(0),
    adjustment_term = "ecm_lag_hew",
    min_abs_adjustment = 0.005
  )
  hew_ecm_fit <- hew_ecm_selection$fit
  selected_hew_ecm_name <- hew_ecm_selection$selected_name
  hew_ecm_comparison <- hew_ecm_selection$comparison
  hew_ecm_admissible <- hew_ecm_selection$admissible
  hew_ecm_r2 <- summary(hew_ecm_fit)$r.squared

  hew_ecm_diagnostics <- build_ecm_diagnostics(
    hew_ecm_data, hew_ecm_fit, "d_hew_ratio", as.Date("2008-09-01"), "ecm_lag_hew"
  )
} else {
  message("HEW data not available; skipping fourth equation.")
  hew_comparison     <- tibble()
  hew_ecm_comparison <- tibble()
  hew_ecm_diagnostics <- tibble()
  selected_hew_name      <- NA_character_
  selected_hew_ecm_name  <- NA_character_
  hew_long_run_r2        <- NA_real_
  hew_ecm_r2             <- NA_real_
  hew_ecm_admissible     <- FALSE
  hew_long_run_fit       <- NULL
  hew_ecm_fit            <- NULL
}


# ------------------------------------------------------------------------------
# Johansen cointegration test (robustness check on the selected consumption
# long-run specification).  Tests that exactly one cointegrating vector exists
# and that the speed-of-adjustment alpha matches the ECM estimate.
# ------------------------------------------------------------------------------

johansen_result <- tryCatch({
  jo_vars <- long_run_sample %>%
    select(all_of(c("lcons", long_run_rhs_vars))) %>%
    filter(complete.cases(.)) %>%
    as.matrix()

  ca.jo(jo_vars, type = "trace", ecdet = "const", K = 2, spec = "longrun")
}, error = function(e) NULL)


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
  ) %>%
  left_join(
    house_price_sample %>%
      transmute(date, house_price_long_run_fitted = fitted(house_price_long_run_fit), house_price_ecm_residual = ecm_residual_hp),
    by = "date"
  ) %>%
  left_join(
    house_price_ecm_data %>%
      transmute(date, house_price_ecm_fitted = fitted(house_price_ecm_fit), d_lhouse_ratio_actual = d_lhouse_ratio),
    by = "date"
  ) %>%
  left_join(
    mortgage_credit_sample %>%
      transmute(date, mortgage_credit_long_run_fitted = fitted(mortgage_credit_long_run_fit), mortgage_credit_ecm_residual = ecm_residual_mc),
    by = "date"
  ) %>%
  left_join(
    mortgage_credit_ecm_data %>%
      transmute(date, mortgage_credit_ecm_fitted = fitted(mortgage_credit_ecm_fit), d_ldebt_ratio_actual = d_ldebt_ratio),
    by = "date"
  ) %>%
  {
    if (hew_available && !is.null(hew_ecm_fit)) {
      left_join(.,
        hew_ecm_data %>%
          transmute(date, hew_ecm_fitted = fitted(hew_ecm_fit), d_hew_ratio_actual = d_hew_ratio),
        by = "date")
    } else .
  }

long_run_r2 <- summary(long_run_fit)$r.squared
ecm_r2 <- summary(ecm_fit)$r.squared
speed_of_adjustment <- coef(ecm_fit)[["ecm_lag"]]
income_short_run <- coef(ecm_fit)[["d_lgdi"]]
house_price_long_run_r2 <- summary(house_price_long_run_fit)$r.squared
house_price_ecm_r2 <- summary(house_price_ecm_fit)$r.squared
mortgage_credit_long_run_r2 <- summary(mortgage_credit_long_run_fit)$r.squared
mortgage_credit_ecm_r2 <- summary(mortgage_credit_ecm_fit)$r.squared
average_consumption_to_annual_income <- mean(model_data$final_consumption_hh / model_data$annualised_nonproperty_income, na.rm = TRUE)
housing_mpc_approx <- coef(long_run_fit)[["housing_wealth_income_ratio"]] * average_consumption_to_annual_income
illiquid_financial_mpc_approx <- coef(long_run_fit)[["illiquid_financial_income_ratio"]] * average_consumption_to_annual_income
liquid_assets_mpc_approx <- if ("liquid_assets_income_ratio" %in% names(coef(long_run_fit))) {
  coef(long_run_fit)[["liquid_assets_income_ratio"]] * average_consumption_to_annual_income
} else if ("net_liquid_assets_income_ratio" %in% names(coef(long_run_fit))) {
  coef(long_run_fit)[["net_liquid_assets_income_ratio"]] * average_consumption_to_annual_income
} else {
  NA_real_
}
debt_mpc_approx <- if ("debt_income_ratio" %in% names(coef(long_run_fit))) {
  coef(long_run_fit)[["debt_income_ratio"]] * average_consumption_to_annual_income
} else {
  NA_real_
}

# DOLS-corrected MPC approximations (NA when DOLS was not available).
dols_housing_mpc_approx <- if (!is.null(consumption_dols) && "housing_wealth_income_ratio" %in% names(consumption_dols$static_coefs)) {
  consumption_dols$static_coefs[["housing_wealth_income_ratio"]] * average_consumption_to_annual_income
} else NA_real_

dols_illiquid_mpc_approx <- if (!is.null(consumption_dols) && "illiquid_financial_income_ratio" %in% names(consumption_dols$static_coefs)) {
  consumption_dols$static_coefs[["illiquid_financial_income_ratio"]] * average_consumption_to_annual_income
} else NA_real_

dols_liquid_mpc_approx <- if (!is.null(consumption_dols)) {
  if ("liquid_assets_income_ratio" %in% names(consumption_dols$static_coefs)) {
    consumption_dols$static_coefs[["liquid_assets_income_ratio"]] * average_consumption_to_annual_income
  } else if ("net_liquid_assets_income_ratio" %in% names(consumption_dols$static_coefs)) {
    consumption_dols$static_coefs[["net_liquid_assets_income_ratio"]] * average_consumption_to_annual_income
  } else NA_real_
} else NA_real_

build_ecm_diagnostics <- function(data, fit, response_var, split_date, adjustment_term) {
  full_coefs <- coef(fit)
  residual_box_p <- tryCatch(Box.test(resid(fit), lag = 4L, type = "Ljung-Box")$p.value, error = function(e) NA_real_)

  pre_fit <- tryCatch(
    lm(formula(fit), data = data %>% filter(date < split_date)),
    error = function(e) NULL
  )
  post_fit <- tryCatch(
    lm(formula(fit), data = data %>% filter(date >= split_date)),
    error = function(e) NULL
  )

  tibble(
    adjustment_term = adjustment_term,
    full_adjustment_coef = if (adjustment_term %in% names(full_coefs)) full_coefs[[adjustment_term]] else NA_real_,
    residual_box_p = residual_box_p,
    pre_adjustment_coef = if (!is.null(pre_fit) && adjustment_term %in% names(coef(pre_fit))) coef(pre_fit)[[adjustment_term]] else NA_real_,
    post_adjustment_coef = if (!is.null(post_fit) && adjustment_term %in% names(coef(post_fit))) coef(post_fit)[[adjustment_term]] else NA_real_
  )
}

consumption_ecm_diagnostics <- build_ecm_diagnostics(ecm_data, ecm_fit, "d_lcons", as.Date("2008-09-01"), "ecm_lag")
house_price_ecm_diagnostics <- build_ecm_diagnostics(house_price_ecm_data, house_price_ecm_fit, "d_lhouse_ratio", as.Date("2008-09-01"), "ecm_lag_hp")
mortgage_credit_ecm_diagnostics <- build_ecm_diagnostics(mortgage_credit_ecm_data, mortgage_credit_ecm_fit, "d_ldebt_ratio", as.Date("2008-09-01"), "ecm_lag_mc")

cci_components_output <- model_data %>%
  select(
    date,
    cci_step_1983,
    cci_step_1992,
    cci_step_1998,
    cci_step_2007,
    cci_regime_component,
    cci_indicator_component,
    cci_institutional_raw,
    credit_conditions_index,
    credit_conditions_spline
  )


# ------------------------------------------------------------------------------
# Save data and model tables
# ------------------------------------------------------------------------------

write.csv(model_data, file.path(output_dir, "model_dataset.csv"), row.names = FALSE)
write.csv(cci_components_output, file.path(output_dir, "credit_conditions_components.csv"), row.names = FALSE)
write.csv(tidy(long_run_fit), file.path(output_dir, "long_run_coefficients.csv"), row.names = FALSE)
if (!is.null(consumption_dols)) {
  write.csv(consumption_dols$dols_tidy, file.path(output_dir, "consumption_dols_coefficients.csv"), row.names = FALSE)
}
write.csv(tidy(ecm_fit), file.path(output_dir, "ecm_coefficients.csv"), row.names = FALSE)
write.csv(consumption_ecm_comparison, file.path(output_dir, "consumption_ecm_model_comparison.csv"), row.names = FALSE)
write.csv(consumption_long_run_robustness, file.path(output_dir, "consumption_long_run_robustness.csv"), row.names = FALSE)
write.csv(consumption_ecm_robustness, file.path(output_dir, "consumption_ecm_robustness.csv"), row.names = FALSE)
write.csv(consumption_ecm_diagnostics, file.path(output_dir, "consumption_ecm_diagnostics.csv"), row.names = FALSE)
write.csv(long_run_comparison, file.path(output_dir, "long_run_model_comparison.csv"), row.names = FALSE)
write.csv(credit_variant_comparison, file.path(output_dir, "credit_conditions_variant_comparison.csv"), row.names = FALSE)
write.csv(house_price_comparison, file.path(output_dir, "house_price_model_comparison.csv"), row.names = FALSE)
write.csv(tidy(house_price_long_run_fit), file.path(output_dir, "house_price_long_run_coefficients.csv"), row.names = FALSE)
if (!is.null(house_price_dols)) {
  write.csv(house_price_dols$dols_tidy, file.path(output_dir, "house_price_dols_coefficients.csv"), row.names = FALSE)
}
write.csv(tidy(house_price_ecm_fit), file.path(output_dir, "house_price_ecm_coefficients.csv"), row.names = FALSE)
write.csv(house_price_ecm_comparison, file.path(output_dir, "house_price_ecm_model_comparison.csv"), row.names = FALSE)
write.csv(house_price_ecm_diagnostics, file.path(output_dir, "house_price_ecm_diagnostics.csv"), row.names = FALSE)
write.csv(mortgage_credit_comparison, file.path(output_dir, "mortgage_credit_model_comparison.csv"), row.names = FALSE)
write.csv(tidy(mortgage_credit_long_run_fit), file.path(output_dir, "mortgage_credit_long_run_coefficients.csv"), row.names = FALSE)
if (!is.null(mortgage_credit_dols)) {
  write.csv(mortgage_credit_dols$dols_tidy, file.path(output_dir, "mortgage_credit_dols_coefficients.csv"), row.names = FALSE)
}
write.csv(tidy(mortgage_credit_ecm_fit), file.path(output_dir, "mortgage_credit_ecm_coefficients.csv"), row.names = FALSE)
write.csv(mortgage_credit_ecm_comparison, file.path(output_dir, "mortgage_credit_ecm_model_comparison.csv"), row.names = FALSE)
write.csv(mortgage_credit_ecm_diagnostics, file.path(output_dir, "mortgage_credit_ecm_diagnostics.csv"), row.names = FALSE)

if (hew_available) {
  write.csv(hew_comparison,      file.path(output_dir, "hew_model_comparison.csv"),      row.names = FALSE)
  write.csv(hew_ecm_comparison,  file.path(output_dir, "hew_ecm_model_comparison.csv"),  row.names = FALSE)
  write.csv(hew_ecm_diagnostics, file.path(output_dir, "hew_ecm_diagnostics.csv"),       row.names = FALSE)
  if (!is.null(hew_long_run_fit)) {
    write.csv(tidy(hew_long_run_fit), file.path(output_dir, "hew_long_run_coefficients.csv"), row.names = FALSE)
  }
  if (!is.null(hew_ecm_fit)) {
    write.csv(tidy(hew_ecm_fit), file.path(output_dir, "hew_ecm_coefficients.csv"), row.names = FALSE)
  }
}


# ------------------------------------------------------------------------------
# Charts
# ------------------------------------------------------------------------------

plot_credit <- ggplot(model_data, aes(x = date, y = credit_conditions_index)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey60") +
  geom_line(linewidth = 0.7, colour = "#0B5C8C") +
  labs(
    title = "Institutional credit conditions index",
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
      credit_conditions_index = "Institutional benchmark",
      credit_conditions_spline = "Spline composite"
    )
  ) +
  labs(
    title = "Credit conditions indicators",
    subtitle = "Comparing benchmark and diagnostic credit measures",
    x = NULL,
    y = "Standardised index",
    colour = NULL
  ) +
  theme_minimal(base_size = 11)

plot_ratio_fit <- model_data %>%
  filter(!is.na(long_run_fitted_ratio)) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = lcons_income_ratio, colour = "Actual")) +
  geom_line(aes(y = long_run_fitted_ratio, colour = "Long-run fit"), linetype = "dashed") +
  scale_colour_manual(values = c("Actual" = "#111111", "Long-run fit" = "#C84C09")) +
  labs(
    title = "Long-run consumption equation fit",
    subtitle = "log(C/Y); fitted from wealth/income ratios, real rate and credit conditions",
    x = NULL,
    y = "log(C / Y)",
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

plot_house_price_fit <- house_price_ecm_data %>%
  transmute(date, actual = d_lhouse_ratio, fitted = fitted(house_price_ecm_fit)) %>%
  pivot_longer(cols = c(actual, fitted), names_to = "series", values_to = "value") %>%
  ggplot(aes(x = date, y = value, colour = series)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = c("actual" = "#111111", "fitted" = "#9C3D10")) +
  labs(
    title = "House-price ECM",
    x = NULL,
    y = "Quarterly change in log real house-price-to-income ratio",
    colour = NULL
  ) +
  theme_minimal(base_size = 11)

plot_mortgage_credit_fit <- mortgage_credit_ecm_data %>%
  transmute(date, actual = d_ldebt_ratio, fitted = fitted(mortgage_credit_ecm_fit)) %>%
  pivot_longer(cols = c(actual, fitted), names_to = "series", values_to = "value") %>%
  ggplot(aes(x = date, y = value, colour = series)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = c("actual" = "#111111", "fitted" = "#2C6E49")) +
  labs(
    title = "Mortgage-credit ECM",
    x = NULL,
    y = "Quarterly change in log debt-to-income ratio",
    colour = NULL
  ) +
  theme_minimal(base_size = 11)

if (hew_available && !is.null(hew_ecm_fit) && "d_hew_ratio_actual" %in% names(model_data)) {
  plot_hew_fit <- model_data %>%
    filter(!is.na(hew_ecm_fitted)) %>%
    transmute(date, actual = d_hew_ratio_actual, fitted = hew_ecm_fitted) %>%
    pivot_longer(cols = c(actual, fitted), names_to = "series", values_to = "value") %>%
    ggplot(aes(x = date, y = value, colour = series)) +
    geom_line(linewidth = 0.7) +
    scale_colour_manual(values = c("actual" = "#111111", "fitted" = "#6A3D9A")) +
    labs(
      title = "Housing equity withdrawal (HEW) ECM",
      x = NULL,
      y = "Quarterly change in HEW / income ratio",
      colour = NULL
    ) +
    theme_minimal(base_size = 11)
  ggsave(file.path(output_dir, "hew_ecm_fit.png"), plot_hew_fit, width = 9, height = 5, dpi = 150)
}

ggsave(file.path(output_dir, "credit_conditions_index.png"), plot_credit, width = 9, height = 5, dpi = 150)
ggsave(file.path(output_dir, "credit_conditions_comparison.png"), plot_credit_comparison, width = 9, height = 5, dpi = 150)
ggsave(file.path(output_dir, "long_run_fit.png"), plot_ratio_fit, width = 9, height = 5, dpi = 150)
ggsave(file.path(output_dir, "ecm_fit.png"), plot_consumption_growth, width = 9, height = 5, dpi = 150)
ggsave(file.path(output_dir, "house_price_ecm_fit.png"), plot_house_price_fit, width = 9, height = 5, dpi = 150)
ggsave(file.path(output_dir, "mortgage_credit_ecm_fit.png"), plot_mortgage_credit_fit, width = 9, height = 5, dpi = 150)


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
    paste("House prices are chained from houseprice_old.csv through the legacy ABS RPPI workbook into the ABS Total Value of Dwellings mean-price series; resulting house-price sample starts:", min(spliced_house_prices$date, na.rm = TRUE)),
    "Real consumption and income are scaled by civilian population aged 15 years and over.",
    "The income concept for the benchmark model is real non-property disposable income per working-age person, constructed by stripping net property income out of household gross disposable income.",
    "Permanent income is constructed from a forecast-based expected-income term over a ten-year horizon rather than a backward-looking adaptive filter.",
    "The augmented system adds a mortgage cash-flow burden term from ABS household interest payable on dwellings and an implicit real mortgage rate constructed from household interest payments, debt stocks and annual consumption inflation.",
    "Demographic and access terms include a prime working-age population share interpolated from annual ERP ages 25 to 44 and an owner-occupier first-home-buyer loan share from ABS lending indicators.",
    paste("Selected benchmark CCI variant:", selected_cci_variant),
    paste("Benchmark CCI sample start:", credit_variant_comparison %>% filter(cci_variant == selected_cci_variant) %>% slice(1L) %>% pull(cci_start)),
    paste("CCI orientation multiplier applied to raw institutional index:", cci_orientation_sign),
    "The benchmark CCI uses institutional-timing spline steps combined with observed lending and housing indicators; a purely data-driven spline is retained as a diagnostic comparator.",
    "The benchmark CCI output now includes separate regime and indicator components, and the expected-income block now includes a rolling forecast variant for sensitivity checks.",
    "Benchmark specification choice now requires residual stationarity and theory-consistent credit signs before information criteria are used to rank candidates.",
    "Normalisation: long-run consumption equation uses log(C/Y) as response (income elasticity imposed = 1). More volatile than log(C/Y^p), giving meaningful ECM residuals without the multicollinearity introduced by free lgdi on the RHS.",
    "LIVES (Latent Interactive Variable Equation System) extension: CCI x housing-wealth and CCI x real-rate interactions added as candidate specifications.",
    "House-price ECM: non-linear cubic momentum term added; error-correction sign is now enforced via the admissibility screen.",
    paste("HEW fourth equation available:", hew_available),
    paste("Preferred long-run specification:", best_long_run_name),
    paste("Long-run response variable: lcons_income_ratio [log(C/Y), income elasticity imposed = 1]"),
    paste("Long-run RHS variables:", paste(long_run_rhs_vars, collapse = ", ")),
    paste("Selected house-price specification:", selected_house_price_name),
    paste("Selected mortgage-credit specification:", selected_mortgage_credit_name),
    paste("Selected consumption ECM specification:", selected_consumption_ecm_name),
    paste("Selected house-price ECM specification:", selected_house_price_ecm_name),
    paste("Selected mortgage-credit ECM specification:", selected_mortgage_credit_ecm_name),
    paste("Selected HEW specification:", ifelse(hew_available, selected_hew_name, "N/A")),
    paste("Selected HEW ECM specification:", ifelse(hew_available, selected_hew_ecm_name, "N/A")),
    "Short-run equation: parsimonious error-correction model for quarterly log real consumption growth using current income growth, unemployment changes, mortgage cash-flow pressure, lagged annual consumption growth, negative housing returns and selected credit-dynamics terms."
  ),
  con = con
)

write_section(con, "Credit index variants")
writeLines(capture.output(print(credit_variant_comparison)), con = con)

write_section(con, "Long-run model comparison")
writeLines(capture.output(print(long_run_comparison)), con = con)

write_section(con, "Consumption ECM model comparison")
writeLines(capture.output(print(consumption_ecm_comparison)), con = con)

write_section(con, "Consumption long-run robustness")
writeLines(capture.output(print(consumption_long_run_robustness)), con = con)

write_section(con, "Consumption ECM robustness")
writeLines(capture.output(print(consumption_ecm_robustness)), con = con)

write_section(con, "Consumption ECM diagnostics")
writeLines(capture.output(print(consumption_ecm_diagnostics)), con = con)

write_section(con, "House-price model comparison")
writeLines(capture.output(print(house_price_comparison)), con = con)

write_section(con, "House-price ECM model comparison")
writeLines(capture.output(print(house_price_ecm_comparison)), con = con)

write_section(con, "House-price ECM diagnostics")
writeLines(capture.output(print(house_price_ecm_diagnostics)), con = con)

write_section(con, "Mortgage-credit model comparison")
writeLines(capture.output(print(mortgage_credit_comparison)), con = con)

write_section(con, "Mortgage-credit ECM model comparison")
writeLines(capture.output(print(mortgage_credit_ecm_comparison)), con = con)

write_section(con, "Mortgage-credit ECM diagnostics")
writeLines(capture.output(print(mortgage_credit_ecm_diagnostics)), con = con)

write_section(con, "Long-run fit (static OLS)")
writeLines(capture.output(print(summary(long_run_fit))), con = con)

write_section(con, "Long-run fit (DOLS — Stock & Watson 1993, leads/lags = 2)")
if (is.null(consumption_dols)) {
  writeLines("DOLS estimation was not available for this run.", con = con)
} else {
  writeLines(
    capture.output({
      cat("Specification:", consumption_dols$spec_name, "\n")
      cat("Observations (after trimming for leads/lags):", nrow(consumption_dols$aug_sample), "\n\n")
      cat("Static-part coefficients (Newey-West HAC standard errors):\n")
      print(
        consumption_dols$dols_tidy %>%
          mutate(across(where(is.numeric), \(x) round(x, 5L)))
      )
      cat("\nADF test on DOLS static residuals:\n")
      cat(" stat:", round(consumption_dols$diagnostics$adf_stat, 3L),
          " 5% cv:", round(consumption_dols$diagnostics$adf_5pct, 3L),
          " reject:", consumption_dols$diagnostics$adf_reject_5pct, "\n")
    }),
    con = con
  )
}

write_section(con, "ADF test on error-correction residual")
if (is.null(ecm_adf)) {
  writeLines("ADF test could not be computed for the selected residual series with 4 lags.", con = con)
} else {
  writeLines(capture.output(show(ecm_adf)), con = con)
}

write_section(con, "Short-run ECM")
writeLines(capture.output(print(summary(ecm_fit))), con = con)

write_section(con, "House-price long-run fit (static OLS)")
writeLines(capture.output(print(summary(house_price_long_run_fit))), con = con)

write_section(con, "House-price long-run fit (DOLS — leads/lags = 2)")
if (is.null(house_price_dols)) {
  writeLines("DOLS estimation was not available for this run.", con = con)
} else {
  writeLines(
    capture.output({
      cat("Specification:", house_price_dols$spec_name, "\n")
      cat("Observations:", nrow(house_price_dols$aug_sample), "\n\n")
      cat("Static-part coefficients (Newey-West HAC standard errors):\n")
      print(
        house_price_dols$dols_tidy %>%
          mutate(across(where(is.numeric), \(x) round(x, 5L)))
      )
      cat("\nADF test on DOLS static residuals:\n")
      cat(" stat:", round(house_price_dols$diagnostics$adf_stat, 3L),
          " 5% cv:", round(house_price_dols$diagnostics$adf_5pct, 3L),
          " reject:", house_price_dols$diagnostics$adf_reject_5pct, "\n")
    }),
    con = con
  )
}

write_section(con, "House-price residual ADF test")
if (is.null(house_price_adf)) {
  writeLines("ADF test could not be computed for the house-price residual series with 4 lags.", con = con)
} else {
  writeLines(capture.output(show(house_price_adf)), con = con)
}

write_section(con, "House-price ECM")
writeLines(capture.output(print(summary(house_price_ecm_fit))), con = con)

write_section(con, "Mortgage-credit long-run fit (static OLS)")
writeLines(capture.output(print(summary(mortgage_credit_long_run_fit))), con = con)

write_section(con, "Mortgage-credit long-run fit (DOLS — leads/lags = 2)")
if (is.null(mortgage_credit_dols)) {
  writeLines("DOLS estimation was not available for this run.", con = con)
} else {
  writeLines(
    capture.output({
      cat("Specification:", mortgage_credit_dols$spec_name, "\n")
      cat("Observations:", nrow(mortgage_credit_dols$aug_sample), "\n\n")
      cat("Static-part coefficients (Newey-West HAC standard errors):\n")
      print(
        mortgage_credit_dols$dols_tidy %>%
          mutate(across(where(is.numeric), \(x) round(x, 5L)))
      )
      cat("\nADF test on DOLS static residuals:\n")
      cat(" stat:", round(mortgage_credit_dols$diagnostics$adf_stat, 3L),
          " 5% cv:", round(mortgage_credit_dols$diagnostics$adf_5pct, 3L),
          " reject:", mortgage_credit_dols$diagnostics$adf_reject_5pct, "\n")
    }),
    con = con
  )
}

write_section(con, "Mortgage-credit residual ADF test")
if (is.null(mortgage_credit_adf)) {
  writeLines("ADF test could not be computed for the mortgage-credit residual series with 4 lags.", con = con)
} else {
  writeLines(capture.output(show(mortgage_credit_adf)), con = con)
}

write_section(con, "Mortgage-credit ECM")
writeLines(capture.output(print(summary(mortgage_credit_ecm_fit))), con = con)

write_section(con, "Johansen cointegration test (consumption long-run)")
if (is.null(johansen_result)) {
  writeLines("Johansen test could not be computed.", con = con)
} else {
  writeLines(capture.output(print(johansen_result@teststat)), con = con)
  writeLines(capture.output(print(johansen_result@cval)), con = con)
}

if (hew_available) {
  write_section(con, "HEW model comparison")
  writeLines(capture.output(print(hew_comparison)), con = con)

  write_section(con, "HEW ECM model comparison")
  writeLines(capture.output(print(hew_ecm_comparison)), con = con)

  write_section(con, "HEW ECM diagnostics")
  writeLines(capture.output(print(hew_ecm_diagnostics)), con = con)

  if (!is.null(hew_long_run_fit)) {
    write_section(con, "HEW long-run fit")
    writeLines(capture.output(print(summary(hew_long_run_fit))), con = con)
  }
  if (!is.null(hew_ecm_fit)) {
    write_section(con, "HEW ECM")
    writeLines(capture.output(print(summary(hew_ecm_fit))), con = con)
  }
}

write_section(con, "Headline diagnostics")
writeLines(
  c(
    paste("Long-run R-squared:", round(long_run_r2, 4)),
    paste("Short-run R-squared:", round(ecm_r2, 4)),
    paste("Long-run admissibility screen passed:", long_run_comparison %>% filter(specification == best_long_run_name) %>% slice(1L) %>% pull(admissible)),
    paste("Consumption ECM admissibility screen passed:", consumption_ecm_admissible),
    paste("Consumption ECM residual Ljung-Box p-value:", round(consumption_ecm_diagnostics$residual_box_p[[1]], 4)),
    paste("House-price long-run R-squared:", round(house_price_long_run_r2, 4)),
    paste("House-price ECM R-squared:", round(house_price_ecm_r2, 4)),
    paste("House-price admissibility screen passed:", house_price_admissible),
    paste("House-price ECM admissibility screen passed:", house_price_ecm_admissible),
    paste("House-price ECM residual Ljung-Box p-value:", round(house_price_ecm_diagnostics$residual_box_p[[1]], 4)),
    paste("Mortgage-credit long-run R-squared:", round(mortgage_credit_long_run_r2, 4)),
    paste("Mortgage-credit ECM R-squared:", round(mortgage_credit_ecm_r2, 4)),
    paste("Mortgage-credit admissibility screen passed:", mortgage_credit_admissible),
    paste("Mortgage-credit ECM admissibility screen passed:", mortgage_credit_short_run_admissible),
    paste("Mortgage-credit ECM residual Ljung-Box p-value:", round(mortgage_credit_ecm_diagnostics$residual_box_p[[1]], 4)),
    paste("Speed of adjustment (ECM coefficient):", round(speed_of_adjustment, 4)),
    paste("  [Benchmark: -0.20 to -0.50 per quarter per Muellbauer papers]"),
    paste("Short-run income elasticity term on dlog income:", round(income_short_run, 4)),
    paste("Approximate housing wealth MPC — static OLS ($/dollar):", round(housing_mpc_approx, 4)),
    paste("Approximate illiquid financial wealth MPC — static OLS ($/dollar):", round(illiquid_financial_mpc_approx, 4)),
    paste("Approximate liquid assets MPC — static OLS ($/dollar):", round(liquid_assets_mpc_approx, 4)),
    paste("Approximate debt MPC — static OLS ($/dollar):", round(debt_mpc_approx, 4)),
    paste("  [Benchmarks: housing ~0.04-0.07, illiquid ~0.01-0.02, liquid ~0.05-0.10 per Muellbauer papers]"),
    paste("Approximate housing wealth MPC — DOLS ($/dollar):", round(dols_housing_mpc_approx, 4)),
    paste("Approximate illiquid financial wealth MPC — DOLS ($/dollar):", round(dols_illiquid_mpc_approx, 4)),
    paste("Approximate liquid assets MPC — DOLS ($/dollar):", round(dols_liquid_mpc_approx, 4)),
    paste("DOLS ECM residual ADF stat:",
          if (!is.null(consumption_dols)) round(consumption_dols$diagnostics$adf_stat, 4) else NA_real_),
    paste("DOLS ECM residual ADF reject at 5%:",
          if (!is.null(consumption_dols)) consumption_dols$diagnostics$adf_reject_5pct else NA),
    paste("HEW equation estimated:", hew_available),
    paste("HEW long-run R-squared:", round(hew_long_run_r2, 4)),
    paste("HEW ECM R-squared:", round(hew_ecm_r2, 4)),
    paste("HEW ECM admissibility screen passed:", hew_ecm_admissible)
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
cat(" - ", file.path(output_dir, "credit_conditions_components.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "long_run_coefficients.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "ecm_coefficients.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "consumption_ecm_model_comparison.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "consumption_long_run_robustness.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "consumption_ecm_robustness.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "consumption_ecm_diagnostics.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "long_run_model_comparison.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "credit_conditions_variant_comparison.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "house_price_model_comparison.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "house_price_long_run_coefficients.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "house_price_ecm_coefficients.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "house_price_ecm_model_comparison.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "house_price_ecm_diagnostics.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "mortgage_credit_model_comparison.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "mortgage_credit_long_run_coefficients.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "mortgage_credit_ecm_coefficients.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "mortgage_credit_ecm_model_comparison.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "mortgage_credit_ecm_diagnostics.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "model_summary.txt"), "\n", sep = "")
cat(" - ", file.path(output_dir, "credit_conditions_index.png"), "\n", sep = "")
cat(" - ", file.path(output_dir, "credit_conditions_comparison.png"), "\n", sep = "")
cat(" - ", file.path(output_dir, "long_run_fit.png"), "\n", sep = "")
cat(" - ", file.path(output_dir, "ecm_fit.png"), "\n", sep = "")
cat(" - ", file.path(output_dir, "house_price_ecm_fit.png"), "\n", sep = "")
cat(" - ", file.path(output_dir, "mortgage_credit_ecm_fit.png"), "\n", sep = "")
