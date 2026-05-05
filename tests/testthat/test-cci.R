# Smoke tests for the credit-conditions index pipeline:
#   - build_credit_regime_basis() should emit four ramp series in [0, 1].
#   - construct_institutional_cci() should run on a small fabricated master-
#     like tibble and return the expected three columns.

# Build a quarterly date vector spanning the regime breakpoints so all four
# ramps reach 1 at the end and start at 0 well before the first breakpoint.
.quarterly_dates <- function(from = "1980-03-01", to = "2015-12-01") {
  seq(as.Date(from), as.Date(to), by = "quarter")
}

test_that("build_credit_regime_basis returns four ramps clipped to [0, 1]", {
  dates <- .quarterly_dates()
  basis <- build_credit_regime_basis(dates)

  expect_named(basis,
               c("date", "cci_step_1983", "cci_step_1992",
                 "cci_step_1998", "cci_step_2007"))
  expect_equal(nrow(basis), length(dates))
  ramps <- basis[, -1]
  for (col in names(ramps)) {
    vals <- ramps[[col]]
    expect_true(all(vals >= 0 & vals <= 1),
                info = paste("ramp out of [0,1]:", col))
  }
  # Earliest ramp should be saturated at 1 by the end of the window.
  expect_equal(basis$cci_step_1983[nrow(basis)], 1)
  # And at zero well before its start (1980-03-01 < 1982-03-01).
  expect_equal(basis$cci_step_1983[1], 0)
})

test_that("construct_institutional_cci returns standardised CCI columns on a fabricated master", {
  set.seed(42)
  dates <- .quarterly_dates()
  n <- length(dates)
  # Fabricate plausible inputs. Exact values are not load-bearing; we only
  # verify the function runs and returns the expected shape.
  master <- tibble::tibble(
    date                   = dates,
    housing_loan_flow      = exp(seq(0, 2, length.out = n)) + abs(rnorm(n, sd = 0.05)),
    house_price_index      = 100 * exp(cumsum(rnorm(n, mean = 0.01, sd = 0.02))),
    debt_income_ratio      = seq(0.5, 1.8, length.out = n),
    first_home_buyer_share = 0.2 + 0.05 * sin(seq_len(n) / 4),
    real_mortgage_rate     = 5 + rnorm(n, sd = 0.3)
  )
  basis <- build_credit_regime_basis(dates)

  out <- construct_institutional_cci(master, basis)

  expect_named(out,
               c("date", "cci_regime_component",
                 "cci_indicator_component", "cci_institutional_raw"))
  expect_equal(nrow(out), n)
  # Standardised columns: mean ~ 0, sd ~ 1 over non-NA observations.
  for (col in c("cci_regime_component", "cci_indicator_component",
                "cci_institutional_raw")) {
    v <- out[[col]]
    v <- v[!is.na(v)]
    expect_lt(abs(mean(v)), 1e-8, label = paste("mean of", col))
    expect_equal(sd(v), 1, tolerance = 1e-8, label = paste("sd of", col))
  }
})
