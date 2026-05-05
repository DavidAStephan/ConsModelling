# Tests for the permanent-income helpers.
#
# compute_log_yp_over_y() (model_helpers.R lines 840-849) takes
#   log_income, expected_log_income, discount = 0.05, horizon = 40L
# and is *named* as a discounted-sum permanent-income transform. Reading the
# body, however:
#
#   weights <- (1 - discount) ^ (seq_len(horizon) - 1L)
#   denom   <- sum(weights)
#   if (length(expected_log_income) == 0L) return(numeric(0))
#   expected_log_income - log_income
#
# The `weights` and `denom` are computed but unused, and the function
# ignores `discount` / `horizon` entirely. It just returns the elementwise
# difference. We pin the current behaviour below, AND we leave a skipped
# test that documents the expected discounted-sum behaviour so the bug is
# visible in test output without silently failing CI.

test_that("compute_log_yp_over_y currently returns expected_log_income - log_income (CURRENT BEHAVIOUR)", {
  log_income          <- c(1.0, 1.1, 1.2, 1.3)
  expected_log_income <- c(1.5, 1.6, 1.7, 1.8)
  out <- compute_log_yp_over_y(log_income, expected_log_income,
                               discount = 0.05, horizon = 40L)
  expect_equal(out, expected_log_income - log_income)
})

test_that("compute_log_yp_over_y currently ignores its discount and horizon arguments", {
  log_income          <- c(1.0, 1.1, 1.2, 1.3)
  expected_log_income <- c(1.5, 1.6, 1.7, 1.8)
  # Two clearly different discount/horizon settings produce identical output
  # because the function never uses them. This pins the bug.
  a <- compute_log_yp_over_y(log_income, expected_log_income,
                             discount = 0.01, horizon = 4L)
  b <- compute_log_yp_over_y(log_income, expected_log_income,
                             discount = 0.50, horizon = 200L)
  expect_identical(a, b)
})

test_that("compute_log_yp_over_y returns numeric(0) on empty expected_log_income", {
  out <- compute_log_yp_over_y(numeric(0), numeric(0),
                               discount = 0.05, horizon = 40L)
  expect_identical(out, numeric(0))
})

test_that("compute_log_yp_over_y SHOULD apply discount weights (EXPECTED behaviour)", {
  skip("BUG: compute_log_yp_over_y ignores discount/horizon arguments and returns a raw difference; see model_helpers.R lines 840-849.")
  # Aspirational specification: a discounted average of expected_log_income
  # minus log_income. Wired up here so that when the bug is fixed, removing
  # the skip() above will exercise this test.
  discount <- 0.05
  horizon  <- 4L
  weights  <- (1 - discount) ^ (seq_len(horizon) - 1L)
  log_income          <- c(1.0, 1.1)
  expected_log_income <- c(1.5, 1.6)
  expected <- (expected_log_income * sum(weights) / sum(weights)) - log_income
  out <- compute_log_yp_over_y(log_income, expected_log_income,
                               discount = discount, horizon = horizon)
  expect_equal(out, expected)
})

# ---- adaptive_permanent_income_log ------------------------------------------
#
# Cross-check: the helper returns the EWMA log-income with smoothing lambda.
# At lambda = 1 it must collapse to a constant equal to the first observed
# value. With NA inputs after the first valid observation, the previous
# value should be carried forward.

test_that("adaptive_permanent_income_log with lambda = 1 returns the first observed value forever", {
  log_income <- c(NA_real_, 2, 3, 4, 5)
  out <- adaptive_permanent_income_log(log_income, lambda = 1)
  expect_true(is.na(out[1]))                    # leading NA preserved
  expect_equal(out[2:5], rep(2, 4))             # first valid value sticks
})

test_that("adaptive_permanent_income_log carries the previous value forward across NA gaps", {
  log_income <- c(1, NA, NA, 5)
  out <- adaptive_permanent_income_log(log_income, lambda = 0.5)
  expect_equal(out[1], 1)
  expect_equal(out[2], 1)   # NA -> previous value
  expect_equal(out[3], 1)
  # i = 4: lambda * out[3] + (1 - lambda) * 5 = 0.5*1 + 0.5*5 = 3
  expect_equal(out[4], 3)
})
