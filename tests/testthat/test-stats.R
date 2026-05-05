# Tests for splice_house_price_series() and run_adf_drift().

# ---- splice_house_price_series ----------------------------------------------
#
# The helper splices a legacy price series onto a current series by:
#   1. Finding overlap dates.
#   2. Computing the mean log gap = log(current) - log(legacy) on the overlap.
#   3. Multiplying the legacy values by exp(gap) for dates STRICTLY before the
#      first date of the current series, then concatenating with current.
# The current series is renamed house_price_index -> house_price_spliced.

test_that("splice scales legacy by mean log gap and uses current verbatim from its start", {
  # Build a small legacy series running over 5 quarters, with a known shape.
  legacy <- tibble::tibble(
    date = as.Date(c("2000-03-01", "2000-06-01", "2000-09-01",
                     "2000-12-01", "2001-03-01")),
    house_price_old = c(100, 110, 120, 125, 130)
  )
  # Current series starts at 2000-09-01 and is exactly 2x the legacy at the
  # overlap rows, so the mean log gap = log(2) and exp(gap) = 2.
  current <- tibble::tibble(
    date = as.Date(c("2000-09-01", "2000-12-01", "2001-03-01", "2001-06-01")),
    house_price_index = c(240, 250, 260, 280)
  )

  out <- splice_house_price_series(current, legacy)

  expect_setequal(names(out), c("date", "house_price_spliced"))
  # Two pre-current legacy rows, then four current rows = 6 unique dates.
  expect_equal(nrow(out), 6L)
  # Legacy pre-current dates are scaled by exp(mean log gap) = 2.
  pre <- out[out$date < as.Date("2000-09-01"), ]
  expect_equal(pre$house_price_spliced, c(200, 220))
  # Current dates carry the raw current values.
  post <- out[out$date >= as.Date("2000-09-01"), ]
  expect_equal(post$house_price_spliced, c(240, 250, 260, 280))
  # Output is sorted ascending in date.
  expect_equal(out$date, sort(out$date))
})

test_that("splice errors with fewer than two overlapping observations", {
  legacy <- tibble::tibble(
    date = as.Date(c("2000-03-01", "2000-06-01")),
    house_price_old = c(100, 110)
  )
  current <- tibble::tibble(
    date = as.Date(c("2000-06-01", "2000-09-01")),  # only one overlap row
    house_price_index = c(220, 230)
  )
  expect_error(
    splice_house_price_series(current, legacy),
    regexp = "two overlapping observations"
  )
})

# ---- run_adf_drift ----------------------------------------------------------
#
# Wraps urca::ur.df(type = "drift"). Should return a one-row tibble with
# adf_stat plus 1/5/10% critical values. On a stationary series the test
# statistic should be MORE NEGATIVE than the 5pct critical value (reject
# unit root). On a true random walk it should NOT.

test_that("ADF rejects unit root on a clearly stationary series", {
  set.seed(42)
  x <- rnorm(200)  # iid normal, classic stationary case
  out <- run_adf_drift(x, lags = 4L)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("adf_stat", "adf_1pct", "adf_5pct", "adf_10pct"))
  expect_equal(nrow(out), 1L)
  expect_true(is.finite(out$adf_stat))
  # Stationary -> reject (test statistic below the 5% critical value).
  expect_lt(out$adf_stat, out$adf_5pct)
})

test_that("ADF does NOT reject unit root on a random walk", {
  set.seed(42)
  x <- cumsum(rnorm(200))  # I(1)
  out <- run_adf_drift(x, lags = 4L)
  expect_true(is.finite(out$adf_stat))
  # Random walk -> do not reject (statistic above the 5% critical value).
  expect_gt(out$adf_stat, out$adf_5pct)
})

test_that("ADF returns NA tibble on input that ur.df cannot handle", {
  # Constant series: ur.df errors out internally (singular regression).
  out <- run_adf_drift(rep(1, 50), lags = 4L)
  expect_s3_class(out, "tbl_df")
  expect_true(is.na(out$adf_stat))
  expect_true(is.na(out$adf_5pct))
})
