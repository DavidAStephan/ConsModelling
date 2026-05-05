# Tests for parse_quarter_label_date().
#
# The function takes a string whose first three characters are a quarter-end
# month abbreviation (Mar / Jun / Sep / Dec) and whose last four characters
# are a year, e.g. "Mar 2020" or "Mar2020" or "Mar-2020". It returns the
# first day of that quarter-end month. Anything not matching the {Mar, Jun,
# Sep, Dec} prefix returns NA.
#
# Note: the prompt brief mentions formats like "2020 Q1" and "1998Q3"; that
# is NOT the format this helper handles. The tests below pin the function's
# actual ABS-style behaviour.

test_that("quarter-end month abbreviations parse to first day of that month", {
  expect_equal(parse_quarter_label_date("Mar 2020"), as.Date("2020-03-01"))
  expect_equal(parse_quarter_label_date("Jun 1998"), as.Date("1998-06-01"))
  expect_equal(parse_quarter_label_date("Sep 1998"), as.Date("1998-09-01"))
  expect_equal(parse_quarter_label_date("Dec 2024"), as.Date("2024-12-01"))
})

test_that("leading and trailing whitespace is tolerated", {
  expect_equal(parse_quarter_label_date("  Mar 2020  "), as.Date("2020-03-01"))
})

test_that("malformed input returns NA", {
  # Non-quarter-end month abbreviation -> NA.
  expect_true(is.na(parse_quarter_label_date("Jan 2020")))
  expect_true(is.na(parse_quarter_label_date("Apr 2020")))
  # Random garbage -> NA.
  expect_true(is.na(parse_quarter_label_date("not a date")))
  # Empty string is too short to slice; lookup returns NA.
  expect_true(is.na(parse_quarter_label_date("")))
})

test_that("vectorised call returns a Date vector", {
  out <- parse_quarter_label_date(c("Mar 2020", "Jun 1998", "Jan 2020"))
  expect_s3_class(out, "Date")
  expect_equal(length(out), 3L)
  expect_equal(out[1], as.Date("2020-03-01"))
  expect_equal(out[2], as.Date("1998-06-01"))
  expect_true(is.na(out[3]))
})
