# Tests for rescale_to_millions().
#
# The function reads a `unit` label, lowercases / squishes it, and rescales
# the numeric x accordingly: $ Billions / Billions -> *1000, $ Millions /
# Millions -> unchanged, anything else -> passthrough.

test_that("billions are multiplied by 1000", {
  expect_equal(rescale_to_millions(1.5, "$ Billions"), 1500)
  expect_equal(rescale_to_millions(2,   "Billions"),   2000)
  # Case- and whitespace-insensitive: lowercase and str_squish are applied.
  expect_equal(rescale_to_millions(1, "  $ BILLIONS  "), 1000)
  expect_equal(rescale_to_millions(1, "billions"),       1000)
})

test_that("millions pass through unchanged", {
  expect_equal(rescale_to_millions(42, "$ Millions"), 42)
  expect_equal(rescale_to_millions(7,  "Millions"),    7)
  expect_equal(rescale_to_millions(0,  "$ MILLIONS"),  0)
})

test_that("unrecognised units are returned unchanged (passthrough branch)", {
  # The TRUE ~ x branch in the case_when keeps the raw value.
  expect_equal(rescale_to_millions(123, "Index"), 123)
  expect_equal(rescale_to_millions(0.5, "Per cent"), 0.5)
})

test_that("NA inputs are preserved", {
  # case_when carries NA on the value side through.
  expect_true(is.na(rescale_to_millions(NA_real_, "$ Billions")))
  expect_true(is.na(rescale_to_millions(NA_real_, "$ Millions")))
})

test_that("vectorisation works element-wise", {
  x     <- c(1, 2, 3)
  units <- c("$ Billions", "$ Millions", "Index")
  expect_equal(rescale_to_millions(x, units), c(1000, 2, 3))
})
