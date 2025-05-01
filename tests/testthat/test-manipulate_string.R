# -----------------------------------------------------------------------------#
# ---------------------------- Tests for cap_first ----------------------------#
# -----------------------------------------------------------------------------#

test_that("cap_first works for normal cases", {
  expect_equal(cap_first("hello"), "Hello")
  expect_equal(cap_first("hELLO"), "Hello")
  expect_equal(cap_first("r programming"), "R programming")
})

test_that("cap_first works for edge cases", {
  expect_equal(cap_first(""), "") # empty string
  expect_equal(cap_first("h"), "H") # single character
  expect_equal(cap_first("1world"), "1world") # starts with a number
  expect_equal(cap_first("@world"), "@world") # starts with a symbol
  expect_equal(cap_first(NA_character_), NA_character_) # NA returns NA
})

test_that("cap_first throws error for invalid inputs", {
  expect_error(cap_first(781), "`s` must be a single character string.")
  expect_error(cap_first(c("hello", "world")), "`s` must be a single character string.")
  expect_error(cap_first(list("hello")), "`s` must be a single character string.")
})

# -----------------------------------------------------------------------------#
# ---------------------------- Tests for lower_first --------------------------#
# -----------------------------------------------------------------------------#

test_that("lower_first works for normal cases", {
  expect_equal(lower_first("Hello"), "hello")
  expect_equal(lower_first("HELLO"), "hELLO")
  expect_equal(lower_first("R Programming"), "r Programming")
})

test_that("lower_first works for edge cases", {
  expect_equal(lower_first(""), "") # empty string
  expect_equal(lower_first("H"), "h") # single character
  expect_equal(lower_first("1World"), "1World") # starts with a number
  expect_equal(lower_first("@World"), "@World") # starts with a symbol
  expect_equal(lower_first(NA_character_), NA_character_) # NA returns NA
})

test_that("lower_first throws error for invalid inputs", {
  expect_error(lower_first(123), "`s` must be a single character string.")
  expect_error(lower_first(c("Hello", "World")), "`s` must be a single character string.")
  expect_error(lower_first(list("Hello")), "`s` must be a single character string.")
})
