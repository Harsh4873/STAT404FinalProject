library(testthat)
library(here)

source(here("R", "bootstrapping_funcs.R"))
  
test_that("calc_bootstrap_stats works on valid simple input", {
  # test with good good inputs
  x1 <- 10
  n1 <- 20
  x2 <- 15
  n2 <- 25
  n_bootstrap <- 1000
  conf_level <- 0.95
  
  result <- calc_bootstrap_stats(x1, n1, x2, n2, n_bootstrap, conf_level)
  
  # verify type
  expect_type(result, "list")
  
  # verify list has what we need
  expected_components <- c("diff_hat", "SE_theoretical", "SE_bootstrap",
                           "CI_theoretical", "CI_bootstrap",
                           "bootstrap_diffs", "conf_level")
  expect_true(all(expected_components %in% names(result)))
  
  # verify validity of output
  expected_diff_hat <- (x1 / n1) - (x2 / n2)
  expect_equal(result$diff_hat, expected_diff_hat)
  
  # verify length
  expect_length(result$bootstrap_diffs, n_bootstrap)
  
  # verify CI
  expect_equal(result$conf_level, conf_level)
  
})


# Input tests (there are too many), self explanatory 


test_that("calc_bootstrap_stats errors when x1 is non-numeric", {
  x1 <- "ten"
  n1 <- 20
  x2 <- 15
  n2 <- 25
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "All inputs \\(x1, n1, x2, n2, n_bootstrap, conf_level\\) must be numeric."
  )
})

test_that("calc_bootstrap_stats errors when n1 is non-numeric", {
  x1 <- 10
  n1 <- "twenty"
  x2 <- 15
  n2 <- 25
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "All inputs \\(x1, n1, x2, n2, n_bootstrap, conf_level\\) must be numeric."
  )
})

test_that("calc_bootstrap_stats errors when x2 is non-numeric", {
  x1 <- 10
  n1 <- 20
  x2 <- "fifteen"
  n2 <- 25
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "All inputs \\(x1, n1, x2, n2, n_bootstrap, conf_level\\) must be numeric."
  )
})

test_that("calc_bootstrap_stats errors when n2 is non-numeric", {
  x1 <- 10
  n1 <- 20
  x2 <- 15
  n2 <- "twenty-five"
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "All inputs \\(x1, n1, x2, n2, n_bootstrap, conf_level\\) must be numeric."
  )
})

test_that("calc_bootstrap_stats errors when n_bootstrap is non-numeric", {
  x1 <- 10
  n1 <- 20
  x2 <- 15
  n2 <- 25
  n_bootstrap <- "one thousand"
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2, n_bootstrap),
    "All inputs \\(x1, n1, x2, n2, n_bootstrap, conf_level\\) must be numeric."
  )
})

test_that("calc_bootstrap_stats errors when conf_level is non-numeric", {
  x1 <- 10
  n1 <- 20
  x2 <- 15
  n2 <- 25
  conf_level <- "ninety-five percent"
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2, conf_level = conf_level),
    "All inputs \\(x1, n1, x2, n2, n_bootstrap, conf_level\\) must be numeric."
  )
})

test_that("calc_bootstrap_stats errors when x1 is not an integer", {
  x1 <- 10.5
  n1 <- 20
  x2 <- 15
  n2 <- 25
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "x1, n1, x2, n2, and n_bootstrap must be an integer)."
  )
})

test_that("calc_bootstrap_stats errors when n1 is not an integer", {
  x1 <- 10
  n1 <- 20.5
  x2 <- 15
  n2 <- 25
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "x1, n1, x2, n2, and n_bootstrap must be an integer)."
  )
})

test_that("calc_bootstrap_stats errors when x2 is not an integer", {
  x1 <- 10
  n1 <- 20
  x2 <- 15.5
  n2 <- 25
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "x1, n1, x2, n2, and n_bootstrap must be an integer)."
  )
})

test_that("calc_bootstrap_stats errors when n2 is not an integer", {
  x1 <- 10
  n1 <- 20
  x2 <- 15
  n2 <- 25.5
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "x1, n1, x2, n2, and n_bootstrap must be an integer)."
  )
})

test_that("calc_bootstrap_stats errors when n_bootstrap is not an integer", {
  x1 <- 10
  n1 <- 20
  x2 <- 15
  n2 <- 25
  n_bootstrap <- 1000.5
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2, n_bootstrap),
    "x1, n1, x2, n2, and n_bootstrap must be an integer )."
  )
})

test_that("calc_bootstrap_stats errors when x1 is negative", {
  x1 <- -10
  n1 <- 20
  x2 <- 15
  n2 <- 25
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "Number of successes \\(x1, x2\\) cannot be negative."
  )
})

test_that("calc_bootstrap_stats errors when x2 is negative", {
  x1 <- 10
  n1 <- 20
  x2 <- -15
  n2 <- 25
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "Number of successes \\(x1, x2\\) cannot be negative."
  )
})

test_that("calc_bootstrap_stats errors when n1 is zero or negative", {
  x1 <- 10
  n1 <- 0
  x2 <- 15
  n2 <- 25
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "Number of observations \\(n1, n2\\) must be greater than zero."
  )
  
  n1 <- -20
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "Number of observations \\(n1, n2\\) must be greater than zero."
  )
})

test_that("calc_bootstrap_stats errors when n2 is zero or negative", {
  x1 <- 10
  n1 <- 20
  x2 <- 15
  n2 <- 0
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "Number of observations \\(n1, n2\\) must be greater than zero."
  )
  
  n2 <- -25
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "Number of observations \\(n1, n2\\) must be greater than zero."
  )
})

test_that("calc_bootstrap_stats errors when x1 exceeds n1", {
  x1 <- 25
  n1 <- 20
  x2 <- 15
  n2 <- 25
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "Number of successes x1 cannot exceed number of observations n1."
  )
})

test_that("calc_bootstrap_stats errors when x2 exceeds n2", {
  x1 <- 10
  n1 <- 20
  x2 <- 30
  n2 <- 25
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "Number of successes x2 cannot exceed number of observations n2."
  )
})

test_that("calc_bootstrap_stats errors when n_bootstrap is zero or negative", {
  x1 <- 10
  n1 <- 20
  x2 <- 15
  n2 <- 25
  
  n_bootstrap <- 0
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2, n_bootstrap),
    "n_bootstrap must be a positive integer."
  )
  
  n_bootstrap <- -100
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2, n_bootstrap),
    "n_bootstrap must be a positive integer."
  )
})

test_that("calc_bootstrap_stats errors when conf_level is not between 0 and 1", {
  x1 <- 10
  n1 <- 20
  x2 <- 15
  n2 <- 25
  
  conf_level <- 1.5
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2, conf_level = conf_level),
    "conf_level must be a number between 0 and 1."
  )
  
  conf_level <- 0
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2, conf_level = conf_level),
    "conf_level must be a number between 0 and 1."
  )
})


#test valid ranges here
test_that("calc_bootstrap_stats returns valid numerical ranges", {
  x1 <- 10
  n1 <- 20
  x2 <- 15
  n2 <- 25
  result <- calc_bootstrap_stats(x1, n1, x2, n2)
  

  expect_true(result$diff_hat >= -1 && result$diff_hat <= 1)
  

  expect_true(result$SE_theoretical >= 0)
  expect_true(result$SE_bootstrap >= 0)
  

  expect_true(result$CI_theoretical[1] <= result$CI_theoretical[2])
  expect_true(result$CI_bootstrap[1] <= result$CI_bootstrap[2])
})


# all of our inputs are validated, so just make sure it works here
test_that("visualize_bootstrap_stats runs without errors", {
  x1 <- 10
  n1 <- 20
  x2 <- 15
  n2 <- 25
  result <- calc_bootstrap_stats(x1, n1, x2, n2)
  
  expect_silent({
    visualize_bootstrap_stats(result$bootstrap_diffs, result$CI_theoretical, result$CI_bootstrap)
  })
})
