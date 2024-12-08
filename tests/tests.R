library(testthat)
library(here)
library(ggplot2)
source(here("R", "sim_funcs.R"))

# Tests for simulation_of_binary_response

# Verify output is a binary vector of correct length
test_that("simulation_of_binary_response returns a binary vector of the correct length", {
  result <- simulation_of_binary_response(p = 0.5, n = 100)
  expect_equal(length(result), 100)
  expect_setequal(unique(result), c(0, 1))
})

# Check proportion of 1s approximates input probability
test_that("simulation_of_binary_response produces expected proportion", {
  set.seed(123)
  result <- simulation_of_binary_response(p = 0.6, n = 10000)
  expect_equal(mean(result), 0.6, tolerance = 0.01)
})


# Confirm output values are 0 or 1
test_that("simulation_of_binary_response errors for invalid probability", {
  expect_error(simulation_of_binary_response(p = 1.2, n = 30), "p has to be between 0 and 1.")
})

# Check if it gives error for negative sample size
test_that("simulation_of_binary_response errors for negative sample size", {
  expect_error(simulation_of_binary_response(p = 0.5, n = -10), "n has to be a single positive integer.")
})

# Tests for simulation_for_two_sample_data

# Confirm DataFrame creation with correct group labels
test_that("simulation_for_two_sample_data returns correct structure and values", {
  data <- simulation_for_two_sample_data(p1 = 0.5, p2 = 0.5, n1 = 50, n2 = 50)
  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 100)
  expect_setequal(unique(data$group), c(1, 2))
  expect_setequal(unique(data$response), c(0, 1))
})

# Check if it gives error for probabilities outside [0, 1]
test_that("simulation_for_two_sample_data errors for invalid probabilities", {
  expect_error(simulation_for_two_sample_data(p1 = -0.1, p2 = 0.5, n1 = 30, n2 = 30), "p1 and p2 have to be between 0 and 1.")
})

# Check if it gives error  for negative sample size
test_that("simulation_for_two_sample_data errors for negative sample sizes", {
  expect_error(simulation_for_two_sample_data(p1 = 0.5, p2 = 0.5, n1 = -30, n2 = 30), "n1 and n2 have to be positive integers.")
})

# Tests for calculate_the_difference_in_proportions

test_that("calculate_the_difference_in_proportions returns correct difference", {
  data <- data.frame(
    group = factor(c(rep(1, 100), rep(2, 100))),
    response = c(rep(1, 60), rep(0, 40), rep(1, 50), rep(0, 50))
  )
  difference <- calculate_the_difference_in_proportions(data)
  expect_equal(difference, 0.6 - 0.5)
})

# Check if it gives error for incorrect data structure
test_that("calculate_the_difference_in_proportions errors for incorrect data structure", {
  expect_error(calculate_the_difference_in_proportions(data = c(1, 2, 3)), "Input data has to be a data frame.")
})

# Test that group column must be a factor
test_that("calculate_the_difference_in_proportions errors when group is not a factor", {
  data <- data.frame(
    group = c(rep(1, 100), rep(2, 100)),
    response = c(rep(1, 60), rep(0, 40), rep(1, 50), rep(0, 50))
  )
  expect_error(calculate_the_difference_in_proportions(data), "group column has to be a factor.")
})

# Tests for repeated_simulations_for_one_simulation

# Confirm correct number of simulations
test_that("repeated_simulations_for_one_simulation returns correct number of simulations", {
  differences <- repeated_simulations_for_one_simulation(p1 = 0.5, p2 = 0.5, n1 = 50, n2 = 50, num_simulations = 100)
  expect_length(differences, 100)
})

# Check if differences are reasonable
test_that("repeated_simulations_for_one_simulation differences are reasonable", {
  set.seed(123)
  differences <- repeated_simulations_for_one_simulation(p1 = 0.6, p2 = 0.5, n1 = 100, n2 = 100, num_simulations = 1000)
  expect_true(all(differences >= -1 & differences <= 1))
})

# Check if it gives error for negative numbers
test_that("repeated_simulations_for_one_simulation errors for negative numbers", {
  expect_error(repeated_simulations_for_one_simulation(p1 = 0.5, p2 = 0.5, n1 = -50, n2 = 50, num_simulations = 100), "n1 and n2 have to be positive integers.")
})

# Tests for visualize_theoretical_sampling_distribution

# Confirm plot
test_that("visualize_theoretical_sampling_distribution generates a plot", {
  differences <- rnorm(1000, mean = 0, sd = 0.1)
  expect_silent(visualize_theoretical_sampling_distribution(simulation_results = differences, true_difference = 0, n1 = 100, n2 = 100))
})

# Check if it gives error for invalid inputs
test_that("visualize_theoretical_sampling_distribution errors for invalid inputs", {
  expect_error(visualize_theoretical_sampling_distribution(simulation_results = "invalid", true_difference = 0, n1 = 100, n2 = 100), "simulation_results has to be a numeric vector.")
})

# Output for sim_funcs.R tests: 
# ==> Testing R file using 'testthat'
# 
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 19 ]
# Test complete

source(here("R", "CL_funcs.R"))

# Tests for visualize_confidence_levels function

test_that("visualize_confidence_levels produces valid plot with correct input", {
  set.seed(123)
  plot <- visualize_confidence_levels(
    p1 = 0.6, 
    p2 = 0.5, 
    n1 = 30, 
    n2 = 30, 
    rep_steps = c(10, 50)
  )
  # Check if output is a ggplot object
  expect_s3_class(plot, "ggplot")
  
  # Check if essential plot elements are present
  expect_s3_class(plot$layers[[1]]$geom, "GeomLine")
  expect_s3_class(plot$layers[[2]]$geom, "GeomHline")

})

test_that("visualize_confidence_levels errors for invalid proportions", {
  # Test p1 > 1
  expect_error(
    visualize_confidence_levels(p1 = 1.2, p2 = 0.5, n1 = 30, n2 = 30, rep_steps = c(10, 50)),
    "Proportions p1 and p2 have to be between 0 and 1."
  )
  
  # Test p2 < 0
  expect_error(
    visualize_confidence_levels(p1 = 0.5, p2 = -0.1, n1 = 30, n2 = 30, rep_steps = c(10, 50)),
    "Proportions p1 and p2 have to be between 0 and 1."
  )
})

test_that("visualize_confidence_levels errors for invalid sample sizes", {
  # Test negative n1
  expect_error(
    visualize_confidence_levels(p1 = 0.6, p2 = 0.5, n1 = -10, n2 = 30, rep_steps = c(10, 50)),
    "Sample sizes n1 and n2 have to be positive integers."
  )
  
  # Test zero n2
  expect_error(
    visualize_confidence_levels(p1 = 0.6, p2 = 0.5, n1 = 30, n2 = 0, rep_steps = c(10, 50)),
    "Sample sizes n1 and n2 have to be positive integers."
  )
})

test_that("visualize_confidence_levels errors for invalid rep_steps", {
  # Test non-numeric rep_steps
  expect_error(
    visualize_confidence_levels(p1 = 0.6, p2 = 0.5, n1 = 30, n2 = 30, rep_steps = "100"),
    "Repetition steps have to be a numeric vector of positive values."
  )
  
  # Test negative rep_steps
  expect_error(
    visualize_confidence_levels(p1 = 0.6, p2 = 0.5, n1 = 30, n2 = 30, rep_steps = c(10, -50)),
    "Repetition steps have to be a numeric vector of positive values."
  )
})

test_that("visualize_confidence_levels produces reasonable confidence levels", {
  set.seed(123)
  # Use larger sample sizes and repetitions for more stable results
  plot <- visualize_confidence_levels(
    p1 = 0.6, 
    p2 = 0.5, 
    n1 = 100, 
    n2 = 100, 
    rep_steps = c(100, 200),
    alpha = 0.05
  )
  
  # Extract data from the plot
  plot_data <- ggplot_build(plot)$data[[1]]
  
  # Check if proportions are between 0 and 1
  expect_true(all(plot_data$y >= 0 & plot_data$y <= 1))
  
  # Check if proportions are close to expected confidence level (0.95)
  expect_true(all(abs(plot_data$y - 0.95) < 0.2))
})

# Output after CL_funcs.R tests: 
# ==> Testing R file using 'testthat'
# 
# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 30 ]
# Test complete

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

test_that("calc_bootstrap_stats errors when x1 is non-numeric", {
  x1 <- "ten"
  n1 <- 20
  x2 <- 15
  n2 <- 25
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "All inputs \\(x1, n1, x2, n2, n_bootstrap, conf_level\\) have to be numeric."
  )
})

test_that("calc_bootstrap_stats errors when n1 is non-numeric", {
  x1 <- 10
  n1 <- "twenty"
  x2 <- 15
  n2 <- 25
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "All inputs \\(x1, n1, x2, n2, n_bootstrap, conf_level\\) have to be numeric."
  )
})

test_that("calc_bootstrap_stats errors when x2 is non-numeric", {
  x1 <- 10
  n1 <- 20
  x2 <- "fifteen"
  n2 <- 25
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "All inputs \\(x1, n1, x2, n2, n_bootstrap, conf_level\\) have to be numeric."
  )
})

test_that("calc_bootstrap_stats errors when n2 is non-numeric", {
  x1 <- 10
  n1 <- 20
  x2 <- 15
  n2 <- "twenty-five"
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "All inputs \\(x1, n1, x2, n2, n_bootstrap, conf_level\\) have to be numeric."
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
    "All inputs \\(x1, n1, x2, n2, n_bootstrap, conf_level\\) have to be numeric."
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
    "All inputs \\(x1, n1, x2, n2, n_bootstrap, conf_level\\) have to be numeric."
  )
})

test_that("calc_bootstrap_stats errors when x1 is not an integer", {
  x1 <- 10.5
  n1 <- 20
  x2 <- 15
  n2 <- 25
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "x1, n1, x2, n2, and n_bootstrap have to be integers."
  )
})

test_that("calc_bootstrap_stats errors when n1 is not an integer", {
  x1 <- 10
  n1 <- 20.5
  x2 <- 15
  n2 <- 25
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "x1, n1, x2, n2, and n_bootstrap have to be integers."
  )
})

test_that("calc_bootstrap_stats errors when x2 is not an integer", {
  x1 <- 10
  n1 <- 20
  x2 <- 15.5
  n2 <- 25
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "x1, n1, x2, n2, and n_bootstrap have to be integers."
  )
})

test_that("calc_bootstrap_stats errors when n2 is not an integer", {
  x1 <- 10
  n1 <- 20
  x2 <- 15
  n2 <- 25.5
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "x1, n1, x2, n2, and n_bootstrap have to be integers."
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
    "x1, n1, x2, n2, and n_bootstrap have to be integers."
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
    "Number of observations \\(n1, n2\\) have to be greater than zero."
  )
  
  n1 <- -20
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "Number of observations \\(n1, n2\\) have to be greater than zero."
  )
})

test_that("calc_bootstrap_stats errors when n2 is zero or negative", {
  x1 <- 10
  n1 <- 20
  x2 <- 15
  n2 <- 0
  
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "Number of observations \\(n1, n2\\) have to be greater than zero."
  )
  
  n2 <- -25
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2),
    "Number of observations \\(n1, n2\\) have to be greater than zero."
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
    "n_bootstrap have to be a positive integer."
  )
  
  n_bootstrap <- -100
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2, n_bootstrap),
    "n_bootstrap have to be a positive integer."
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
    "conf_level have to be a number between 0 and 1."
  )
  
  conf_level <- 0
  expect_error(
    calc_bootstrap_stats(x1, n1, x2, n2, conf_level = conf_level),
    "conf_level have to be a number between 0 and 1."
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

# Output after bootstrapping_funcs.R tests: 
# ==> Testing R file using 'testthat'

# [ FAIL 0 | WARN 0 | SKIP 0 | PASS 64 ]
# Test complete