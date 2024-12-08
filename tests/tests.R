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
    "Repetition steps (rep_steps) have to be a numeric vector of positive values."
  )
  
  # Test negative rep_steps
  expect_error(
    visualize_confidence_levels(p1 = 0.6, p2 = 0.5, n1 = 30, n2 = 30, rep_steps = c(10, -50)),
    "Repetition steps (rep_steps) have to be a numeric vector of positive values."
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