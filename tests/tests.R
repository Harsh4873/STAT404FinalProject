library(testthat)
library(sim_funcs) 

# Tests for simulation_of_binary_response
test_that("simulation_of_binary_response returns correct length and binary values", {
  result <- simulation_of_binary_response(p = 0.5, n = 100)
  expect_length(result, 100)
  expect_true(all(result %in% c(0, 1)))
})

test_that("simulation_of_binary_response approximates input probability", {
  set.seed(123)
  result <- simulation_of_binary_response(p = 0.6, n = 10000)
  observed_prop <- mean(result)
  expect_true(abs(observed_prop - 0.6) < 0.01)
})

test_that("simulation_of_binary_response errors for invalid probability", {
  expect_error(simulation_of_binary_response(p = 1.2, n = 30), "Probability 'p' must be between 0 and 1.")
})

test_that("simulation_of_binary_response errors for negative sample size", {
  expect_error(simulation_of_binary_response(p = 0.5, n = -10), "Sample size 'n' must be a single positive integer.")
})

# Tests for simulation_for_two_sample_data
test_that("simulation_for_two_sample_data returns correct DataFrame structure", {
  data <- simulation_for_two_sample_data(p1 = 0.5, p2 = 0.5, n1 = 50, n2 = 50)
  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 100)
  expect_true(all(data$group %in% c(1, 2)))
  expect_true(all(data$response %in% c(0, 1)))
})

test_that("simulation_for_two_sample_data errors for invalid probabilities", {
  expect_error(simulation_for_two_sample_data(p1 = -0.1, p2 = 0.5, n1 = 30, n2 = 30), "Probabilities 'p1' and 'p2' must be between 0 and 1.")
})

test_that("simulation_for_two_sample_data errors for negative sample sizes", {
  expect_error(simulation_for_two_sample_data(p1 = 0.5, p2 = 0.5, n1 = -30, n2 = 30), "Sample sizes 'n1' and 'n2' must be positive integers.")
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

test_that("calculate_the_difference_in_proportions errors for incorrect data structure", {
  expect_error(calculate_the_difference_in_proportions(data = c(1, 2, 3)), "Input 'data' must be a data frame.")
})

test_that("calculate_the_difference_in_proportions handles missing values appropriately", {
  data <- data.frame(
    group = factor(c(rep(1, 100), rep(2, 100))),
    response = c(rep(1, 60), rep(0, 40), rep(1, 50), NA)
  )
  expect_error(calculate_the_difference_in_proportions(data), "'response' column must contain only 0s and 1s.")
})

# Tests for repeated_simulations_for_one_simulation
test_that("repeated_simulations_for_one_simulation returns correct number of simulations", {
  differences <- repeated_simulations_for_one_simulation(p1 = 0.5, p2 = 0.5, n1 = 50, n2 = 50, num_simulations = 100)
  expect_length(differences, 100)
})

test_that("repeated_simulations_for_one_simulation differences are reasonable", {
  set.seed(123)
  differences <- repeated_simulations_for_one_simulation(p1 = 0.6, p2 = 0.5, n1 = 100, n2 = 100, num_simulations = 1000)
  expect_true(all(differences >= -1 & differences <= 1))
})

test_that("repeated_simulations_for_one_simulation errors for negative numbers", {
  expect_error(repeated_simulations_for_one_simulation(p1 = 0.5, p2 = 0.5, n1 = -50, n2 = 50, num_simulations = 100), "'n1' must be a positive integer.")
})

# Tests for visualize_theoretical_sampling_distribution
test_that("visualize_theoretical_sampling_distribution generates a plot", {
  differences <- rnorm(1000, mean = 0, sd = 0.1)
  expect_silent(visualize_theoretical_sampling_distribution(simulation_results = differences, true_difference = 0, n1 = 100, n2 = 100))
})

test_that("visualize_theoretical_sampling_distribution errors for invalid inputs", {
  expect_error(visualize_theoretical_sampling_distribution(simulation_results = "invalid", true_difference = 0, n1 = 100, n2 = 100), "'simulation_results' must be a numeric vector.")
})