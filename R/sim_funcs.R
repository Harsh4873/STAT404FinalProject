# simulation_of_binary_response
simulation_of_binary_response <- function(p, n) {
  
  # error checking 
  if (!is.numeric(p) || length(p) != 1) {
    stop("Probability 'p' must be a single numeric value.")
  }
  if (p < 0 || p > 1) {
    stop("Probability 'p' must be between 0 and 1.")
  }
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != floor(n)) {
    stop("Sample size 'n' must be a single positive integer.")
  }

  # Get n random samples from the distribution with probability p
  responses <- rbinom(n, size = 1, prob = p)

  # Return binary vector
  return(responses)
}

# simulation_for_two_sample_data
simulation_for_two_sample_data <- function(p1, p2, n1, n2) {
  
  # error checking 
  args <- list(p1 = p1, p2 = p2, n1 = n1, n2 = n2)
  for (arg_name in names(args)) {
    arg <- args[[arg_name]]
    if (!is.numeric(arg) || length(arg) != 1) {
      stop(paste("'", arg_name, "' must be a single numeric value.", sep = ""))
    }
  }
  
  if (p1 < 0 || p1 > 1 || p2 < 0 || p2 > 1) {
    stop("Probabilities 'p1' and 'p2' must be between 0 and 1.")
  }
  if (n1 <= 0 || n1 != floor(n1) || n2 <= 0 || n2 != floor(n2)) {
    stop("Sample sizes 'n1' and 'n2' must be positive integers.")
  }

  # Get binary responses for group 1 using p1 and n1
  # Get binary responses for group 2 using p2 and n2
  group1_responses <- simulation_of_binary_response(p1, n1)
  group2_responses <- simulation_of_binary_response(p2, n2)
  
  #Create a new data frame with group labels and responses
  data <- data.frame(
    group = factor(c(rep(1, n1), rep(2, n2))),
    response = c(group1_responses, group2_responses)
  )

  # Return data frame
  return(data)
}

# calculate_the_difference_in_proportions
calculate_the_difference_in_proportions <- function(data) {
  
  # error checking 
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  required_cols <- c("group", "response")
  if (!all(required_cols %in% names(data))) {
    stop("Data frame must contain 'group' and 'response' columns.")
  }
  if (!is.factor(data$group)) {
    stop("'group' column must be a factor.")
  }
  if (!all(data$response %in% c(0, 1))) {
    stop("'response' column must contain only 0s and 1s.")
  }
  
  # Calculate proportion for group 1
  # Calculate proportion for group 2
  prop_group1 <- mean(data$response[data$group == 1])
  prop_group2 <- mean(data$response[data$group == 2])
  
  # Calculate the difference in proportions
  difference <- prop_group1 - prop_group2
  
  #Return the difference in proportions
  return(difference)
}

# repeated_simulations_for_one_simulation
repeated_simulations_for_one_simulation <- function(p1, p2, n1, n2, num_simulations) {
  
  # error checking 
  args <- list(p1 = p1, p2 = p2, n1 = n1, n2 = n2, num_simulations = num_simulations)
  for (arg_name in names(args)) {
    arg <- args[[arg_name]]
    if (!is.numeric(arg) || length(arg) != 1) {
      stop(paste("'", arg_name, "' must be a single numeric value.", sep = ""))
    }
  }
  
  if (p1 < 0 || p1 > 1 || p2 < 0 || p2 > 1) {
    stop("Probabilities 'p1' and 'p2' must be between 0 and 1.")
  }
  if (n1 <= 0 || n1 != floor(n1) || n2 <= 0 || n2 != floor(n2)) {
    stop("Sample sizes 'n1' and 'n2' must be positive integers.")
  }
  if (num_simulations <= 0 || num_simulations != floor(num_simulations)) {
    stop("'num_simulations' must be a positive integer.")
  }
  # Initialize empty vector to store differences
  differences <- numeric(num_simulations)
  
  # Create for loop for each simulation; Get two-sample data; Calculate difference in proportions; Store difference
  for (i in 1:num_simulations) {
    sim_data <- simulation_for_two_sample_data(p1, p2, n1, n2)
    differences[i] <- calculate_the_difference_in_proportions(sim_data)
  }

  # Return vector of differences
  return(differences)
}

# visualize_theoretical_sampling_distribution
visualize_theoretical_sampling_distribution <- function(simulation_results, true_difference, n1, n2) {
  
  # error checking 
  if (!is.numeric(simulation_results)) {
    stop("'simulation_results' must be a numeric vector.")
  }
  if (!is.numeric(true_difference) || length(true_difference) != 1) {
    stop("'true_difference' must be a single numeric value.")
  }
  if (!is.numeric(n1) || length(n1) != 1 || n1 <= 0 || n1 != floor(n1)) {
    stop("'n1' must be a positive integer.")
  }
  if (!is.numeric(n2) || length(n2) != 1 || n2 <= 0 || n2 != floor(n2)) {
    stop("'n2' must be a positive integer.")
  }
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  library(ggplot2)
  
  # Create histogram or density plot of simulation results
  # Add vertical line for the difference
  #Add labels and title
  df <- data.frame(difference = simulation_results)
  
  p <- ggplot(df, aes(x = difference)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 0.02, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_density(color = "blue", linewidth = 1) +
    geom_vline(xintercept = true_difference, color = "red", linetype = "dashed", linewidth = 1) +
    labs(
      title = "Sampling Distribution of Difference in Proportions",
      x = "Difference in Proportions (p1 - p2)",
      y = "Density"
    ) +
    theme_minimal()
  print(p)
}
