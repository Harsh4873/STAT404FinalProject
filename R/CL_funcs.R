# This function simulates repeated sampling of two binary groups with given proportions and sample sizes. 
# It calculates confidence intervals for the difference in proportions and visualizes the proportion of intervals containing the true difference for varying numbers of repetitions.

# p1 = Numeric. True proportion for group 1 (0 <= p1 <= 1).
# p2 = Numeric. True proportion for group 2 (0 <= p2 <= 1).
# n1 = Integer. Sample size for group 1 (n1 > 0).
# n2 = Integer. Sample size for group 2 (n2 > 0).

# rep_steps = Numeric vector. Different values for the number of repetitions.

# alpha = Numeric. Significance level for the confidence interval.

# A visualization of the empirical confidence levels against repetitions.

# Examples: visualize_confidence_levels(p1 = 0.6, p2 = 0.5, n1 = 30, n2 = 30, 
# rep_steps = c(10, 50, 100, 500, 1000))

visualize_confidence_levels <- function(p1, p2, n1, n2, rep_steps, alpha = 0.05) {
  # input validation
  if (!is.numeric(n1) || !is.numeric(n2) || n1 <= 0 || n2 <= 0) {
    stop("Sample sizes n1 and n2 must be positive integers.")
  }
  if (!is.numeric(p1) || !is.numeric(p2) || p1 < 0 || p1 > 1 || p2 < 0 || p2 > 1) {
    stop("Proportions p1 and p2 must be between 0 and 1.")
  }
  if (!is.numeric(rep_steps) || any(rep_steps <= 0)) {
    stop("Repetition steps (rep_steps) must be a numeric vector of positive values.")
  }
  
  # nested function whoch simulates the confidence intervals for a given number of repetitions
  simulate_ci <- function(reps) {
    # vector which tracks if the true difference is contained in CI for each repetition
    contain_true_diff <- numeric(reps)
    true_diff <- p1 - p2  
    # the true difference in proportions (above)
    
    for (i in seq_len(reps)) {
      # simulates binary responses for both groups (group 1 and 2)
      group1 <- rbinom(n1, 1, p1)  
      group2 <- rbinom(n2, 1, p2)  
      
      # calculates the sample proportions
      p1_hat <- mean(group1)
      p2_hat <- mean(group2)
      
      # the standard error of the difference in proportions
      se <- sqrt((p1_hat * (1 - p1_hat) / n1) + (p2_hat * (1 - p2_hat) / n2))
      
      # computes the confidence interval using Z crit value
      z <- qnorm(1 - alpha / 2)
      ci <- c((p1_hat - p2_hat) - z * se, (p1_hat - p2_hat) + z * se)
      
      # checks to see if true difference lies within the ci
      contain_true_diff[i] <- true_diff >= ci[1] && true_diff <= ci[2]
    }
    
    mean(contain_true_diff)  
  }
  
  # applies the simulation across all the repetition steps
  results <- sapply(rep_steps, simulate_ci)
  
  # creates a df for visualization
  results_df <- data.frame(Repetitions = rep_steps, Proportion = results)
  
  # visualization using the ggplot2 package
  library(ggplot2)
  ggplot(results_df, aes(x = Repetitions, y = Proportion)) +
    geom_line(color = "blue", size = 1) +
    geom_hline(yintercept = 1 - alpha, linetype = "dashed", color = "red") +
    labs(
      title = "Empirical Confidence Levels for Repeated Sampling",
      x = "Number of Repetitions",
      y = "Proportion of CIs Containing True Difference"
    ) +
    theme_minimal()
}
