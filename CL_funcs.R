visualize_confidence_levels <- function(p1, p2, n1, n2, rep_steps, alpha = 0.05) {
  if (!is.numeric(n1) || !is.numeric(n2) || n1 <= 0 || n2 <= 0) {
    stop("Sample sizes n1 and n2 must be positive integers.")
  }
  if (!is.numeric(p1) || !is.numeric(p2) || p1 < 0 || p1 > 1 || p2 < 0 || p2 > 1) {
    stop("Proportions p1 and p2 must be between 0 and 1.")
  }
  
  simulate_ci <- function(reps) {
    contain_true_diff <- numeric(reps)
    true_diff <- p1 - p2
    
    for (i in seq_len(reps)) {
      group1 <- rbinom(n1, 1, p1)
      group2 <- rbinom(n2, 1, p2)
      
      p1_hat <- mean(group1)
      p2_hat <- mean(group2)
      se <- sqrt((p1_hat * (1 - p1_hat) / n1) + (p2_hat * (1 - p2_hat) / n2))
      
      z <- qnorm(1 - alpha / 2)
      ci <- c((p1_hat - p2_hat) - z * se, (p1_hat - p2_hat) + z * se)
      
      contain_true_diff[i] <- true_diff >= ci[1] && true_diff <= ci[2]
    }
    
    mean(contain_true_diff) 
  }
  
  results <- sapply(rep_steps, simulate_ci)
  
  results_df <- data.frame(Repetitions = rep_steps, Proportion = results)
  
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

# Example
visualize_confidence_levels(p1 = 0.6, p2 = 0.5, n1 = 30, n2 = 30, rep_steps = c(10, 50, 100, 500, 1000))
