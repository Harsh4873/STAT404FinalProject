calc_bootstrap_stats <- function(x1, n1, x2, n2, n_bootstrap = 1000, conf_level = 0.95){
  
  #x1 and x2 are the number of successes in group 1/2
  #n1 and n2 are the number of observations in group 1/2
  #n_bootstrap is, unsurprisingly, the number of bootstraps, default 1000 but it can be changed
  #conf_level is the confidence level or our 1- alpha, default 0.95 or alpha = 0.05
  
  # validation
  
  if (!is.numeric(x1) || !is.numeric(n1) || !is.numeric(x2) || !is.numeric(n2) ||
      !is.numeric(n_bootstrap) || !is.numeric(conf_level)) {
    stop("All inputs (x1, n1, x2, n2, n_bootstrap, conf_level) have to be numeric.")
  }
  
  # Check for integer values using a more reliable method
  if (!all(c(x1, n1, x2, n2, n_bootstrap) == floor(c(x1, n1, x2, n2, n_bootstrap)))) {
    stop("x1, n1, x2, n2, and n_bootstrap have to be integers.")
  }
  
  if (x1 < 0 || x2 < 0) {
    stop("Number of successes (x1, x2) cannot be negative.")
  }
  
  if (n1 <= 0 || n2 <= 0) {
    stop("Number of observations (n1, n2) have to be greater than zero.")
  }
  
  if (x1 > n1) {
    stop("Number of successes x1 cannot exceed number of observations n1.")
  }
  
  if (x2 > n2) {
    stop("Number of successes x2 cannot exceed number of observations n2.")
  }
  
  if (n_bootstrap <= 0) {
    stop("n_bootstrap have to be a positive integer.")
  }
  
  if (conf_level <= 0 || conf_level >= 1) {
    stop("conf_level have to be a number between 0 and 1.")
  }
  
  # we get our proportions here
  p1_hat <- x1 / n1
  p2_hat <- x2 / n2
  diff_hat <- p1_hat - p2_hat
  
  
  # calculate the theoretical standard error
  SE_theoretical <- sqrt( (p1_hat * (1 - p1_hat)) / n1 + (p2_hat * (1 - p2_hat)) / n2 )
  
  # then get our theoretical confidence interval
  alpha <- 1 - conf_level
  Z <- qnorm(1 - alpha / 2)
  
  CI_theoretical <- diff_hat + c(-1, 1) * Z * SE_theoretical
  
  
  # two vectors for the bootstrapping process
  vec1 <- c(rep(1, x1), rep(0, n1 - x1))
  vec2 <- c(rep(1, x2), rep(0, n2 - x2))
  
  # vector to store the statistic of interest
  bootstrap_diffs <- numeric(n_bootstrap)
  
  # bootstrapping algorithm
  for (i in 1:n_bootstrap){
    
    # sample from both with replacement
    samp1 = sample(vec1, size = n1, replace = TRUE)
    samp2 = sample(vec2, size = n2, replace = TRUE)
    
    #calculate our statistic of interest, p hat1 - p hat2
    bootstrap_p1 <- mean(samp1)
    bootstrap_p2 <- mean(samp2)
    
    # append the difference to our list
    bootstrap_diffs[i] <- bootstrap_p1 - bootstrap_p2
    
  }
  
  # calculate standard error and confidence interval
  bootstrap_SE = sd(bootstrap_diffs)
  
  bootstrap_CI <- diff_hat + c(-1, 1) * Z * bootstrap_SE
  
  # store everything in a big list
  results <- list(
    diff_hat = diff_hat,
    SE_theoretical = SE_theoretical,
    SE_bootstrap = bootstrap_SE,
    CI_theoretical = CI_theoretical,
    CI_bootstrap = bootstrap_CI,
    bootstrap_diffs = bootstrap_diffs,
    conf_level = conf_level #added to make the call make more sense for the visualization
  )
  
  return(results)
}


visualize_bootstrap_stats <- function(bootstrap_diffs, CI_theoretical, CI_bootstrap, conf_level = 0.95) {
  
  
  # needed to make the ablines show up
  x_min <- min(bootstrap_diffs, CI_theoretical, CI_bootstrap)
  x_max <- max(bootstrap_diffs, CI_theoretical, CI_bootstrap)
  
  # histogram for the differences
  hist(bootstrap_diffs, breaks = 30, probability = TRUE,
       main = "Bootstrap Distribution of Difference in Proportions",
       xlab = "Difference in Proportions", col = "lightgray",
       xlim = c(x_min, x_max))
  
  # two lines for the difference between our theoretical and actual confidence intervals, each abline will plot two lines for each end of the confidence interval
  # since earlier each CI is calculated as both ends
  abline(v = CI_theoretical, col = "blue", lwd = 2, lty = 2)
  abline(v = CI_bootstrap, col = "red", lwd = 2, lty = 2)
  
  # legend to differentiate the two
  legend("topright", legend = c("Theoretical CI", "Bootstrap CI"),
    col = c("blue", "red"), lwd = 2, lty = 2)

  
}