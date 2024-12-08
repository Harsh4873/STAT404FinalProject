# Permutation Tests
# These tests are intended to redistribute the group numbers along the observations and calculate the statistic of interest.
# The purpose of these tests is to see if there is a distinctive difference between the observed test statistic and the reallocated test statistic.

library(ggplot2)

responses = function(p1, p2, n) { #Function to take in the responses of the given data
  if (p1 < 0 || p2 < 0 || p1 >1 || p2 > 1) { #Ensures that all the probabilities are valid and legal probabilities.
    stop("P1 and P2 can not be less than 0 or greater than 1")
  }
  response = c(rbinom(n, 1, p1), rbinom(n, 1, p2))
  return(response) #Returns the randomized probabilities when correct
}

permutation.test = function (group.labels, responses, num.reps) {
  # Input validation
  if (!is.numeric(num.reps)) {
    stop("Num_reps must be a valid number")
  }
  if (num.reps <= 0) {
    stop("Can not have negative permutations")
  }
  if (length(group.labels) != length(responses)) {
    stop("Group labels and responses must have the same length")
  }
  
  # Group.labels = the labels 1 or 2 assigned to each piece of observed data
  # Responses = the actual pieces of data responses
  # num.reps = number of times to permutate the test
  # Check to make sure it is a valid test
  if (!is.numeric(num.reps) || num.reps <= 0) {
    stop("Number of reps must be a real number greater than 0")
  }
  
  # Find the test statistic
  group1 = responses[group.labels==1] # Break the responses down by whether they are in group 1 or group 2
  group2 = responses[group.labels==2]
  p1.hat = mean(group1) # Calculate the p1 hat and p2 hat statistics
  p2.hat = mean(group2)
  observed.test.statistic = (p1.hat - p2.hat) / sqrt((p1.hat*(1 - p1.hat)) / length(group1) + (p2.hat*(1 - p2.hat)) / length(group2))
  
  # Store test statistic in results
  permuted.test.statistics <- numeric(num.reps)
  permuted.test.statistics[1] <- observed.test.statistic
  
  # Process to do the permutations with the different sample groups
  for (i in seq_len(num.reps)) {
    permuted.labels = sample(group.labels)  # Randomize the group labels
    perm.group1 = responses[permuted.labels == 1] # Place the randomized responses into their respective groups
    perm.group2 = responses[permuted.labels == 2] 
    perm.p1.hat = mean(perm.group1) # Calculate the statistic mean
    perm.p2.hat = mean(perm.group2)
    permuted.test.statistics[i] = (perm.p1.hat - perm.p2.hat) / sqrt((perm.p1.hat * (1 - perm.p1.hat)) / length(perm.group1) + (perm.p2.hat * (1 - perm.p2.hat)) / length(perm.group2))
  }
  
  # Null Mean and Standard Deviation
  null_mean <- 0  
  null_sd <- sqrt((p1.hat * (1 - p1.hat)) / length(group1) + (p2.hat * (1 - p2.hat)) / length(group2))
  
  # Plotting function to display the data
  data <- rnorm(1000, mean = null_mean, sd = null_sd)
  density.data <- density(data)
  density.df <- data.frame(x = density.data$x, y = density.data$y) # Density data frame
  ggplot(data = density.df, aes(x = .data$x, y = .data$y)) + # Use .data$ to explicitly reference columns
    geom_line(color = "blue", linewidth = 1) +      
    geom_histogram(aes(y = after_stat(density)), binwidth = 0.1, fill = "green", alpha = 0.5) +  
    labs(title = "Density Curve", x = "Test Statistic", y = "Density") +
    theme_grey()
}