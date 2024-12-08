library(testthat)
library(ggplot2)

permutation_test = function(group.labels, responses, num_reps, alpha = 0.05) {
  #Check to make sure it is a valid test
  if (!is.numeric(num_reps) || num_reps <= 0) {
    stop("Can not have negative permutations")
  }
  
  #Find the test statistic
  group1 = responses[group.labels==1]
  group2 = responses[group.labels==2]
  p1.hat = mean(group1)
  p2.hat = mean(group2)
  observed_test_statistic = (p1.hat-p2.hat) / sqrt((p1.hat*(1 -p1.hat)) / length(group1)+(p2.hat*(1 - p2.hat)) / length(group2))
  
  #Process to do the permutations with the different sample groups
  permuted_test_statistics = numeric(num_reps)
  for (i in seq_len(num_reps)) {
    permuted_labels = sample(group.labels)  # Randomize the group labels
    perm_group1 = responses[permuted_labels == 1] #Place the randomized responses into their respective groups
    perm_group2 = responses[permuted_labels == 2] 
    perm_p1.hat = mean(perm_group1) #Calculate the statistic mean
    perm_p2.hat = mean(perm_group2)
    permuted_test_statistics[i] = (perm_p1.hat - perm_p2.hat) / 
      sqrt((perm_p1.hat * (1 - perm_p1.hat)) / length(perm_group1) + (perm_p2.hat * (1 - perm_p2.hat)) / length(perm_group2))
  }
  
  #Null Mean and Standard Deviation
  null_mean = 0  
  null_sd = sqrt((p1.hat * (1 - p1.hat)) / length(group1) + (p2.hat * (1 - p2.hat)) / length(group2))
  
  #PLotting function to display the data
  data = rnorm(1000, mean = 0, sd = 1)
  density_data = density(data) #Find the density
  density_df = data.frame(x = density_data$x, y = density_data$y) #Density data frame
  ggplot(density_df, aes(x = x, y = y)) + #Plotting function
    geom_line(color = "blue", size = 1) +      #Plot the curve for the density
    geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "green", alpha = 0.5) +  # Plot histogram with density scale
    labs(title = "Density Curve", x = "Test Statistic", y = "Density") +
    theme_grey()
}

# Example usage
set.seed(123)
n = 50
p1 = 0.6
p2 = 0.4
responses = c(rbinom(n, 1, p1), rbinom(n, 1, p2))
group.labels = c(rep(1, n), rep(2, n))
permutation_test(group.labels, responses, num_reps = 1000)

set.seed(120)
n = 70
p1 = 0.4
p2 = 0.9
responses = c(rbinom(n, 1, p1), rbinom(n, 1, p2))
group.labels = c(rep(1, n), rep(2, n))
permutation_test(group.labels, responses, num_reps = -10)

test_that("Permutation test works", permutation_test(c(rep(1, n), rep(2, n)), c(rbinom(n, 1, .4), rbinom(n, 1, .6)), num_reps = 1000))