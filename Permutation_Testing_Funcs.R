# Permutation Tests
# These tests are intended to redistribute the group numbers along the observations and calculate the statistic of interest.
# The purpose of these tests is to see if there is a distinctive difference between the observed test statistic and the reallocated test statistic.

library(testthat)
library(ggplot2)

permutation.test = function (group.labels, responses, num.reps) {
# Group.labels = the labels 1 or 2 assigned to each piece of observed data
# Responses = the actual pieces of data responses
# num_reps = number of times to permutate the test
  #Check to make sure it is a valid test
  if (!is.numeric(num.reps) || num.reps <= 0) {
    stop("Can not have negative permutations")
  }
  
  #Find the test statistic
  group1 = responses[group.labels==1] #break the reponses down by whether they are in group 1 or group 2
  group2 = responses[group.labels==2]
  p1.hat = mean(group1 #Calculate the p1 hat and p2 hat statistics
  p2.hat = mean(group2)
  observed.test.statistic = (p1.hat-p2.hat) / sqrt((p1.hat*(1 -p1.hat)) / length(group1)+(p2.hat*(1 - p2.hat)) / length(group2))
  
  #Process to do the permutations with the different sample groups
  permuted.test.statistics = numeric(num.reps)
  for (i in seq_len(num.reps)) {
    permuted.labels = sample(group.labels)  # Randomize the group labels
    perm.group1 = responses[permuted.labels == 1] #Place the randomized responses into their respective groups
    perm.group2 = responses[permuted.labels == 2] 
    perm.p1.hat = mean(perm.group1) #Calculate the statistic mean
    perm.p2.hat = mean(perm.group2)
    permuted.test.statistics[i] = (perm.p1.hat - perm.p2.hat) /sqrt((perm.p1.hat * (1 - perm.p1.hat)) / length(perm.group1) + (perm.p2.hat * (1 - perm.p2.hat)) / length(perm.group2))
  }
  
  #Null Mean and Standard Deviation
  null.mean = 0  
  null.sd = sqrt((p1.hat * (1 - p1.hat)) / length(group1) + (p2.hat * (1 - p2.hat)) / length(group2))
  
  #PLotting function to display the data
  data = rnorm(1000, mean = 0, sd = 1)
  density.data = density(data) #Find the density
  density.df = data.frame(x = density.data$x, y = density.data$y) #Density data frame
  ggplot(density.df, aes(x = x, y = y)) + #Plotting function
    geom_line(color = "blue", size = 1) +      #Plot the curve for the density
    geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "green", alpha = 0.5) +  # Plot the histogram with density scale
    labs(title = "Density Curve", x = "Test Statistic", y = "Density") +
    theme_grey()
}

# Example usage
set.seed(1)
n = 50
p1 = 0.6
p2 = 0.4
responses = c(rbinom(n, 1, p1), rbinom(n, 1, p2))
group.labels = c(rep(1, n), rep(2, n))
permutation.test(group.labels, responses, num.reps = 1000)
