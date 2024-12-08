# The functions are listed first, the tests are at the BOTTOM

library(testthat)
library(ggplot2)

responses = function(p1, p2, n) {
  if (p1 < 0 || p2 < 0 || p1 >1 || p2 > 1) {
    stop("P1 and P2 can not be less than 0 or greater than 1")
  }
  response = c(rbinom(n, 1, p1), rbinom(n, 1, p2))
  return(response)
}

permutation.test = function (group.labels, responses, num.reps) {
  #Check to make sure it is a valid test
  if (!is.numeric(num.reps) || num.reps <= 0) {
    stop("Can not have negative permutations")
  }
  
  #Find the test statistic
  group1 = responses[group.labels==1]
  group2 = responses[group.labels==2]
  p1.hat = mean(group1)
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
    permuted.test.statistics[i] = (perm.p1.hat - perm.p2.hat) / 
      sqrt((perm.p1.hat * (1 - perm.p1.hat)) / length(perm.group1) + (perm.p2.hat * (1 - perm.p2.hat)) / length(perm.group2))
  }
  
  #Null Mean and Standard Deviation
  null.mean = 0  
  null.sd = sqrt((p1.hat * (1 - p1.hat)) / length(group1) + (p2.hat * (1 - p2.hat)) / length(group2))
  
  #Plotting function to display the data
  data = rnorm(1000, mean = 0, sd = 1)
  density.data = density(data) #Find the density
  density.df = data.frame(x = density.data$x, y = density.data$y) #Density data frame
  ggplot(density.df, aes(x = x, y = y)) + #Plotting function
    geom_line(color = "blue", size = 1) +      #Plot the curve for the density
    geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "green", alpha = 0.5) +  # Plot histogram with density scale
    labs(title = "Density Curve", x = "Test Statistic", y = "Density") +
    theme_grey()
}














# Hard tests / examples
set.seed(1)
n = 50
p1 = 0.6
p2 = 0.4
group.labels = c(rep(1, n), rep(2, n))
permutation.test(group.labels, responses(p1,p2,n), num.reps = 1000)
#Example passes correctly

set.seed(2)
n = 50
p1 = -0.6
p2 = 0.4
group.labels = c(rep(1, n), rep(2, n))
permutation.test(group.labels, responses(p1,p2,n), num.reps = 1000)
#Example correctly throws error, p1 can not be negative

set.seed(3)
n = 50
p1 = 1.6
p2 = 0.4
group.labels = c(rep(1, n), rep(2, n))
permutation.test(group.labels, responses(p1,p2,n), num.reps = 1000)
#Example correctly throws error, p1 can not exceed 1

set.seed(4)
n = 70
p1 = 0.4
p2 = 0.9
group.labels = c(rep(1, n), rep(2, n))
permutation.test(group.labels, responses(p1,p2,n), num.reps = -10)
#Example correctly throws error, num.reps can not be negative


#Test_That functions
test_that("RESPONSES function works", {
  set.seed(1)
  n = 50
  p1 = 0.6
  p2 = 0.4
  
  # Test for valid inputs
  response = responses(p1, p2, n)
  expect_length(response, 2 * n)
  expect_true(all(response %in% c(0, 1))) # Responses should be 0 or 1
  
  # Test for invalid probabilities
  expect_error(responses(-0.6, 0.4, n), "P1 and P2 can not be less than 0 or greater than 1")
  expect_error(responses(1.6, 0.4, n), "P1 and P2 can not be less than 0 or greater than 1")
  expect_error(responses(0.6, -0.4, n), "P1 and P2 can not be less than 0 or greater than 1")
})

test_that("PERMUTATION.TEST function works", {
  set.seed(1)
  n = 50
  p1 = 0.6
  p2 = 0.4
  group.labels = c(rep(1, n), rep(2, n))
  responses_data = responses(p1, p2, n)
  
  #ensure there are no errors from p1,p2,n inputs
  expect_silent(permutation.test(group.labels, responses_data, num.reps = 1000))
  
  #test to make sure the num_reps is valid
  expect_error(permutation.test(group.labels, responses_data, num.reps = -50), "Can not have negative permutations")
  expect_error(permutation.test(group.labels, responses_data, num.reps = "fifty"), "Num_reps must be a valid number") #Fails because "fifty" is not a number
})