library(testthat)

# Confidence Level Analysis Test Cases (Ritvik V)

# First a test case if provided a valid simple input
visualize_confidence_levels(p1 = 0.6, p2 = 0.5, n1 = 30, n2 = 30, rep_steps = c(10, 50, 100))
# For invalid proportions p1 and p2
visualize_confidence_levels(p1 = 1.2, p2 = 0.5, n1 = 30, n2 = 30, rep_steps = c(10, 50, 100))
# For non-positive sample size n1
visualize_confidence_levels(p1 = 0.6, p2 = 0.5, n1 = -10, n2 = 30, rep_steps = c(10, 50, 100))
# For non-numeric rep_steps
visualize_confidence_levels(p1 = 0.6, p2 = 0.5, n1 = 30, n2 = 30, rep_steps = "100")
