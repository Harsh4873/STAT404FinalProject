# STAT404FinalProject

# Discussion

We will divide the sections and then complete the designs, testing, and examples on all of these using a divide and conquer approach while adhering to the top down design. Harsh will focus on implementing the design, examples, and testing for Core Simulating Functions and Sampling Distribution Visualization.  Ritvik will focus on creating the  Confidence Interval analyses. Race will design and facilitate the Permutation test. Finally, Chris will focus on implementing the bootstrap analysis. 

# Project Goal and Overview

**Project Goal**

This project is designed to give experience on R software development by using the fundamentals and best practices covered in STAT 404 including but not limited to simulations, top down design, testing, and signal conditioning. It is a stripped down version of creating an R package.

**Project Overview**

Your group will create functions with appropriate tests to simulate and visualize fundamentals learned in 211/212 and beyond. Specifically, you will use repeated simulations to communicate the following ideas:

1. **Visualize theoretical sampling distributions**
2. **Confidence levels**
3. **Permutation Test**
4. **Bootstrap uncertainty**

**Testing**

A comprehensive suite of unit tests will be developed using the `testthat` package to ensure the correctness and robustness of all functions. At a minimum, tests will verify:

1. Correct functionality on valid simple inputs.
2. Proper error handling when provided with invalid inputs.

Emphasizing simpler functions for testing facilitates easier maintenance and reduces the likelihood of bugs.

**Directory Structure**

The provided R project template includes the following structure:

1. **R/** subdirectory
   - Contains all function definitions in `.R` scripts with adequate comments detailing inputs and outputs.

2. **tests/** subdirectory
   - Contains all tests using `testthat` in `.R` scripts.

3. **Examples.Rmd**
   - A narrative document showcasing the package's functionality with basic examples.

4. **AuthorContributions.Rmd**
   - Tracks the contributions of each group member, detailing roles in documentation, programming, debugging, etc.

5. **STAT404Final.Rproj**
   - R project file for opening in RStudio (should remain unchanged).

Function definitions are confined to `.R` files within the `R/` subdirectory. The examples and tests will source these `.R` files to utilize the functions, maintaining a clear separation of code, documentation, and testing.