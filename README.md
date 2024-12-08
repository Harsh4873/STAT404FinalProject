# STAT404FinalProject

# Discussion

We have divided the project into distinct sections and assigned each team member specific responsibilities to ensure efficient progress and integration of our work. Utilizing a divide and conquer approach aligned with a top-down design philosophy, each member focuses on their area of expertise:

- **Harsh Dave** is implementing the design, examples, and testing for **Core Simulation Functions** and **Sampling Distribution Visualization**.
- **Ritvik** is focused on creating the **Confidence Interval Analyses**.
- **Race** is responsible for designing and facilitating the **Permutation Test**.
- **Chris** is implementing the **Bootstrap Analysis**.

# Project Goal and Overview

**Project Goal**

This project is designed to provide hands-on experience in R software development by applying fundamentals and best practices covered in STAT 404. Key focus areas include simulations, top-down design, testing, and signal conditioning, culminating in the creation of a simplified R package.

**Project Overview**

Our group will develop functions with appropriate tests to simulate and visualize foundational concepts taught in STAT 211/212 and beyond. Specifically, we will employ repeated simulations to illustrate the following ideas:

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