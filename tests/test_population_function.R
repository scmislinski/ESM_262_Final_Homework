##### Testing the Function ####
#testing library
library(testthat)
source("C:/Users/sabri/OneDrive/Desktop/ENV MODELING/Comp-Env-Science/ESM_262_Final_Homework-main/R/population_function.R")  # Adjust path as needed

# Test that the function returns expected output structure
test_that("Function returns a list with correct elements", {
  result <- population_growth(N = 500, K = 1000, r = 0.2)

  expect_type(result, "list")  # Should return a list
  expect_named(result, c("growth_rate", "next_population"))  # Should contain these names
})

# Test that population increases when N is below K
test_that("Population increases when below carrying capacity", {
  result <- population_growth(N = 500, K = 1000, r = 0.2)
  expect_gt(result$next_population, 500)  # Next population should be greater than initial
})

# Test that function throws an error for invalid inputs
test_that("Error is thrown for non-numeric inputs", {
  expect_error(population_growth("a", 1000, 0.2), "All inputs must be numeric")
  expect_error(population_growth(500, "b", 0.2), "All inputs must be numeric")
  expect_error(population_growth(500, 1000, "c"), "All inputs must be numeric")
})

#all these passed on the first run

test_that("Error is thrown for negative or zero values", {
  expect_error(population_growth(0, 1000, 0.2), "Population size (N) and carrying capacity (K) must be positive", fixed = TRUE)
  expect_error(population_growth(500, 0, 0.2), "Population size (N) and carrying capacity (K) must be positive", fixed = TRUE)
})


test_that("Error is thrown if initial population exceeds carrying capacity", {
  expect_error(population_growth(1500, 1000, 0.2), "Initial population \\(N\\) cannot exceed carrying capacity \\(K\\)")
})

test_that("Error is thrown for growth rate outside valid range", {
  expect_error(population_growth(500, 1000, -0.1), "Growth rate \\(r\\) should be between 0 and 1")
  expect_error(population_growth(500, 1000, 1.5), "Growth rate \\(r\\) should be between 0 and 1")
})

# Test edge case when N is close to K
test_that("Population change is minimal when N is close to K", {
  result <- population_growth(N = 990, K = 1000, r = 0.2)
  expect_lt(result$next_population - 990, 10)  # Small change expected
})

