#' Fish Population Dynamics Using a Logistic Growth Model
#'
#'
#' @param N_0 initial population size
#' @param K carrying capacity of the species
#' @param r the growth rate
#' @return N_t Population size at time t
#'
# function definition

population_growth <- function(N, K, r = 0.2) {
  # error checking 1 numeric inputs
  if(!is.numeric(N) || !is.numeric(K) || !is.numeric(r)) {
    stop("Error: All inputs must be numeric.")
  } #end of numeric error checking

  # error checking 2
  if (N <= 0 || K <= 0) {
    stop("Error: Population size (N) and carrying capacity (K) must be positive.")
  } #end of error checking to make sure the population size

  if (N > K) {
    stop("Error: Initial population (N) cannot exceed carrying capacity (K).")
  } # end of error checking to make sure the initial population isn't larger than the carrying capacity.

  if (r <= 0 || r > 1) {
    stop("Error: Growth rate (r) should be between 0 and 1.")
  } #end of error checking to make sure growth rate is between 0 and 1

  growth_rate <- r * N * (1 - N / K)
  next_population <- N + growth_rate
  return(list(growth_rate = growth_rate, next_population = next_population))
} #end of function

