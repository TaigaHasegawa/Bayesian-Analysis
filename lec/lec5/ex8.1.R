### Example 8.1:  Population Proportion: Numeric Integration ###

n <- 70

y <- 12


### Define likelihood and prior (up to proportionality)

like <- function(pi) pi^y * (1-pi)^(n-y)

prior <- function(pi) dbeta(pi, 0.5, 0.5)


### Compute posterior expectation of pi

numerator <- integrate(function(pi) pi * prior(pi) * like(pi), 0, 1)
numerator

denominator <- integrate(function(pi) prior(pi) * like(pi), 0, 1)
denominator

numerator$value / denominator$value
