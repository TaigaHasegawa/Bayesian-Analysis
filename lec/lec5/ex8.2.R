### Example 8.2:  Comparing Population Proportions: Numeric Integration ###

n1 <- 19

y1 <- 8

n2 <- 51

y2 <- 25


eps <- 1e-20


### Define likelihood and prior (up to proportionality)

like <- function(pi1, pi2) pi1^y1 * (1-pi1)^(n1-y1) *
                           pi2^y2 * (1-pi2)^(n2-y2)

prior <- function(pi1, pi2) dbeta(pi1, 0.5, 0.5) *
                            dbeta(pi2, 0.5, 0.5)


### Compute probability pi1 < pi2

integrand <- function(pi1, pi2) prior(pi1,pi2) * like(pi1,pi2)


inner.integral.num <- function(pi2)
    integrate(integrand, 0, pi2, pi2, rel.tol=eps)$value

numerator <- integrate(Vectorize(inner.integral.num), 0, 1, rel.tol=eps)
numerator


inner.integral.denom <- function(pi2)
    integrate(integrand, 0, 1, pi2, rel.tol=eps)$value

denominator <- integrate(Vectorize(inner.integral.denom), 0, 1, rel.tol=eps)
denominator


numerator$value / denominator$value
