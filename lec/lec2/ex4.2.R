### Example 4.2:  Population Proportion --- Bayesian Methods ###

y <- 12

n <- 70

### (a) Credible Intervals

  # 95% equal-tailed credible interval under Uniform(0,1) prior
  # (based on analytical solution)

qbeta(c(0.025, 0.975), y+1, n-y+1)


  # Approximate 95% HPD credible interval under Uniform(0,1) prior

dx <- 1/10000

pi.grid <- seq(0, 1, dx)

density.cut <- quantile(dbeta(rbeta(10000, y+1, n-y+1), y+1, n-y+1), 0.05)

range(pi.grid[dbeta(pi.grid, y+1, n-y+1) > density.cut])

?dbeta
  # Another way to find the 95% HPD credible interval

density.diff <- function(x) dbeta(qbeta(x, y+1, n-y+1), y+1, n-y+1) -
                            dbeta(qbeta(0.95+x, y+1, n-y+1), y+1, n-y+1)

lower.area <- uniroot(density.diff, c(0,0.05))$root
?uniroot
lower.area
c(qbeta(lower.area, y+1, n-y+1), qbeta(0.95+lower.area, y+1, n-y+1))


### (b) Posterior Probabilities (of hypotheses)

  # Posterior prob. that pi >= 0.3   (at least 30% own pets)
  # under Uniform(0,1) prior:

1 - pbeta(0.3, y+1, n-y+1)
