### Example 7.2:  Two-Parameter Normal Inference --- Noninformative Prior ###

n <- 24

ybar <- 7.8730

s <- 0.05353


# Using the standard noninformative prior:

# Marginal posterior inference for mu

mu.scale <- s^2 / n

  # Posterior mean and variance

mu.posterior.mean <- ybar

mu.posterior.var <- (n-1) * mu.scale / (n-3)

mu.posterior.mean
sqrt(mu.posterior.var)

  # 95% credible interval (equal-tailed)

ybar + qt(c(0.025, 1-0.025), n-1) * sqrt(mu.scale)

  # Test of H0: mu >= 7.9379 (minimum legal weight)

post.prob.H0 <- 1 - pt((7.9379 - ybar) / sqrt(mu.scale), n-1)

post.prob.H0


# Marginal posterior inference for sigma^2

alpha.posterior <- (n-1) / 2

beta.posterior <-  (n-1) * s^2 / 2

  # Posterior mean

sigma.2.posterior.mean <- beta.posterior / (alpha.posterior - 1)

sigma.2.posterior.mean

  # 95% credible interval (equal-tailed)

(n-1) * s^2 / qchisq(c(1-0.025, 1-0.975), n-1)

s^2  # compare posterior mean and interval with s^2


# Graphing the joint posterior (in mu and sigma^2)

mu.grid <- seq(7.8, 8.0, len=100)

sigma.2.grid <- seq(0.001, 0.01, len=100)

dinversegamma <-
  function(y, alpha, beta)
    beta^alpha * exp(-beta/y) / (gamma(alpha) * y^(alpha+1))

posterior.density <- 
  function(mu,sigma.2) 
    dnorm(mu, ybar, sqrt(sigma.2/n)) *
    dinversegamma(sigma.2, alpha.posterior, beta.posterior)
                       
contour(mu.grid, sigma.2.grid,
        outer(mu.grid, sigma.2.grid, posterior.density),
        main="Joint Posterior (Noninformative Prior)",
        xlab=expression(mu), ylab=expression(sigma^2))

