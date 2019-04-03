### Example 7.1:  Two-Parameter Normal Inference --- Conjugate Prior ###

n <- 24

ybar <- 7.8730

s <- 0.05353


# Use an informative conjugate prior:

# For mu, a conditionally normal prior centered at the legal standard weight, 
#   with a "prior sample size" of 2:

mu0 <- 7.9876

kappa <- 2

# For sigma^2, an inverse-gamma prior with alpha and beta chosen
#   to give it an expected value of 0.025^2 and variance of 2*(0.025^4):

sigma.2.prior.mean <- 0.025^2

sigma.2.prior.var <- 2*(0.025^4)

alpha <- sigma.2.prior.mean^2 / sigma.2.prior.var + 2

beta <- (alpha-1) * sigma.2.prior.mean

alpha
beta


# Marginal posterior inference for mu

mu1 <- (kappa*mu0 + n*ybar) / (kappa + n)

tau1.2 <- (alpha + n/2) / (beta + 0.5 * (n-1)*s^2 +
                           0.5 * kappa*n*(mu0-ybar)^2/(kappa + n))

mu.scale <- 1 / (tau1.2 * (kappa+n))

mu.df <- 2*alpha + n

  # Posterior mean and variance

mu.posterior.mean <- mu1

mu.posterior.var <- (mu.df / (mu.df - 2)) * mu.scale

mu.posterior.mean
sqrt(mu.posterior.var)

  # 95% credible interval (equal-tailed)

mu1 + qt(c(0.025, 1-0.025), mu.df) * sqrt(mu.scale)

ybar  # compare posterior mean and interval with ybar

  # Test of H0: mu >= 7.9379 (minimum legal weight)

post.prob.H0 <- 1 - pt((7.9379 - mu1) / sqrt(mu.scale), mu.df)

post.prob.H0


# Marginal posterior inference for sigma^2

alpha.posterior <- alpha + n/2

beta.posterior <- beta + 0.5 * (n-1)*s^2 +
                  0.5 * kappa*n*(mu0-ybar)^2/(kappa + n)

  # Posterior mean

sigma.2.posterior.mean <- beta.posterior / (alpha.posterior - 1)

sigma.2.posterior.mean

  # 95% credible interval (equal-tailed)

1 / qgamma(c(1-0.025, 1-0.975), alpha.posterior, beta.posterior)

s^2  # compare posterior mean and interval with s^2


# Graphing the joint posterior (in mu and sigma^2)

mu.grid <- seq(7.8, 8.0, len=100)

sigma.2.grid <- seq(0.001, 0.01, len=100)

dinversegamma <-
  function(y, alpha, beta)
    beta^alpha * exp(-beta/y) / (gamma(alpha) * y^(alpha+1))

posterior.density <- 
  function(mu,sigma.2) 
    dnorm(mu, mu1, sqrt(sigma.2/(kappa+n))) *
    dinversegamma(sigma.2, alpha.posterior, beta.posterior)
                       
contour(mu.grid, sigma.2.grid,
        outer(mu.grid, sigma.2.grid, posterior.density),
        main="Joint Posterior (Informative Prior)",
        xlab=expression(mu), ylab=expression(sigma^2))

