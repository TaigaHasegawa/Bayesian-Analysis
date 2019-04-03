### Example 8.3:  Comparing Normal Means: Independent Sampling ###

n1 <- 24

ybar1 <- 7.8730

s1 <- 0.05353

n2 <- 123

ybar2 <- 7.9725

s2 <- 0.01409


### Independent draws from posterior ...

Nsim <- 100000

sigma1.2s <- 1 / rgamma(Nsim, (n1-1)/2, (n1-1)*s1^2/2)
sigma2.2s <- 1 / rgamma(Nsim, (n2-1)/2, (n2-1)*s2^2/2)

mu1s <- rnorm(Nsim, ybar1, sqrt(sigma1.2s/n1))
mu2s <- rnorm(Nsim, ybar2, sqrt(sigma2.2s/n2))


### Posterior inference based on empirical distribution of draws ...

  # approx. posterior density curve for mu1 - mu2 (histogram and smooth)

hist(mu1s - mu2s, freq=FALSE)
lines(density(mu1s - mu2s), col="blue")

  # approx. posterior mean and standard deviation of mu1 - mu2

mean(mu1s - mu2s)
sd(mu1s - mu2s)

  # Monte Carlo error of the approx. posterior mean of mu1 - mu2

sd(mu1s - mu2s) / sqrt(Nsim)

  # approx. 95% credible interval for mu1 - mu2

quantile(mu1s - mu2s, c(0.025, 0.975))

  # approx. posterior probability that mu1 < mu2

mean(mu1s < mu2s)


### For comparison: Frequentist Welch Approximate 95% CI

df.approx <- (s1^2/n1 + s2^2/n2)^2 /
             ((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))

ybar1 - ybar2 + qt(c(0.025, 0.975), df.approx) *
                sqrt(s1^2/n1 + s2^2/n2)


### Does sigma1^2 really exceed sigma2^2?

  # approx. posterior density curve for sigma1^2/sigma2^2 (histogram and smooth)

hist(sigma1.2s / sigma2.2s, freq=FALSE)
lines(density(sigma1.2s / sigma2.2s), col="blue")

  # Find approx. posterior prob. of H0: sigma1^2 <= sigma2^2 ...

mean(sigma1.2s <= sigma2.2s)


  # For comparison: Frequentist F-test p-value

1 - pf(s1^2/s2^2, n1-1, n2-1)
