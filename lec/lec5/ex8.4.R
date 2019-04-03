### Example 8.4:  Gibbs Sampler for Semi-Conjugate Prior (Normal Sample) ###

n <- 24

ybar <- 7.8730

s <- 0.05353


### Specify prior (values comparable to Example 7.1)

mu0 <- 7.9876

sigma0.2 <- 0.0009375 / (2.5 - 1) / 2

alpha <- 2.5

beta <- 0.0009375


### Run Gibbs Sampler ...

  # set up

Nsim <- 100000

mus <- numeric(Nsim)
sigma.2s <- numeric(Nsim)

  # initialize

mus[1] <- 8

sigma.2s[1] <- 1 / rgamma(1, alpha + n/2, 
                          beta + ((n-1)*s^2 + n*(ybar-mus[1])^2)/2)

  # iterate

for(k in 2:Nsim){
  mus[k] <- rnorm(1, (mu0/sigma0.2 + n*ybar/sigma.2s[k-1]) /
                     (1/sigma0.2 + n/sigma.2s[k-1]), 
                  sqrt(1/(1/sigma0.2 + n/sigma.2s[k-1])))

  sigma.2s[k] <- 1 / rgamma(1, alpha + n/2, 
                            beta + ((n-1)*s^2 + n*(ybar-mus[k])^2)/2)
}


### Posterior inference ... (compare Example 7.1)

  # Posterior mean and std. dev. of mu

mean(mus)
sd(mus)

  # 95% credible interval for mu (equal-tailed)

quantile(mus, c(0.025, 0.975))

ybar  # compare posterior mean and interval with ybar

  # Post. prob. of H0: mu >= 7.9379 (minimum legal weight)

mean(mus >= 7.9379)

  # Posterior mean of sigma^2

mean(sigma.2s)

  # 95% credible interval for sigma^2 (equal-tailed)

quantile(sigma.2s, c(0.025, 0.975))

s^2  # compare posterior mean and interval with s^2


### Visualizing the first few steps of the "sample path" ...

maxit <- 20  # number of iterates to show

plot(mus[1:maxit], sigma.2s[1:maxit], type="n", main="Start of Sample Path",
     xlab=expression(mu), ylab=expression(sigma^2))
arrows(mus[1:(maxit-1)], sigma.2s[1:(maxit-1)], 
       mus[2:maxit], sigma.2s[2:maxit], length=0.1, angle=30,
       col=rainbow(maxit-1))
points(mus[1:maxit], sigma.2s[1:maxit], pch=20)


### Graphing the joint posterior (in mu and sigma^2) ...

library(MASS)  # provides kde2d

contour(kde2d(mus, sigma.2s, n=100),
        main="Joint Posterior (Informative Semi-Conjugate Prior)",
        xlab=expression(mu), ylab=expression(sigma^2))c

