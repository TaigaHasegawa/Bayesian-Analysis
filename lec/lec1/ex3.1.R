### Example 3.1:  Population Proportion: Likelihood, Priors, Posteriors ###

n <- 70

### (a) Binomial probability models --- for example ...

pi <- c(0.05, 0.1, 0.3, 0.5)

par(mfrow=c(2,2))

for(i in 1:length(pi))
  plot(0:n, dbinom(0:n,n,pi[i]), type="h",
       xlab=bquote(pi == .(pi[i])), ylab="Density")

?dbinom
### (b) Binomial model likelihood

dx <- 1/1000  # pi grid spacing

pi.grid <- seq(0, 1, dx)

y <- 12

par(mfrow=c(1,1))

plot(pi.grid, dbinom(y,n,pi.grid), type="l",
     xlab=expression(pi), main=paste("Likelihood for y =",y))

?dbinom
### (c) Prior densities

par(mfrow=c(2,2))

  # flat

flat.prior <- dunif(pi.grid,0,1)

plot(pi.grid, flat.prior, type="l",
     ylim=c(0,1), xlab=expression(pi), main="Flat (Uniform)")

  # beta (matched to class mean and standard deviation)

mean <- 0.262
sd <- 0.175

alpha <- mean*(mean*(1-mean)/sd^2 - 1)
beta <- alpha*(1/mean - 1)

beta.prior <- dbeta(pi.grid,alpha,beta)

plot(pi.grid, beta.prior, type="l",
     xlab=expression(pi), main="Beta")

  # five-point (discrete)

pt.prior <- list(pi=c(0.05,0.15,0.25,0.35,0.5),
                 density=c(0.10,0.30,0.30,0.15,0.15))

plot(pt.prior$pi, pt.prior$density, type="h",
     xlim=c(0,1), ylim=c(0,1), xlab=expression(pi), main="Five-Point")

  # histogram

hist.prior <- function(pi) ifelse(pi < 0.15, 0.25/0.15,
                           ifelse(pi < 0.25, 0.30/0.1,
                           ifelse(pi < 0.35, 0.25/0.1,
                           ifelse(pi < 0.45, 0.05/0.1,
                                             0.15/0.55))))

plot(pi.grid, hist.prior(pi.grid), type="l",
     ylim=c(0,max(hist.prior(pi.grid))), xlab=expression(pi), main="Histogram")


### (d) Posterior densities

par(mfrow=c(2,2))

  # flat prior

post.unscaled <- flat.prior * dbinom(y,n,pi.grid)

posterior <- post.unscaled / sum(post.unscaled * dx)

plot(pi.grid, posterior, type="l",
     xlab=expression(pi), main="Posterior: Flat Prior")

  # beta prior

post.unscaled <- beta.prior * dbinom(y,n,pi.grid)

posterior <- post.unscaled / sum(post.unscaled * dx)

plot(pi.grid, posterior, type="l",
     xlab=expression(pi), main="Posterior: Beta Prior")

  # five-point (discrete) prior

pt.post.unscaled <- pt.prior$density * dbinom(y,n,pt.prior$pi)

pt.posterior <- pt.post.unscaled / sum(pt.post.unscaled)

plot(pt.prior$pi, pt.posterior, type="h",
     xlab=expression(pi), main="Posterior: Five-Point Prior", xlim=c(0,1))

  # histogram prior

post.unscaled <- hist.prior(pi.grid) * dbinom(y,n,pi.grid)

posterior <- post.unscaled / sum(post.unscaled * dx)

plot(pi.grid, posterior, type="l",
     xlab=expression(pi), main="Posterior: Histogram Prior")

