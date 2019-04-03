### Example 10.1:  Shark Attacks --- Poisson Loglinear Regression ###

library(rjags)


### Set up data, initializations, and model

d <- read.table("ex10.1data.txt", header=TRUE)

inits <- list(list(beta0=0, beta1=0),
              list(beta0=1, beta1=1),
              list(beta0=-1, beta1=-1))

m <- jags.model("ex10.1model.bug", d, inits, n.chains=3)


### Make a preliminary run of 1000 iterations, with monitoring

x <- coda.samples(m, c("beta0","beta1","lambda","y[14]","beta1.gt.0"),
                  n.iter=1000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

autocorr.plot(x[1], ask=TRUE)

gelman.diag(x, autoburnin=FALSE, multivariate=FALSE)


### Run 10000 more iterations

x <- coda.samples(m, c("beta0","beta1","lambda","y[14]","beta1.gt.0"),
                  n.iter=10000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

gelman.diag(x, autoburnin=FALSE, multivariate=FALSE)


### Run 100000 more iterations

x <- coda.samples(m, c("beta0","beta1","lambda","y[14]","beta1.gt.0"),
                  n.iter=100000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

gelman.diag(x, autoburnin=FALSE, multivariate=FALSE)

gelman.plot(x, autoburnin=FALSE, ask=TRUE)


### Check stats after burn-in

summary(window(x, 60000))

  # Verify: Time-series SE less than 1/20 of SD

plot(window(x[,c("beta0","beta1","y[14]","beta1.gt.0")], 60000),
     trace=FALSE, ask=TRUE)
