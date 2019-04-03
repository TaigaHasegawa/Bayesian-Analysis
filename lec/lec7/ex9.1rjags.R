### Example 9.1:  Dyestuff Data --- Normal Random-Effects ###

library(rjags)


### Set up data, initializations, and model

d <- list(y=read.table("ex9.1data.txt"))

inits <- list(list(mu0=1500, tausqW=0.001, tausqB=0.001), 
              list(mu0=3000, tausqW=1, tausqB=1), 
              list(mu0=0, tausqW=0.00001, tausqB=0.00001))

m <- jags.model("ex9.1model.bug", d, inits, n.chains=3)

  # mu[i] values are automatically initialized


### Make a preliminary run of 1000 iterations, with monitoring

x <- coda.samples(m, c("mu","mu0","sigmasqW","sigmasqB","rho"), n.iter=1000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

autocorr.plot(x[1], ask=TRUE)

gelman.diag(x, autoburnin=FALSE)


### Run 10000 more iterations

x <- coda.samples(m, c("mu","mu0","sigmasqW","sigmasqB","rho"), n.iter=10000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

autocorr.plot(x[1], ask=TRUE)

gelman.diag(x, autoburnin=FALSE)

gelman.plot(x, autoburnin=FALSE, ask=TRUE)


### Check stats after burn-in

summary(window(x, 4000, 11000))

  # Verify: Time-series SE less than 1/20 of SD

plot(window(x, 4000, 11000), trace=FALSE, ask=TRUE)


### Plot sigmasqB versus sigmasqW (log scale)

plot(as.matrix(x[,c("sigmasqW","sigmasqB")]), log="xy", pch=".", cex=2)
