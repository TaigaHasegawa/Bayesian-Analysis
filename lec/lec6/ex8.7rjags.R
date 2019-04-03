### Example 8.7:  Wikipedia Article Modifications --- with Prediction ###

library(rjags)


### Set up data, initializations, and model

d <- read.table("ex8.7data.txt", header=TRUE)

inits <- list(list(mu=5, tausq=1.5), 
              list(mu=0, tausq=15), 
              list(mu=10, tausq=0.15))

m <- jags.model("ex8.7model.bug", d, inits, n.chains=3)

  # znew is automatically initialized


### Make a preliminary run of 1000 iterations, with monitoring

x <- coda.samples(m, c("ynew","ynew.le.30"), n.iter=1000)


### Assess convergence

plot(x)  # preliminary trace plots and densities

autocorr.plot(x[1])

gelman.diag(x, autoburnin=FALSE)

gelman.plot(x, autoburnin=FALSE)


### Check stats after burn-in

summary(window(x, 700, 1000))

  # Verify: Time-series SE less than 1/20 of SD

plot(window(x, 700, 1000), trace=FALSE)
