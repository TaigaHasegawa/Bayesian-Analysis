### Example 8.6:  Wikipedia Article Modifications ###

library(rjags)


### Set up data, initializations, and model

d <- read.table("ex8.6data.txt", header=TRUE)

inits <- list(list(mu=5, tausq=1.5), 
              list(mu=0, tausq=15), 
              list(mu=10, tausq=0.15))

m <- jags.model("ex8.6model.bug", d, inits, n.chains=3)


### Make a preliminary run of 1000 iterations, with monitoring

x <- coda.samples(m, c("mu","sigmasq","expmu"), n.iter=1000)


### Assess convergence

plot(x)  # preliminary trace plots and densities

autocorr.plot(x[1])

gelman.diag(x, autoburnin=FALSE)

gelman.plot(x, autoburnin=FALSE)
?gelman.diag

### Check stats after burn-in

summary(window(x, 500, 1000))

  # Verify: Time-series SE less than 1/20 of SD

plot(window(x, 500, 1000), trace=FALSE)
