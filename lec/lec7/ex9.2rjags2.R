### Example 9.2:  Airliner Fatalities --- Poisson Hierarchical Model ###
### Vague Version ###

library(rjags)


### Set up data, initializations, and model

d <- read.table("ex9.2data.txt", header=TRUE)

inits <- list(list(alpha=1,beta=1),
              list(alpha=0.01,beta=0.01),
              list(alpha=100,beta=100),
              list(alpha=0.01,beta=100),
              list(alpha=100,beta=0.01))

m <- jags.model("ex9.2model2.bug", d, inits, n.chains=5)

  # lambda values are automatically initialized


### Make a preliminary run of 1000 iterations, with monitoring

x <- coda.samples(m, c("lambda","alpha","beta","lambdamean","lambdavar",
                       "lambdanew"), n.iter=1000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

autocorr.plot(x[1], ask=TRUE)

gelman.diag(x, autoburnin=FALSE)


### Run 10000 more iterations

x <- coda.samples(m, c("lambda","alpha","beta","lambdamean","lambdavar",
                       "lambdanew"), n.iter=10000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

gelman.diag(x, autoburnin=FALSE)

gelman.plot(x, autoburnin=FALSE, ask=TRUE)


### Check stats after burn-in

summary(window(x, 6000, 12000))

  # Verify: Time-series SE less than 1/20 of SD

plot(window(x, 6000, 12000), trace=FALSE, ask=TRUE)
