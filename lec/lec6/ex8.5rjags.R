### Example 8.5:  R rjags code for Population proportion example ###

library(rjags)  # automatically loads coda package


### Set up data, initializations, and model

d <- list(y=12, n=70, alpha=0.5, beta=0.5)

inits <- list(list(pi=0.1), list(pi=0.5), list(pi=0.9))

m <- jags.model("ex8.5model.bug", d, inits, n.chains=3, n.adapt=0)
?jags.model
                                        # n.adapt=0 just for illustration


### Make a preliminary run of 1000 iterations, monitoring pi

x <- coda.samples(m, "pi", n.iter=1000)


### Assess convergence

traceplot(x) # plot the chains

autocorr.plot(x[1]) # plot autocorrelations of 1st chain

gelman.diag(x, autoburnin=FALSE) # Gelman-Rubin statistic

gelman.plot(x, autoburnin=FALSE) # Gelman-Rubin statistic plot

?gelman.plot
### Satisfactory by iteration 400, so check stats ...

summary(window(x, 400, 1000)) # the summarized results, iterations 400:1000

  # Note: Time-series SE less than 1/20 of SD

plot(window(x, 400, 1000), trace=FALSE) # plot the approximate density
