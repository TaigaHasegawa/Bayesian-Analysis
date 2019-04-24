### Extra Example 2: Newcomb Data --- Robust Analysis (outliers) ###

library(MASS)

newcomb

hist(newcomb, breaks=50)


### Set up data, initializations, and model

library(rjags)

d <- list(y = newcomb)

inits <- list(list(mu=1000, sigmasq=1000000, nuinv=0.01),
              list(mu=1000, sigmasq=0.01, nuinv=0.99),
              list(mu=-1000, sigmasq=1000000, nuinv=0.99),
              list(mu=-1000, sigmasq=0.01, nuinv=0.01))

m <- jags.model("exextra.2model.bug", d, inits, n.chains=4, n.adapt=1000)


### Routine burn-in

update(m, 1000)


### Assess convergence and mixing

x <- coda.samples(m, c("mu","sigmasq","nuinv"), n.iter=2000)

gelman.diag(x, autoburnin=FALSE)

gelman.plot(x, autoburnin=FALSE)

autocorr.plot(x[1], ask=TRUE)
autocorr.plot(x[2], ask=TRUE)
autocorr.plot(x[3], ask=TRUE)
autocorr.plot(x[4], ask=TRUE)

plot(x, smooth=FALSE, ask=TRUE)


### Sample posterior

x <- coda.samples(m, c("mu","sigmasq","nuinv","yrep"), n.iter=2000)

effectiveSize(x[, c("mu","sigmasq","nuinv")])

summary(x[, c("mu","sigmasq","nuinv")])


mean(newcomb)

mean(newcomb[-c(2,54)])

t.test(newcomb)$conf.int

t.test(newcomb[-c(2,54)])$conf.int


densplot(x[,"mu"])

dtscaled <- function(x, mu, sigma, nu) dt((x-mu)/sigma, nu)/sigma

curve(dtscaled(x, mean(newcomb), sd(newcomb)/sqrt(length(newcomb)),
               length(newcomb)-1), add=TRUE, col="blue")
  # posterior density of mu under a normal model (w/ improper prior)


densplot(x[,"nuinv"])



### Do posterior predictive checks, using min and max as discrepancies

yrep <- as.matrix(x)[, paste("yrep[",1:length(newcomb),"]", sep="")]

mean(apply(yrep, 1, min) <= min(newcomb))  # p.b for min

mean(apply(yrep, 1, max) >= max(newcomb))  # p.b for max

  # similar checks on the normal model fail (try yourself)
