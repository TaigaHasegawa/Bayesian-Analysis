### Example 10.2:  Baby Rat Growth --- Hierarchical Normal (Univariate) ###

library(rjags)


### Set up data, initializations, and model

ages <- c(8, 15, 22, 29, 36)

d <- list(Y = read.table("ex10.2data.txt"),
          x = ages,
          xbar = mean(ages))

inits <- list(list(tausq.y=1, beta0=0, beta1=0,
                   tausq.alpha0=1, tausq.alpha1=1),
              list(tausq.y=100, beta0=100, beta1=100,
                   tausq.alpha0=100, tausq.alpha1=100),
              list(tausq.y=0.01, beta0=-100, beta1=-100,
                   tausq.alpha0=0.01, tausq.alpha1=0.01))

m <- jags.model("ex10.2model.bug", d, inits, n.chains=3)


### Make a preliminary run of 1000 iterations, with monitoring

x <- coda.samples(m, c("beta0","beta1","sigma.y","sigma.alpha0","sigma.alpha1"),
                  n.iter=1000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

autocorr.plot(x[1], ask=TRUE)

gelman.diag(x, autoburnin=FALSE)


### Run 10000 more iterations

x <- coda.samples(m, c("beta0","beta1","sigma.y","sigma.alpha0","sigma.alpha1"),
                  n.iter=10000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

gelman.diag(x, autoburnin=FALSE)

gelman.plot(x, autoburnin=FALSE, ask=TRUE)


### Check stats after burn-in

summary(window(x, 4000))

  # Verify: Time-series SE less than 1/20 of SD

plot(window(x, 4000), trace=FALSE, ask=TRUE)


### Posterior correlations between beta0 and beta1?

plot(as.matrix(window(x, 4000)[,c("beta0","beta1")]), pch=".")

   # none apparent, probably because x values are mean-centered
