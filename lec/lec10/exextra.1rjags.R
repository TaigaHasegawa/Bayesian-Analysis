### Extra Example 1:  Boat Races --- Latent Time-Series GLM ###

library(rjags)


### Set up data, initializations, and model

boatraces <- read.table("exextra.1data.txt", header=TRUE)

all.years <- min(boatraces$year):(max(boatraces$year)+1)
  # add a final year to predict next year's race

oxford.win <- numeric(length(all.years))
oxford.win[] <- NA

oxford.win[match(boatraces$year,all.years)][boatraces$winner=="Oxford"] <- 1
oxford.win[match(boatraces$year,all.years)][boatraces$winner=="Cambridge"] <- 0

d <- list(oxfordwin=oxford.win, yearstand=as.vector(scale(all.years)))


inits <- list(list(beta0=0, beta1=0, tausq=1, rho=0),
              list(beta0=1, beta1=1, tausq=10, rho=0.5),
              list(beta0=-1, beta1=-1, tausq=0.1, rho=-0.5))

m <- jags.model("exextra.1model.bug", d, inits, n.chains=3)


### Make a preliminary run of 1000 iterations, with monitoring

length(oxford.win)

x <- coda.samples(m, c("beta0","beta1","sigmasq","rho","pi[191]"), n.iter=1000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

autocorr.plot(x[1], ask=TRUE)

gelman.diag(x, autoburnin=FALSE, multivariate=FALSE)


### Run 10000 more iterations

x <- coda.samples(m, c("beta0","beta1","sigmasq","rho","pi[191]"), n.iter=10000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

gelman.diag(x, autoburnin=FALSE, multivariate=FALSE)

gelman.plot(x, autoburnin=FALSE, ask=TRUE)


### Run 100000 more iterations

x <- coda.samples(m, c("beta0","beta1","sigmasq","rho","pi[191]",
                       "oxfordwin[191]","rho.gt.0"), n.iter=100000)


### Assess convergence

plot(x, smooth=FALSE, ask=TRUE)

gelman.diag(x, autoburnin=FALSE, multivariate=FALSE)

gelman.plot(x, autoburnin=FALSE, ask=TRUE)


### Check stats

summary(x)

  # Verify: Time-series SE less than 1/20 of SD

plot(x, trace=FALSE, ask=TRUE)



### Run 100000 more iterations for sampling pi (optional)

x <- coda.samples(m, c("pi"), n.iter=100000, thin=20)

summ.x <- summary(x)

pi.means <- summ.x$statistics[paste0("pi[",1:191,"]"),"Mean"]

pdf("posteriorprobs.pdf",12,5)
plot(all.years, pi.means, type="l", xlab="Year", ylab="Prob. Oxford Wins",
     main="Posterior Means of Oxford Win Probabilities")
abline(h=0.5,lty=3,col="red")
dev.off()
