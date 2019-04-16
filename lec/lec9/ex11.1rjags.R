### Example 11.1:  Baby Rat Growth --- DIC for Normal Regressions ###

library(rjags)


### Set up data, initializations, and models

ages <- c(8, 15, 22, 29, 36)

d <- list(Y = read.table("ex11.1data.txt"),
          x = ages,
          xbar = mean(ages),
          Omega = rbind(c(200, 0),
                        c(0, 0.2)),
          mu0 = c(0,0),
          Sigma0.inv = rbind(c(1.0E-6, 0),
                             c(0, 1.0E-6)))

inits1 <- list(list(tausq.y=1, beta=c(0,0),
                    Sigma.alpha.inv=diag(2)),
               list(tausq.y=100, beta=c(100,100),
                    Sigma.alpha.inv=100*diag(2)),
               list(tausq.y=0.01, beta=c(-100,-100),
                    Sigma.alpha.inv=0.01*diag(2)))

m1 <- jags.model("ex11.1model1.bug", d, inits1, n.chains=3)

inits2 <- list(list(tausq.y=1, beta0=0, beta1=0,
                    tausq.alpha0=1, tausq.alpha1=1),
               list(tausq.y=100, beta0=100, beta1=100,
                    tausq.alpha0=100, tausq.alpha1=100),
               list(tausq.y=0.01, beta0=-100, beta1=-100,
                    tausq.alpha0=0.01, tausq.alpha1=0.01))

m2 <- jags.model("ex11.1model2.bug", d, inits2, n.chains=3)

inits3 <- list(list(tausq.y=1, beta0=0, beta1=0, tausq.alpha0=1),
               list(tausq.y=100, beta0=100, beta1=100, tausq.alpha0=100),
               list(tausq.y=0.01, beta0=-100, beta1=-100, tausq.alpha0=0.01))

m3 <- jags.model("ex11.1model3.bug", d, inits3, n.chains=3)

inits4 <- list(list(tausq.y=1, beta0=0, beta1=0),
               list(tausq.y=100, beta0=100, beta1=100),
               list(tausq.y=0.01, beta0=-100, beta1=-100))

m4 <- jags.model("ex11.1model4.bug", d, inits4, n.chains=3)


### Do routine burn-in of 10000 iterations

update(m1, 10000)

update(m2, 10000)

update(m3, 10000)

update(m4, 10000)


### Collect samples and approximate DIC values (Plummer's version)

dic.samples(m1, 100000, type="pD")

dic.samples(m2, 100000, type="pD")

dic.samples(m3, 100000, type="pD")

dic.samples(m4, 100000, type="pD")

  # Note: dic.samples requires at least two chains in each model
