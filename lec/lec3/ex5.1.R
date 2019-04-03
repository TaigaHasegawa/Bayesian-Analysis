### Example 5.1:  Population Proportion --- Sensitivity Analysis ###

n <- 70

y <- 12


### Consider the same four priors as in Example 3.1 
### and compute (1) posterior mean, and (2) prob that pi >= 0.3

d.pi <- 1/1000  # pi grid spacing

pi.grid <- seq(0, 1, d.pi)

post.means <- numeric(4)

post.probs <- numeric(4)


  # flat (uniform) prior

flat.prior <- dunif(pi.grid,0,1)

post.unscaled <- flat.prior * dbinom(y,n,pi.grid)

posterior <- post.unscaled / sum(post.unscaled * d.pi)

post.means[1] <- sum(pi.grid * posterior * d.pi)

post.probs[1] <- sum(posterior[pi.grid >= 0.3] * d.pi)


  # beta prior (matched to class mean and standard deviation)

mean <- 0.262
sd <- 0.175

alpha <- mean*(mean*(1-mean)/sd^2 - 1)
beta <- alpha*(1/mean - 1)

beta.prior <- dbeta(pi.grid,alpha,beta)

post.unscaled <- beta.prior * dbinom(y,n,pi.grid)

posterior <- post.unscaled / sum(post.unscaled * d.pi)

post.means[2] <- sum(pi.grid * posterior * d.pi)

post.probs[2] <- sum(posterior[pi.grid >= 0.3] * d.pi)


  # five-point prior (discrete)

pt.prior <- list(pi=c(0.05,0.15,0.25,0.35,0.5),
                 density=c(0.10,0.30,0.30,0.15,0.15))

pt.post.unscaled <- pt.prior$density * dbinom(y,n,pt.prior$pi)

pt.posterior <- pt.post.unscaled / sum(pt.post.unscaled)

post.means[3] <- sum(pt.prior$pi * pt.posterior)

post.probs[3] <- sum(pt.posterior[pt.prior$pi >= 0.3])


  # histogram prior

hist.prior.func <- function(pi) ifelse(pi < 0.15, 0.25/0.15,
                                ifelse(pi < 0.25, 0.30/0.1,
                                ifelse(pi < 0.35, 0.25/0.1,
                                ifelse(pi < 0.45, 0.05/0.1,
                                                  0.15/0.55))))

post.unscaled <- hist.prior.func(pi.grid) * dbinom(y,n,pi.grid)

posterior <- post.unscaled / sum(post.unscaled * d.pi)

post.means[4] <- sum(pi.grid * posterior * d.pi)

post.probs[4] <- sum(posterior[pi.grid >= 0.3] * d.pi)


  # Make table of results

data.frame(post.means, post.probs,
           row.names=c("Flat","Beta","Five-Point","Histogram"))
