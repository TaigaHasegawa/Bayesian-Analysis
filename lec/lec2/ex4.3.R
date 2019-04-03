### Example 4.3:  Population Proportion --- Posterior Predictive Distrib. ###

n <- 70

y <- 12

n.star <- 10

y.star <- 0:n.star


# using a uniform prior (alpha = beta = 1)


  # Method 1: Using Explicit Formula

post.pred.probs <- choose(n.star, y.star) * (n+1) * choose(n, y) /
                   ((n+n.star+1) * choose(n+n.star,y+y.star))

plot(y.star, post.pred.probs, type="h",
     ylim=c(0,1), xlab=expression(y^symbol("*")),
     main="Posterior Predictive Density")


  # Method 2: Using Numerical Integration

integrand <- function(pi) dbinom(y.star[i],n.star,pi) * dbeta(pi,y+1,n-y+1)

post.pred.probs <- numeric(n.star+1)

for(i in 1:length(y.star))
  post.pred.probs[i] <- integrate(integrand,0,1)$value

plot(y.star, post.pred.probs, type="h",
     ylim=c(0,1), xlab=expression(y^symbol("*")),
     main="Posterior Predictive Density")
