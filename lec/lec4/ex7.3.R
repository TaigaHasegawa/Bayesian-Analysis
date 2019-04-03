### Example 7.3:  Two-Parameter Normal Prediction --- Noninformative Prior ###

n <- 24

ybar <- 7.8730

s <- 0.05353


# Using the standard noninformative prior ...

# Posterior prediction for a "new" Y

ystar.scale <- s^2 * (1 + 1/n)

  # 95% posterior prediction interval (equal-tailed)

ybar + qt(c(0.025, 1-0.025), n-1) * sqrt(ystar.scale)

  # Posterior probability that "new" Y >= 7.9379 (minimum legal weight)

post.prob <- 1 - pt((7.9379 - ybar) / sqrt(ystar.scale), n-1)

post.prob

