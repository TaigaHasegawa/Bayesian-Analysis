### Example 4.1:  Population Proportion --- Frequentist Methods ###

y <- 12

n <- 70

## Exact Inference

  # to get point estimate and 95% confidence interval ...

binom.test(y, n)

  # e.g. test H0: at least 30% have pets

binom.test(y, n, p=0.3, alternative="less")

