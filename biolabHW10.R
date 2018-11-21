# Q1

pBin <- function(x, n, p)
{
  y <- seq(0, x, by = 1)
  return(sum(choose(n, y) * (p ^ y) * (1 - p) ^ (n - y)))
}

# the results are the same
pBin(2, 10, 0.5)
pbinom(2, 10, 0.5)

# =================================================================

# Q2

simuPower <- function(n, delta, sd = 1, sig.level = 0.05, type = 'one.sample')
{
  simuT <- rt(10000, n - 1)
  a <- quantile(simuT, 1 - sig.level / 2) - delta * sqrt(n) / sd
  return(sum(simuT >= a) / 10000)
}

# the results are very close
simuPower(30, 0.5)
power.t.test(n = 30, delta = 0.5, sd = 1, sig.level = 0.05, type = 'one.sample')

