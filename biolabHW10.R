# Q1

pBin <- function(x, n, p)
{
  return(sum(rbinom(10000, n, p) <= x) / 10000)
}

pBin(2, 10, 0.5)
mean(replicate(n=10000, pBin(2, 10, 0.5)))
pbinom(2, 10, 0.5)
# the result of function pBin is not stable, so use replicate statement to run the function for multiple times and take the mean as the result
# the results of replicate pBin and pbinom is very close

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

