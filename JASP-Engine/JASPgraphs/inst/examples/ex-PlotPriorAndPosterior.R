n <- 100
x <- seq(-3, 3, length.out = n)
dfLines <- data.frame(
  x = x,
  y = c(dnorm(x, 0, 1), dnorm(x, 1, .5)),
  g = rep(c("Prior", "Posterior"), each = n)
)

PlotPriorAndPosterior(dfLines)
dfPoints <- data.frame(
  x = c(0, 0),
  y = dnorm(0, c(0, 1), c(1, .5)),
  g = c("Prior", "Posterior")
)

PlotPriorAndPosterior(dfLines, dfPoints)

# add BF pizza
BF10 <- dnorm(0, 1, .5) / dnorm(0, 0, 1)
PlotPriorAndPosterior(dfLines, dfPoints, BF10,      bfType = "BF10") # default
PlotPriorAndPosterior(dfLines, dfPoints, 1 / BF10,  bfType = "BF01")
PlotPriorAndPosterior(dfLines, dfPoints, log(BF10), bfType = "LogBF10")

# change hypothesis
PlotPriorAndPosterior(dfLines, dfPoints, BF10, hypothesis = "smaller")
PlotPriorAndPosterior(dfLines, dfPoints, BF10, hypothesis = "greater")

# add credible interval
CRI <- qnorm(c(0.025, 0.975), 1, .5)
PlotPriorAndPosterior(dfLines, dfPoints, CRI = CRI)
PlotPriorAndPosterior(dfLines, dfPoints, CRI = CRI, drawCRItxt = FALSE)

# add median
median <- qnorm(0.5, 1, .5)
PlotPriorAndPosterior(dfLines, dfPoints, median = median)

# or combine them all
PlotPriorAndPosterior(dfLines, dfPoints, BF10, CRI = CRI, median = median)
