n <- 100
dfLines <- data.frame(
  x = seq_len(n),
  y = c(0, rnorm(n - 1, seq_len(n - 1) / 30, .5)) # log Bayes factor
)

PlotRobustnessSequential(
  dfLines      = dfLines,
  xName        = "n",
)

BF10 <- exp(tail(dfLines, 1)$y)
PlotRobustnessSequential(
  dfLines      = dfLines,
  xName        = "n",
  BF           = BF10,
  bfType       = "BF10"
)
