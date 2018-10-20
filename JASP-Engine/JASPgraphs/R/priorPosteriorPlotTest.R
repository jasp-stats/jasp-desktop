#
# a <- 50
# b <- 50
#
# x <- seq(0, 1, length.out = 1e3)
# y0 <- dbeta(x, 1, 1)
# y1 <- dbeta(x, a, b)
# dfLines <- data.frame(x = x, y = c(y0, y1), g = rep(c("Prior", "Posterior"), c(length(y0), length(y1))))
#
# PlotPriorAndPosterior(dfLines, xName = expression(theta))
#
# # add points to plot
# dfPoints <- data.frame(
#   x = .5,
#   g = c("prior", "posterior")
# )
# dfPoints$y <- dbeta(dfPoints$x, c(1, a), c(1, b))
#
# PlotPriorAndPosterior(dfLines, dfPoints)
#
# # add CRI to plot
# CRI <- qbeta(c(.025, 0.975), a, b)
#
# p <- PlotPriorAndPosterior(dfLines, dfPoints, CRI = CRI)
# p
#
# # add median to plot
# median <- qbeta(.5, a, b)
#
# p <- PlotPriorAndPosterior(dfLines, dfPoints, CRI = CRI, median = median)
# p
#
# # add Bayes factor to plot
#
# BF <- dfPoints$y[2] / dfPoints$y[1]
# p <- PlotPriorAndPosterior(dfLines, dfPoints, CRI = CRI, median = median, BF = BF)
# p
