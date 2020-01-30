# Data for sequential
# 
set.seed(1)
n <- 100
dfLines <- data.frame(
  x = seq_len(n),
  y = c(0, rnorm(n - 1, seq_len(n - 1) / 30, .5)) # log Bayes factor
)


# Sequential plot
# 
PlotRobustnessSequential(
  dfLines      = dfLines,
  xName        = "n",
)

# Sequential plot add info
# 
BF10 <- exp(tail(dfLines, 1)$y)
PlotRobustnessSequential(
  dfLines      = dfLines,
  xName        = "n",
  BF           = BF10,
  bfType       = "BF10"
)

# Data for robustness plot
n <- 100
x <- seq_len(n)/100
y <- cos(pi*x)

dfLines <- data.frame(
  x = x,
  y = y
)

# Robustness plot
# 
PlotRobustnessSequential(
  dfLines      = dfLines,
  xName        = "Prior width"
)

# In JASP, we often prefer more information in the plot
x <- dfLines$x[c(15, 30, 50, 80)]
y <- dfLines$y[c(15, 30, 50, 80)]
maxBFrVal <- x[1]
maxBF10 <- y[1]
BF10user <- y[2]
BF10w <- y[3]
BF10ultra <- y[4]

BFsubscript <- "[0][1]"
label1 <- c(
  gettextf("max BF%s", BFsubscript),
  gettext("user prior"),
  gettext("wide prior"),
  gettext("ultrawide prior")
)
# some failsafes to parse translations as expressions
label1[1] <- gsub(pattern = "\\s+", "~", label1[1])
label1[-1] <- paste0("\"", label1[-1], "\"")
label1 <- paste0("paste(", label1, ", ':')")

BFandSubscript <- gettextf("BF%s", BFsubscript)
BFandSubscript <- gsub(pattern = "\\s+", "~", BFandSubscript)
label2 <- c(
  gettextf("%s at r==%s",      format(maxBF10,  digits = 4), format(maxBFrVal, digits = 4)),
  paste0(BFandSubscript, "==", format(BF10user, digits = 4)),
  paste0(BFandSubscript, "==", format(BF10w,    digits = 4)),
  paste0(BFandSubscript, "==", format(BF10ultra,digits = 4))
)
label2[1L] <- gsub(pattern = "\\s+", "~", label2[1])


dfPoints <- data.frame(
  x = x,
  y = c(maxBF10, BF10user, BF10w, BF10ultra),
  g = label1,
  label1 = JASPgraphs::parseThis(label1),
  label2 = JASPgraphs::parseThis(label2),
  stringsAsFactors = FALSE
)

PlotRobustnessSequential(
  dfLines      = dfLines,
  xName        = expression(paste("Prior width ", kappa)), 
  dfPoints     = dfPoints
)

# convenience function for showing plots side by side. You may need to click zoom in Rstudio
# to properly view the plots.
showSideBySide <- function(..., nrow = 1L, ncol = ...length()) {
  require(gridExtra)
  jaspgraphplot2grob <- function(x) {
    if (!inherits(x, "JASPgraphsPlot")) return(x)
    else return(x$plotFunction(x$subplots, args = x$plotArgs, grob = TRUE))
  }
  gridExtra::grid.arrange(gridExtra::arrangeGrob(grobs = lapply(list(...), jaspgraphplot2grob),  nrow = nrow, ncol = ncol))
}

# arrow labels can be modified
g1 <- PlotRobustnessSequential(dfLines = dfLines, arrowLabel = c("top", "bottom"))
g2 <- PlotRobustnessSequential(dfLines = dfLines, arrowLabel = JASPgraphs::parseThis(c("alpha", "beta")))
showSideBySide(g1, g2)

# text in the top right (evidence text) can be modified
g1 <- PlotRobustnessSequential(dfLines = dfLines, BF = 1)
g2 <- PlotRobustnessSequential(dfLines = dfLines, BF = 1, evidenceTxt = c("I'm above!", "I'm below!"))
g3 <- PlotRobustnessSequential(dfLines = dfLines, BF = 1, evidenceTxt = JASPgraphs::parseThis(c("alpha", "omega")))
showSideBySide(g1, g2, g3)
