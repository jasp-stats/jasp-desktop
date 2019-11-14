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


# Robustness plot add info
# Note(Alexander): At the moment, I need to do quite some work to get the right BF label, 
# perhaps it's easier if we incorporate this into a function with as argument BF10, BF01, logBF10
# Furthermore, adding digits=4 manually seems a bit redundant. Would it be nice if we can set this jaspGraphs wide?
# Moreover, I can't get this right for fewer than 4 points. 
# 
dfPoints <- data.frame(
  x = c(x[15], x[30], x[50], x[80]),
  y = c(y[15], y[30], y[50], y[80]),
  g = JASPgraphs::parseThis(c(
    sprintf("paste(max, ~%s, ':',   phantom(phollll), %s, ~at, ~'r'==%s)", "BF['+'][0]", format(y[15],   digits = 4), format(x[15], digits = 4)),
    sprintf("paste(user~prior, ':', phantom(phll[0]), ~%s==%s)",           "BF[0][1]", format(y[30],  digits = 4)),
    sprintf("paste(wide~prior, ':', phantom(ph[0][0]), ~%s==%s)",          "BF['+'][alpha]", format(y[50],     digits = 4)),
    sprintf("paste(ultrawide~prior, ':', ~%s==%s)",                        "log(BF['+']['-'])", format(y[80], digits = 4))
  )),
  stringsAsFactors = FALSE
)

PlotRobustnessSequential(
  dfLines      = dfLines,
  xName        = expression(paste("Prior width ", kappa)), 
  dfPoints     = dfPoints
)
