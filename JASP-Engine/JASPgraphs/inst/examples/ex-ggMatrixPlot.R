## Not run:
library(ggplot2)
data("diamonds", package = "ggplot2")
vars  <- colnames(diamonds)[c(1, 5)]
nvars <- length(vars)

plotMatrix <- matrix(
  data = list(), 
  nrow = nvars, 
  ncol = nvars, 
  dimnames = list(vars, vars)
)

for (i in seq_along(vars)) for (j in seq_along(vars)) {

  plotMatrix[[i, j]] <-
    ggplot(data = diamonds, aes_string(x = vars[i], y = vars[j])) + 
    geom_point() + 
    geom_rangeframe() + 
    themeJaspRaw()

}

ggMatrixPlot(plotMatrix)

# gives an idea about how ggMatrixPlot works, you can add labels on all sides
# of the center (where the plots should be).
ggMatrixPlot(debug = TRUE)
## End(Not run)