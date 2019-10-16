library(ggplot2)
value <- c(25, 25, 50)
gg <- letters[1:3]
ga <- letters[4:6]

g <- plotPieChart(value, gg)
print(g)
plotPieChart(value, gg, ga)
plotPieChart(value, gg, ga, showAxisTicks = FALSE)
plotPieChart(value, gg, ga, showAxisTicks = FALSE, legend.position = "none") # hide the legend

# axis can still be modified
print(g + scale_y_continuous(breaks = c(50, 75, 0)))
print(g + scale_y_continuous(breaks = seq(0, 100, 10)))
cm <- c(0, cumsum(value))
breaks <- 100 - (cm[-1] + cm[-length(cm)]) / 2
print(g + scale_y_continuous(breaks = breaks, labels = gg))

# something more extreme:
value <- rpois(25, 10)
g <- as.character(seq_len(25))
plotPieChart(value, g)
