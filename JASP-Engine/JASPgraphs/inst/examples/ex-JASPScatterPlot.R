x <- rnorm(100)
y <- rgamma(100, 1, 1)

JASPScatterPlot(x, y, xName = "Normal", yName = expression(gamma))
JASPScatterPlot(x, y, xName = "Normal", yName = expression(gamma), forceLinearSmooth = TRUE)
JASPScatterPlot(x, y, xName = "Normal", yName = expression(gamma), forceLinearSmooth = TRUE, smoothCIValue = .99)
JASPScatterPlot(x, y, xName = "Normal", yName = "Gamma", addSmooth = FALSE)
JASPScatterPlot(x, y, xName = "Normal", yName = "Gamma", addSmoothCI = FALSE)

JASPScatterPlot(x, y, xName = "Normal", yName = "Gamma", plotAbove = "histogram")
JASPScatterPlot(x, y, xName = "Normal", yName = "Gamma", plotAbove = "histogram", plotRight = "histogram")

JASPScatterPlot(x, y, xName = "Normal", yName = "Gamma", plotAbove = "histogram", plotRight = "none")
JASPScatterPlot(x, y, xName = "Normal", yName = "Gamma", plotAbove = "none",      plotRight = "none")

# we can do the same with a grouping variable
data("mtcars")
JASPScatterPlot(x = mtcars$mpg, y = mtcars$disp, group = mtcars$cyl)
JASPScatterPlot(x = mtcars$mpg, y = mtcars$disp, group = mtcars$cyl, plotAbove = "histogram", plotRight = "histogram")

## NOT RUN
# spot the differences (if you find any report them to the package maintainer!)
plot <- JASPScatterPlot(x, y, xName = "Normal", yName = "Gamma", addSmooth = FALSE, emulateGgMarginal = TRUE)
plot
ggExtra::ggMarginal(plot$subplots[[1]])

