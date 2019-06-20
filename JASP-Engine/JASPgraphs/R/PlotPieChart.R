#' Make a Pie Chart
#'
#' @param value the value for each group.
#' @param group character of factor that indicates which value belongs to which group.
#' @param legendName Character, title of the legend.
#' @param legendLabels character vector with names for the legend.
#' @param showAxisText Logical, should the axis text be shown?
#' @param asPercentages Logical, should value be transformed to percentages? Recommended to be true.
#' @param ... Arguments to be passed to \code{\link{themeJasp}}.
#'
#' @return a ggplot object.
#' @export
#'
#' @examples
#' value <- c(25, 25, 50)
#' gg <- letters[1:3]
#' ga <- letters[4:6]
#'
#' g <- plotPieChart(value, gg)
#' print(g)
#' plotPieChart(value, gg, ga)
#' plotPieChart(value, gg, ga, showAxisTicks = FALSE)
#' plotPieChart(value, gg, ga, showAxisTicks = FALSE, legend.position = "none") # hide the legend
#'
#' # axis can still be modified
#' print(g + scale_y_continuous(breaks = seq(0, 100, 10)))
#' cm <- c(0, cumsum(value))
#' breaks <- 100 - (cm[-1] + cm[-length(cc)]) / 2
#' print(g + scale_y_continuous(breaks = breaks, labels = gg))
#'
#' # something more extreme:
#' value <- rpois(25, 10)
#' g <- as.character(seq_len(25))
#' plotPieChart(value, g)
#'
#'
#'
#'
#' @importFrom ggplot2 ggplot geom_bar coord_polar scale_fill_discrete labs element_text element_line element_blank
plotPieChart <- function(value, group,
                         legendName = deparse(substitute(group)),
                         legendLabels = if (is.factor(group)) levels(group) else unique(group),
                         showAxisText = TRUE, showAxisTicks = showAxisText, asPercentages = TRUE,
                         ...) {

  if (!is.numeric(value))
    stop("value should be numeric!")
  if (!(is.character(group) || is.factor(group)))
    stop("group should be a character or factor!")
  if (length(value) != length(group))
    stop("value and group should have the same length!")

  # change the default arguments for themeJasp
  dots <- list(...)
  dots <- setDefaults(dots, legend.position = "right")

  if (asPercentages)
    value <- value / sum(value) * 100

  df <- data.frame(
    y = value,
    g = factor(group)
  )

  graph <- ggplot(df, aes(x = "", y=value, fill=group))+
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    scale_fill_discrete(name = legendName, breaks = legendLabels) +
    labs(x = "", y = "")

  return(do.call(themeJasp, c(list(graph = graph), dots)) + theme(
    axis.text.x = if (showAxisText) element_text() else element_blank(),
    axis.ticks  = element_blank()
  ))

}

