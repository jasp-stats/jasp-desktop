#' Make a Pie Chart
#'
#' @param value The value for each group.
#' @param group Character of factor that indicates which value belongs to which group.
#' @param legendName Character, title of the legend.
#' @param legendLabels Character vector with names for the legend.
#' @param legendColors Character vector with colors.
#' @param showAxisText Logical, should the axis text be shown?
#' @param asPercentages Logical, should value be transformed to percentages? Recommended to be true.
#' @param showAxisTicks Logical, should the ticks on the polar coodinates be shown?
#' @param palette Character vector with palette name. If this option is set then legendColors is ignored. If palette is NULL then legendColors are used.
#' @param ... Arguments to be passed to \code{\link{themeJasp}}.
#'
#' @return a ggplot object.
#' @export
#'
#' @example inst/examples/ex-PlotPieChart.R
#'
#' @importFrom ggplot2 ggplot geom_bar coord_polar scale_fill_manual labs element_text element_line element_blank
plotPieChart <- function(value, group,
                         legendName = deparse(substitute(group)),
                         legendLabels = if (is.factor(group)) levels(group) else unique(group),
                         legendColors = NULL,
                         showAxisText = TRUE, showAxisTicks = showAxisText, asPercentages = TRUE,
                         palette = getGraphOption("palette"),
                         ...) {

  if (!is.numeric(value))
    stop("value should be numeric!")
  if (!(is.character(group) || is.factor(group)))
    stop("group should be a character or factor!")
  if (length(value) != length(group))
    stop("value and group should have the same length!")

  # change the default arguments for themeJasp
  dots <- list(...)
  dots <- setDefaults(dots,
                      legend.position = "right",
                      bty = "o")

  if (asPercentages)
    value <- value / sum(value) * 100

  nUnique <- length(unique(group))
  if (length(legendColors) == 1L && legendColors %in% names(JASPgraphs_data)) {
    legendColors <- JASPgraphs_data[[legendColors]][["colors"]]
    legendColors <- scales::gradient_n_pal(legendColors)(seq(0, 1, length.out = nUnique))
  }
  
  df <- data.frame(
    y = value,
    g = factor(group)
  )

  scale_fill <- NULL
  if (!is.null(palette)) {
    scale_fill <- scale_JASPfill_discrete(palette = palette, name = legendName,
                                          breaks = legendLabels)
  } else if (!is.null(legendColors)) {
    scale_fill <- scale_fill_manual(values = legendColors, name = legendName, breaks = legendLabels)
  }

  graph <- ggplot(df, aes(x = "", y=value, fill=group)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    scale_fill +
    labs(x = "", y = "")

  return(do.call(themeJasp, c(list(graph = graph), dots)) + theme(
    axis.text.x = if (showAxisText) element_text() else element_blank(),
    axis.ticks  = element_blank()
  ))

}
