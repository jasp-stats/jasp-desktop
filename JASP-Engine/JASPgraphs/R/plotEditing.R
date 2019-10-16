# TODO:
#
# - drop limits from modifyable options?
# - introduce expand as option?
# - breaks[1] >= limits[1] ? expand[1] = breaks[1] - limits[1]] : expands[1] <- 0
#
# better handling of non-standard ggplot scales
# for example, breaks may be NULL, waiver(), a numeric vector, or a function
#
#

fromJSON  <- function(x) jsonlite::fromJSON(x, TRUE, FALSE, FALSE)
toJSON    <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, null = "null")

#' @importFrom ggplot2 layer_scales is.ggplot ggplot_build

`%|NW|%` <- function(a, b) if (!(is.null(a) || is.waive(a))) a else b

# all axis types in ggplot
AxisTypes <- c(
  "ScaleContinuous",
  "ScaleContinuousDate",
  "ScaleContinuousDatetime",
  "ScaleDiscrete"
)

getAxisType <- function(x) {
  UseMethod("getAxisType", x)
}

getAxisType.list <- function(x) {
  # for output of layer_scales(plot)
  scaleX <- match(class(x[[1L]]), AxisTypes)
  scaleY <- match(class(x[[2L]]), AxisTypes)
  scaleX <- scaleX[!is.na(scaleX)]
  scaleY <- scaleY[!is.na(scaleY)]
  scaleX <- AxisTypes[scaleX]
  scaleY <- AxisTypes[scaleY]
  return(c("x" = scaleX, "y" = scaleY))
}

getAxisType.ggplot_built <- function(x) {
  return(c(
    "x" = class(x[["layout"]][["panel_scales_x"]][[1L]])[[2L]],
    "y" = class(x[["layout"]][["panel_scales_y"]][[1L]])[[2L]]
  ))
}

getAxisType.ggplot <- function(x) {
  return(getAxisType.list(layer_scales(x, i = 1L, j = 1L)))
}

getAxisTitle <- function(x, xory) {
  if (xory == "x") {
    return(x[["layout"]][["panel_scales_x"]][[1L]][["name"]] %|NW|% x[["plot"]][["labels"]][["x"]])
  } else {
    return(x[["layout"]][["panel_scales_y"]][[1L]][["name"]] %|NW|% x[["plot"]][["labels"]][["y"]])
  }
}


evenly_spaced <- function(x) {
  by <- x[2L] - x[1L]
  return(all((x[-length(x)] - x[-1L] - by) <= .Machine[["double.eps"]]))
}

getAxisInfo <- function(x, opts, ggbuild) {
  UseMethod("getAxisInfo", x)
}

getAxisInfo.ScaleContinuousPosition <- function(x, opts, ggbuild) {

  xory <- x[["aesthetics"]][1L]
  nms2keep <- c("labels", "major_source")
  nms2give <- c("labels", "breaks")
  opts2keep <- opts[paste(xory, nms2keep, sep = ".")]
  names(opts2keep) <- nms2give
  if (is.waive(x[["expand"]])) {
    opts2keep[["expand"]] <- ggplot2:::expand_default(x)
  } else {
    opts2keep[["expand"]] <- x[["expand"]]
  }

  opts2keep[["title"]] <- getAxisTitle(ggbuild, xory)

  return(opts2keep)

}

getAxisInfo.ScaleDiscretePosition <- function(x, opts, ggbuild) {

  xory <- x[["aesthetics"]][1L]
  return(list(
    labels = x[["get_labels"]](),
    shown  = x[["get_limits"]](),
    title  = getAxisTitle(ggbuild, xory)
  ))

}

internalUpdateAxis <- function(currentAxis, newSettings) {
  if (!is.null(newSettings[["title"]]))
    currentAxis[["name"]] <- newSettings[["title"]]
  UseMethod("internalUpdateAxis", currentAxis)
}

internalUpdateAxis.ScaleContinuousPosition <- function(currentAxis, newSettings) {

  # newSettings only contains modified settings!
  if (!is.null(newSettings[["labels"]]))
    currentAxis[["labels"]] <- c(newSettings[["labels"]])

  if (!is.null(newSettings[["breaks"]])) {
    currentAxis[["breaks"]] <- sort(newSettings[["breaks"]])
    currentAxis[["limits"]] <- range(currentAxis[["limits"]], newSettings[["breaks"]])
    # TODO: see if some plot element fall outside of the new limits!
  }

  if (!is.null(newSettings[["expand"]])) {
    currentAxis[["expand"]] <- newSettings[["expand"]]
  }

  return(currentAxis)
}

internalUpdateAxis.ScaleDiscretePosition <- function(currentAxis, newSettings) {

  # newSettings only contains not modified settings!
  if (!is.null(newSettings[["shown"]]))
    currentAxis[["limits"]] <- newSettings[["shown"]]

  if (!is.null(newSettings[["labels"]]))
    currentAxis[["labels"]] <- newSettings[["labels"]]

  return(currentAxis)
}

validateOptions <- function(newOptions, oldOptions) {

  if (is.character(newOptions)) {
    newOptions <- fromJSON(newOptions)
  } else if (!is.list(newOptions)) {
    stop("options should be an R list or a json string!")
  }

  if (newOptions[["xAxis"]][["type"]] != oldOptions[["xAxis"]][["type"]] ||
      newOptions[["yAxis"]][["type"]] != oldOptions[["yAxis"]][["type"]]) {
    stop("The axis type in the new options list does not match the graph!")
  }

  return(newOptions)
}

#' @export
plotEditingOptions <- function(graph, asJSON = FALSE) {
  UseMethod("plotEditingOptions", graph)
}

#' @export
plotEditingOptions.gg <- function(graph, asJSON = FALSE) {
  return(plotEditingOptions.ggplot(graph, asJSON))
}

#' @export
plotEditingOptions.ggplot <- function(graph, asJSON = FALSE) {

  ggbuild <- ggplot_build(graph)
  return(plotEditingOptions.ggplot_built(ggbuild, asJSON))

}

#' @export
plotEditingOptions.ggplot_built <- function(ggbuild, asJSON = FALSE) {

  # only relevant for continuous scales?
  opts <- ggbuild[["layout"]][["coord"]][["labels"]](ggbuild[["layout"]][["panel_params"]])[[1L]]
  axisTypes <- getAxisType(ggbuild)
  currentAxis <- ggbuild[["layout"]][["get_scales"]](1L)

  xSettings <- getAxisInfo(currentAxis[["x"]], opts, ggbuild)
  ySettings <- getAxisInfo(currentAxis[["y"]], opts, ggbuild)

  out <- list(xAxis = list(
      type     = axisTypes[["x"]],
      settings = xSettings
    ), yAxis = list(
      type     = axisTypes[["y"]],
      settings = ySettings
    )
  )

  if (asJSON)
    out <- toJSON(out)

  return(out)
}

optionsDiff <- function(lst1, lst2) {

  lst1[["xAxis"]][["settings"]] <- lst1[["xAxis"]][["settings"]][
    !unlist(mapply(identical, lst1[["xAxis"]][["settings"]], lst2[["xAxis"]][["settings"]],
                   SIMPLIFY = FALSE, USE.NAMES = FALSE))
  ]

  lst1[["yAxis"]][["settings"]] <- lst1[["yAxis"]][["settings"]][
    !unlist(mapply(identical, lst1[["yAxis"]][["settings"]], lst2[["yAxis"]][["settings"]],
                   SIMPLIFY = FALSE, USE.NAMES = FALSE))
  ]

  return(lst1)
}

#' @export
plotEditing <- function(graph, newOptions) {

  if (!is.ggplot(graph))
    stop("graph should be a ggplot2")

  ggbuild     <- ggplot_build(graph)
  oldOptions  <- plotEditingOptions(ggbuild)
  newOptions  <- validateOptions(newOptions, oldOptions)
  diffOptions <- optionsDiff(newOptions, oldOptions)

  currentAxis <- ggbuild[["layout"]][["get_scales"]](1L)

  if (length(diffOptions[["xAxis"]][["settings"]]) > 0L)
    graph <- graph + internalUpdateAxis(currentAxis[["x"]], diffOptions[["xAxis"]][["settings"]])

  if (length(diffOptions[["yAxis"]][["settings"]]) > 0L)
    graph <- graph + internalUpdateAxis(currentAxis[["y"]], diffOptions[["yAxis"]][["settings"]])

  return(graph)

}
