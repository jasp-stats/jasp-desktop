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

# TODO: copied from common, do this more elegantly!
fromJSON  <- function(x) jsonlite::fromJSON(x, TRUE, FALSE, FALSE)
toJSON    <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, null = "null")
.extractErrorMessage <- function(error) {
  
  split <- base::strsplit(as.character(error), ":")[[1]]
  last <- split[[length(split)]]
  stringr::str_trim(last)
}



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

expand_default <- function(scale, discrete = c(0, 0.6, 0, 0.6), continuous = c(0.05, 0, 0.05, 0)) {
  # copy of ggplot2:::expand_default to please R CMD check about :::
  a <- scale$expand
  if (!is.waive(a))
    return(a)
  else if (scale$is_discrete())
    return(discrete)
  else 
    return(continuous)
}

getAxisInfo.ScaleContinuousPosition <- function(x, opts, ggbuild) {

  xory <- x[["aesthetics"]][1L]
  nms2keep <- c("labels", "major_source")
  nms2give <- c("labels", "breaks")
  opts2keep <- opts[paste(xory, nms2keep, sep = ".")]
  names(opts2keep) <- nms2give
  if (is.waive(x[["expand"]])) {
    opts2keep[["expand"]] <- expand_default(x)
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

    if (is.numeric(currentAxis[["limits"]])) 
      currentAxis[["limits"]] <- range(currentAxis[["limits"]], newSettings[["breaks"]])
    else
      currentAxis[["limits"]] <- range(newSettings[["breaks"]])
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

#' @title Get the editable options for a graph 
#' @param graph a ggplot2 object
#' @param asJSON should the list be converted to JSON?
#'
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
plotEditingOptions.ggplot_built <- function(graph, asJSON = FALSE) {

  # only relevant for continuous scales?
  e <- try({
    opts <- graph[["layout"]][["coord"]][["labels"]](graph[["layout"]][["panel_params"]])[[1L]]
    axisTypes <- getAxisType(graph)
    currentAxis <- graph[["layout"]][["get_scales"]](1L)
    
    xSettings <- getAxisInfo(currentAxis[["x"]], opts, graph)
    ySettings <- getAxisInfo(currentAxis[["y"]], opts, graph)
    
    out <- list(xAxis = list(
      type     = axisTypes[["x"]],
      settings = xSettings
    ), yAxis = list(
      type     = axisTypes[["y"]],
      settings = ySettings
    )
    )
  })
  
  if (inherits(e, "try-error")) {
    out <- list(error = paste("computing plotEditingOptions gave an error:", .extractErrorMessage(e)))
    if (asJSON)
      return(toJSON(out))
    else
      return(out)
  }

  if (asJSON) {
    out <- try(toJSON(out))
    if (inherits(out, "try-error"))
      out <- toJSON(list(error = paste("converting plotEditingOptions to JSON gave an error:", .extractErrorMessage(out))))
  }

  return(out)
}

#' @export
plotEditingOptions.default <- function(graph, asJSON = FALSE) {
  out <- list(error = paste("cannot create plotEditingOptions for object of class:", paste(class(graph), collapse = ",")))
  if (asJSON)
    return(toJSON(out))
  else
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

#' @title Edit a plot
#' @param graph a ggplot2 object
#' @param newOptions an options list
#'
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
