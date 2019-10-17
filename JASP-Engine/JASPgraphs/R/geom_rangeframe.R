#' @importFrom grid gpar segmentsGrob gTree gList unit
#' @importFrom ggplot2 .pt

# geom_rangeframe is adapted from ggthemes::geom_rangeframe, but it uses the panel_scales
# to compute the endpoints of the lines rather than the data (as ggthemes::geom_rangeframe does)

ggname <- function(prefix, grob) {
  # copy of ggthemes:::ggname
  grob$name <- grid::grobName(grob, prefix)
  grob
}

#' Add base R's bty = 'n' to ggplot2 objects.
#'
#' @description Axis lines which extend to the maximum and minimum of the axis breaks. The implementation and documentation is largely adapted from \code{\link[ggthemes]{geom_rangeframe}}.
#'
#'@section Aesthetics:
#' \itemize{
#' \item colour
#' \item size
#' \item linetype
#' \item alpha
#' }
#'
#' @inheritParams ggplot2::geom_point
#' @param sides A string that controls which sides of the plot the frames appear on.
#'   It can be set to a string containing any of \code{'trbl'}, for top, right,
#'   bottom, and left. By default, only the bottom and left axes lines are drawn. 
#'   Note that this is checked at drawing time, so 'b' always means bottom even when using \code{\link[ggplot2]{coord_flip}}.
#' @param panelInfo A list that specifies what information is drawn from what component of panel_scales. 
#' Usually, \code{x.major} corresponds to the bottom axis. However, if a scale is used to move e.g., 
#' the x-axis to be above the plot then this needs to be adjusted to \code{x.sec.major_source}. By default,
#' this argument assumes that axis lines above and right of a plot use \code{\link[ggplot2]{sec_axis}} and are
#' therefore draw information from \code{*.sec.major_source}. You can partially set this list; if e.g., 
#' \code{"t"} is missing it will be filled with its default value.
#' 
#'
#' @references Tufte, Edward R. (2001) The Visual Display of Quantitative Information, Chapter 6.
#' @references Jeffrey B. Arnold (2019). ggthemes: Extra Themes, Scales and Geoms for 'ggplot2'. R package version 4.2.0. https://CRAN.R-project.org/package=ggthemes
#'
#' @export
#'
#' @example inst/examples/ex-geom_rangeframe.R
geom_rangeframe <- function(mapping = NULL,
                            data = NULL,
                            stat = "identity",
                            position = "identity",
                            ...,
                            sides = "bl",
                            panelInfo = list("t" = "x.sec.major", "r" = "y.sec.major",
                                             "b" = "x.major", "l" = "y.major"),
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = FALSE) {

  layer(
    # we need to pass some data, otherwise the layer is skipped when no data is inherited
    data = if (is.null(data)) data.frame(x = 1) else data,
    mapping = mapping,
    stat = stat,
    geom = GeomRangeFrame,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      sides     = sides,
      panelInfo = panelInfo,
      na.rm     = na.rm,
      ...
    )
  )
}

#' @rdname geom_rangeframe
#' @usage NULL
#' @format NULL
#' @export
GeomRangeFrame <- ggplot2::ggproto("GeomRangeFrame", ggplot2::Geom,
  optional_aes = c("x", "y"),

  draw_panel = function(data, panel_scales, coord, sides = "bl",
                        panelInfo = list("t" = "x.sec.major", "r" = "y.sec.major",
                                         "b" = "x.major", "l" = "y.major"),
                        lineend = "butt", color = "black",
                        alpha = 1, linetype = 1, size = getGraphOption("bty")[["lwdX"]],
                        adj = getGraphOption("axisTickWidth"), extendForAxisTicks = TRUE) {

    panelInfo <- setDefaults(lst = panelInfo,
                             "t" = "x.sec.major", "r" = "y.sec.major", "b" = "x.major", "l" = "y.major")

    rugs <- list()
    data <- coord[["transform"]](data, panel_scales)
    gp <- gpar(col = scales::alpha(color, alpha),
               lty = linetype,
               lwd = size * ggplot2::.pt,
               lineend = lineend)

    # magic conversion size
    adj <- unit(0.375 * .pt * adj, "pt")

    if (grepl("b", sides)) {
      rr <- range(panel_scales[[panelInfo[["b"]]]])
      rugs[["x_b"]] <- ggname(
        "range_x_b",
        segmentsGrob(x0 = unit(rr[1L], "native") - adj,
                     x1 = unit(rr[2L], "native") + adj,
                     y0 = unit(0, "npc"),
                     y1 = unit(0, "npc"),
                     gp = gp))
    }

    if (grepl("t", sides)) {
      rr <- range(panel_scales[[panelInfo[["t"]]]])
      rugs[["x_t"]] <- ggname(
        "range_x_t",
        segmentsGrob(x0 = unit(rr[1L], "native") - adj,
                     x1 = unit(rr[2L], "native") + adj,
                     y0 = unit(1, "npc"),
                     y1 = unit(1, "npc"),
                     gp = gp))
    }

    if (grepl("l", sides)) {
      rr <- range(panel_scales[[panelInfo[["l"]]]])
      rugs[["y_l"]] <- ggname(
        "range_y_l",
        segmentsGrob(y0 = unit(rr[1L], "native") - adj,
                     y1 = unit(rr[2L], "native") + adj,
                     x0 = unit(0, "npc"),
                     x1 = unit(0, "npc"),
                     gp = gp))
    }

    if (grepl("r", sides)) {
      rr <- range(panel_scales[[panelInfo[["r"]]]])
      rugs[["y_r"]] <- ggname(
        "range_y_r",
        segmentsGrob(y0 = unit(rr[1L], "native") - adj,
                     y1 = unit(rr[2L], "native") + adj,
                     x0 = unit(1, "npc"),
                     x1 = unit(1, "npc"),
                     gp = gp))
    }
    ggname("geom_rangeframe", gTree(children = do.call("gList", rugs)))
  },
  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),

  draw_key = ggplot2::draw_key_path
)

