
#'@importFrom ggplot2 continuous_scale discrete_scale

JASPgraphs_data <- list2env(list(
  # discrete color scales
  colorblind  = list(colors = RColorBrewer::brewer.pal(8L, "Dark2")),
  colorblind2 = list(colors = RColorBrewer::brewer.pal(8L, "Set2")),
  colorblind3 = list(colors = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")), # from ggthemes
  viridis     = list(colors = viridisLite::viridis(256L)), # viridis::scale_color_viridis
  blue        = list(colors = c("#d1e1ec", "#b3cde0", "#6497b1", "#005b96", "#03396c", "#011f4b")), # bayesplot::color_scheme_get("blue") 
  gray        = list(colors = c("#DFDFDF", "#bfbfbf", "#999999", "#737373", "#505050", "#383838")), # bayesplot::color_scheme_get("gray")
  ggplot2     = list(colors = c("#F8766D", "#CD9600", "#7CAE00", "#00BE67", "#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC"),
                     fun    = scales::hue_pal())
))

#'@title JASP color palettes
#'@param palette Palette to choose from.
#'@param asFunction Should a function be returned or the raw colors? If a function is returned, it either takes a single integer or a vector in 0, 1 as input.
#'@param ... Further arguments for \code{\link[ggplot2]{scale_colour_continuous}}.
#'
#'@details For ggplot2s, the convenience functions \code{scale_JASPcolor_\*} and \code{scale_JASPfill_\*} exist.
#'
#'@return Either a character vector of colors or a function.
#'@export
#'@example inst/examples/ex-colorPalettes.R
#'@rdname colors
JASPcolors <- function(palette = getGraphOption("palette"), asFunction = FALSE) {

  if (!is.character(palette)) {
    stop("palette must be character!")
  } else if (!palette %in% names(JASPgraphs_data)) {
    stop(sprintf("palette was %s but must be one of %s", as.character(palette), paste(names(JASPgraphs_data), collapse = ", ")))
  }
  colors <- JASPgraphs_data[[palette]][["colors"]]
  if (asFunction) {
    fun <- JASPgraphs_data[[palette]][["fun"]]
    if (!is.null(fun))
      return(fun)

    return(function(n) {
      scales::gradient_n_pal(colors, values = NULL)(seq(0, 1, length.out = n))
    })
  } else {
    return(colors)
  }
}

# functions below mimic setup from viridis::scale_color_viridis
#'@rdname colors
#'@export
scale_JASPcolor_continuous <- function(palette = getGraphOption("palette"), ...) {
  ggplot2::scale_color_gradientn(colours = JASPcolors(palette = palette), ...)
}

#'@rdname colors
#'@export
scale_JASPfill_continuous <- function(palette = getGraphOption("palette"), ...) {
  ggplot2::scale_fill_gradientn(colours = JASPcolors(palette = palette), ...)
}

#'@rdname colors
#'@export
scale_JASPcolor_discrete <- function(palette = getGraphOption("palette"), ...) {
  discrete_scale("color", "JASPcolor", JASPcolors(palette = palette, asFunction = TRUE), ...)
}

#'@rdname colors
#'@export
scale_JASPfill_discrete <- function(palette = getGraphOption("palette"), ...) {
  discrete_scale("fill", "JASPcolor", JASPcolors(palette = palette, asFunction = TRUE), ...)
}

