
#'@importFrom ggplot2 continuous_scale discrete_scale

# from ggthemes::ggthemes_data
JASPgraphs_data <- new.env()
JASPgraphs_data[["colorblind"]][["value"]] <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
  "#D55E00", "#CC79A7"
)

#'@title JASP color palettes
#'@param n Number of colors desired.
#'@param palette Palette to choose from.
#'@param asFunction Should a function be returned or the raw colors? If a function is returned, it either takes a single integer or a vector in 0, 1 as input.
#'@param ... Further arguments for \code{\link{ggplot2::scale_color_manual}} or \code{\link{ggplot2::scale_fill_manual}}.
#'
#'@details For ggplot2s, the convenience functions \code{scale_JASPcolor_\*} and \code{scale_JASPfill_\*} exist.
#'
#'@return Either a character vector of colors or a function.
#'@export
#'@example inst/examples/ex-colorPalettes.R
#'@rdname colors
JASPcolors <- function(n, palette = getGraphOption("palette")[["palette"]], asFunction = FALSE) {

  if (!palette %in% names(JASPgraphs_data)) {
    stop(sprintf("palette was %s but must be one of %s", as.character(palette), names(JASPgraphs_data)))
  }
  colors <- JASPgraphs_data[[palette]][["value"]]

  if (asFunction) {

    return(
      function(n) {
        if (length(n) == 1L) { # passed to discrete scale
          scales::gradient_n_pal(colors, values = NULL)(seq(0, 1, length.out = n))
        } else {# passed to continuous scale
          scales::gradient_n_pal(colors, values = NULL)(n)
        }
      }
    )
  } else {
    if (n <= length(colors)) {
      return(colors[seq_len(n)])
    } else {
      return(scales::gradient_n_pal(colors, values = NULL)(seq(0, 1, length.out = n)))
    }
  }
}

#'@rdname colors
#'@export
scale_JASPcolor_continuous <- function(palette = getGraphOption("palette")[["Function"]], ...) {
  continuous_scale("color", "JASPcolor", palette, ...)
}

#'@rdname colors
#'@export
scale_JASPfill_continuous <- function(palette = getGraphOption("palette")[["Function"]], ...) {
  continuous_scale("fill", "JASPcolor", palette, ...)
}

#'@rdname colors
#'@export
scale_JASPcolor_discrete <- function(palette = getGraphOption("palette")[["Function"]], ...) {
  discrete_scale("color", "JASPcolor", palette, ...)
}

#'@rdname colors
#'@export
scale_JASPfill_discrete <- function(palette = getGraphOption("palette")[["Function"]], ...) {
  discrete_scale("fill", "JASPcolor", palette, ...)
}

