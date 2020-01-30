#' @importFrom grid unit.c unit

parse_safe <- function(text) {
  # equivalent to ggplot2:::parse_safe
  stopifnot(is.character(text))
  out <- vector("expression", length(text))
  for (i in seq_along(text)) {
    expr <- parse(text = text[[i]])
    out[[i]] <- if (length(expr) == 0) 
      NA
    else expr[[1]]
  }
  out
}

just_dir <- function(x, tol = 0.001) {
  # ggplot2:::just_dir
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}

compute_just <- function(just, x) {
  # equivalent to ggplot2:::parse_safe
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]
  unname(c(left = 0, center = 0.5, right = 1, bottom = 0, middle = 0.5, 
           top = 1)[just])
}

#' @rdname geom_aligned_text
#' @usage NULL
#' @format NULL
#' @export
GeomAlignedText <- ggplot2::ggproto(
  "alignedtext",
  ggplot2::GeomText,
  required_aes = c("x", "y", "label1", "label2"),
  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE, prepend = NULL) {
    
    lab1 <- data$label1
    if (parse) {
      lab1 <- parse_safe(as.character(lab1))
    }

    lab2 <- data$label2
    if (parse) {
      lab2 <- parse_safe(as.character(lab2))
    }
    
    if (!is.null(prepend) && is.character(prepend)) {
      allxoffset <- grid::stringWidth(prepend)
    }
    
    xoffset <- grid::stringWidth(" ")
    if (!is.null(lab1)) {
      labelLengths <- grid::stringWidth(lab1)
      xoffset <- xoffset + max(labelLengths)# - labelLengths
    }
    
    data <- coord$transform(data, panel_params)
    if (is.character(data$vjust)) {
      data$vjust <- ggplot2:::compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- ggplot2:::compute_just(data$hjust, data$x)
    }
    
    # we need to modify the default here (specifying unit rather than numeric) because otherwise
    # the strwidth unit cannot be passed.
    grid::textGrob(c(lab1, lab2),
             x = unit.c(unit(data$x, "native") + allxoffset, unit(data$x, "native") + xoffset + allxoffset), 
             y = unit.c(unit(data$y, "native"), unit(data$y, "native")),
             default.units = "native",
             hjust = c(data$hjust, data$hjust), 
             vjust = c(data$vjust, data$vjust), 
             rot = c(data$angle, data$angle), 
             gp = grid::gpar(
               col = scales::alpha(rep(data$colour, 2), rep(data$alpha, 2)), 
               fontsize   = rep(data$size * .pt, 2),
               fontfamily = rep(data$family, 2),
               fontface   = rep(data$fontface, 2),
               lineheight = rep(data$lineheight, 2)
             ),
             check.overlap = check_overlap)
  }
)


#' Add aligned text to plots
#' 
#' @description TODO
#'
#'@section Aesthetics:
#' \itemize{
#' \item colour
#' \item size
#' \item linetype
#' \item alpha
#' }
#'
#' @inheritParams ggplot2::geom_text
#'
#' @return
#' @export
geom_aligned_text <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", 
                              ..., parse = FALSE, nudge_x = 0, nudge_y = 0, check_overlap = FALSE, 
                              na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.", 
           call. = FALSE)
    }
    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomAlignedText, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(parse = parse, check_overlap = check_overlap, 
                      na.rm = na.rm, ...))
}


# df <- data.frame(x=0, 
#                  y=c(-1,0,1), 
#                  lab1=c("aa","b","c3f2f^2"), 
#                  lab2=c("aa","b","c[0][1]"),
#                  stringsAsFactors=FALSE)
# 
# ggplot(aes(x=x, y=y), data=df) + 
#   geom_aligned_text(aes(label1=lab1, label2 = lab2), colour="red", hjust = "left", parse = TRUE) + 
#   ylim(c(-5, 5))
 
