#' @export
axesLabeller <- function(x, ...) {

  if (length(x) == 0)
    return(character())
  xnum <- suppressWarnings(as.numeric(x))
  if (all(is.na(xnum))) {
    return(x)
  } else if (max(abs(xnum), na.rm = TRUE) > 1e4) {
    return(jaspLabelScientific(xnum, ...))
  } else {
    return(jaspLabelNumber(xnum, ...))
  }
}

axesLabelScientific <- function(x, digits = 3, scale = 1, prefix = "", suffix = "",
                                decimal.mark = ".", trim = TRUE, ...) {
  if (length(x) == 0)
    return(character())
  x <- signif(x * scale, digits)
  paste0(prefix, format(x, decimal.mark = decimal.mark, trim = trim,
                        scientific = TRUE, ...), suffix)
}

axesLabelNumber <- function(x, accuracy = 1, scale = 1, prefix = "", suffix = "",
                            big.mark = " ", decimal.mark = ".", trim = TRUE, ...) {

  if (length(x) == 0)
    return(character())
  accuracy <- accuracy %||% precision(x)
  x <- round_any(x, accuracy/scale)
  nsmall <- -floor(log10(accuracy))
  nsmall <- min(max(nsmall, 0), 20)
  ret <- format(scale * x, big.mark = big.mark, decimal.mark = decimal.mark,
                trim = trim, nsmall = nsmall, scientific = FALSE, ...)
  ret <- paste0(prefix, ret, suffix)
  ret[is.infinite(x)] <- as.character(x[is.infinite(x)])
  ret
}

precision <- function(x) {
  if (all(is.infinite(x))) {
    return(1)
  }
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  span <- if (zero_range(rng))
    abs(rng[1])
  else diff(rng)
  if (span == 0) {
    return(1)
  }
  10^floor(log10(span))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

round_any <- function(x, accuracy, f = round) f(x/accuracy) * accuracy

zero_range <- function(x, tol = 1000 * .Machine$double.eps) {
  if (length(x) == 1)
    return(TRUE)
  if (length(x) != 2)
    stop("x must be length 1 or 2")
  if (any(is.na(x)))
    return(NA)
  if (x[1] == x[2])
    return(TRUE)
  if (all(is.infinite(x)))
    return(FALSE)
  m <- min(abs(x))
  if (m == 0)
    return(FALSE)
  abs((x[1] - x[2])/m) < tol
}

#' @export
scale_x_continuous <- function(name = waiver(), breaks = axesTicks, minor_breaks = waiver(),
    labels = axesLabeller, limits = NA, expand = waiver(), oob = censor,
    na.value = NA_real_, trans = "identity", position = "bottom",
    sec.axis = waiver()) {
    sc <- continuous_scale(c("x", "xmin", "xmax", "xend", "xintercept",
        "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper"),
        "position_c", identity, name = name, breaks = breaks,
        minor_breaks = minor_breaks, labels = labels, limits = limits,
        expand = expand, oob = oob, na.value = na.value, trans = trans,
        guide = "none", position = position, super = ScaleContinuousPosition)
    if (!is.waive(sec.axis)) {
        if (is.formula(sec.axis))
            sec.axis <- sec_axis(sec.axis)
        if (!is.sec_axis(sec.axis))
            stop("Secondary axes must be specified using 'sec_axis()'")
        sc$secondary.axis <- sec.axis
    }
    sc
}


# TODO:
# with functions
# JASPgraphs::scale_*_*(name = waiver(), breaks = JASPfunction,
#  minor_breaks = waiver(), labels = JASPfunction, limits = NA,
#  expand = waiver(), oob = censor, na.value = NA_real_,
#  trans = "identity", position = "bottom", sec.axis = waiver())