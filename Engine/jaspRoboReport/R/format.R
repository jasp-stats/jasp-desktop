#'
#' Formatting helpers for RoboReport scripts.
#'
#' Generic formatting functions for common statistical reporting needs.
#' These produce strings suitable for embedding in markdown text via
#' `el_md()`.

# ============================================================================
# p-values
# ============================================================================

#' Format a p-value for APA-style reporting.
#'
#' Returns "p < .001" for small values, otherwise "p = .xxx" (no leading
#' zero, 3 decimals).
#'
#' @param p Numeric p-value.
#' @return Character string.
#' @export
fmt_p <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  if (is.na(p)) return("p = NA")
  if (p < 0.001) return("p < .001")
  sprintf("p = %.3f", p)
}

# ============================================================================
# Confidence intervals
# ============================================================================

#' Format a confidence interval.
#'
#' @param lower Numeric lower bound.
#' @param upper Numeric upper bound.
#' @param digits Number of decimal places (default 2).
#' @return Character string like "95% CI [0.12, 0.45]".
#' @export
fmt_ci <- function(lower, upper, digits = 2) {
  fmt <- paste0("%.", digits, "f")
  sprintf(paste0("95%% CI [", fmt, ", ", fmt, "]"), lower, upper)
}

# ============================================================================
# Effect sizes
# ============================================================================

#' Format an effect size with magnitude label.
#'
#' Uses Cohen's conventions for d: <0.2 negligible, <0.5 small,
#' <0.8 medium, >=0.8 large.
#'
#' @param d Numeric effect size (Cohen's d).
#' @param name Name prefix (default "Cohen's d").
#' @param digits Decimal places (default 2).
#' @return Character string like "Cohen's d = 0.45 (small)".
#' @export
fmt_effect_size <- function(d, name = "Cohen's d", digits = 2) {
  if (is.na(d)) return(paste(name, "= NA"))
  magnitude <- if (abs(d) < 0.2) "negligible"
               else if (abs(d) < 0.5) "small"
               else if (abs(d) < 0.8) "medium"
               else "large"
  sprintf(paste0("%s = %.", digits, "f (%s)"), name, d, magnitude)
}

# ============================================================================
# General number formatting
# ============================================================================

#' Format a test statistic with degrees of freedom.
#'
#' @param stat The statistic value (t, F, chi-square, etc.).
#' @param df Degrees of freedom (numeric).
#' @param stat_name Name prefix (default "t").
#' @param digits Decimal places for the statistic (default 2).
#' @return Character string like "t(98) = 2.34".
#' @export
fmt_stat <- function(stat, df, stat_name = "t", digits = 2) {
  df <- suppressWarnings(as.numeric(df))
  if (is.na(df))
    sprintf(paste0("%s = %.", digits, "f"), stat_name, stat)
  else
    sprintf(paste0("%s(%.0f) = %.", digits, "f"), stat_name, df, stat)
}

#' Format a mean and standard deviation pair.
#'
#' @param mean Numeric mean.
#' @param sd Numeric standard deviation.
#' @param digits Decimal places (default 2).
#' @return Character string like "M = 5.23, SD = 1.45".
#' @export
fmt_mean_sd <- function(mean, sd, digits = 2) {
  fmt <- paste0("%.", digits, "f")
  sprintf(paste0("M = ", fmt, ", SD = ", fmt), mean, sd)
}
