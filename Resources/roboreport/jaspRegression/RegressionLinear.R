# ==============================================================================
# RoboReport Script: Linear Regression Report
# ------------------------------------------------------------------------------
# Target:      jaspRegression::RegressionLinear
# Version:     >=0.20.0
# Description: Classical linear regression report with model fit (incl.
#              Cohen's f-squared effect size), coefficient interpretation,
#              collinearity diagnostics, and residual checks. Honours the
#              predictor-entry method (enter / stepwise).
# ==============================================================================
#
# Sourced and invoked by jaspRoboReport::run_script() -> roboreport_main(id).
#
# Pipeline:
#   1. Read the source analysis's options (dependent, predictors, model terms).
#   2. Plan: preserve the user's model, force on a rich reporting set
#      (coefficient CIs, collinearity statistics, descriptives, residual plots).
#   3. Create + run an enhanced sibling analysis.
#   4. Extract the model tables from the sibling's RDS.
#   5. Build prose interleaved with references to the tables + residual plots.
#   6. Compose the report directly into the sibling (which then shows the report).

# --- helpers -----------------------------------------------------------------

num <- function(x) suppressWarnings(as.numeric(x))

# The final model is the last row of the model summary (the most complex model
# for enter/forward/stepwise, or the final reduced model for backward).
full_model <- function(summary_tab) {
  summary_tab$model[nrow(summary_tab)]
}

# Cohen's f2 conventions for interpreting R2 magnitude.
r2_magnitude <- function(r2) {
  r2 <- num(r2)
  if (is.na(r2)) return(NA_character_)
  if (r2 < 0.02) "negligible"
  else if (r2 < 0.13) "small"
  else if (r2 < 0.26) "medium"
  else "large"
}

sig_label <- function(p) {
  p <- num(p)
  if (is.na(p)) return("of unknown significance")
  if (p < 0.05) "statistically significant" else "not statistically significant"
}

# Cohen's f-squared conventions for interpreting the model effect size.
f2_magnitude <- function(f2) {
  f2 <- num(f2)
  if (is.na(f2)) return(NA_character_)
  if (f2 < 0.02) "negligible"
  else if (f2 < 0.15) "small"
  else if (f2 < 0.35) "medium"
  else "large"
}

# Describe the predictor-entry method.
method_sentence <- function(method) {
  if (identical(method, "backward")) return("Predictors were selected using a backward stepwise procedure.")
  if (identical(method, "forward"))  return("Predictors were selected using a forward stepwise procedure.")
  if (identical(method, "stepwise")) return("Predictors were selected using a stepwise procedure.")
  "All predictors were entered simultaneously."
}

# --- 1. plan -----------------------------------------------------------------

plan_report <- function(opts) {
  dependent  <- opts$dependent$value
  covariates <- opts$covariates$value
  report_opts <- modifyList(opts, list(
    coefficientEstimate   = TRUE,
    coefficientCi         = TRUE,
    collinearityStatistic = TRUE,
    descriptives          = TRUE,
    modelFit              = TRUE,
    rSquaredChange        = TRUE,
    # Residual diagnostic plots forced OFF: they error when the data contain
    # missing values ("missing values and NaN's not allowed"). They can be
    # re-enabled in the analysis options for complete data.
    residualQqPlot        = FALSE,
    residualVsFittedPlot  = FALSE
  ))
  list(options = report_opts,
       flow = list(dependent  = dependent,
                   covariates = covariates,
                   method     = if (is.null(opts$method)) "enter" else opts$method))
}

# --- 2. extract --------------------------------------------------------------
# The RDS stores the model tables under `modelContainer` using SHORT local keys
# (summaryTable, anovaTable, ...); el_ref() uses the FULL meta names
# (modelContainer_summaryTable, ...) when composing.

get_results <- function(analysisId) {
  raw <- rr_results(analysisId)
  mc  <- rr_get(raw, "modelContainer")
  list(
    summary = rr_select(rr_get(mc, "summaryTable"),
                        c("model", "R", "R2", "adjR2", "RMSE", "R2c", "df1", "df2", "p")),
    anova   = rr_select(rr_get(mc, "anovaTable"),
                        c("model", "cases", "SS", "df", "MS", "F", "p")),
    coeff   = rr_select(rr_get(mc, "coeffTable"),
                        c("model", "name", "unstandCoeff", "SE", "standCoeff",
                          "t", "p", "lower", "upper", "tolerance", "VIF")),
    desc    = rr_select(rr_get(mc, "descriptivesTable"),
                        c("var", "N", "mean", "SD", "SE"))
  )
}

# --- 3. prose ----------------------------------------------------------------

build_abstract <- function(data, flow) {
  summ <- data$summary
  full <- full_model(summ)
  s <- summ[summ$model == full, , drop = FALSE][1, ]
  a <- data$anova
  arow <- a[a$model == full & a$cases == "Regression", , drop = FALSE]
  Fv <- if (nrow(arow) > 0) num(arow$F[1]) else NA

  r2  <- num(s$R2); adj <- num(s$adjR2); p <- num(s$p)
  df1 <- num(s$df1); df2 <- num(s$df2)
  k <- length(flow$covariates)
  pred_list <- paste0("**", flow$covariates, "**", collapse = ", ")
  f2 <- if (!is.na(r2) && r2 >= 0 && r2 < 1) r2 / (1 - r2) else NA
  f2_txt <- if (!is.na(f2)) sprintf(" The model effect size (Cohen's f-squared) was %.3f (%s).", f2, f2_magnitude(f2)) else ""

  paste0(
    "## Abstract\n\n",
    sprintf("A classical linear regression model was fitted to predict **%s** from %d predictor(s): %s. ",
            flow$dependent, k, pred_list),
    method_sentence(flow$method), " ",
    sprintf("The model accounted for R-squared = %.3f (%s) of the variance in the outcome (adjusted R-squared = %.3f).",
            r2, r2_magnitude(r2), adj),
    f2_txt, " ",
    sprintf("The overall model was %s, F(%g, %g) = %.2f, %s. ",
            sig_label(p), df1, df2, Fv, fmt_p(p)),
    "Coefficient estimates, collinearity diagnostics, and residual diagnostics are reported below."
  )
}

build_model_fit <- function(data, flow) {
  summ <- data$summary
  full <- full_model(summ)
  s <- summ[summ$model == full, , drop = FALSE][1, ]
  a <- data$anova
  arow <- a[a$model == full & a$cases == "Regression", , drop = FALSE]
  Fv <- if (nrow(arow) > 0) num(arow$F[1]) else NA

  r2 <- num(s$R2); adj <- num(s$adjR2); rmse <- num(s$RMSE)
  df1 <- num(s$df1); df2 <- num(s$df2); p <- num(s$p)
  f2 <- if (!is.na(r2) && r2 >= 0 && r2 < 1) r2 / (1 - r2) else NA
  f2_txt <- if (!is.na(f2)) sprintf(" The effect size (Cohen's f-squared) was %.3f (%s).", f2, f2_magnitude(f2)) else ""

  paste0(
    "### Model Fit\n\n",
    sprintf("The regression model explained %.1f%% of the variance in **%s** (R-squared = %.3f, adjusted R-squared = %.3f; RMSE = %.2f).",
            r2 * 100, flow$dependent, r2, adj, rmse),
    f2_txt, " ",
    sprintf("The overall fit was %s, F(%g, %g) = %.2f, %s.",
            sig_label(p), df1, df2, Fv, fmt_p(p))
  )
}

build_coefficients <- function(data) {
  summ <- data$summary
  full <- full_model(summ)
  cf <- data$coeff
  cf <- cf[cf$model == full & cf$name != "(Intercept)", , drop = FALSE]

  if (nrow(cf) == 0)
    return("### Coefficients\n\nNo predictor coefficients were available for the full model.")

  parts <- c("### Coefficients", "",
             "Holding the other predictors constant, the estimated effect of each predictor was:")

  for (i in seq_len(nrow(cf))) {
    nm   <- cf$name[i]
    b    <- num(cf$unstandCoeff[i]); se <- num(cf$SE[i])
    beta <- num(cf$standCoeff[i]); tv <- num(cf$t[i]); p <- num(cf$p[i])
    lo   <- num(cf$lower[i]); hi <- num(cf$upper[i])

    ci <- if (!is.na(lo) && !is.na(hi)) sprintf(", 95%% CI [%.2f, %.2f]", lo, hi) else ""
    beta_txt <- if (!is.na(beta)) sprintf("; standardized beta = %.2f", beta) else ""

    parts <- c(parts, sprintf(
      "- **%s**: B = %.2f (SE = %.2f)%s, t = %.2f, %s%s -- %s.",
      nm, b, se, beta_txt, tv, fmt_p(p), ci, sig_label(p)))
  }
  paste(parts, collapse = "\n")
}

build_collinearity <- function(data) {
  summ <- data$summary
  full <- full_model(summ)
  cf <- data$coeff
  cf <- cf[cf$model == full & cf$name != "(Intercept)", , drop = FALSE]
  if (nrow(cf) == 0 || !("VIF" %in% names(cf))) return(NULL)
  vif <- num(cf$VIF)
  if (all(is.na(vif))) return(NULL)

  max_vif <- max(vif, na.rm = TRUE)
  verdict <- if (max_vif < 5) {
    "No problematic multicollinearity was detected (all VIF < 5); the predictors appear to provide largely independent information."
  } else if (max_vif < 10) {
    "Moderate multicollinearity was detected (some VIF between 5 and 10); coefficient estimates should be interpreted with care."
  } else {
    "Severe multicollinearity was detected (VIF > 10); coefficient estimates are unstable and redundant predictors should be considered for removal."
  }

  lines <- vapply(seq_len(nrow(cf)), function(i)
    sprintf("- **%s**: VIF = %.2f, tolerance = %.2f.",
            cf$name[i], num(cf$VIF[i]), num(cf$tolerance[i])),
    character(1))

  paste(c("### Collinearity Diagnostics", "", verdict, lines), collapse = "\n")
}

build_assumptions <- function() {
  paste(
    "### Regression Assumptions", "",
    paste0("Linear regression assumes a linear relationship between the predictors and the outcome, ",
           "approximately normally distributed residuals, and homoscedasticity (constant residual variance). ",
           "These assumptions can be inspected with the residual diagnostic plots available in the analysis options ",
           "(the Q-Q plot of standardised residuals for normality, and the residuals-vs-fitted plot for linearity and homoscedasticity)."),
    sep = "\n")
}

build_conclusion <- function(data, flow) {
  summ <- data$summary
  full <- full_model(summ)
  s <- summ[summ$model == full, , drop = FALSE][1, ]
  r2 <- num(s$R2)

  cf <- data$coeff
  cf <- cf[cf$model == full & cf$name != "(Intercept)", , drop = FALSE]
  p <- num(cf$p)
  sig_names <- cf$name[!is.na(p) & p < 0.05]

  sig_txt <- if (length(sig_names) > 0) {
    sprintf("The statistically significant predictors were %s.",
            paste0("**", sig_names, "**", collapse = ", "))
  } else {
    "No individual predictor reached statistical significance at the .05 level."
  }

  paste(
    "## Conclusion", "",
    sprintf(paste0("The regression model explained %.1f%% of the variance in **%s** (R-squared = %.3f). %s ",
                   "These findings describe association within the supplied data and assume the linear model is appropriately specified."),
            r2 * 100, flow$dependent, r2, sig_txt),
    sep = "\n")
}

# --- main --------------------------------------------------------------------

roboreport_main <- function(analysisId) {
  opts <- rr_get_options(analysisId)
  plan <- plan_report(opts)
  sib  <- rr_create_and_run("jaspRegression", "RegressionLinear", plan$options)
  data <- get_results(sib)

  elements <- list(
    el_md(build_abstract(data, plan$flow)),
    el_ref("modelContainer_summaryTable"),
    el_md(build_model_fit(data, plan$flow)),
    el_ref("modelContainer_anovaTable"),
    el_md(build_coefficients(data)),
    el_ref("modelContainer_coeffTable")
  )

  collin <- build_collinearity(data)
  if (!is.null(collin))
    elements <- c(elements, list(el_md(collin)))

  elements <- c(elements, list(
    el_ref("modelContainer_descriptivesTable"),
    el_md(build_assumptions()),
    el_md(build_conclusion(data, plan$flow))
  ))

  rr_compose_results(sib, elements)
}
