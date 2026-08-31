# ==============================================================================
# RoboReport Script: Correlation Report
# ------------------------------------------------------------------------------
# Target:      jaspRegression::Correlation
# Version:     >=0.20.0
# Description: Comprehensive, assumption-driven correlation report. Reports
#              Pearson's r, Spearman's rho and Kendall's tau-b with p-values,
#              Vovk-Sellke MPR, bootstrap 95% CIs, Fisher's-z effect sizes and
#              covariance; runs Shapiro-Wilk normality checks and emphasises the
#              appropriate coefficients; includes scatter plots and heatmaps.
#              Honours the user's alternative hypothesis and partial-out
#              variables (partial correlations).
# ==============================================================================
#
# Sourced and invoked by jaspRoboReport::run_script() -> roboreport_main(id).
#
# Pipeline:
#   1. Read the source analysis's options (variables, methods, alternative,
#      partial-out variables).
#   2. Plan: preserve the user's variables / alternative / naAction / partial-out;
#      force on all three coefficients + Shapiro-Wilk assumption checks + CIs +
#      effect sizes + Vovk-Sellke + covariance + scatter plots (no density) +
#      heatmaps.
#   3. Create + run an enhanced sibling analysis.
#   4. Extract the correlation table + Shapiro-Wilk tables from the sibling RDS.
#   5. Build assumption-driven prose interleaved with result references.
#   6. Compose the report directly into the sibling (which then shows it).

# --- helpers -----------------------------------------------------------------

num <- function(x) suppressWarnings(as.numeric(x))

# Cohen-style magnitude label for a correlation coefficient.
r_strength <- function(r) {
  r <- abs(num(r))
  if (is.na(r)) return(NA_character_)
  if (r < 0.10) "negligible"
  else if (r < 0.30) "small"
  else if (r < 0.50) "medium"
  else "large"
}

# Human-readable description of the alternative hypothesis.
alt_description <- function(alt) {
  if (identical(alt, "greater")) return("one-tailed, testing for positive associations")
  if (identical(alt, "less"))    return("one-tailed, testing for negative associations")
  "two-tailed, testing for any association"
}

# --- 1. plan -----------------------------------------------------------------

plan_report <- function(opts) {
  vars    <- opts$variables$value
  partial <- opts$partialOutVariables$value
  report_opts <- modifyList(opts, list(
    # all three coefficients for a comprehensive comparison
    pearson        = TRUE,
    spearman       = TRUE,
    kendallsTauB   = TRUE,
    # reporting infrastructure
    significanceReport  = TRUE,
    significanceFlagged = TRUE,
    sampleSize          = TRUE,
    ci                  = TRUE,
    ciBootstrap         = TRUE,   # bootstrap CIs (also required for partial correlations)
    effectSize          = TRUE,   # Fisher's z
    vovkSellke          = TRUE,   # Vovk-Sellke MPR
    covariance          = TRUE,
    # assumption checks (bivariate/pairwise Shapiro-Wilk only; multivariate dropped)
    assumptionCheckMultivariateShapiro = FALSE,
    assumptionCheckPairwiseShapiro     = TRUE,
    # plots: scatter with CI + prediction intervals (NO density), plus heatmaps
    scatterPlot                   = TRUE,
    scatterPlotCi                 = TRUE,
    scatterPlotPredictionInterval = TRUE,
    scatterPlotDensity            = FALSE,
    heatmapPlot                   = TRUE
  ))
  list(options = report_opts,
       flow = list(vars = vars, partial = partial, alternative = opts$alternative))
}

# --- 2. extract --------------------------------------------------------------
# Top-level elements: mainTable (correlation table), assumptionsContainer
# { pairwiseShapiro }, corrPlot (scatter), heatmaps.

get_results <- function(analysisId) {
  raw   <- rr_results(analysisId)
  assum <- rr_get(raw, "assumptionsContainer")
  main  <- rr_select(rr_get(raw, "mainTable"), c(
    "variable1", "variable2", "sample.size",
    "pearson_estimate",  "pearson_p.value",  "pearson_lower.ci",  "pearson_upper.ci",
    "spearman_estimate", "spearman_p.value", "spearman_lower.ci", "spearman_upper.ci",
    "kendall_estimate",  "kendall_p.value",  "kendall_lower.ci",  "kendall_upper.ci",
    "covariance"))
  list(
    main         = main,
    pairShapiro  = rr_select(rr_get(assum, "pairwiseShapiro"), c("var1", "var2", "W", "p")),
    hasScatter   = !is.null(rr_get(raw, "corrPlot")),
    hasHeatmaps  = !is.null(rr_get(raw, "heatmaps"))
  )
}

# --- normality assessment ----------------------------------------------------

assess_normality <- function(data) {
  pair <- data$pairShapiro
  if (is.null(pair) || nrow(pair) == 0 || !("p" %in% names(pair)))
    return(list(tested = FALSE, nPairs = 0, nViol = 0, allViol = FALSE))
  p <- num(pair$p)
  nPairs <- length(p)
  nViol  <- sum(!is.na(p) & p < 0.05)
  list(tested = TRUE, nPairs = nPairs, nViol = nViol,
       allViol = (nPairs > 0 && nViol == nPairs))
}

# --- correlation summary -----------------------------------------------------

summarize_correlations <- function(main) {
  np <- nrow(main)
  pcols <- intersect(c("pearson_p.value", "spearman_p.value", "kendall_p.value"), names(main))
  sigPair <- vapply(seq_len(np), function(i) {
    ps <- unlist(lapply(pcols, function(cc) num(main[[cc]][i])))
    any(!is.na(ps) & ps < 0.05)
  }, logical(1))
  nSig <- sum(sigPair)
  strongPair <- NA_character_; strongR <- NA_real_
  if ("pearson_estimate" %in% names(main) && np > 0) {
    idx <- which.max(abs(num(main$pearson_estimate)))
    if (length(idx) == 1) {
      strongPair <- sprintf("%s and %s", main$variable1[idx], main$variable2[idx])
      strongR    <- num(main$pearson_estimate[idx])
    }
  }
  list(nPairs = np, nSig = nSig, strongPair = strongPair, strongR = strongR)
}

# --- 3. prose ----------------------------------------------------------------

build_abstract <- function(data, flow, norm, summ) {
  n <- length(flow$vars)
  var_list <- paste0("**", flow$vars, "**", collapse = ", ")
  nn <- if ("sample.size" %in% names(data$main) && nrow(data$main) > 0)
    num(data$main$sample.size[1]) else NA

  partial_txt <- if (length(flow$partial) > 0)
    sprintf(" A partial correlation analysis was conducted, conditioning on **%s**.",
            paste0(flow$partial, collapse = ", ")) else ""

  method_txt <- if (norm$tested && norm$nViol > 0)
    "Given violations of bivariate normality, the rank-based coefficients (Spearman's rho and Kendall's tau-b) are emphasised alongside Pearson's r."
  else
    "Pearson's r is reported as the primary coefficient, with the rank-based Spearman's rho and Kendall's tau-b reported for robustness."

  finding_txt <- if (summ$nSig == 0)
    "None of the pairwise associations reached statistical significance."
  else
    sprintf("%d of %d pairwise associations reached statistical significance.", summ$nSig, summ$nPairs)

  paste0(
    "## Abstract\n\n",
    sprintf("A correlation analysis was conducted on %d continuous variables (%s%s) to examine the strength and direction of their pairwise associations.",
            n, var_list, if (!is.na(nn)) sprintf("; N = %d", nn) else ""),
    partial_txt, " ", method_txt, " ",
    sprintf("All tests were %s. ", alt_description(flow$alternative)),
    finding_txt
  )
}

build_assumptions_intro <- function() {
  paste0("## Assumption Checks\n\n",
    "The Shapiro-Wilk test was used to assess bivariate normality for each variable pair. ",
    "Significant results (p < .05) indicate departures from normality, which affect the validity of Pearson's r; the rank-based coefficients (Spearman's rho, Kendall's tau-b) require only monotonicity and are robust to such departures.")
}

build_assumptions_recommendation <- function(norm) {
  if (!norm$tested)
    return("Normality test results were not available for this analysis.")
  if (norm$allViol) {
    sprintf("Because bivariate normality was rejected for all %d pairs, Spearman's rho and Kendall's tau-b are the more appropriate coefficients. Pearson's r is reported for completeness but should be interpreted with caution.", norm$nPairs)
  } else if (norm$nViol > 0) {
    sprintf("Bivariate normality was rejected for %d of %d pairs. For those pairs the rank-based coefficients (Spearman's rho, Kendall's tau-b) are more appropriate, while Pearson's r is reported for completeness.", norm$nViol, norm$nPairs)
  } else {
    "Bivariate normality was not rejected for any pair; Pearson's r is appropriate, with the rank-based coefficients reported for robustness."
  }
}

build_table_intro <- function(flow) {
  heading <- if (length(flow$partial) > 0) "## Partial Correlation Table" else "## Correlation Table"
  partial_note <- if (length(flow$partial) > 0)
    " Because this is a partial correlation, confidence intervals are bootstrap-based, and standard errors of the Fisher's-z effect size are unavailable for the non-parametric coefficients."
  else ""
  paste0(heading, "\n\n",
    "The table below displays Pearson's r, Spearman's rho, and Kendall's tau-b for all variable pairs, ",
    "along with p-values, Vovk-Sellke MPR values, bootstrap 95% confidence intervals, Fisher's-z effect sizes (with standard errors), and covariances.",
    partial_note)
}

build_scatter_intro <- function() {
  paste0("## Scatter Plots\n\n",
    "Scatter plots with confidence intervals and prediction intervals provide a visual check of each relationship.")
}

build_heatmap_intro <- function() {
  paste0("## Heatmaps\n\n",
    "Heatmaps give a colour-coded overview of the correlation matrices for Pearson's r, Spearman's rho, and Kendall's tau-b.")
}

build_conclusion <- function(data, norm, summ) {
  parts <- c("## Conclusion", "")
  if (summ$nSig == 0) {
    parts <- c(parts, sprintf(
      "Across all three correlation coefficients (Pearson, Spearman, Kendall), none of the %d pairwise associations reached statistical significance, and the 95%% confidence intervals for the coefficients generally cross zero. The Vovk-Sellke MPR values indicate the data provide little to no evidence in favour of true associations. In summary, no reliable linear or monotonic relationships were detected among these variables in this sample.",
      summ$nPairs))
  } else {
    strong_txt <- if (!is.na(summ$strongPair))
      sprintf(" The strongest association was between %s (Pearson r = %.2f, %s).",
              summ$strongPair, summ$strongR, r_strength(summ$strongR)) else ""
    parts <- c(parts, sprintf(
      "%d of %d pairwise associations reached statistical significance.%s Where bivariate normality was violated, the rank-based coefficients (Spearman's rho, Kendall's tau-b) provide the more trustworthy evidence; the coefficients and their confidence intervals in the table above should be interpreted accordingly.",
      summ$nSig, summ$nPairs, strong_txt))
  }
  paste(parts, collapse = "\n")
}

# --- main --------------------------------------------------------------------

roboreport_main <- function(analysisId) {
  opts <- rr_get_options(analysisId)
  plan <- plan_report(opts)
  sib  <- rr_create_and_run("jaspRegression", "Correlation", plan$options)
  data <- get_results(sib)

  norm <- assess_normality(data)
  summ <- summarize_correlations(data$main)
  flow <- plan$flow

  elements <- list(
    el_md(build_abstract(data, flow, norm, summ)),
    el_md(build_assumptions_intro()),
    el_ref("assumptionsContainer"),
    el_md(build_assumptions_recommendation(norm)),
    el_md(build_table_intro(flow)),
    el_ref("mainTable")
  )
  if (data$hasScatter)
    elements <- c(elements, list(el_md(build_scatter_intro()), el_ref("corrPlot")))
  if (data$hasHeatmaps)
    elements <- c(elements, list(el_md(build_heatmap_intro()), el_ref("heatmaps")))
  elements <- c(elements, list(el_md(build_conclusion(data, norm, summ))))

  rr_compose_results(sib, elements)
}
