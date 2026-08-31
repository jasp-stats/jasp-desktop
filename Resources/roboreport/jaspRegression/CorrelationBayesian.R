# ==============================================================================
# RoboReport Script: Bayesian Correlation Report
# ------------------------------------------------------------------------------
# Target:      jaspRegression::CorrelationBayesian
# Version:     >=0.20.0
# Description: Comprehensive, assumption-aware Bayesian correlation report.
#              Reports Pearson's r, Spearman's rho and Kendall's tau-b with
#              Bayes factors (in the user's chosen scale: BF10 / BF01 /
#              Log BF10) and central credible intervals; runs a Bayesian
#              linearity check and emphasises the appropriate coefficients;
#              includes pairwise (scatter, prior/posterior, robustness) and
#              matrix plots. Honours the user's alternative hypothesis.
#              (The Bayesian correlation has no partial-out option.)
# ==============================================================================
#
# Sourced and invoked by jaspRoboReport::run_script() -> roboreport_main(id).
#
# Pipeline:
#   1. Read the source analysis's options (variables, methods, alternative,
#      Bayes-factor type).
#   2. Plan: preserve the user's variables / alternative / bayesFactorType;
#      force on all three coefficients + Bayes factors + credible intervals +
#      linearity check + pairwise/matrix plots (density disabled).
#   3. Create + run an enhanced sibling analysis.
#   4. Extract the correlation table + linearity table from the sibling RDS.
#   5. Build assumption-aware prose interleaved with result references.
#   6. Compose the report directly into the sibling (which then shows it).

# --- helpers -----------------------------------------------------------------

num <- function(x) suppressWarnings(as.numeric(x))

# Display label for the user's Bayes-factor scale.
bf_label <- function(bfType) {
  if (identical(bfType, "BF01"))    return("BF01")
  if (identical(bfType, "LogBF10")) return("Log(BF10)")
  "BF10"
}

# Convert a reported Bayes factor (in the user's scale) to BF10 for
# interpretation. bf_evidence_category() expects BF10 (BF10 > 1 = evidence
# for H1 / an association). Defensive against zero-length / non-finite cells:
# a method whose posterior could not be computed (e.g. Spearman's "posterior
# is too peaked") yields an empty/"NaN" cell, which we treat as NA.
to_bf10 <- function(value, bfType) {
  value <- num(value)
  if (length(value) == 0 || !is.finite(value[1])) return(NA_real_)
  value <- value[1]
  if (identical(bfType, "BF01"))    return(1 / value)
  if (identical(bfType, "LogBF10")) return(exp(value))
  value
}

# Format a Bayes factor value, using scientific notation for very large/small.
fmt_bf_value <- function(bf) {
  bf <- num(bf)
  if (length(bf) == 0 || !is.finite(bf[1])) return("NA")
  bf <- bf[1]
  if (bf >= 1000 || (bf > 0 && bf < 0.001)) sprintf("%.2e", bf) else sprintf("%.2f", bf)
}

# Report a Bayes factor in the user's scale with an evidence category.
fmt_bf_report <- function(value, bfType) {
  info <- bf_evidence_category(to_bf10(value, bfType))
  sprintf("%s = %s (%s evidence for %s)", bf_label(bfType), fmt_bf_value(value),
          info$category, info$direction)
}

# Human-readable description of the alternative hypothesis.
alt_description <- function(alt) {
  if (identical(alt, "greater")) return("one-tailed, testing for positive associations")
  if (identical(alt, "less"))    return("one-tailed, testing for negative associations")
  "two-tailed, testing for any association"
}

# --- 1. plan -----------------------------------------------------------------

plan_report <- function(opts) {
  vars <- opts$variables$value
  report_opts <- modifyList(opts, list(
    # all three coefficients for a comprehensive comparison
    pearson  = TRUE,
    spearman = TRUE,
    kendall  = TRUE,
    # reporting infrastructure
    bayesFactorReport         = TRUE,
    sampleSize                = TRUE,
    ci                        = TRUE,
    supportCorrelationFlagged = TRUE,
    # assumption check (Bayesian linearity test)
    linearityTest = TRUE,
    # plots: pairwise (scatter + prior/posterior + robustness) and matrix (no density)
    scatterPlot        = TRUE,
    priorPosteriorPlot = TRUE,
    bfRobustnessPlot   = TRUE,
    matrixPlot         = TRUE,
    matrixPlotDensity  = FALSE
  ))
  list(options = report_opts,
       flow = list(vars       = vars,
                   alternative = opts$alternative,
                   bfType      = opts$bayesFactorType,
                   ciLevel     = if (is.null(opts$ciLevel)) 0.95 else opts$ciLevel))
}

# --- 2. extract --------------------------------------------------------------
# Top-level elements: corBayesTable (correlation table), linearityTestTable
# (linearity check), pairsPlotCollection (per-pair plots), matrixPlot.

get_results <- function(analysisId) {
  raw  <- rr_results(analysisId)
  main <- rr_select(rr_get(raw, "corBayesTable"), c(
    "variable1", "variable2", "n",
    "pearsonstat",  "pearsonbf",  "pearsonlowerCi",  "pearsonupperCi",
    "spearmanstat", "spearmanbf", "spearmanlowerCi", "spearmanupperCi",
    "kendallstat",  "kendallbf",  "kendalllowerCi",  "kendallupperCi"))
  list(
    main          = main,
    linearity     = rr_select(rr_get(raw, "linearityTestTable"), c("pair", "BF")),
    hasLinearity  = !is.null(rr_get(raw, "linearityTestTable")),
    hasPairsPlot  = !is.null(rr_get(raw, "pairsPlotCollection")),
    hasMatrixPlot = !is.null(rr_get(raw, "matrixPlot"))
  )
}

# --- linearity assessment ----------------------------------------------------
# The linearity test compares a linear vs. a quadratic fit per pair. The
# reported BF (in the user's scale) is converted to BF_ql (evidence for the
# quadratic / non-linear model); BF_ql > 1 indicates non-linearity, for which
# the rank-based (monotonic) coefficients are more appropriate.

assess_linearity <- function(data, bfType) {
  lt <- data$linearity
  if (is.null(lt) || nrow(lt) == 0 || !("BF" %in% names(lt)))
    return(list(tested = FALSE, nPairs = 0, nNonlin = 0, allNonlin = FALSE))
  bf_ql  <- sapply(num(lt$BF), function(v) to_bf10(v, bfType))
  nPairs <- length(bf_ql)
  nNonlin <- sum(!is.na(bf_ql) & bf_ql > 1)
  list(tested = TRUE, nPairs = nPairs, nNonlin = nNonlin,
       allNonlin = (nPairs > 0 && nNonlin == nPairs))
}

# --- correlation summary -----------------------------------------------------

summarize_correlations <- function(main, bfType) {
  if (is.null(main) || nrow(main) == 0)
    return(list(nPairs = 0, nAssoc = 0, strongIdx = NA_integer_))
  np <- nrow(main)
  bfcols <- intersect(c("pearsonbf", "spearmanbf", "kendallbf"), names(main))
  # per pair: strongest evidence for an association across methods (as BF10)
  pairBf10 <- vapply(seq_len(np), function(i) {
    bfs <- unlist(lapply(bfcols, function(cc) to_bf10(main[[cc]][i], bfType)))
    bfs <- bfs[!is.na(bfs)]
    if (length(bfs) == 0) NA_real_ else max(bfs)
  }, numeric(1))
  nAssoc <- sum(!is.na(pairBf10) & pairBf10 > 1)
  # strongest pair by Pearson BF10 (index into main)
  strongIdx <- NA_integer_
  if ("pearsonbf" %in% names(main) && np > 0) {
    bf10 <- sapply(num(main$pearsonbf), function(v) to_bf10(v, bfType))
    idx <- which.max(bf10)
    if (length(idx) == 1 && !is.na(bf10[idx])) strongIdx <- idx
  }
  list(nPairs = np, nAssoc = nAssoc, strongIdx = strongIdx)
}

# --- 3. prose ----------------------------------------------------------------

build_abstract <- function(data, flow, lin, summ) {
  vars <- flow$vars
  n <- length(vars)
  # Guard against zero-length inputs: a missing/empty variable list or sample
  # size would otherwise feed character(0)/numeric(0) into sprintf() below and
  # raise "argument is of length zero".
  var_list <- if (n > 0) paste0("**", vars, "**", collapse = ", ") else "no variables"
  nn <- if (!is.null(data$main) && "n" %in% names(data$main) && nrow(data$main) > 0)
    num(data$main$n[1]) else NA_real_
  nn <- if (length(nn) == 1) nn else NA_real_   # guarantee a scalar for is.na()

  method_txt <- if (lin$tested && lin$nNonlin > 0)
    "Given evidence of non-linearity for some pairs, the rank-based coefficients (Spearman's rho and Kendall's tau-b), which capture monotonic associations, are emphasised alongside Pearson's r."
  else
    "Pearson's r is reported as the primary coefficient, with the rank-based Spearman's rho and Kendall's tau-b reported for robustness."

  finding_txt <- if (summ$nAssoc == 0)
    "For none of the pairs did the data provide evidence for an association (the Bayes factors favoured the null hypothesis)."
  else
    sprintf("For %d of %d pairs the data provided evidence for an association.", summ$nAssoc, summ$nPairs)

  paste0(
    "## Abstract\n\n",
    sprintf("A Bayesian correlation analysis was conducted on %d continuous variables (%s%s) to examine the strength and direction of their pairwise associations.",
            n, var_list, if (!is.na(nn)) sprintf("; N = %d", nn) else ""),
    " ", method_txt, " ",
    sprintf("Evidence was quantified with %s, and all tests were %s. ",
            bf_label(flow$bfType), alt_description(flow$alternative)),
    finding_txt
  )
}

build_assumptions_intro <- function(flow) {
  paste0("## Assumption Checks\n\n",
    "The linearity of each pairwise relationship was assessed with a Bayesian model comparison between a linear and a quadratic fit. ",
    sprintf("A %s greater than 1 indicates evidence for a non-linear (quadratic) relationship, for which the rank-based coefficients (Spearman's rho, Kendall's tau-b) are more appropriate than Pearson's r.",
            bf_label(flow$bfType)))
}

build_assumptions_recommendation <- function(lin) {
  if (!lin$tested)
    return("Linearity test results were not available for this analysis.")
  if (lin$allNonlin) {
    sprintf("The linearity test found evidence of non-linearity for all %d pairs; Spearman's rho and Kendall's tau-b are the more appropriate coefficients, while Pearson's r is reported for completeness and should be interpreted with caution.", lin$nPairs)
  } else if (lin$nNonlin > 0) {
    sprintf("The linearity test found evidence of non-linearity for %d of %d pairs; for those pairs the rank-based coefficients (Spearman's rho, Kendall's tau-b) are more appropriate, while Pearson's r is reported for completeness.", lin$nNonlin, lin$nPairs)
  } else {
    "The linearity test found no evidence of non-linearity for any pair; Pearson's r is appropriate, with the rank-based coefficients reported for robustness."
  }
}

build_table_intro <- function(flow) {
  paste0("## Correlation Table\n\n",
    sprintf("The table displays Pearson's r, Spearman's rho, and Kendall's tau-b for all variable pairs, with %s quantifying the evidence for an association and central %g%% credible intervals.",
            bf_label(flow$bfType), 100 * flow$ciLevel))
}

build_pairs_intro <- function() {
  paste0("## Pairwise Plots\n\n",
    "For each variable pair, the scatter plot, the prior and posterior distribution of the correlation coefficient, and the Bayes-factor robustness check (as a function of the prior width) are displayed.")
}

build_matrix_intro <- function() {
  paste0("## Correlation Matrix Plot\n\n",
    "The correlation matrix plot provides a colour-coded overview of the pairwise relationships.")
}

build_conclusion <- function(data, lin, summ, flow) {
  parts <- c("## Conclusion", "")
  if (summ$nAssoc == 0) {
    parts <- c(parts, sprintf(
      "Across all three correlation coefficients (Pearson, Spearman, Kendall), the data provided no evidence for an association for any of the %d pairs; the Bayes factors favoured the null hypothesis and the credible intervals for the coefficients generally include zero. In summary, no reliable linear or monotonic relationships were detected among these variables.",
      summ$nPairs))
  } else {
    strong_txt <- ""
    if (!is.na(summ$strongIdx)) {
      i <- summ$strongIdx
      strong_txt <- sprintf(" The strongest evidence was for the association between %s and %s, with %s.",
                            data$main$variable1[i], data$main$variable2[i],
                            fmt_bf_report(data$main$pearsonbf[i], flow$bfType))
    }
    linearity_txt <- if (lin$tested && lin$nNonlin > 0)
      " Where the linearity test indicated non-linearity, the rank-based coefficients (Spearman's rho, Kendall's tau-b) provide the more appropriate measure of association."
    else ""
    parts <- c(parts, sprintf(
      "For %d of %d pairs the data provided evidence for an association.%s%s The Bayes factors and credible intervals in the table above should be interpreted accordingly.",
      summ$nAssoc, summ$nPairs, strong_txt, linearity_txt))
  }
  paste(parts, collapse = "\n")
}

# --- main --------------------------------------------------------------------

roboreport_main <- function(analysisId) {
  opts <- rr_get_options(analysisId)
  plan <- plan_report(opts)
  sib  <- rr_create_and_run("jaspRegression", "CorrelationBayesian", plan$options)
  data <- get_results(sib)

  flow <- plan$flow
  lin  <- assess_linearity(data, flow$bfType)
  summ <- summarize_correlations(data$main, flow$bfType)

  elements <- list(el_md(build_abstract(data, flow, lin, summ)))
  if (data$hasLinearity) {
    elements <- c(elements, list(
      el_md(build_assumptions_intro(flow)),
      el_ref("linearityTestTable"),
      el_md(build_assumptions_recommendation(lin))))
  }
  elements <- c(elements, list(
    el_md(build_table_intro(flow)),
    el_ref("corBayesTable")))
  if (data$hasPairsPlot)
    elements <- c(elements, list(el_md(build_pairs_intro()), el_ref("pairsPlotCollection")))
  if (data$hasMatrixPlot)
    elements <- c(elements, list(el_md(build_matrix_intro()), el_ref("matrixPlot")))
  elements <- c(elements, list(el_md(build_conclusion(data, lin, summ, flow))))

  rr_compose_results(sib, elements)
}
