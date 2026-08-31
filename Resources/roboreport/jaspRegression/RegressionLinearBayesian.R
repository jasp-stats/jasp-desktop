# ==============================================================================
# RoboReport Script: Bayesian Linear Regression Report
# ------------------------------------------------------------------------------
# Target:      jaspRegression::RegressionLinearBayesian
# Version:     >=0.20.0
# Description: Bayesian linear regression report with BAS model comparison,
#              posterior model probabilities, model-averaged coefficient
#              summaries, and inclusion Bayes factors. Honours the user's
#              Bayes-factor type (BF10 / BF01 / Log BF10).
# ==============================================================================
#
# Sourced and invoked by jaspRoboReport::run_script() -> roboreport_main(id).
#
# Pipeline:
#   1. Read the source analysis's options (dependent, predictors, model terms).
#   2. Plan: preserve the user's model, force on a rich reporting set
#      (posterior summaries, model comparison, descriptives, residual plots).
#   3. Create + run an enhanced sibling analysis.
#   4. Extract the model-comparison and posterior-summary tables from the RDS.
#   5. Build prose interleaved with references to the tables + plots.
#   6. Compose the report directly into the sibling (which then shows the report).

# --- helpers -----------------------------------------------------------------

num <- function(x) suppressWarnings(as.numeric(x))

# Format a Bayes factor value, using scientific notation for very large/small.
fmt_bf_value <- function(bf) {
  bf <- num(bf)
  if (is.na(bf)) return('NA')
  if (bf >= 1000 || (bf > 0 && bf < 0.001)) sprintf('%.2e', bf) else sprintf('%.2f', bf)
}

# Display label for the user's Bayes-factor scale.
bf_label <- function(bfType) {
  if (identical(bfType, 'BF01'))    return('BF01')
  if (identical(bfType, 'LogBF10')) return('Log(BF10)')
  'BF10'
}

# Label for the inclusion Bayes factor column (depends on the BF type).
bfincl_label <- function(bfType) {
  if (identical(bfType, 'LogBF10')) return('Log(BF_inclusion)')
  'BF_inclusion'
}

# Convert an inclusion BF column value to BF_inclusion (BF10 scale) for
# interpretation. Only LogBF10 stores the value log-transformed; BF10 and
# BF01 both store BF_inclusion on the BF10 scale.
bfincl_to_bf10 <- function(value, bfType) {
  value <- num(value)
  if (is.na(value)) return(NA_real_)
  if (identical(bfType, 'LogBF10')) return(exp(value))
  value
}

# --- 1. plan -----------------------------------------------------------------

plan_report <- function(opts) {
  dependent  <- opts$dependent$value
  covariates <- opts$covariates$value
  report_opts <- modifyList(opts, list(
    # NOTE: bayesFactorType is deliberately NOT forced -- respect the user's
    # choice (BF10 / BF01 / LogBF10); the prose interprets it accordingly.
    posteriorSummaryTable      = TRUE,
    posteriorSummaryPlot       = TRUE,
    descriptives               = TRUE,
    inclusionProbabilitiesPlot = TRUE,
    # Residual diagnostic plots forced OFF: they error when the data contain
    # missing values ("missing values and NaN's not allowed"). The conditional
    # el_ref()s below skip them when absent. Re-enable for complete data.
    residualsVsFittedPlot      = FALSE,
    qqPlot                     = FALSE
  ))
  list(options = report_opts,
       flow = list(dependent  = dependent,
                   covariates = covariates,
                   bfType     = if (is.null(opts$bayesFactorType)) 'BF10' else opts$bayesFactorType))
}

# --- 2. extract --------------------------------------------------------------
# RDS short keys: basreg{ modelComparisonTable, postSumContainer{ postSumTable,
# postSumPlot }, inclusionProbabilitiesPlot, ResidualsVsFittedPlot, qqPlot },
# and descriptivesTable at the top level. el_ref() uses the full meta names
# (e.g. basreg_modelComparisonTable).

get_results <- function(analysisId) {
  raw    <- rr_results(analysisId)
  basreg <- rr_get(raw, 'basreg')
  psc    <- rr_get(basreg, 'postSumContainer')
  list(
    modelComp = rr_select(rr_get(basreg, 'modelComparisonTable'),
                          c('Models', 'priorProbModel', 'postProbModel', 'BFM', 'BF', 'R2')),
    postSum   = rr_select(rr_get(psc, 'postSumTable'),
                          c('coefficient', 'pInclprior', 'pIncl', 'BFincl', 'mean', 'sd', 'lowerCri', 'upperCri')),
    plots = list(
      postSum   = !is.null(rr_get(psc, 'postSumPlot')),
      inclusion = !is.null(rr_get(basreg, 'inclusionProbabilitiesPlot')),
      resid     = !is.null(rr_get(basreg, 'ResidualsVsFittedPlot')),
      qq        = !is.null(rr_get(basreg, 'qqPlot'))
    )
  )
}

# --- 3. prose ----------------------------------------------------------------

build_abstract <- function(data, flow) {
  mc <- data$modelComp
  best <- mc[which.max(num(mc$postProbModel)), ]
  k <- length(flow$covariates)
  pred_list <- paste0('**', flow$covariates, '**', collapse = ', ')
  paste0(
    '## Abstract\n\n',
    sprintf('A Bayesian linear regression model was fitted to predict **%s** from %d predictor(s): %s. ', flow$dependent, k, pred_list),
    'Bayesian adaptive sampling (BAS) was used to explore the model space under a JZS prior on the coefficients. ',
    sprintf('Evidence was quantified with %s. ', bf_label(flow$bfType)),
    sprintf('The best-fitting model (%s) had posterior model probability P(M|data) = %.3f. ', best$Models, num(best$postProbModel)),
    'Model comparison, model-averaged coefficient estimates, and inclusion Bayes factors are reported below.'
  )
}

build_model_comparison <- function(data) {
  mc <- data$modelComp
  parts <- c('### Model Comparison', '',
             'Models are ordered by posterior model probability. P(M) is the prior and P(M|data) the posterior model probability:')
  for (i in seq_len(nrow(mc))) {
    parts <- c(parts, sprintf(
      '- **%s**: P(M) = %.3f, P(M|data) = %.3f, R2 = %.3f.',
      mc$Models[i], num(mc$priorProbModel[i]), num(mc$postProbModel[i]), num(mc$R2[i])))
  }
  best <- mc[which.max(num(mc$postProbModel)), ]
  parts <- c(parts, sprintf(
    'The best model (%s) has the highest posterior probability (P(M|data) = %.3f).',
    best$Models, num(best$postProbModel)))
  paste(parts, collapse = '\n')
}

build_coefficients <- function(data, flow) {
  ps <- data$postSum
  ps <- ps[ps$coefficient != 'Intercept', , drop = FALSE]
  if (nrow(ps) == 0)
    return('### Coefficients\n\nNo predictor coefficient summaries were available.')
  parts <- c('### Model-Averaged Coefficients', '',
             'For each predictor, the model-averaged posterior mean, 95% credible interval, posterior inclusion probability, and inclusion Bayes factor are reported:')
  for (i in seq_len(nrow(ps))) {
    nm    <- ps$coefficient[i]
    mean  <- num(ps$mean[i]); sd <- num(ps$sd[i])
    lo    <- num(ps$lowerCri[i]); hi <- num(ps$upperCri[i])
    pincl <- num(ps$pIncl[i]); bfincl <- num(ps$BFincl[i])
    ci <- if (!is.na(lo) && !is.na(hi)) sprintf(', 95%% CI [%.2f, %.2f]', lo, hi) else ''
    cat_i <- bf_evidence_category(bfincl_to_bf10(bfincl, flow$bfType))$category
    parts <- c(parts, sprintf(
      '- **%s**: posterior mean = %.2f (SD = %.2f)%s; P(incl|data) = %.3f; %s = %s (%s evidence for inclusion).',
      nm, mean, sd, ci, pincl, bfincl_label(flow$bfType), fmt_bf_value(bfincl), cat_i))
  }
  paste(parts, collapse = '\n')
}

build_assumptions <- function() {
  paste('### Model Assumptions', '',
        paste0('The inclusion-probabilities plot summarises the marginal posterior inclusion probability of each predictor. ',
               'The linearity, normality, and homoscedasticity assumptions can be inspected with the residual diagnostic plots ',
               '(Q-Q plot of model-averaged residuals and residuals-vs-fitted plot) available in the analysis options.'),
        sep = '\n')
}

build_conclusion <- function(data, flow) {
  mc <- data$modelComp
  best <- mc[which.max(num(mc$postProbModel)), ]
  ps <- data$postSum
  ps <- ps[ps$coefficient != 'Intercept', , drop = FALSE]
  incl_idx <- which(num(ps$pIncl) > 0.5)
  incl_txt <- if (length(incl_idx) > 0) {
    sprintf('The predictors most strongly supported for inclusion were %s.',
            paste0('**', ps$coefficient[incl_idx], '**', collapse = ', '))
  } else {
    'No predictor had a posterior inclusion probability exceeding 0.5.'
  }
  body <- paste(
    sprintf('The best model (%s) explained R2 = %.3f of the variance in **%s** with posterior probability %.3f.',
            best$Models, num(best$R2), flow$dependent, num(best$postProbModel)),
    incl_txt,
    'These results quantify relative evidence under the specified priors and do not imply causation.',
    sep = ' ')
  paste('## Conclusion', '', body, sep = '\n')
}

# --- main --------------------------------------------------------------------

roboreport_main <- function(analysisId) {
  opts <- rr_get_options(analysisId)
  plan <- plan_report(opts)
  sib  <- rr_create_and_run('jaspRegression', 'RegressionLinearBayesian', plan$options)
  data <- get_results(sib)

  elements <- list(
    el_md(build_abstract(data, plan$flow)),
    el_ref('basreg_modelComparisonTable'),
    el_md(build_model_comparison(data)),
    el_ref('basreg_postSumContainer_postSumTable'),
    el_md(build_coefficients(data, plan$flow))
  )
  if (data$plots$postSum)
    elements <- c(elements, list(el_ref('basreg_postSumContainer_postSumPlot')))
  if (data$plots$inclusion)
    elements <- c(elements, list(el_ref('basreg_inclusionProbabilitiesPlot')))
  elements <- c(elements, list(
    el_ref('descriptivesTable'),
    el_md(build_assumptions())
  ))
  if (data$plots$resid)
    elements <- c(elements, list(el_ref('basreg_ResidualsVsFittedPlot')))
  if (data$plots$qq)
    elements <- c(elements, list(el_ref('basreg_qqPlot')))
  elements <- c(elements, list(el_md(build_conclusion(data, plan$flow))))

  rr_compose_results(sib, elements)
}
