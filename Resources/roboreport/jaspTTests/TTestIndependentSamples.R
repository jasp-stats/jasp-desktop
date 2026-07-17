# ==============================================================================
# RoboReport Script: Independent Samples T-Test Report
# ------------------------------------------------------------------------------
# Target:      jaspTTests::TTestIndependentSamples
# Version:     >=0.20.0
# Description: Assumption-driven report with test selection rationale,
#              effect size interpretation, and practical significance flags.
# ==============================================================================
#
# This script generates an annotated report for an Independent Samples T-Test.
# It is sourced and called by jaspRoboReport::run_script(), which invokes
# roboreport_main(analysisId).
#
# Pipeline:
#   1. Read the source analysis's initial options.
#   2. Plan the report: map options (enable all tests, assumptions, etc.)
#      and decide the interpretation flow.
#   3. Create a sibling analysis with the mapped options and run it.
#   4. Extract results from the sibling's RDS.
#   5. Build annotation elements (prose interleaved with result references).
#   6. Commit as an annotation on the sibling.

# ------------------------------------------------------------------------------
# 1. Plan: read initial options -> mapped options + flow decisions
# ------------------------------------------------------------------------------

plan_report <- function(opts) {

  # Extract user's variable selections for interpretation context
  dependent_vars <- opts$dependent$value
  group_var      <- opts$group$value
  hypothesis     <- opts$alternative

  # Map: merge user's options with what the report needs.
  # The user's dependent, group, alternative, naAction are preserved.
  report_opts <- modifyList(opts, list(
    # Enable all three test types for comprehensive comparison
    student      = TRUE,
    welch        = TRUE,
    mannWhitneyU = TRUE,
    # Effect sizes
    effectSize   = TRUE,
    effectSizeCi = TRUE,
    # Mean difference + CI
    meanDifference   = TRUE,
    meanDifferenceCi = TRUE,
    # Assumption checks
    normalityTest            = TRUE,
    equalityOfVariancesTest = TRUE,
    # Descriptives table + plots
    descriptives     = TRUE,
    descriptivesPlot = TRUE
  ))

  # Flow: for ttest, always the same interpretation path.
  # (Complex analyses would branch here based on opts.)
  flow <- list(
    dependent_vars = dependent_vars,
    group_var      = group_var,
    hypothesis     = hypothesis
  )

  list(options = report_opts, flow = flow)
}

# ------------------------------------------------------------------------------
# 2. Extract: read results from the RDS
# ------------------------------------------------------------------------------

get_results <- function(analysisId) {
  raw <- rr_results(analysisId)

  # The statistic column is named "t" when only Student is enabled,
  # but "Statistic" when multiple test types are enabled. Select whichever.
  stat_col <- if ("Statistic" %in% names(raw$ttest)) "Statistic" else "t"
  main <- rr_select(raw$ttest, c(
    "v", "test", stat_col, "df", "p",
    "md", "sed",
    "d", "effectSizeSe",
    "lowerCIlocationParameter", "upperCIlocationParameter",
    "lowerCIeffectSize", "upperCIeffectSize"
  ))
  names(main)[names(main) == stat_col] <- "statistic"

  # Descriptives table (optional - key "table" inside "ttestDescriptives")
  desc_container <- rr_get(raw, "ttestDescriptives")
  descriptives <- rr_select(
    rr_get(desc_container, "table"),
    c("variable", "group", "N", "mean", "sd", "se")
  )

  # Assumption checks (optional)
  assumptions <- rr_get(raw, "AssumptionChecks")
  normality <- rr_select(
    rr_get(assumptions, "ttestNormalTable"),
    c("dep", "W", "p")
  )
  variance <- rr_select(
    rr_get(assumptions, "equalityVariance"),
    c("variable", "fStat", "dfOne", "dfTwo", "p")
  )

  list(
    main         = main,
    descriptives = descriptives,
    normality    = normality,
    variance     = variance
  )
}

# ------------------------------------------------------------------------------
# 3. Decide: based on assumption checks, pick the appropriate test per variable
# ------------------------------------------------------------------------------

# Returns a data.frame with columns: variable, recommended_test, reason,
# normality_ok (lgl), variances_equal (lgl)
decide_tests <- function(data) {
  vars <- unique(data$main$v)
  results <- data.frame(
    variable       = vars,
    recommended    = NA_character_,
    reason         = NA_character_,
    normality_ok   = NA,
    variances_equal = NA,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(vars)) {
    v <- vars[i]

    # Check normality (Shapiro-Wilk) for this variable
    normality_ok <- TRUE
    if (!is.null(data$normality) && v %in% data$normality$dep) {
      p_norm <- data$normality$p[data$normality$dep == v][1]
      normality_ok <- !is.na(p_norm) && p_norm >= 0.05
    }

    # Check variance equality for this variable
    variances_equal <- TRUE
    if (!is.null(data$variance) && v %in% data$variance$variable) {
      p_var <- data$variance$p[data$variance$variable == v][1]
      variances_equal <- !is.na(p_var) && p_var >= 0.05
    }

    # Decision logic:
    #   Normality violated  -> Mann-Whitney (non-parametric)
    #   Normality OK, unequal variances -> Welch (robust)
    #   Normality OK, equal variances   -> Student (most powerful)
    if (!normality_ok) {
      recommended <- "Mann-Whitney"
      reason      <- "normality violated"
    } else if (!variances_equal) {
      recommended <- "Welch"
      reason      <- "variances unequal"
    } else {
      recommended <- "Student"
      reason      <- "assumptions met"
    }

    results$recommended[i]    <- recommended
    results$reason[i]         <- reason
    results$normality_ok[i]   <- normality_ok
    results$variances_equal[i] <- variances_equal
  }

  results
}

# ------------------------------------------------------------------------------
# 4. Build: compose annotation elements
# ------------------------------------------------------------------------------

build_elements <- function(data, opts, flow) {
  m <- data$main
  decisions <- decide_tests(data)
  elements  <- list()

  n_vars <- length(flow$dependent_vars)

  # --- Build the entire report as one markdown block ---
  report <- character()

  # === Report header ===
  report <- c("### Report")

  # === Assumption Checks ===
  report <- c(report, "#### Assumption Checks")

  if (!is.null(data$normality)) {
    norm_lines <- sapply(seq_len(nrow(data$normality)), function(i) {
      dep <- data$normality$dep[i]
      W   <- data$normality$W[i]
      p   <- data$normality$p[i]
      if (is.na(p)) {
        sprintf("- **%s**: W = %.3f, %s", dep, W, fmt_p(p))
      } else if (p < 0.05) {
        sprintf("- **%s**: W = %.3f, %s. We **reject** the null hypothesis of normality (p < .05): the residuals are not normally distributed.", dep, W, fmt_p(p))
      } else {
        sprintf("- **%s**: W = %.3f, %s. We **retain** the null hypothesis of normality (p >= .05): no evidence of non-normality.", dep, W, fmt_p(p))
      }
    })
    report <- c(report,
      glue::glue("**Normality of residuals (Shapiro-Wilk):**\n{paste(norm_lines, collapse='\n')}"))
  }

  if (!is.null(data$variance)) {
    var_lines <- sapply(seq_len(nrow(data$variance)), function(i) {
      v   <- data$variance$variable[i]
      Fv  <- data$variance$fStat[i]
      df1 <- data$variance$dfOne[i]
      df2 <- data$variance$dfTwo[i]
      p   <- data$variance$p[i]
      if (is.na(p)) {
        sprintf("- **%s**: F(%g, %g) = %.3f, %s", v, df1, df2, Fv, fmt_p(p))
      } else if (p < 0.05) {
        sprintf("- **%s**: F(%g, %g) = %.3f, %s. We **reject** the null hypothesis of equal variances (p < .05): heteroscedasticity is present.", v, df1, df2, Fv, fmt_p(p))
      } else {
        sprintf("- **%s**: F(%g, %g) = %.3f, %s. We **retain** the null hypothesis of equal variances (p >= .05): homoscedasticity is tenable.", v, df1, df2, Fv, fmt_p(p))
      }
    })
    report <- c(report,
      glue::glue("**Homogeneity of variances:**\n{paste(var_lines, collapse='\n')}"))
  }

  # === Test Selection Rationale ===
  report <- c(report, "#### Test Selection Rationale")
  reasoning_lines <- sapply(seq_len(nrow(decisions)), function(i) {
    v <- decisions$variable[i]
    reason <- decisions$reason[i]
    explanation <- if (reason == "normality violated") {
      "the normality assumption is violated (Shapiro-Wilk p < .05), so the non-parametric Mann-Whitney U test is reported as it does not require normally distributed residuals"
    } else if (reason == "variances unequal") {
      "normality holds but variances are unequal, so Welch's t-test is reported as it is robust to heteroscedasticity"
    } else {
      "both normality and homoscedasticity assumptions are met, so Student's t-test is reported as the most powerful option"
    }
    sprintf("- **%s**: %s", v, explanation)
  })
  report <- c(report,
    glue::glue("{paste(reasoning_lines, collapse='\n')}"))

  # === Results Discussion ===
  report <- c(report, "#### Results Discussion")

  n_sig <- 0
  discussion_parts <- character()

  for (i in seq_len(nrow(decisions))) {
    v         <- decisions$variable[i]
    test_name <- decisions$recommended[i]

    row <- m[m$v == v & m$test == test_name, , drop = FALSE]
    if (nrow(row) == 0) next

    stat   <- row$statistic[1]
    df_val <- row$df[1]
    p_val  <- row$p[1]
    d_val  <- row$d[1]
    md_val <- row$md[1]
    d_lo   <- row$lowerCIeffectSize[1]
    d_hi   <- row$upperCIeffectSize[1]
    md_lo  <- row$lowerCIlocationParameter[1]
    md_hi  <- row$upperCIlocationParameter[1]

    is_sig <- !is.na(p_val) && p_val < 0.05
    if (is_sig) n_sig <- n_sig + 1

    stat_str <- if (test_name == "Mann-Whitney") {
      sprintf("U = %.0f", stat)
    } else if (is.na(df_val)) {
      sprintf("t = %.2f", stat)
    } else {
      fmt_stat(stat, df_val, stat_name = "t")
    }

    if (!is.na(d_val)) {
      mag <- if (abs(d_val) < 0.2) "negligible"
             else if (abs(d_val) < 0.5) "small"
             else if (abs(d_val) < 0.8) "medium"
             else "large"
      ci_part <- if (!is.na(d_lo) && !is.na(d_hi) && (d_lo != 0 || d_hi != 0))
        sprintf(", 95%% CI [%.2f, %.2f]", d_lo, d_hi)
      else ""
      effect_text <- sprintf(
        "The effect size is %s (Cohen's d = %.2f%s), indicating %s practical significance.",
        mag, d_val, ci_part,
        if (mag == "negligible") "little to no"
        else if (mag == "small") "minor"
        else if (mag == "medium") "moderate"
        else "substantial"
      )
    } else {
      effect_text <- "No effect size estimate is available."
    }

    md_text <- if (test_name != "Mann-Whitney" && !is.na(md_val)) {
      md_ci <- if (!is.na(md_lo) && !is.na(md_hi))
        sprintf(" (95%% CI [%.3f, %.3f])", md_lo, md_hi)
      else ""
      sprintf(" The mean difference is %.3f%s.", md_val, md_ci)
    } else ""

    if (is_sig) {
      stmt <- sprintf(
        "For **%s**, the %s yields %s, %s. This is statistically significant, %s.%s",
        v, test_name, stat_str, fmt_p(p_val), effect_text, md_text
      )
      if (!is.na(d_val) && abs(d_val) < 0.2)
        stmt <- paste0(stmt,
          " However, the negligible effect size suggests this finding may lack practical importance.")
    } else {
      stmt <- sprintf(
        "For **%s**, the %s yields %s, %s. This is not statistically significant. %s%s",
        v, test_name, stat_str, fmt_p(p_val), effect_text, md_text
      )
      if (!is.na(d_val) && abs(d_val) >= 0.5)
        stmt <- paste0(stmt,
          " However, the non-trivial effect size suggests the study may be underpowered for this comparison.")
    }
    discussion_parts <- c(discussion_parts, stmt)
  }

  summary_line <- if (n_sig == 0)
    "Overall, no statistically significant differences between groups were detected."
  else if (n_sig == nrow(decisions))
    "Overall, all recommended tests revealed statistically significant differences."
  else
    sprintf("Overall, %d of %d comparisons reached statistical significance.", n_sig, nrow(decisions))

  report <- c(report, summary_line, discussion_parts)

  # --- Element order: results table, then report, then supporting tables ---

  # 1. Main results table at the top
  elements <- c(elements, list(el_ref("ttest")))

  # 2. The full report narrative as one markdown block
  elements <- c(elements, list(el_md(paste(report, collapse = "\n\n"))))

  # 3. Supporting tables
  if (!is.null(data$normality) || !is.null(data$variance))
    elements <- c(elements, list(el_ref("AssumptionChecks")))
  if (!is.null(data$descriptives))
    elements <- c(elements, list(el_ref("ttestDescriptives")))

  elements
}

# ------------------------------------------------------------------------------
# 4. Entry point - required by the RoboReport contract
# ------------------------------------------------------------------------------

roboreport_main <- function(analysisId) {

  # Read the source analysis info + initial options
  state <- rr_get_analyses_state(as.integer(analysisId),
                                include_options = TRUE,
                                options_meta_diff = FALSE)
  if (length(state$analyses) == 0)
    stop("Analysis not found: ", analysisId, call. = FALSE)

  src <- state$analyses[[1]]
  initial_opts <- src$options

  # Plan: map options + decide flow
  plan <- plan_report(initial_opts)

  # Create and run a sibling analysis with the mapped options
  sibling_id <- rr_create_and_run(
    module   = src$module,
    analysis = src$name,
    options  = plan$options
  )

  # Extract results from the sibling
  data <- get_results(sibling_id)

  # Build and compose results into the sibling (overwrites its results
  # with the report: prose interleaved with referenced result elements).
  elements <- build_elements(data, initial_opts, plan$flow)
  rr_compose_results(sibling_id, elements)

  invisible(NULL)
}
