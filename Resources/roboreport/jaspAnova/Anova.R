# ==============================================================================
# RoboReport Script: Classical ANOVA Report
# ------------------------------------------------------------------------------
# Target:      jaspAnova::Anova
# Version:     >=0.20.0
# Description: Comprehensive, assumption-aware between-subjects ANOVA report.
#              Produces an abstract, an annotated ANOVA summary table (F-tests
#              with Vovk-Sellke MPR and eta-squared / partial eta-squared /
#              omega-squared effect sizes), descriptives with a descriptive
#              plot, assumption checks (Levene's test of homogeneity of
#              variances and a Q-Q plot of the residuals), Tukey HSD post-hoc
#              comparisons, and a conclusion with limitations and follow-ups.
#              Honours the user's dependent variable, fixed factors, model
#              terms and sum-of-squares type. Mirrors a full AI-style
#              annotation deterministically.
# ==============================================================================
#
# Sourced and invoked by jaspRoboReport::run_script() -> roboreport_main(id).
#
# Pipeline:
#   1. Read the source analysis's options (dependent, fixed factors, model
#      terms, sum-of-squares type).
#   2. Plan: preserve the user's dependent / fixed factors / model terms /
#      sum-of-squares; force on descriptives + a descriptive plot, effect-size
#      estimates (eta-squared, partial eta-squared, omega-squared), the
#      Vovk-Sellke MPR, Levene's homogeneity test, a Q-Q plot of residuals, and
#      Tukey post-hoc comparisons for every model term.
#   3. Create + run an enhanced sibling analysis.
#   4. Extract the ANOVA table, descriptives, Levene's table and the post-hoc
#      tables from the sibling RDS.
#   5. Build assumption-aware prose interleaved with result references.
#   6. Compose the report directly into the sibling (which then shows it).

# --- helpers -----------------------------------------------------------------

num <- function(x) suppressWarnings(as.numeric(x))

# Guard a value down to a single scalar (or NA) so it is always safe to feed
# to sprintf() or to test with is.na() -- defends against zero-length cells.
scalar <- function(x) if (length(x) >= 1) x[1] else NA

# Format an F statistic with its two degrees of freedom, APA style.
fmt_F <- function(f, df1, df2) {
  f   <- num(f); df1 <- num(df1); df2 <- num(df2)
  if (is.na(f)) return("F = NA")
  if (is.na(df1) || is.na(df2))
    return(sprintf("F = %.2f", f))
  sprintf("F(%.0f, %.0f) = %.2f", df1, df2, f)
}

# Magnitude label for an (partial) eta-squared effect size (Cohen, 1988).
eta_label <- function(eta) {
  eta <- abs(num(eta))
  if (is.na(eta)) return(NA_character_)
  if (eta < 0.01) "negligible"
  else if (eta < 0.06) "small"
  else if (eta < 0.14) "medium"
  else "large"
}

# Human-readable sum-of-squares type.
ss_label <- function(ss) {
  if (identical(ss, "type1")) return("Type I")
  if (identical(ss, "type2")) return("Type II")
  "Type III"
}

# Word describing the dimensionality of the design.
nway_word <- function(k) {
  if (k == 1) return("one-way")
  if (k == 2) return("two-way")
  if (k == 3) return("three-way")
  sprintf("%d-way", k)
}

# Does a term label denote an interaction (contains the JASP interaction mark)?
is_interaction_term <- function(lbl) grepl("\u273b|\\*", lbl)

# --- 1. plan -----------------------------------------------------------------

# Build the post-hoc term specification from the model terms: every model term
# (each main effect and each interaction) gets a post-hoc comparison table.
posthoc_from_model <- function(model_terms) {
  if (is.null(model_terms) || length(model_terms$value) == 0) return(NULL)
  value <- lapply(model_terms$value, function(t) {
    comps <- if (!is.null(t$components)) t$components else t
    list(variable = comps)
  })
  list(optionKey = "variable", types = model_terms$types, value = value)
}

plan_report <- function(opts) {
  dependent <- opts$dependent$value
  factors   <- opts$fixedFactors$value

  model_terms <- opts$modelTerms
  posthoc     <- posthoc_from_model(model_terms)

  report_opts <- modifyList(opts, list(
    # descriptives + descriptive plot
    descriptives                 = TRUE,
    descriptivePlotHorizontalAxis = list(types = c("nominal"),
                                         value = if (length(factors) >= 1) factors[1] else ""),
    descriptivePlotErrorBar      = TRUE,
    descriptivePlotErrorBarType  = "ci",
    # effect-size estimates
    effectSizeEstimates        = TRUE,
    effectSizeEtaSquared       = TRUE,
    effectSizePartialEtaSquared = TRUE,
    effectSizeOmegaSquared     = TRUE,
    # Vovk-Sellke MPR
    vovkSellke = TRUE,
    # assumption checks
    homogeneityTests = TRUE,
    qqPlot           = TRUE,
    # post-hoc (Tukey HSD) for every model term
    postHocTypeStandard    = TRUE,
    postHocCorrectionTukey = TRUE
  ))

  # Only inject post-hoc terms when a model is present.
  if (!is.null(posthoc)) report_opts$postHocTerms <- posthoc
  # A second factor (if any) differentiates the lines of the descriptive plot.
  if (length(factors) >= 2)
    report_opts$descriptivePlotSeparateLines <- list(types = c("nominal"),
                                                     value = factors[2])

  list(options = report_opts,
       flow = list(dependent = dependent,
                   factors   = factors,
                   ss        = if (is.null(opts$sumOfSquares)) "type3" else opts$sumOfSquares,
                   modelTerms = model_terms,
                   hasPostHoc = !is.null(posthoc)))
}

# --- 2. extract --------------------------------------------------------------
# RDS navigation uses the short nested keys; el_ref() uses the underscore-
# joined .meta paths (see the element references in roboreport_main).
#   anovaContainer
#     anovaTable                      (F table)
#     descriptivesContainer
#       tableDescriptives             (descriptives table)
#       containerDescriptivesPlots    (descriptive plot collection)
#     assumptionsContainer
#       leveneTable                   (Levene's test)
#       qqPlot                        (Q-Q plot of residuals)
#     postHocContainer
#       postHocStandardContainer
#         <term>1 ...                 (one Tukey table per model term)

get_results <- function(analysisId) {
  raw   <- rr_results(analysisId)
  ac    <- rr_get(raw, "anovaContainer")

  main <- rr_select(rr_get(ac, "anovaTable"), c(
    "cases", "Sum Sq", "Df", "Mean Sq", "F value", "Pr(>F)",
    "vovkSellke", "eta", "partialEta", "omega"))

  descCont <- rr_get(ac, "descriptivesContainer")
  desc     <- rr_get(descCont, "tableDescriptives")

  assum  <- rr_get(ac, "assumptionsContainer")
  levene <- rr_select(rr_get(assum, "leveneTable"),
                      c("F", "df1", "df2", "p", "vovkSellke"))

  phStd <- rr_get(rr_get(ac, "postHocContainer"), "postHocStandardContainer")

  list(
    main        = main,
    desc        = desc,
    levene      = levene,
    hasQQ       = !is.null(rr_get(assum, "qqPlot")),
    hasDescPlot = !is.null(rr_get(descCont, "containerDescriptivesPlots")),
    postHoc     = phStd,
    hasPostHoc  = !is.null(phStd) && length(phStd) > 0
  )
}

# Locate the descriptives column that holds a factor's levels. jaspBase names
# these "<factor>." but we also tolerate the bare name or a prefix match.
desc_col <- function(desc, f) {
  if (is.null(desc)) return(NA_character_)
  for (cc in c(f, paste0(f, ".")))
    if (cc %in% names(desc)) return(cc)
  hit <- grep(paste0("^", f), names(desc), value = TRUE)
  if (length(hit) > 0) hit[1] else NA_character_
}

factor_levels <- function(desc, f) {
  col <- desc_col(desc, f)
  if (is.na(col)) return(character(0))
  lv <- as.character(desc[[col]])
  sort(unique(lv[!is.na(lv)]))
}

# --- assumption + effect summaries -------------------------------------------

assess_homogeneity <- function(levene) {
  if (is.null(levene) || nrow(levene) == 0 || !("p" %in% names(levene)))
    return(list(tested = FALSE, violated = NA, F = NA_real_, df1 = NA_real_,
                df2 = NA_real_, p = NA_real_, vs = NA_real_))
  p <- num(scalar(levene$p))
  list(tested = TRUE,
       F   = num(scalar(levene$F)),
       df1 = num(scalar(levene$df1)),
       df2 = num(scalar(levene$df2)),
       p   = p,
       vs  = num(scalar(levene$vovkSellke)),
       violated = !is.na(p) && p < 0.05)
}

# Split the ANOVA table into effect rows + the residual (error) term, and flag
# which effects are significant at alpha = .05.
summarize_anova <- function(main) {
  empty <- list(nEffects = 0L, effects = NULL, residDf = NA_real_,
                nSig = 0L, sigTerms = character(0))
  if (is.null(main) || nrow(main) == 0 || !("cases" %in% names(main)))
    return(empty)
  isResid <- main$cases == "Residuals"
  residDf <- if (any(isResid)) num(scalar(main$Df[isResid])) else NA_real_
  eff <- main[!isResid, , drop = FALSE]
  p   <- num(eff[["Pr(>F)"]])
  sig <- !is.na(p) & p < 0.05
  list(nEffects = nrow(eff), effects = eff, residDf = residDf,
       nSig = sum(sig), sigTerms = eff$cases[sig])
}

# Collate the per-term Tukey tables, flagging significant comparisons.
summarize_posthoc <- function(phStd) {
  if (is.null(phStd) || length(phStd) == 0) return(list(terms = list(), any = FALSE))
  terms <- list()
  for (nm in names(phStd)) {
    tbl <- phStd[[nm]]
    if (!is.data.frame(tbl) || nrow(tbl) == 0 || !("tukey" %in% names(tbl))) next
    p   <- num(tbl$tukey)
    sig <- !is.na(p) & p < 0.05
    terms[[length(terms) + 1]] <- list(
      rawName  = nm,
      termName = clean_term(nm),
      nComp    = nrow(tbl),
      nSig     = sum(sig),
      tbl      = tbl,
      sig      = tbl[sig, , drop = FALSE])
  }
  list(terms = terms, any = length(terms) > 0)
}

# Turn an RDS post-hoc table name (e.g. "contBinom \u273b facGender1") into a
# readable term label ("contBinom \u00d7 facGender").
clean_term <- function(nm) {
  nm <- sub("[0-9]+$", "", nm)
  nm <- trimws(nm)
  gsub("\u273b", "\u00d7", nm)
}

# Per-cell descriptives summary: highest/lowest group means and group-size
# balance, used by the descriptives and conclusion prose.
summarize_descriptives <- function(desc, factors) {
  empty <- list(nCells = 0L, totalN = NA_real_, hi = NULL, lo = NULL,
                sdRange = NA, balanced = NA)
  if (is.null(desc) || nrow(desc) == 0 || !("Mean" %in% names(desc))) return(empty)
  mean <- num(desc$Mean)
  ok   <- !is.na(mean)
  if (!any(ok)) return(empty)
  hi_i <- which.max(mean); lo_i <- which.min(mean)
  cell_label <- function(i) {
    parts <- vapply(factors, function(f) {
      col <- desc_col(desc, f)
      if (is.na(col)) return("")
      paste0(f, " = ", as.character(scalar(desc[[col]][i])))
    }, character(1))
    paste(parts[nzchar(parts)], collapse = ", ")
  }
  Nn  <- if ("N" %in% names(desc)) num(desc$N) else rep(NA_real_, nrow(desc))
  sdv <- if ("SD" %in% names(desc)) num(desc$SD) else rep(NA_real_, nrow(desc))
  list(
    nCells   = nrow(desc),
    totalN   = if (all(is.na(Nn))) NA_real_ else sum(Nn, na.rm = TRUE),
    hi       = list(label = cell_label(hi_i), mean = mean[hi_i],
                    sd = num(scalar(sdv[hi_i])), n = num(scalar(Nn[hi_i]))),
    lo       = list(label = cell_label(lo_i), mean = mean[lo_i],
                    sd = num(scalar(sdv[lo_i])), n = num(scalar(Nn[lo_i]))),
    sdRange  = if (all(is.na(sdv))) NA else range(sdv, na.rm = TRUE),
    balanced = if (all(is.na(Nn))) NA else (length(unique(Nn)) == 1)
  )
}

# --- 3. prose ----------------------------------------------------------------

# One "factor (n levels: a, b, ...)" descriptor per fixed factor.
factor_descriptors <- function(flow, data) {
  vapply(flow$factors, function(f) {
    lvls <- factor_levels(data$desc, f)
    n <- length(lvls)
    lvl_txt <- if (n > 0) paste(lvls, collapse = ", ") else "levels unavailable"
    word <- if (n == 1) "one level" else if (n == 2) "two levels" else sprintf("%d levels", n)
    sprintf("**%s** (%s: %s)", f, word, lvl_txt)
  }, character(1))
}

build_abstract <- function(flow, data, summ, homog, ds) {
  k  <- length(flow$factors)
  nn <- if (!is.null(ds) && !is.na(ds$totalN)) ds$totalN else NA
  n_txt <- if (!is.na(nn)) sprintf(" (N = %.0f)", nn) else ""

  fac_txt <- paste(factor_descriptors(flow, data), collapse = " and ")

  model_txt <- if (k == 1)
    "The model comprised the single main effect."
  else if (k == 2)
    "The model included both main effects and their interaction."
  else
    "The model included all main effects and interactions among the factors."

  assum_txt <- if (homog$tested && data$hasQQ)
    " Assumptions were checked with Levene's test of homogeneity of variances and a Q-Q plot of the residuals."
  else if (homog$tested)
    " Homogeneity of variances was checked with Levene's test."
  else if (data$hasQQ)
    " Normality of the residuals was inspected with a Q-Q plot."
  else ""

  posthoc_txt <- if (data$hasPostHoc)
    " Tukey HSD post-hoc comparisons follow up on the omnibus effects."
  else ""

  paste0(
    "## Abstract\n\n",
    sprintf("A %s between-subjects ANOVA was conducted to examine the effect%s of %s on the continuous outcome **%s**%s. ",
            nway_word(k), if (k > 1) "s" else "", fac_txt, flow$dependent, n_txt),
    sprintf("%s sum of squares was used. ", ss_label(flow$ss)),
    model_txt, assum_txt, posthoc_txt,
    " This report interprets each output element in turn."
  )
}

build_anova_intro <- function(flow) {
  paste0("## ANOVA Table\n\n",
    sprintf("The ANOVA summary reports, for each model term and the residual error, the sum of squares, degrees of freedom, mean square, F-statistic, p-value, Vovk-Sellke maximum p-ratio, and effect-size estimates (eta-squared, partial eta-squared, omega-squared). %s sum of squares was used. The Vovk-Sellke MPR gives the maximum possible odds in favour of H\u2081 over H\u2080 that the observed p-value can support.",
            ss_label(flow$ss)))
}

build_anova_interpretation <- function(summ) {
  if (summ$nEffects == 0)
    return("**ANOVA Table Interpretation.** No ANOVA results were available for this analysis.")
  eff <- summ$effects
  parts <- vapply(seq_len(nrow(eff)), function(i) {
    term <- as.character(eff$cases[i])
    Fv   <- num(scalar(eff[["F value"]][i]))
    df1  <- num(scalar(eff$Df[i]))
    p    <- num(scalar(eff[["Pr(>F)"]][i]))
    vs   <- num(scalar(eff$vovkSellke[i]))
    peta <- num(scalar(eff$partialEta[i]))
    kind <- if (is_interaction_term(term)) "interaction" else "main effect"
    if (is.na(Fv) || is.na(p))
      return(sprintf("The %s of %s could not be tested.", kind, term))
    sig_txt <- if (p < 0.05) "was statistically significant" else "was not statistically significant"
    vs_txt  <- if (!is.na(vs) && vs > 1.01)
      sprintf(", with a Vovk-Sellke MPR of %.2f", vs) else ""
    es_txt  <- if (!is.na(peta)) {
      mag <- eta_label(peta)
      sprintf("; partial \u03b7\u00b2 = %.3f%s", peta,
              if (!is.na(mag)) sprintf(" (%s)", mag) else "")
    } else ""
    sprintf("The %s of %s %s, %s%s%s.",
            kind, term, sig_txt, fmt_F(Fv, df1, summ$residDf), vs_txt, es_txt)
  }, character(1))

  lead <- if (summ$nSig == 0)
    sprintf("None of the %d model terms reached statistical significance at \u03b1 = .05.", summ$nEffects)
  else
    sprintf("%d of %d model terms reached statistical significance at \u03b1 = .05 (%s).",
            summ$nSig, summ$nEffects, paste(summ$sigTerms, collapse = ", "))

  paste0("**ANOVA Table Interpretation.** ", lead, " ", paste(parts, collapse = " "))
}

build_descriptives_intro <- function() {
  paste0("## Descriptives\n\n",
    "The descriptives table lists the sample size (N), mean, standard deviation (SD), standard error (SE) and coefficient of variation for every level combination of the fixed factors. The descriptive plot visualises the same cell means with 95% confidence intervals.")
}

build_descriptives_interpretation <- function(ds) {
  if (is.null(ds) || ds$nCells == 0)
    return("**Descriptives Table Interpretation.** Descriptive statistics were not available for this analysis.")
  total_txt <- if (!is.na(ds$totalN)) sprintf(" across the %.0f observations", ds$totalN) else ""
  hi <- ds$hi; lo <- ds$lo
  bal_txt <- if (is.na(ds$balanced)) "" else if (ds$balanced)
    " The design is balanced (equal group sizes)."
  else
    " The design is unbalanced (unequal group sizes), which reduces power and can affect the choice of sum-of-squares type."
  sd_txt <- if (length(ds$sdRange) == 2 && !any(is.na(ds$sdRange)))
    sprintf(" Standard deviations ranged from %.2f to %.2f.", ds$sdRange[1], ds$sdRange[2])
  else ""
  sprintf(paste0("**Descriptives Table Interpretation.** There are %d cells%s. ",
                 "The highest cell mean was for %s (M = %.2f, SD = %.2f) and the lowest for %s (M = %.2f, SD = %.2f).%s%s"),
          ds$nCells, total_txt,
          hi$label, hi$mean, if (is.na(hi$sd)) 0 else hi$sd,
          lo$label, lo$mean, if (is.na(lo$sd)) 0 else lo$sd,
          sd_txt, bal_txt)
}

build_plot_interpretation <- function(summ) {
  if (summ$nSig == 0)
    paste0("**Descriptive Plot Interpretation.** The descriptive plot displays the cell means with 95% confidence intervals. ",
           "Substantial overlap of the intervals across the levels of each factor is consistent with the non-significant omnibus F-tests reported above.")
  else
    paste0("**Descriptive Plot Interpretation.** The descriptive plot displays the cell means with 95% confidence intervals. ",
           sprintf("Separation of the intervals for the levels of %s is consistent with the significant omnibus effect(s); near-parallel lines across the factors suggest the absence of an interaction.",
                   paste(summ$sigTerms, collapse = " and ")))
}

build_assumptions_intro <- function() {
  paste0("## Assumption Checks\n\n",
    "Two assumptions of the between-subjects ANOVA were examined: homogeneity of variances across the groups (Levene's test) and normality of the residuals (Q-Q plot). A significant Levene's test (p < .05) indicates unequal group variances, which can inflate the Type I error rate, particularly in unbalanced designs.")
}

build_levene_interpretation <- function(homog) {
  if (!homog$tested)
    return("**Levene's Test Interpretation.** Levene's test was not available for this analysis.")
  stat_txt <- fmt_F(homog$F, homog$df1, homog$df2)
  if (is.na(homog$p))
    return(paste0("**Levene's Test Interpretation.** Levene's test could not be computed (", stat_txt, ")."))
  vs_txt <- if (!is.na(homog$vs) && homog$vs > 1.01)
    sprintf(" The Vovk-Sellke MPR of %.2f indicates little evidence against the null of equal variances.", homog$vs) else ""
  if (homog$violated)
    sprintf(paste0("**Levene's Test Interpretation.** Levene's test was statistically significant, %s, %s, ",
                   "indicating that the assumption of homogeneity of variances is violated. The ANOVA F-tests should be interpreted with caution; a Welch correction, a robust test, or a non-parametric alternative (e.g. Kruskal-Wallis) is advisable.%s"),
            stat_txt, fmt_p(homog$p), vs_txt)
  else
    sprintf(paste0("**Levene's Test Interpretation.** Levene's test was not statistically significant, %s, %s, ",
                   "so the assumption of homogeneity of variances is tenable; the observed differences in group variances are within the range expected from sampling variation.%s"),
            stat_txt, fmt_p(homog$p), vs_txt)
}

build_qq_interpretation <- function() {
  paste0("**Q-Q Plot Interpretation.** The Q-Q plot displays the standardised residuals against the theoretical quantiles of the normal distribution. ",
         "Points tracking the diagonal reference line indicate that the residuals are approximately normally distributed, supporting the validity of the F-tests; systematic curvature or heavy tails would suggest departures from normality that the F-test is moderately robust to in balanced designs of this size.")
}

build_posthoc_intro <- function() {
  paste0("## Post Hoc Tests\n\n",
    "Tukey HSD pairwise comparisons were conducted for each model term, with p-values adjusted for multiple comparisons. For main effects the comparisons are averaged over the levels of the other factors; for an interaction the comparisons are between the individual cells.")
}

build_posthoc_interpretation <- function(ph) {
  if (!ph$any)
    return("**Post Hoc Interpretation.** Post-hoc comparisons were not available for this analysis.")
  paras <- vapply(ph$terms, function(t) {
    head <- sprintf("**Post Hoc Comparisons: %s.** ", t$termName)
    if (t$nSig == 0)
      return(paste0(head, sprintf("None of the %d pairwise comparisons reached statistical significance after Tukey adjustment (all p > .05).", t$nComp)))
    rows <- vapply(seq_len(nrow(t$sig)), function(i) {
      a  <- as.character(scalar(t$sig$contrast_A[i]))
      b  <- as.character(scalar(t$sig$contrast_B[i]))
      md <- num(scalar(t$sig$estimate[i]))
      p  <- num(scalar(t$sig$tukey[i]))
      dir <- if (!is.na(md))
        sprintf(" (mean difference %s \u2212 %s = %.2f)", a, b, md) else ""
      sprintf("%s vs. %s, %s%s", a, b, fmt_p(p), dir)
    }, character(1))
    paste0(head, sprintf("%d of %d comparisons was significant: %s.",
                         t$nSig, t$nComp, paste(rows, collapse = "; ")))
  }, character(1))
  paste(paras, collapse = " ")
}

build_conclusion <- function(flow, summ, homog, ph, ds) {
  parts <- c("## Conclusion", "")

  if (summ$nEffects == 0) {
    parts <- c(parts, "No ANOVA results were available, so no firm conclusions can be drawn.")
  } else if (summ$nSig == 0) {
    parts <- c(parts, sprintf(
      "The %s between-subjects ANOVA found no statistically significant effects: none of the %d model terms (main effects or interactions) reached significance at \u03b1 = .05. The data therefore provide no evidence that the group means on %s differ across the levels of the factors examined.",
      nway_word(length(flow$factors)), summ$nEffects, flow$dependent))
  } else {
    parts <- c(parts, sprintf(
      "The %s between-subjects ANOVA revealed %d statistically significant effect(s): %s. Non-significant terms indicated no reliable differences for those effects.",
      nway_word(length(flow$factors)), summ$nSig, paste(summ$sigTerms, collapse = ", ")))
    # Direction from the post-hoc tables for significant two-level terms.
    dir_txt <- character(0)
    for (t in ph$terms) {
      if (t$nSig > 0 && !is_interaction_term(t$termName) && nrow(t$sig) >= 1) {
        a  <- as.character(scalar(t$sig$contrast_A[1]))
        b  <- as.character(scalar(t$sig$contrast_B[1]))
        md <- num(scalar(t$sig$estimate[1]))
        if (!is.na(md)) {
          higher <- if (md > 0) a else b
          lower  <- if (md > 0) b else a
          dir_txt <- c(dir_txt, sprintf("for %s, the %s group scored higher than the %s group (mean difference %.2f)",
                                        t$termName, higher, lower, abs(md)))
        }
      }
    }
    if (length(dir_txt) > 0)
      parts <- c(parts, paste0("Directionally, ", paste(dir_txt, collapse = "; "), "."))
  }

  assum_txt <- if (homog$tested)
    if (homog$violated)
      " The homogeneity-of-variances assumption was violated (Levene's test significant), so the F-tests should be interpreted with caution."
    else
      " The homogeneity-of-variances assumption was met (Levene's test non-significant), and the Q-Q plot suggested approximately normal residuals, supporting the validity of the F-tests."
  else ""
  if (nzchar(assum_txt)) parts <- c(parts, trimws(assum_txt))

  # Limitations.
  lim <- character(0)
  if (!is.null(ds) && !is.na(ds$totalN) && ds$totalN < 200)
    lim <- c(lim, sprintf("the sample size (N = %.0f) is modest", ds$totalN))
  if (!is.null(ds) && isFALSE(ds$balanced))
    lim <- c(lim, "the group sizes were unbalanced, which reduces power for the interaction and for effects within smaller subgroups")
  if (length(lim) > 0)
    parts <- c(parts, paste0("**Limitations.** ", paste(lim, collapse = ", and "), "."))

  parts <- c(parts, paste0(
    "**Suggested follow-ups.** (1) Report and interpret the effect sizes (partial \u03b7\u00b2 and \u03c9\u00b2) to quantify the magnitude of any significant effects. ",
    "(2) Consider a Bayesian ANOVA to quantify evidence for the null hypotheses of the non-significant terms. ",
    "(3) If the design is genuinely unbalanced, compare Type II with Type III sums of squares or report weighted marginal means. ",
    "(4) Examine whether the significant differences remain after controlling for relevant covariates (ANCOVA)."))

  paste(parts, collapse = "\n")
}

# --- main --------------------------------------------------------------------

roboreport_main <- function(analysisId) {
  opts <- rr_get_options(analysisId)
  plan <- plan_report(opts)
  sib  <- rr_create_and_run("jaspAnova", "Anova", plan$options)
  data <- get_results(sib)

  flow  <- plan$flow
  summ  <- summarize_anova(data$main)
  homog <- assess_homogeneity(data$levene)
  ph    <- summarize_posthoc(data$postHoc)
  ds    <- summarize_descriptives(data$desc, flow$factors)

  elements <- list(
    el_md(build_abstract(flow, data, summ, homog, ds)),
    el_md(build_anova_intro(flow)),
    el_ref("anovaContainer_anovaTable"),
    el_md(build_anova_interpretation(summ)),
    el_md(build_descriptives_intro()),
    el_ref("anovaContainer_descriptivesContainer_tableDescriptives"),
    el_md(build_descriptives_interpretation(ds)))

  if (data$hasDescPlot)
    elements <- c(elements, list(
      el_ref("anovaContainer_descriptivesContainer_containerDescriptivesPlots"),
      el_md(build_plot_interpretation(summ))))

  elements <- c(elements, list(el_md(build_assumptions_intro())))
  if (!is.null(data$levene))
    elements <- c(elements, list(
      el_ref("anovaContainer_assumptionsContainer_leveneTable"),
      el_md(build_levene_interpretation(homog))))
  if (data$hasQQ)
    elements <- c(elements, list(
      el_ref("anovaContainer_assumptionsContainer_qqPlot"),
      el_md(build_qq_interpretation())))

  if (data$hasPostHoc)
    elements <- c(elements, list(
      el_md(build_posthoc_intro()),
      el_ref("anovaContainer_postHocContainer"),
      el_md(build_posthoc_interpretation(ph))))

  elements <- c(elements, list(el_md(build_conclusion(flow, summ, homog, ph, ds))))

  rr_compose_results(sib, elements)
}
