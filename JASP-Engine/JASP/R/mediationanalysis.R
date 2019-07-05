#
# Copyright (C) 2013-2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

MediationAnalysis <- function(jaspResults, dataset, options, ...) {
  jaspResults$addCitation("Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/")

  # Read dataset
  dataset <- .medReadData(dataset, options)
  ready  <- .medCheckErrors(dataset, options)

  # Perform mediation analysis
  medResult <- .medComputeResults(jaspResults, dataset, options, ready)

  # Output functions
  .medParTable(jaspResults, medResult, options, ready)
  .medRsquared(jaspResults, medResult, options, ready)
  .medPathPlot(jaspResults, medResult, options, ready)
  .medSyntax(  jaspResults, medResult, options, ready)

}

# Preprocessing functions ----
.medReadData <- function(dataset, options) {
  if (!is.null(dataset)) return(dataset)

  vars <- c(options$predictor, options$mediators, options$dependent, options$confounds)
  return(.readDataSetToEnd(columns = vars))
}

.medCheckErrors <- function(dataset, options) {
  if (length(options$dependent) == 0 || length(options$mediators) == 0 || length(options$predictor) == 0) return(FALSE)
  
  # Exogenous variables can be binary or continuous
  exo <- ifelse(length(options$confounds) > 0, options$confounds, options$predictor)
  # Endogenous variables need to be scale or ordinal
  endo <- c(options$mediators, options$dependent)
  
  customChecks <- list(
    checkExogenous = function() {
      admissible <- vapply(exo, function(exo_var) {
        var <- na.omit(dataset[[.v(exo_var)]])
        if ((is.character(var) || is.factor(var)) && length(unique(var)) != 2) {
          return(FALSE)
        }
        if (is.ordered(var)) return(FALSE)
        return(TRUE)
      }, TRUE)
      if (!all(admissible))
        paste("Not all exogenous variables are admissible.",
              "Inadmissible exogenous variables:",
              paste(exo[!admissible], collapse = ","),
              ". Only binary or continuous exogenous variables allowed.")
    },
    
    checkEndogenous = function() {
      if (length(options$confounds) > 0) endo <- c(endo, options$predictor)
      admissible <- vapply(endo, function(endo_var) {
        var <- na.omit(dataset[[.v(endo_var)]])
        if (!(is.ordered(var) || is.numeric(var))) {
          return(FALSE)
        }
        return(TRUE)
      }, TRUE)
      if (!all(admissible))
        paste("Not all endogenous variables are admissible.",
              "Inadmissible endogenous variables:",
              paste(endo[!admissible], collapse = ","),
              ". Only scale or ordinal endogenous variables allowed.")
    }
    
  )
  
  .hasErrors(dataset, type = c('observations', 'variance', 'infinity'), custom = customChecks, 
             all.target = c(endo, exo), observations.amount = '< 2', exitAnalysisIfErrors = TRUE)
  
  return(TRUE)
}

# Results functions ----
.medComputeResults <- function(jaspResults, dataset, options, ready) {
  if (!ready) return()
  if (!is.null(jaspResults[["stateMedResult"]])) return(jaspResults[["stateMedResult"]]$object)
  
  medResult <- try(lavaan::sem(
    model           = .medToLavMod(options),
    data            = dataset,
    se              = ifelse(options$se == "bootstrap", "standard", options$se),
    mimic           = options$mimic,
    estimator       = options$estimator,
    std.ov          = options$std
  ))
  
  if (inherits(medResult, "try-error")) {
    errmsg <- paste("Estimation failed\nMessage:\n", attr(medResult, "condition")$message)
    .quitAnalysis(.decodeVarsInMessage(names(dataset), errmsg))
  }

  if (options$se == "bootstrap") {
    jaspResults$startProgressbar(options$bootstrapNumber)
    
    boot_1      <- lavaan::bootstrapLavaan(medResult, R = 1)
    bootres     <- matrix(0, options$bootstrapNumber, length(boot_1))
    bootres[1,] <- boot_1
    i <- 2L
    while (i < options$bootstrapNumber) {
      boot_i      <- lavaan::bootstrapLavaan(medResult, 1)
      if (length(boot_i) == 0) next # try again upon failure
      bootres[i,] <- boot_i
      jaspResults$progressbarTick()
      i <- i + 1L
    }
    
    medResult@boot       <- list(coef = bootres)
    medResult@Options$se <- "bootstrap"
  }
  
  jaspResults[["stateMedResult"]] <- createJaspState(medResult)
  jaspResults[["stateMedResult"]]$dependOn(c(
    "predictor", "mediators", "dependent", "confounds", "includemeanstructure", 
    "bootstrapNumber", "fixManifestInterceptsToZero", "mimic", "se", "estimator", 
    "std"))
  return(medResult)
}

.medToLavMod <- function(options, base64 = TRUE) {

  if (!base64) .v <- I

  n_pred <- length(options$predictor)
  n_medi <- length(options$mediators)
  n_deps <- length(options$dependent)
  n_conf <- length(options$confounds)

  title    <- "
  # ---------------------------------
  # Mediation model generated by JASP
  # ---------------------------------\n
  "
  dep_part <- "# dependent regression"
  for (d in 1:n_deps) {
    dep_part <- paste0(dep_part, "\n", .v(options$dependent[d]), " ~")
    for (m in 1:n_medi) {
      par_name <- paste0(" b", d, m)
      dep_part <- paste0(dep_part, par_name, "*", .v(options$mediators[m]), " +")
    }
    for (p in 1:n_pred) {
      par_name <- paste0(" c", d, p)
      dep_part <- paste0(dep_part, par_name, "*", .v(options$predictor[p]))
      if (p != n_pred)
        dep_part <- paste0(dep_part, " +")
    }
  }
  dep_part <- paste0(dep_part, "\n\n")

  med_part <- "# mediator regression"
  for (m in 1:n_medi) {
    med_part <- paste0(med_part, "\n", .v(options$mediators[m]), " ~")
    for (p in 1:n_pred) {
      par_name <- paste0(" a", m, p)
      med_part <- paste0(med_part, par_name, "*", .v(options$predictor[p]))
      if (p != n_pred)
        med_part <- paste0(med_part, " +")
    }
  }
  med_part <- paste0(med_part, "\n\n")

  conf_part     <- NULL
  pred_res_part <- NULL
  if (n_conf > 0) {
    conf_part <- "# confounder adjustment"
    for (var in c(options$predictor, options$mediators, options$dependent)) {
      conf_part <- paste0(conf_part, "\n", .v(var), " ~ ", paste(.v(options$confounds), collapse = " + "))
    }
    conf_part <- paste0(conf_part, "\n\n")
    if (n_pred > 1) {
      pred_res_part <- "# predictor residual covariance"
      idx_mat  <- which(upper.tri(diag(n_pred)), arr.ind = TRUE)
      for (i in 1:nrow(idx_mat)) {
        v1 <- .v(options$predictor[idx_mat[i,1]])
        v2 <- .v(options$predictor[idx_mat[i,2]])
        pred_res_part <- paste0(pred_res_part, "\n", v1, " ~~ ", v2)
      }
      pred_res_part <- paste0(pred_res_part, "\n\n")
    }
  }

  med_res_part <- NULL
  if (n_medi > 1) {
    med_res_part <- "# mediator residual covariance"
    idx_mat  <- which(upper.tri(diag(n_medi)), arr.ind = TRUE)
    for (i in 1:nrow(idx_mat)) {
      v1 <- .v(options$mediators[idx_mat[i,1]])
      v2 <- .v(options$mediators[idx_mat[i,2]])
      med_res_part <- paste0(med_res_part, "\n", v1, " ~~ ", v2)
    }
    med_res_part <- paste0(med_res_part, "\n\n")
  }

  res_part <- NULL
  if (n_deps > 1) {
    res_part <- "# dependent residual covariance"
    idx_mat  <- which(upper.tri(diag(n_deps)), arr.ind = TRUE)
    for (i in 1:nrow(idx_mat)) {
      v1 <- .v(options$dependent[idx_mat[i,1]])
      v2 <- .v(options$dependent[idx_mat[i,2]])
      res_part <- paste0(res_part, "\n", v1, " ~~ ", v2)
    }
    res_part <- paste0(res_part, "\n\n")
  }


  dec_part <- "# effect decomposition"
  for (d in 1:n_deps) {
    for (p in 1:n_pred) {
      dec_part <- paste0(dec_part, "\n# y", d, " ~ x", p)
      parnames <- c()
      for (m in 1:n_medi) {
        par_name <- paste0("ind_x", p, "_m", m, "_y", d)
        parnames <- c(parnames, par_name)
        a_name   <- paste0("a", m, p)
        b_name   <- paste0("b", d, m)
        dec_part <- paste0(dec_part, "\n", par_name, " := ", a_name, "*", b_name)
      }
      dec_part <- paste0(dec_part, "\nind_x", p, "_y", d, "    := ", paste(parnames, collapse = " + "))
      dec_part <- paste0(dec_part, "\ntot_x", p, "_y", d, "    := ", "ind_x", p, "_y", d, " + c", d, p, "\n")
    }
  }

  return(paste0(title, dep_part, med_part, conf_part,
                pred_res_part, med_res_part, res_part,
                dec_part))
}

# Output functions ----

.medParTable <- function(jaspResults, medResult, options, ready) {
  showOpts <- c("showdir", "showind", "showtotind", "showtot", "showres")
  if (!any(options[showOpts])) return()
  
  jaspResults[["parest"]] <- pecont <- createJaspContainer("Parameter estimates")
  pecont$dependOn(options = c("ciWidth", "bootCItype"), optionsFromObject = jaspResults[["stateMedResult"]])
  pecont$position <- 0

  if (is.null(medResult)) {
    pecont[["dir"]] <- dirtab <- createJaspTable(title = "Direct effects")
    pecont[["ind"]] <- indtab <- createJaspTable(title = "Indirect effects")
    pecont[["tot"]] <- tottab <- createJaspTable(title = "Total effects")

    dirtab$addColumnInfo(name = "est",    title = "Estimate",   type = "number")
    dirtab$addColumnInfo(name = "se",     title = "Std. Error", type = "number")
    dirtab$addColumnInfo(name = "z",      title = "z-value",    type = "number")
    dirtab$addColumnInfo(name = "pvalue", title = "p",          type = "number")

    indtab$addColumnInfo(name = "est",    title = "Estimate",   type = "number")
    indtab$addColumnInfo(name = "se",     title = "Std. Error", type = "number")
    indtab$addColumnInfo(name = "z",      title = "z-value",    type = "number")
    indtab$addColumnInfo(name = "pvalue", title = "p",          type = "number")

    tottab$addColumnInfo(name = "est",    title = "Estimate",   type = "number")
    tottab$addColumnInfo(name = "se",     title = "Std. Error", type = "number")
    tottab$addColumnInfo(name = "z",      title = "z-value",    type = "number")
    tottab$addColumnInfo(name = "pvalue", title = "p",          type = "number")

    return()
  }

  pe <- lavaan::parameterEstimates(medResult, boot.ci.type = options$bootCItype, 
                                   level = options$ciWidth)
  
  se_type <- switch(options$se,
    "bootstrap" = "Delta method",
    "standard"  = "Delta method",
    "robust"    = "Robust"
  )
  ci_type <- switch(options$se,
    "bootstrap" = switch(options$bootCItype, 
      "perc"       = "percentile bootstrap", 
      "norm"       = "normal theory bootstrap", 
      "bca.simple" = "bias-corrected percentile bootstrap"
    ),
    "standard"  = "normal theory",
    "robust"    = "robust"
  )
  se_message <- paste(se_type, "standard errors,", ci_type, "confidence intervals")
  
  ## direct effects
  if (options[["showdir"]]) {
    pecont[["dir"]] <- dirtab <- createJaspTable(title = "Direct effects")
    dirtab$dependOn("showdir")
    
    dirtab$addColumnInfo(name = "lhs",      title = "",           type = "string")
    dirtab$addColumnInfo(name = "op",       title = "",           type = "string")
    dirtab$addColumnInfo(name = "rhs",      title = "",           type = "string")
    dirtab$addColumnInfo(name = "est",      title = "Estimate",   type = "number", format = "sf:4;dp:3")
    dirtab$addColumnInfo(name = "se",       title = "Std. Error", type = "number", format = "sf:4;dp:3")
    dirtab$addColumnInfo(name = "z",        title = "z-value",    type = "number", format = "sf:4;dp:3")
    dirtab$addColumnInfo(name = "pvalue",   title = "p",          type = "number", format = "dp:3;p:.001")
    dirtab$addColumnInfo(name = "ci.lower", title = "Lower",      type = "number", format = "sf:4;dp:3",
                         overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))
    dirtab$addColumnInfo(name = "ci.upper", title = "Upper",      type = "number", format = "sf:4;dp:3",
                         overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))
    dirtab$addFootnote(se_message)
    
    pe_dir <- pe[substr(pe$label, 1, 1) == "c", ]
    dirtab[["lhs"]]      <- .unv(pe_dir$rhs)
    dirtab[["op"]]       <- rep("\u2192", nrow(pe_dir))
    dirtab[["rhs"]]      <- .unv(pe_dir$lhs)
    dirtab[["est"]]      <- pe_dir$est
    dirtab[["se"]]       <- pe_dir$se
    dirtab[["z"]]        <- pe_dir$z
    dirtab[["pvalue"]]   <- pe_dir$pvalue
    dirtab[["ci.lower"]] <- pe_dir$ci.lower
    dirtab[["ci.upper"]] <- pe_dir$ci.upper
  }
  

  ## indirect effects
  if (options[["showind"]]) {
    pecont[["ind"]] <- indtab <- createJaspTable(title = "Indirect effects")
    indtab$dependOn("showind")
    
    indtab$addColumnInfo(name = "x",        title = "",           type = "string")
    indtab$addColumnInfo(name = "op1",      title = "",           type = "string")
    indtab$addColumnInfo(name = "m",        title = "",           type = "string")
    indtab$addColumnInfo(name = "op2",      title = "",           type = "string")
    indtab$addColumnInfo(name = "y",        title = "",           type = "string")
    indtab$addColumnInfo(name = "est",      title = "Estimate",   type = "number", format = "sf:4;dp:3")
    indtab$addColumnInfo(name = "se",       title = "Std. Error", type = "number", format = "sf:4;dp:3")
    indtab$addColumnInfo(name = "z",        title = "z-value",    type = "number", format = "sf:4;dp:3")
    indtab$addColumnInfo(name = "pvalue",   title = "p",          type = "number", format = "dp:3;p:.001")
    indtab$addColumnInfo(name = "ci.lower", title = "Lower",      type = "number", format = "sf:4;dp:3",
                         overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))
    indtab$addColumnInfo(name = "ci.upper", title = "Upper",      type = "number", format = "sf:4;dp:3",
                         overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))
    indtab$addFootnote(se_message)
    
    pe_ind <- pe[pe$op == ":=" & vapply(gregexpr("_", pe$lhs), length, 1) == 3, ]
    
    indtab[["x"]]        <- .unv(rep(options$predictor, each = length(options$mediators) * length(options$dependent)))
    indtab[["op1"]]      <- rep("\u2192", nrow(pe_ind))
    indtab[["m"]]        <- .unv(rep(options$mediators, length(options$dependent) * length(options$predictor)))
    indtab[["op2"]]      <- rep("\u2192", nrow(pe_ind))
    indtab[["y"]]        <- .unv(rep(rep(options$dependent, each = length(options$mediators)), length(options$predictor)))
    indtab[["est"]]      <- pe_ind$est
    indtab[["se"]]       <- pe_ind$se
    indtab[["z"]]        <- pe_ind$z
    indtab[["pvalue"]]   <- pe_ind$pvalue
    indtab[["ci.lower"]] <- pe_ind$ci.lower
    indtab[["ci.upper"]] <- pe_ind$ci.upper
  }

  ## total indirect effects
  if (options[["showtotind"]] && length(options$mediators) > 1) {
    pecont[["tti"]] <- ttitab <- createJaspTable(title = "Total indirect effects")
    ttitab$dependOn("showtotind")
    
    ttitab$addColumnInfo(name = "lhs",      title = "",           type = "string")
    ttitab$addColumnInfo(name = "op",       title = "",           type = "string")
    ttitab$addColumnInfo(name = "rhs",      title = "",           type = "string")
    ttitab$addColumnInfo(name = "est",      title = "Estimate",   type = "number", format = "sf:4;dp:3")
    ttitab$addColumnInfo(name = "se",       title = "Std. Error", type = "number", format = "sf:4;dp:3")
    ttitab$addColumnInfo(name = "z",        title = "z-value",    type = "number", format = "sf:4;dp:3")
    ttitab$addColumnInfo(name = "pvalue",   title = "p",          type = "number", format = "dp:3;p:.001")
    ttitab$addColumnInfo(name = "ci.lower", title = "Lower",      type = "number", format = "sf:4;dp:3",
                         overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))
    ttitab$addColumnInfo(name = "ci.upper", title = "Upper",      type = "number", format = "sf:4;dp:3",
                         overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))
    ttitab$addFootnote(se_message)
    
    pe_tti <- pe[pe$op == ":=" & substr(pe$lhs, 1, 3) == "ind" & vapply(gregexpr("_", pe$lhs), length, 1) == 2,]

    ttitab[["lhs"]]      <- .unv(rep(options$predictor, each = length(options$dependent)))
    ttitab[["op"]]       <- rep("\u2192", nrow(pe_tti))
    ttitab[["rhs"]]      <- .unv(rep(options$dependent, length(options$predictor)))
    ttitab[["est"]]      <- pe_tti$est
    ttitab[["se"]]       <- pe_tti$se
    ttitab[["z"]]        <- pe_tti$z
    ttitab[["pvalue"]]   <- pe_tti$pvalue
    ttitab[["ci.lower"]] <- pe_tti$ci.lower
    ttitab[["ci.upper"]] <- pe_tti$ci.upper
  }

  if (options[["showtot"]]) {
    ## total effects
    pecont[["tot"]] <- tottab <- createJaspTable(title = "Total effects")
    tottab$dependOn("showtot")

    tottab$addColumnInfo(name = "lhs",      title = "",           type = "string")
    tottab$addColumnInfo(name = "op",       title = "",           type = "string")
    tottab$addColumnInfo(name = "rhs",      title = "",           type = "string")
    tottab$addColumnInfo(name = "est",      title = "Estimate",   type = "number", format = "sf:4;dp:3")
    tottab$addColumnInfo(name = "se",       title = "Std. Error", type = "number", format = "sf:4;dp:3")
    tottab$addColumnInfo(name = "z",        title = "z-value",    type = "number", format = "sf:4;dp:3")
    tottab$addColumnInfo(name = "pvalue",   title = "p",          type = "number", format = "dp:3;p:.001")
    tottab$addColumnInfo(name = "ci.lower", title = "Lower",      type = "number", format = "sf:4;dp:3",
                         overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))
    tottab$addColumnInfo(name = "ci.upper", title = "Upper",      type = "number", format = "sf:4;dp:3",
                         overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))
    tottab$addFootnote(se_message)
    pe_tot <- pe[pe$op == ":=" & substr(pe$lhs, 1, 3) == "tot",]
    
    tottab[["lhs"]]      <- .unv(rep(options$predictor, length(options$dependent)))
    tottab[["op"]]       <- rep("\u2192", nrow(pe_tot))
    tottab[["rhs"]]      <- .unv(rep(options$dependent, each = length(options$predictor)))
    tottab[["est"]]      <- pe_tot$est
    tottab[["se"]]       <- pe_tot$se
    tottab[["z"]]        <- pe_tot$z
    tottab[["pvalue"]]   <- pe_tot$pvalue
    tottab[["ci.lower"]] <- pe_tot$ci.lower
    tottab[["ci.upper"]] <- pe_tot$ci.upper
    
  }


  ## residual covariances
  if (options[["showres"]] && length(c(options$mediators, options$dependent)) > 2) {
    pecont[["res"]] <- restab <- createJaspTable(title = "Residual covariances")
    restab$addColumnInfo(name = "lhs",      title = "",           type = "string")
    restab$addColumnInfo(name = "op",       title = "",           type = "string")
    restab$addColumnInfo(name = "rhs",      title = "",           type = "string")
    restab$addColumnInfo(name = "est",      title = "Estimate",   type = "number", format = "sf:4;dp:3")
    restab$addColumnInfo(name = "se",       title = "Std. Error", type = "number", format = "sf:4;dp:3")
    restab$addColumnInfo(name = "z",        title = "z-value",    type = "number", format = "sf:4;dp:3")
    restab$addColumnInfo(name = "pvalue",   title = "p",          type = "number", format = "dp:3;p:.001")
    restab$addColumnInfo(name = "ci.lower", title = "Lower",      type = "number", format = "sf:4;dp:3",
                         overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))
    restab$addColumnInfo(name = "ci.upper", title = "Upper",      type = "number", format = "sf:4;dp:3",
                         overtitle = paste0(options$ciWidth * 100, "% Confidence Interval"))
    restab$addFootnote(se_message)
    pe_res <- pe[pe$op == "~~" &
                   pe$lhs != pe$rhs &
                   !.unv(pe$lhs) %in% options$predictor &
                   !.unv(pe$lhs) %in% options$confounds,]

    restab[["lhs"]]      <- .unv(pe_res$lhs)
    restab[["op"]]       <- rep("\u2194", nrow(pe_res))
    restab[["rhs"]]      <- .unv(pe_res$rhs)
    restab[["est"]]      <- pe_res$est
    restab[["se"]]       <- pe_res$se
    restab[["z"]]        <- pe_res$z
    restab[["pvalue"]]   <- pe_res$pvalue
    restab[["ci.lower"]] <- pe_res$ci.lower
    restab[["ci.upper"]] <- pe_res$ci.upper
  }

}

.medRsquared <- function(jaspResults, medResult, options, errors) {
  if (!options$rsquared || !is.null(jaspResults[["rsquared"]])) return()
  
  jaspResults[["rsquared"]] <- tabr2 <- createJaspTable("R-Squared")
  tabr2$addColumnInfo(name = "__var__", title = "", type = "string")
  tabr2$addColumnInfo(name = "rsq", title = "R\u00B2", type = "number", format = "sf:4;dp:3")
  tabr2$dependOn(options = "rsquared", optionsFromObject = jaspResults[["stateMedResult"]])
  tabr2$position <- 1
  if (is.null(medResult)) return()
  
  r2res              <- lavaan::inspect(medResult, "r2")
  tabr2[["__var__"]] <- .unv(names(r2res))
  tabr2[["rsq"]]     <- r2res
}

.medPathPlot <- function(jaspResults, medResult, options, ready) {
  if (!options$pathplot || !ready) return()

  # compute the layout from the options
  n_pred <- length(options$predictor)
  n_medi <- length(options$mediators)
  n_deps <- length(options$dependent)
  n_conf <- length(options$confounds)

  n_totl <- n_pred + n_medi + n_deps + n_conf
  pred_l <- cbind(rep(-1, n_pred), .medPathLayout(n_pred))
  deps_l <- cbind(rep(1,  n_deps), .medPathLayout(n_deps))
  if (n_medi == 1 && n_pred == 1 && n_deps == 1) {
    medi_l <- cbind(0, .25)
  } else {
    medi_l <- cbind(rep(0,  n_medi), .medPathLayout(n_medi))
  }
  if (n_conf > 0) {
    conf_l <- cbind(rep(-2, n_conf), .medPathLayout(n_conf) + 0.25)
  } else {
    conf_l <- NULL
  }
  # creat a qgraph object using semplot
  png()
  pp <- semPlot::semPaths(
    object         = .medLavToPlotObj(medResult),
    layout         = rbind(deps_l, medi_l, pred_l, conf_l),
    intercepts     = FALSE,
    reorder        = FALSE,
    whatLabels     = ifelse(options$plotpars, "par", "name"),
    edge.color     = "black",
    color          = list(lat = "#EAEAEA", man = "#EAEAEA", int = "#FFFFFF"),
    border.width   = 1.5,
    edge.label.cex = 0.9,
    lty            = 2,
    title          = FALSE,
    sizeMan        = round(8*exp(-n_totl/80)+1)
  )
  dev.off()
  
  # post-process plot
  pp <- .medPlotPostProcess(pp, options)

  jaspResults[["plot"]] <- createJaspPlot(pp, title = "Path plot", width = 600, height = 400)
  jaspResults[["plot"]]$dependOn(optionsFromObject = jaspResults[["stateMedResult"]], 
                                 options = c("pathplot", "plotpars"))
  jaspResults[["plot"]]$position <- 2
}

.medPathLayout <- function(n, min = -1, max = 1) {
  ss <- seq(min, max, length.out = n + 2)
  return(rev(ss[-c(1, n + 2)]))
}

.medLavToPlotObj <- function(lavResult, options) {
  # Create semplot model and unv the names of the manifest variables
  # Sorry, this code is really ugly but all it does is replace names for plot.
  semPlotMod <- semPlot::semPlotModel(list(lavResult), list(mplusStd = "std"))[[1]]

  manifests <- semPlotMod@Vars$name[semPlotMod@Vars$manifest]
  semPlotMod@Vars$name[semPlotMod@Vars$manifest] <- .unv(manifests)
  semPlotMod@Pars$lhs <- vapply(semPlotMod@Pars$lhs, function(v) ifelse(nchar(v) > 0, .unv(v), ""), "")
  semPlotMod@Pars$rhs <- vapply(semPlotMod@Pars$rhs, function(v) ifelse(nchar(v) > 0, .unv(v), ""), "")

  return(semPlotMod)
}

.medPlotPostProcess <- function(plt, options) {
  node_names <- names(plt$graphAttributes$Nodes$names)
  confounds_idx <- which(node_names %in% options$confounds)
  predictor_idx <- which(node_names %in% options$predictor)
  dependent_idx <- which(node_names %in% options$dependent)
  
  # remove focus from confounder edges
  confound_edges <- plt$Edgelist$from %in% confounds_idx
  plt$graphAttributes$Edges$labels[confound_edges] <- ""
  plt$graphAttributes$Edges$lty[confound_edges] <- 3
  plt$graphAttributes$Edges$color[confound_edges] <- "#888888FF"
  
  # place unidirectional edge labels at 1/3
  uni_edges <- !plt$Edgelist$bidirectional
  plt$graphAttributes$Edges$edge.label.position[uni_edges] <- 1/3
  
  return(plt)
}

.medSyntax <- function(jaspResults, medResult, options, ready) {
  if (!options$showSyntax || !ready) return()
  jaspResults[["syntax"]] <- createJaspHtml(.medToLavMod(options, FALSE), class = "jasp-code", title = "Model syntax")
  jaspResults[["syntax"]]$dependOn(optionsFromObject = jaspResults[["stateMedResult"]])
  jaspResults[["syntax"]]$position <- 3
}

