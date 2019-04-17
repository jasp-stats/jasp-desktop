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
  
  #jaspResults$title <- "Mediation Analysis<br/><span style='color:#888888;font-family:monospace;font-size:12px;font-weight:normal;'>Powered by lavaan.org</span>"
  jaspResults$addCitation("Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36. URL http://www.jstatsoft.org/v48/i02/")
  
  # Read dataset
  dataset <- .medReadData(dataset, options)
  
  .medComputeResults(jaspResults, dataset, options)
  .medParTable(jaspResults, dataset, options)
  
  if (length(options$dependent) > 0 && length(options$mediators) > 0 && length(options$predictor) > 0) {
    
    if (options$pathplot) {
      jaspResults[["plot"]] <- createJaspPlot(.medPathPlot(jaspResults[["stateMedResult"]]$object, options), title = "Path plot", width = 600, height = 400)
    }
    if (options$showSyntax) {
      jaspResults[["syntax"]] <- createJaspHtml(.medToLavMod(options, FALSE), class = "jasp-code", title = "Model")
    }
  }
}

.medReadData <- function(dataset, options) {
  if (!is.null(dataset)) return(dataset)
  
  vars <- c(options$predictor, options$mediators, options$dependent)
  return(.readDataSetToEnd(columns = vars))
}

.medComputeResults <- function(jaspResults, dataset, options) {
  medResult <- try(lavaan::sem(
    model           = .medToLavMod(options),
    data            = dataset,
    meanstructure   = options$includemeanstructure,
    se              = "robust",
    mimic           = options$mimic,
    estimator       = options$estimator
  ))
  
  if (inherits(medResult, "try-error")) JASP:::.quitAnalysis("Estimation failed")
  jaspResults[["stateMedResult"]] <- createJaspState(medResult)
}

.medToLavMod <- function(options, base64 = TRUE) {
  
  if (!base64) .v <- I
  
  n_pred <- length(options$predictor)
  n_medi <- length(options$mediators)
  n_deps <- length(options$dependent)
  
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
  
  res_part <- NULL
  if (n_deps > 1) {
    res_part <- "# residual covariance"
    idx_mat  <- which(upper.tri(diag(n_deps)), arr.ind = TRUE)
    for (i in 1:nrow(idx_mat)) {
      v1 <- .v(options$dependent[idx_mat[i,1]])
      v2 <- .v(options$dependent[idx_mat[i,2]])
      res_part <- paste0(res_part, "\n", v1, " ~~ ", v2)
    }
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
  
  return(paste(dep_part, med_part, res_part, dec_part, sep = "\n\n"))
}

.medParTable <- function(jaspResults, dataset, options) {
  pe <- lavaan::parameterEstimates(jaspResults[["stateMedResult"]]$object, boot.ci.type = "bca.simple")
  jaspResults[["parest"]] <- pecont <- createJaspContainer("Parameter estimates")
  
  ## direct effects
  pecont[["dir"]] <- dirtab <- createJaspTable(title = "Direct effects")
  
  dirtab$addColumnInfo(name = "lhs",      title = "",           type = "string", combine = TRUE)
  dirtab$addColumnInfo(name = "op",       title = "",           type = "string")
  dirtab$addColumnInfo(name = "rhs",      title = "",           type = "string")
  dirtab$addColumnInfo(name = "est",      title = "Estimate",   type = "number", format = "sf:4;dp:3")
  dirtab$addColumnInfo(name = "se",       title = "Std. Error", type = "number", format = "sf:4;dp:3")
  dirtab$addColumnInfo(name = "z",        title = "z-value",    type = "number", format = "sf:4;dp:3")
  dirtab$addColumnInfo(name = "pvalue",   title = "p",          type = "number", format = "dp:3;p:.001")
  dirtab$addColumnInfo(name = "ci.lower", title = "Lower",      type = "number", format = "sf:4;dp:3",
                       overtitle = "Confidence Interval")
  dirtab$addColumnInfo(name = "ci.upper", title = "Upper",      type = "number", format = "sf:4;dp:3",
                       overtitle = "Confidence Interval")
  
  pe_dir <- pe[substr(pe$label, 1, 1) == "c", ]
  dirtab[["lhs"]]      <- .unv(pe_dir$rhs)
  dirtab[["op"]]       <- rep("\u2B62", nrow(pe_dir))
  dirtab[["rhs"]]      <- .unv(pe_dir$lhs)
  dirtab[["est"]]      <- pe_dir$est
  dirtab[["se"]]       <- pe_dir$se
  dirtab[["z"]]        <- pe_dir$z
  dirtab[["pvalue"]]   <- pe_dir$pvalue
  dirtab[["ci.lower"]] <- pe_dir$ci.lower
  dirtab[["ci.upper"]] <- pe_dir$ci.upper
  
  ## indirect effects
  pecont[["ind"]] <- indtab <- createJaspTable(title = "Indirect effects")
  indtab$addColumnInfo(name = "x",        title = "",           type = "string", combine = TRUE)
  indtab$addColumnInfo(name = "op1",      title = "",           type = "string")
  indtab$addColumnInfo(name = "m",        title = "",           type = "string")
  indtab$addColumnInfo(name = "op2",      title = "",           type = "string")
  indtab$addColumnInfo(name = "y",        title = "",           type = "string")
  indtab$addColumnInfo(name = "est",      title = "Estimate",   type = "number", format = "sf:4;dp:3")
  indtab$addColumnInfo(name = "se",       title = "Std. Error", type = "number", format = "sf:4;dp:3")
  indtab$addColumnInfo(name = "z",        title = "z-value",    type = "number", format = "sf:4;dp:3")
  indtab$addColumnInfo(name = "pvalue",   title = "p",          type = "number", format = "dp:3;p:.001")
  indtab$addColumnInfo(name = "ci.lower", title = "Lower",      type = "number", format = "sf:4;dp:3",
                       overtitle = "Confidence Interval")
  indtab$addColumnInfo(name = "ci.upper", title = "Upper",      type = "number", format = "sf:4;dp:3",
                       overtitle = "Confidence Interval")
  
  pe_ind <- pe[pe$op == ":=" & vapply(gregexpr("_", pe$lhs), length, 1) == 3, ]
  
  indtab[["x"]]        <- .unv(rep(options$predictor, each = length(options$mediators) * length(options$dependent)))
  indtab[["op1"]]      <- rep("\u2B62", nrow(pe_ind))
  indtab[["m"]]        <- .unv(rep(options$mediators, length(options$dependent) * length(options$predictor)))
  indtab[["op2"]]      <- rep("\u2B62", nrow(pe_ind))
  indtab[["y"]]        <- .unv(rep(rep(options$dependent, each = length(options$mediators)), length(options$predictor)))
  indtab[["est"]]      <- pe_ind$est
  indtab[["se"]]       <- pe_ind$se
  indtab[["z"]]        <- pe_ind$z
  indtab[["pvalue"]]   <- pe_ind$pvalue
  indtab[["ci.lower"]] <- pe_ind$ci.lower
  indtab[["ci.upper"]] <- pe_ind$ci.upper
  
  ## total indirect effects
  if (length(options$mediators) > 1) {
    pecont[["tti"]] <- ttitab <- createJaspTable(title = "Total indirect effects")
    ttitab$addColumnInfo(name = "lhs",      title = "",           type = "string", combine = TRUE)
    ttitab$addColumnInfo(name = "op",       title = "",           type = "string")
    ttitab$addColumnInfo(name = "rhs",      title = "",           type = "string")
    ttitab$addColumnInfo(name = "est",      title = "Estimate",   type = "number", format = "sf:4;dp:3")
    ttitab$addColumnInfo(name = "se",       title = "Std. Error", type = "number", format = "sf:4;dp:3")
    ttitab$addColumnInfo(name = "z",        title = "z-value",    type = "number", format = "sf:4;dp:3")
    ttitab$addColumnInfo(name = "pvalue",   title = "p",          type = "number", format = "dp:3;p:.001")
    ttitab$addColumnInfo(name = "ci.lower", title = "Lower",      type = "number", format = "sf:4;dp:3",
                         overtitle = "Confidence Interval")
    ttitab$addColumnInfo(name = "ci.upper", title = "Upper",      type = "number", format = "sf:4;dp:3",
                         overtitle = "Confidence Interval")
    
    pe_tti <- pe[pe$op == ":=" & substr(pe$lhs, 1, 3) == "ind" & vapply(gregexpr("_", pe$lhs), length, 1) == 2,]
    
    ttitab[["lhs"]]      <- .unv(rep(options$predictor, each = length(options$dependent)))
    ttitab[["op "]]      <- rep("\u2B62", nrow(pe_tti))
    ttitab[["rhs"]]      <- .unv(rep(options$dependent, length(options$predictor)))
    ttitab[["est"]]      <- pe_tti$est
    ttitab[["se"]]       <- pe_tti$se
    ttitab[["z"]]        <- pe_tti$z
    ttitab[["pvalue"]]   <- pe_tti$pvalue
    ttitab[["ci.lower"]] <- pe_tti$ci.lower
    ttitab[["ci.upper"]] <- pe_tti$ci.upper
  }
  
  ## total effects
  pecont[["tot"]] <- tottab <- createJaspTable(title = "Total effects")
  
}

.medPathPlot <- function(lavmod, options) {
  
  # compute the layout from the options
  n_pred <- length(options$predictor)
  n_medi <- length(options$mediators)
  n_deps <- length(options$dependent)
  pred_l <- cbind(rep(-1, n_pred), .medPathLayout(n_pred))
  deps_l <- cbind(rep(1,  n_deps), .medPathLayout(n_deps))
  if (n_medi == 1) {
    medi_l <- cbind(0, .25)
  } else {
    medi_l <- cbind(rep(0,  n_medi), .medPathLayout(n_medi))
  }
  
  # creat a qgraph object using semplot
  png()
  pp <- semPlot::semPaths(
    object         = lavmod, 
    layout         = rbind(deps_l, medi_l, pred_l), 
    reorder        = FALSE,
    whatLabels     = ifelse(options$plotpars, "par", "name"),
    edge.color     = "black",
    color          = list(lat = "#EAEAEA", man = "#EAEAEA", int = "#FFFFFF"),
    border.width   = 1.5,
    edge.label.cex = 0.9,
    lty            = 2,
    title          = FALSE)
  dev.off()
  return(pp)
}

.medPathLayout <- function(n, min = -1, max = 1) {
  ss <- seq(min, max, length.out = n + 2)
  return(rev(ss[-c(1, n + 2)]))
}
