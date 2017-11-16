context("ANCOVA")

# does not test
# - descriptives table/plot & Q-Q plot (uses same code as ANOVA)
# - if analysis handles too few observations

test_that("Main table results match", {
  options <- JASPTools::analysisOptions("Ancova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facFive"
  options$covariates <- "contGamma"
  options$wlsWeights <- "facFifty"
  options$modelTerms <- list(
    list(components="facFive"),
    list(components="contGamma")
  )
  options$effectSizeEstimates <- TRUE
  options$effectSizeEtaSquared <- TRUE
  options$effectSizeOmegaSquared <- TRUE
  options$effectSizePartialEtaSquared <- TRUE
  options$VovkSellkeMPR <- TRUE

  refTables <- list(
    type1 = list("facFive", 181.151987151139, 4, 45.2879967877848, 1.86433860843651,
                 0.123166554163148, "TRUE", 0.0733818170125722, 0.0735023545150279,
                 0.0336895280828883, 1.42623435171942, "contGamma", 4.04832694150032,
                 1, 4.04832694150032, 0.166654582934621, 0.684030683889986, "TRUE",
                 0.00163991348646033, 0.00176978340871019, 0, 1, "Residual",
                 2283.42194856002, 94, 24.2917228570215, "", "", "TRUE", "",
                 "", "", ""),
    type2 = list("facFive", 185.196464516179, 4, 46.2991161290447, 1.90596263597919,
                 0.115857414147638, "TRUE", 0.0748974625326393, 0.0750202880830827,
                 0.0352547206519188, 1.47317659150752, "contGamma", 4.04832694150036,
                 1, 4.04832694150036, 0.166654582934623, 0.684030683889984, "TRUE",
                 0.00163723112216545, 0.00176978340871021, 0, 1, "Residual",
                 2283.42194856002, 94, 24.2917228570215, "", "", "TRUE", "",
                 "", "", ""),
    type3 = list("facFive", 185.196464516179, 4, 46.2991161290447, 1.9059626359792,
                 0.115857414147638, "TRUE", 0.0748974625326393, 0.0750202880830827,
                 0.0352547206519188, 1.47317659150752, "contGamma", 4.04832694150036,
                 1, 4.04832694150036, 0.166654582934621, 0.684030683889986, "TRUE",
                 0.00163723112216545, 0.00176978340871021, 0, 1, "Residual",
                 2283.42194856002, 94, 24.2917228570215, "", "", "TRUE", "",
                 "", "", "")
  )

  for (type in c("type1", "type2", "type3")) {
    options$sumOfSquares <- type
    results <- JASPTools::run("Ancova", "debug.csv", options, view=FALSE, quiet=TRUE)
    table <- results[["results"]][["anova"]][["data"]]
    expect_equal_tables(table, refTables[[type]], label=paste("Table with SS", type))
  }
})

test_that("Homogeneity of Variances table results match", {
  options <- JASPTools::analysisOptions("Ancova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facExperim"
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components="facExperim"),
    list(components="contGamma")
  )
  options$homogeneityTests <- TRUE
  options$VovkSellkeMPR <- TRUE
  results <- JASPTools::run("Ancova", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["assumptionsObj"]][["levene"]][["data"]]
  expect_equal_tables(table, list(2.72159218177061, 1, 98, 0.102201011380302, 1.57819444559362, 1))
})

test_that("Contrasts table results match", {
  options <- JASPTools::analysisOptions("Ancova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facFive"
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components="facFive"),
    list(components="contGamma")
  )

  refTables <- list(
    deviation = list("2 - 1, 2, 3, 4, 5", 0.200720248979633, 0.213572066533122, 0.939824445386935,
                     0.349716429527487, "TRUE", "3 - 1, 2, 3, 4, 5", -0.326355030521638,
                     0.213419672724688, -1.52917032603005, 0.129580520593875, "FALSE",
                     "4 - 1, 2, 3, 4, 5", 0.163006179525351, 0.213771994762986, 0.762523546202018,
                     0.447656831363019, "FALSE", "5 - 1, 2, 3, 4, 5", -0.173968024641619,
                     0.214943819683978, -0.809365093155023, 0.42034917204134, "FALSE"),
    simple = list("2 - 1", -0.0128247244642718, 0.0686555288850329, -0.186798130792168,
                  0.852221484436515, "TRUE", "3 - 1", 0.0925903314359824, 0.0685899078052411,
                  1.34991188060625, 0.180285987179756, "FALSE", "4 - 1", -0.00528191057341539,
                  0.067423619795445, -0.0783391723766844, 0.937724778659557, "FALSE",
                  "5 - 1", 0.0621129302599787, 0.0691441627901286, 0.898310540667162,
                  0.371315247360673, "FALSE"),
    difference = list("2 - 1", -0.0320618111606796, 0.171638822212583, -0.186798130792167,
                      0.852221484436515, "TRUE", "3 - 1, 2", 0.165004489446864, 0.0976530401157261,
                      1.68970151109808, 0.0943992176806134, "FALSE", "4 - 1, 2, 3",
                      -0.0398380577883153, 0.0687792108945597, -0.579216557883865,
                      0.563828556165239, "FALSE", "5 - 1, 2, 3, 4", 0.0434920061604049,
                      0.0537359549209945, 0.809365093155025, 0.42034917204134, "FALSE"),
    Helmert = list("1 - 2, 3, 4, 5", -0.170745783322843, 0.272701151705157, -0.626127840880747,
                   0.532748563022963, "TRUE", "2 - 3, 4, 5", -0.313159207525603,
                   0.274645898379152, -1.14022896163293, 0.257087871753455, "FALSE",
                   "3 - 4, 5", 0.320874107963505, 0.291421643235482, 1.10106478160314,
                   0.273679968781506, "FALSE", "4 - 5", -0.336974204166971, 0.340503320254199,
                   -0.98963558979515, 0.324892868709605, "FALSE"),
    repeated = list("1 - 2", -0.136596626658275, 0.218160921364126, -0.626127840880748,
                    0.532748563022963, "TRUE", "2 - 3", -0.337316875637908, 0.261868076071243,
                    -1.28811759225717, 0.200866880591653, "FALSE", "3 - 4", -0.0109618451162697,
                    0.260492056743455, -0.0420813027979022, 0.966523141496909, "FALSE",
                    "4 - 5", -0.17396802464162, 0.214943819683978, -0.809365093155028,
                    0.420349172041338, "FALSE"),
    polynomial = list("linear", 0.208344567699659, 0.240588115184784, 0.865980298069335,
                      0.388705894709638, "TRUE", "quadratic", -0.0572582720504826,
                      0.238398138250912, -0.240179191291413, 0.810714351679878, "FALSE",
                      "cubic", 0.0743566940225005, 0.243521087108506, 0.305339857444745,
                      0.760782433052004, "FALSE", "quartic", 0.412402551299919, 0.237982457186854,
                      1.73291156068751, 0.0863908131438485, "FALSE")
  )

  contrasts <- c("deviation", "simple", "difference", "Helmert", "repeated", "polynomial")
  for (contrast in contrasts) {
    options$contrasts <- list(list(contrast=contrast, variable="facFive"))
    results <- JASPTools::run("Ancova", "debug.csv", options, view=FALSE, quiet=TRUE)
    table <- results[["results"]][["contrasts"]][["collection"]][[1]][["data"]]
    expect_equal_tables(table, refTables[[contrast]], label=paste("Table with contrast", contrast))
  }
})

test_that("Post Hoc table results match", {
  options <- JASPTools::analysisOptions("Ancova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facExperim"
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components="facExperim"),
    list(components="contGamma")
  )
  options$postHocTestEffectSize <- TRUE
  options$postHocTestsBonferroni <- TRUE
  options$postHocTestsHolm <- TRUE
  options$postHocTestsScheffe <- TRUE
  options$postHocTestsTukey <- TRUE
  options$postHocTestsVariables <- "facExperim"
  results <- JASPTools::run("Ancova", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["posthoc"]][["collection"]][[1]][["data"]]
  expect_equal_tables(table,
    list("control", "experimental", -0.0830902357515323, 0.21391801479091,
         -0.388420937024623, -0.0388420937024623, 0.698555762823947,
         0.927393971055831, 0.698555762823947, 0.698555762823947, "TRUE")
  )
})

test_that("Marginal Means table results match", {
  options <- JASPTools::analysisOptions("Ancova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facExperim"
  options$covariates <- "contGamma"
  options$modelTerms <- list(
    list(components="facExperim"),
    list(components="contGamma")
  )
  options$marginalMeansCompareMainEffects <- TRUE
  options$marginalMeansTerms <- "facExperim"

  refTables <- list(
    none = list(1, -0.230293705415766, 0.151049119466849, -0.530084395048618,
                0.0694969842170856, -1.52462792387419, 0.13060580966841, 1,
                2, -0.147203469664234, 0.151049119466849, -0.446994159297086,
                0.152587219968618, -0.974540402379113, 0.332212375969363, 0),
    Bonferroni = list(1, -0.230293705415766, 0.151049119466849, -0.530084395048618,
                      0.0694969842170856, -1.52462792387419, 0.13060580966841, 1,
                      2, -0.147203469664234, 0.151049119466849, -0.446994159297086,
                      0.152587219968618, -0.974540402379113, 0.332212375969363, 0),
    Sidak = list(1, -0.230293705415766, 0.151049119466849, -0.530084395048618,
                 0.0694969842170856, -1.52462792387419, 0.13060580966841, 1,
                 2, -0.147203469664234, 0.151049119466849, -0.446994159297086,
                 0.152587219968618, -0.974540402379113, 0.332212375969363, 0)
  )

  for (adjustment in c("none", "Bonferroni", "Sidak")) {
    options$marginalMeansCIAdjustment <- adjustment
    results <- JASPTools::run("Ancova", "debug.csv", options, view=FALSE, quiet=TRUE)
    table <- results[["results"]][["marginalMeans"]][["collection"]][[1]][["data"]]
    expect_equal_tables(table, refTables[[adjustment]], label=paste("Table with CI adjustment", adjustment))
  }
})

test_that("Analysis handles errors", {
  options <- JASPTools::analysisOptions("Ancova")
  options$dependent <- "debInf"
  options$fixedFactors <- "contBinom"
  options$modelTerms <- list(list(components="contBinom"))
  results <- JASPTools::run("Ancova", "debug.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["anova"]][["error"]][["errorType"]], "badData",
                   label="Inf dependent check")

 options <- JASPTools::analysisOptions("Ancova")
 options$dependent <- "contNormal"
 options$covariates <- "debInf"
 options$fixedFactors <- "contBinom"
 options$modelTerms <- list(list(components="contBinom"))
 results <- JASPTools::run("Ancova", "debug.csv", options, view=FALSE, quiet=TRUE)
 expect_identical(results[["results"]][["anova"]][["error"]][["errorType"]], "badData",
                  label="Inf covariate check")

  options$dependent <- "contNormal"
  options$covariates <- ""
  options$fixedFactors <- "contBinom"
  options$wlsWeights <- "debInf"
  options$modelTerms <- list(list(components="contBinom"))
  results <- JASPTools::run("Ancova", "debug.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["anova"]][["error"]][["errorType"]], "badData",
                  label="Inf WLS weights check")

  options$dependent <- "contNormal"
  options$fixedFactors <- "debSame"
  options$modelTerms <- list(list(components="debSame"))
  results <- JASPTools::run("Ancova", "debug.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["anova"]][["error"]][["errorType"]], "badData",
                  label="1-level factor check")

  options$dependent <- "debSame"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(list(components="facFive"))
  results <- JASPTools::run("Ancova", "debug.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["anova"]][["error"]][["errorType"]], "badData",
                  label="No variance check")

  options$dependent <- "contGamma"
  options$fixedFactors <- "facFive"
  options$wlsWeights <- "contNormal"
  options$modelTerms <- list(list(components="facFive"))
  results <- JASPTools::run("Ancova", "debug.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["anova"]][["error"]][["errorType"]], "badData",
                  label="Negative WLS weights check")
})
