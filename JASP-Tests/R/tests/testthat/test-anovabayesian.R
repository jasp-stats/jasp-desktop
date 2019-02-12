context("Bayesian ANOVA")

# does not test
# - descriptives table (code is from regular ANOVA)
# - descriptives plot (code is from regular ANOVA)
# - bftype (01, 10)

initOpts <- function() {
  options <- jasptools::analysisOptions("AnovaBayesian")
  options$sampleMode <- "manual"
  options$fixedSamplesNumber <- 50
  return(options)
}

test_that("Main table results match", {
  set.seed(0)
  options <- initOpts()
  options$dependent <- "contNormal"
  options$fixedFactors <- c("facGender", "facFive")
  options$randomFactors <- "facExperim"
  options$priorFixedEffects <- 0.4
  options$priorRandomEffects <- 1.5
  options$modelTerms <- list(
    list(components="facGender", isNuisance=FALSE),
    list(components="facFive", isNuisance=FALSE),
    list(components="facExperim", isNuisance=TRUE),
    list(components=c("facGender", "facFive"), isNuisance=FALSE)
  )

  refTables <- list(
    nullModelTop = list("Null model (incl. facExperim)", 0.2, -0.594672751877366, 0.134816578924395,
                        0, "", "facGender", 0.2, -0.28722095052267, 0.630131621377202,
                        0.307451801354696, 13.7355911669398, "facFive", 0.2, -1.24424524718345,
                        -0.61670420719205, -0.649572495306089, 12.4382643482459, "facGender + facFive",
                        0.2, -0.997414136054835, -0.349308383227457, -0.40274138417747,
                        9.31843477413068, "facGender + facFive + facGender<unicode><unicode><unicode>facFive",
                        0.2, -1.14281197270266, -0.508311164396966, -0.548139220825293,
                        27.3699066120848),
    bestModelTop = list("facGender", 0.2, -0.224979999753508, 0.77036505277832, 0, "",
                        "Null model (incl. facExperim)", 0.2, -0.63697968879273, 0.0789763886292554,
                        -0.411999689039223, 20.3818205689136, "facGender + facFive",
                        0.2, -1.06635989051332, -0.42532677067774, -0.84137989075981,
                        21.4710504386678, "facFive", 0.2, -1.33455292282619, -0.711911214299153,
                        -1.10957292307269, 21.0528996183343, "facGender + facFive + facGender<unicode><unicode><unicode>facFive",
                        0.2, -1.3818577904349, -0.761385841073298, -1.1568777906814,
                        21.731493430306)
  )

  for (order in c("nullModelTop", "bestModelTop")) {
    options$bayesFactorOrder <- order
    results <- jasptools::run("AnovaBayesian", "test.csv", options)
    table <- results[["results"]][["model comparison"]][["data"]]
    expect_equal_tables(table, refTables[[order]], label=paste("Table with order", order))
  }
})

test_that("Effects table results match", {
  set.seed(0)
  options <- initOpts()
  options$dependent <- "contNormal"
  options$fixedFactors <- list("facFive", "contBinom")
  options$effects <- TRUE
  options$modelTerms <- list(
    list(components="facFive", isNuisance=FALSE),
    list(components="contBinom", isNuisance=FALSE),
    list(components=c("facFive", "contBinom"), isNuisance=FALSE)
  )

  refTables <- list(
    allModels = list("facFive", 0.6, 0.130329318915725, 0.0999070274533764, "contBinom",
                     0.6, 0.215198578129195, 0.182805120830189, "facFive<unicode><unicode><unicode>contBinom",
                     0.2, 0.00408798728805838, 0.0164190701020926),
    matchedModels = list("facFive", 0.4, 0.125726268310478, 0.144492921488286, "contBinom",
                         0.4, 0.210639434646256, 0.26825960145967, "facFive<unicode><unicode><unicode>contBinom",
                         0.2, 0.0041531263651184, 0.182973144016417)
  )

  for (effectsType in c("allModels", "matchedModels")) {
    options$effectsType <- effectsType
    results <- jasptools::run("AnovaBayesian", "test.csv", options)
    table <- results[["results"]][["effects"]][["data"]]
    expect_equal_tables(table, refTables[[effectsType]], label=paste("Table with effects type", effectsType))
  }
})

test_that("Post-hoc Comparisons table results match", {
  options <- jasptools::analysisOptions("AnovaBayesian")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(
    list(components="facFive", isNuisance=FALSE)
  )
  options$postHocTestsNullControl <- TRUE
  options$postHocTestsVariables <- "facFive"
  results <- jasptools::run("AnovaBayesian", "test.csv", options)
  table <- results[["results"]][["posthoc"]][["collection"]][[1]][["data"]]
  expect_equal_tables(table,
    list(1, 2, 0.319507910772894, 0.0997312866070229, -0.505650193633655,
         0.00713501139221108, "TRUE", 1, 3, 0.319507910772894, 0.260338331517325,
         -0.0889434982573415, 0.00346528957238797, "FALSE", 1, 4, 0.319507910772894,
         0.0988240290518637, -0.509619059344969, 0.0071287698725277,
         "FALSE", 1, 5, 0.319507910772894, 0.139031227413715, -0.361369258553702,
         0.00848479926592481, "FALSE", 2, 3, 0.319507910772894, 0.30046754229221,
         -0.0266840507861397, 0.00368467308944596, "FALSE", 2, 4, 0.319507910772894,
         0.0989443005428758, -0.509090832692873, 0.00712946511175494,
         "FALSE", 2, 5, 0.319507910772894, 0.151606948038875, -0.323762510268505,
         0.00859149880568525, "FALSE", 3, 4, 0.319507910772894, 0.23744955199206,
         -0.12891026091494, 0.0042533257083258, "FALSE", 3, 5, 0.319507910772894,
         0.104702418542221, -0.484524901778361, 0.00721167204576887,
         "FALSE", 4, 5, 0.319507910772894, 0.138002738227672, -0.364593911742112,
         0.00846064110496598, "FALSE")
  )
})

test_that("Analysis handles errors", {
  options <- initOpts()
  options$dependent <- "debInf"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(list(components="facFive", isNuisance=FALSE))
  results <- jasptools::run("AnovaBayesian", "test.csv", options)
  expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
                   label="Inf check")

  options$dependent <- "contNormal"
  options$fixedFactors <- "debSame"
  options$modelTerms <- list(list(components="debSame", isNuisance=FALSE))
  results <- jasptools::run("AnovaBayesian", "test.csv", options)
  expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
                   label="1-level factor check")

  options$dependent <- "contNormal"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(list(components="facFive", isNuisance=TRUE))
  results <- jasptools::run("AnovaBayesian", "test.csv", options)
  expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
                   label="All nuisance check")

  # options$dependent <- "debSame"
  # options$fixedFactors <- "facFive"
  # options$modelTerms <- list(list(components="facFive", isNuisance=FALSE))
  # results <- jasptools::run("AnovaBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="No variance check")

  options$dependent <- "debMiss99"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(list(components="facFive", isNuisance=FALSE))
  results <- jasptools::run("AnovaBayesian", "test.csv", options)
  expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
                 label="Too few obs check")
})
