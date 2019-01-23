context("Bayesian ANCOVA")

# does not test
# - descriptives table (code from regular ANOVA)
# - descriptives plot (code from regular ANOVA)
# - bftype (01, 10)

initOpts <- function() {
  options <- jasptools::analysisOptions("AncovaBayesian")
  options$sampleMode <- "manual"
  options$fixedSamplesNumber <- 50
  return(options)
}

test_that("Main table results match", {
  set.seed(0)
  options <- initOpts()
  options$dependent <- "contNormal"
  options$fixedFactors <- "facGender"
  options$randomFactors <- "facFive"
  options$covariates <- "contGamma"
  options$priorCovariates <- 0.3
  options$priorRandomEffects <- 1.2
  options$modelTerms <- list(
    list(components="facGender", isNuisance=FALSE),
    list(components="facFive", isNuisance=TRUE),
    list(components="contGamma", isNuisance=FALSE)
  )

  refTables <- list(
    nullModelTop = list("Null model (incl. facFive)", 0.25, -0.557159597272472, 0.0609616076207224,
                        0, "", "facGender", 0.25, -0.293047614582535, 0.493235485474366,
                        0.264111982689937, 29.6656710072444, "contGamma", 0.25, -1.12934210184216,
                        -0.61871766039617, -0.572182504569689, 15.885810482332, "facGender + contGamma",
                        0.25, -0.856200286718923, -0.313955333846312, -0.299040689446451,
                        9.83211717425022),
    bestModelTop = list("facGender", 0.25, -0.267241266752112, 0.547550995086289, 0, "",
                        "Null model (incl. facFive)", 0.25, -0.581045856142161, 0.0282510445044043,
                        -0.313804589390049, 10.5414217188718, "facGender + contGamma",
                        0.25, -0.904975901109745, -0.370131448423163, -0.637734634357633,
                        14.932174588461, "contGamma", 0.25, -1.13850449013975, -0.628606544880642,
                        -0.871263223387639, 15.235554204653)
  )

  for (order in c("nullModelTop", "bestModelTop")) {
    options$bayesFactorOrder <- order
    results <- jasptools::run("AncovaBayesian", "test.csv", options)
    table <- results[["results"]][["model comparison"]][["data"]]
    expect_equal_tables(table, refTables[[order]], label=paste("Table with order", order))
  }
})

test_that("Effects table results match", {
  set.seed(0)
  options <- initOpts()
  options$dependent <- "contNormal"
  options$covariates <- "contGamma"
  options$fixedFactors <- "contBinom"
  options$effects <- TRUE
  options$modelTerms <- list(
    list(components="contGamma", isNuisance=FALSE),
    list(components="contBinom", isNuisance=FALSE),
    list(components=c("contGamma", "contBinom"), isNuisance=FALSE)
  )

  refTables <- list(
    allModels = list("contGamma", 0.6, 0.226438035053417, 0.195147508378382, "contBinom",
                     0.6, 0.244646736629283, 0.215922578651091, "contGamma<unicode><unicode><unicode>contBinom",
                     0.2, 0.0356936478531283, 0.148059370442442),
    matchedModels = list("contGamma", 0.4, 0.193766080181001, 0.251463756140853, "contBinom",
                         0.4, 0.211903947712595, 0.281631799589681, "contGamma<unicode><unicode><unicode>contBinom",
                         0.2, 0.0356812041725066, 0.784742840790345)
  )

  for (effectsType in c("allModels", "matchedModels")) {
    options$effectsType <- effectsType
    results <- jasptools::run("AncovaBayesian", "test.csv", options)
    table <- results[["results"]][["effects"]][["data"]]
    expect_equal_tables(table, refTables[[effectsType]], label=paste("Table with effects type", effectsType))
  }
})

test_that("Post-hoc Comparisons table results match", {
  options <- jasptools::analysisOptions("AncovaBayesian")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(
    list(components="facFive", isNuisance=FALSE)
  )
  options$postHocTestsNullControl <- TRUE
  options$postHocTestsVariables <- "facFive"
  results <- jasptools::run("AncovaBayesian", "test.csv", options)
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
  results <- jasptools::run("AncovaBayesian", "test.csv", options)
  expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
                   label="Inf dependent check")

  options$dependent <- "contNormal"
  options$fixedFactors <- list()
  options$covariates <- "debInf"
  options$modelTerms <- list(list(components="debInf", isNuisance=FALSE))
  results <- jasptools::run("AncovaBayesian", "test.csv", options)
  expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
                  label="Inf covariate check")

  options$dependent <- "contNormal"
  options$fixedFactors <- "debSame"
  options$covariates <- list()
  options$modelTerms <- list(list(components="debSame", isNuisance=FALSE))
  results <- jasptools::run("AncovaBayesian", "test.csv", options)
  expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
                   label="1-level factor check")

  options$dependent <- "contNormal"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(list(components="facFive", isNuisance=TRUE))
  results <- jasptools::run("AncovaBayesian", "test.csv", options)
  expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
                   label="All nuisance check")

  # options$dependent <- "debSame"
  # options$fixedFactors <- "facFive"
  # options$modelTerms <- list(list(components="facFive", isNuisance=FALSE))
  # results <- jasptools::run("AncovaBayesian", "test.csv", options)
  # expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
  #                  label="No variance check")

  options$dependent <- "debMiss99"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(list(components="facFive", isNuisance=FALSE))
  results <- jasptools::run("AncovaBayesian", "test.csv", options)
  expect_identical(results[["results"]][["model comparison"]][["error"]][["errorType"]], "badData",
                  label="Too few obs check")
})
