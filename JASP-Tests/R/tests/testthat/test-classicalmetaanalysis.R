context("Meta Analysis")

metaAnalysisMethods <- c("Fixed Effects", "Maximum Likelihood", 
                         "Restricted ML", "DerSimonian-Laird", 
                         "Hedges", "Hunter-Schmidt", "Sidik-Jonkman", 
                         "Paule-Mandel") #"Empirical Bayes" causing problems

test_that("Fixed and Random Effects table results match", {
  options <- jasptools::analysisOptions("ClassicalMetaAnalysis")
  options$dependent  <- "contNormal"
  options$wlsWeights <- "contGamma"
  methods <- metaAnalysisMethods
  expectedTables <- list(
    list(1, "Omnibus test of Model Coefficients", 0.183822096880818, 1.76644946418514,
         99, "Test of Residual Heterogeneity", 2.0476025760519e-05, 167.546611260491
    ),
    list(1, "Omnibus test of Model Coefficients", 0.338540231375106, 0.915951480901639,
         99, "Test of Residual Heterogeneity", 2.0476025760519e-05, 167.546611260491
    ),
    list(1, "Omnibus test of Model Coefficients", 0.34252408597755, 0.900961285242127,
         99, "Test of Residual Heterogeneity", 2.0476025760519e-05, 167.546611260491
    ),
    list(1, "Omnibus test of Model Coefficients", 0.280180139806477, 1.16621588928646,
         99, "Test of Residual Heterogeneity", 2.0476025760519e-05, 167.546611260491
    ),
    list(1, "Omnibus test of Model Coefficients", 0.183822096880818, 1.76644946418514,
         99, "Test of Residual Heterogeneity", 2.0476025760519e-05, 167.546611260491
    ),
    list(1, "Omnibus test of Model Coefficients", 0.268173659026294, 1.22606505101284,
         99, "Test of Residual Heterogeneity", 2.0476025760519e-05, 167.546611260491
    ),
    list(1, "Omnibus test of Model Coefficients", 0.277353008949627, 1.18001467051959,
         99, "Test of Residual Heterogeneity", 2.0476025760519e-05, 167.546611260491
    ),
    list(1, "Omnibus test of Model Coefficients", 0.147192795654459, 2.10110431898561,
         99, "Test of Residual Heterogeneity", 2.0476025760519e-05, 167.546611260491
    )
  )
  for(i in 1:length(methods)) {
    options$method <- methods[i]
    results <- jasptools::run("ClassicalMetaAnalysis", "test.csv", options, view = FALSE)
    table <- results[["results"]][["fixRandTable"]][["data"]]
    makeTestTable(table)
    expect_equal_tables(table, expectedTables[[i]])
  }
})

test_that("Coefficients table results match", {
  options <- jasptools::analysisOptions("ClassicalMetaAnalysis")
  options$dependent  <- "contNormal"
  options$wlsWeights <- "contGamma"
  methods <- metaAnalysisMethods
  expectedTables <- list(
    list(-0.108777577319913, "intrcpt", 0.183822096880818, 0.0818443630855796,
         -1.32907842664951),
    list(-0.1448847208816, "intrcpt", 0.338540231375106, 0.151386223058999,
         -0.957053541293088),
    list(-0.144598811134704, "intrcpt", 0.34252408597755, 0.152339195413899,
         -0.94918980464506),
    list(-0.149819007417134, "intrcpt", 0.280180139806477, 0.138732252710499,
         -1.07991476019474),
    list(-0.108777577319913, "intrcpt", 0.183822096880818, 0.0818443630855796,
         -1.32907842664951),
    list(-0.150978089683887, "intrcpt", 0.268173659026294, 0.136350636446993,
         -1.10727821752838),
    list(-0.150088160742086, "intrcpt", 0.277353008949627, 0.138166492341491,
         -1.08628480175302),
    list(-0.16380009902283, "intrcpt", 0.147192795654459, 0.113003098603563,
         -1.44951865078915)
  )
  for(i in 1:length(methods)) {
    options$method <- methods[i]
    results <- jasptools::run("ClassicalMetaAnalysis", "test.csv", options, view = FALSE)
    table <- results[["results"]][["coeffTable"]][["data"]]
    makeTestTable(table)
    expect_equal_tables(table, expectedTables[[i]])
  }
})

test_that("Fit Measures table results match", {
  options <- jasptools::analysisOptions("ClassicalMetaAnalysis")
  options$dependent  <- "contNormal"
  options$wlsWeights <- "contGamma"
  options$modelFit   <- TRUE
  methods <- metaAnalysisMethods
  expectedTables <- list(
    list(-218.079649448198, "Log-likelihood", 167.546611260491, "Deviance",
         438.159298896395, "AIC", 440.764469082383, "BIC", 438.200115222925,
         "AICc"),
    list(-185.9792474307, "Log-likelihood", 103.345807225497, "Deviance",
         375.958494861401, "AIC", 381.168835233377, "BIC", 376.082206201607,
         "AICc"),
    list(-184.642510604585, "Log-likelihood", 369.285021209171, "Deviance",
         373.285021209171, "AIC", 378.47526090944, "BIC", 373.410021209171,
         "AICc"),
    list(-186.605655760841, "Log-likelihood", 104.598623885777, "Deviance",
         377.211311521681, "AIC", 382.421651893657, "BIC", 377.335022861887,
         "AICc"),
    list(-218.079649448198, "Log-likelihood", 167.546611260491, "Deviance",
         440.159298896395, "AIC", 445.369639268371, "BIC", 440.283010236601,
         "AICc"),
    list(-186.883475350824, "Log-likelihood", 105.154263065744, "Deviance",
         377.766950701649, "AIC", 382.977291073625, "BIC", 377.890662041855,
         "AICc"),
    list(-186.666521286721, "Log-likelihood", 104.720354937539, "Deviance",
         377.333042573443, "AIC", 382.543382945419, "BIC", 377.456753913649,
         "AICc"),
    list(-193.366964751911, "Log-likelihood", 118.121241867919, "Deviance",
         390.733929503823, "AIC", 395.944269875799, "BIC", 390.857640844029,
         "AICc")
  )
  for(i in 1:length(methods)) {
    options$method <- methods[i]
    results <- jasptools::run("ClassicalMetaAnalysis", "test.csv", options, view = FALSE)
    table <- results[["results"]][["fitMeasuresTable"]][["data"]]
    makeTestTable(table)
    expect_equal_tables(table, expectedTables[[i]])
  }
})

test_that("Covariace Matrix table results match", {
  options <- jasptools::analysisOptions("ClassicalMetaAnalysis")
  options$dependent  <- "contNormal"
  options$wlsWeights <- "contGamma"
  options$covariates <- c("contcor1", "facFifty")
  options$modelTerms <- list(
    list(components="contcor1"),
    list(components="facFifty")
  )
  options$regressionCoefficientsCovarianceMatrix <- TRUE
  methods <- metaAnalysisMethods
  expectedTables <- list(
    list(-218.079649448198, "Log-likelihood", 167.546611260491, "Deviance",
         438.159298896395, "AIC", 440.764469082383, "BIC", 438.200115222925,
         "AICc"),
    list(-185.9792474307, "Log-likelihood", 103.345807225497, "Deviance",
         375.958494861401, "AIC", 381.168835233377, "BIC", 376.082206201607,
         "AICc"),
    list(-184.642510604585, "Log-likelihood", 369.285021209171, "Deviance",
         373.285021209171, "AIC", 378.47526090944, "BIC", 373.410021209171,
         "AICc"),
    list(-186.605655760841, "Log-likelihood", 104.598623885777, "Deviance",
         377.211311521681, "AIC", 382.421651893657, "BIC", 377.335022861887,
         "AICc"),
    list(-218.079649448198, "Log-likelihood", 167.546611260491, "Deviance",
         440.159298896395, "AIC", 445.369639268371, "BIC", 440.283010236601,
         "AICc"),
    list(-186.883475350824, "Log-likelihood", 105.154263065744, "Deviance",
         377.766950701649, "AIC", 382.977291073625, "BIC", 377.890662041855,
         "AICc"),
    list(-186.666521286721, "Log-likelihood", 104.720354937539, "Deviance",
         377.333042573443, "AIC", 382.543382945419, "BIC", 377.456753913649,
         "AICc"),
    list(-193.366964751911, "Log-likelihood", 118.121241867919, "Deviance",
         390.733929503823, "AIC", 395.944269875799, "BIC", 390.857640844029,
         "AICc")
  )
  for(i in 1:length(methods)) {
    options$method <- methods[i]
    results <- jasptools::run("ClassicalMetaAnalysis", "test.csv", options)
    table <- results[["results"]][["fitMeasuresTable"]][["data"]]
    makeTestTable(table)
    expect_equal_tables(table, expectedTables[[i]])
  }
})

test_that("Rank Correlation Test table results match", {
  options <- jasptools::analysisOptions("ClassicalMetaAnalysis")
  options$dependent  <- "contNormal"
  options$wlsWeights <- "contGamma"
  methods <- metaAnalysisMethods
  options$rSquaredChange <- TRUE
  
})

test_that("Regression Test table results match", {
  options <- jasptools::analysisOptions("ClassicalMetaAnalysis")
  options$dependent  <- "contNormal"
  options$wlsWeights <- "contGamma"
  methods <- metaAnalysisMethods
  options$funnelPlotAsymmetryTest <- TRUE
  
})

test_that("Influence Measures table results match", {
  options <- jasptools::analysisOptions("ClassicalMetaAnalysis")
  options$dependent  <- "contNormal"
  options$wlsWeights <- "contGamma"
  methods <- metaAnalysisMethods
  options$residualsCasewiseDiagnostics <- TRUE
  
})

test_that("File Drawer Analysis table results match", {
  options <- jasptools::analysisOptions("ClassicalMetaAnalysis")
  options$dependent  <- "contNormal"
  options$wlsWeights <- "contGamma"
  methods <- metaAnalysisMethods
  options$plotResidualsCovariates <- TRUE
  
})

test_that("Forest plot match", {
  options <- jasptools::analysisOptions("ClassicalMetaAnalysis")
  options$dependent  <- "contNormal"
  options$wlsWeights <- "contGamma"
  methods <- metaAnalysisMethods
  options$forestPlot <- TRUE
  
})

test_that("Funnel plot match", {
  options <- jasptools::analysisOptions("ClassicalMetaAnalysis")
  options$dependent  <- "contNormal"
  options$wlsWeights <- "contGamma"
  methods <- metaAnalysisMethods
  options$funnelPlot <- TRUE
  
})

test_that("Profile plot match", {
  options <- jasptools::analysisOptions("ClassicalMetaAnalysis")
  options$dependent  <- "contNormal"
  options$wlsWeights <- "contGamma"
  methods <- metaAnalysisMethods
  options$plotResidualsPredicted <- TRUE
  
})

test_that("Trim Fill plot match", {
  options <- jasptools::analysisOptions("ClassicalMetaAnalysis")
  options$dependent  <- "contNormal"
  options$wlsWeights <- "contGamma"
  methods <- metaAnalysisMethods
  options$trimFillPlot <- TRUE
  
  
})

test_that("Diagnostics plot match", {
  options <- jasptools::analysisOptions("ClassicalMetaAnalysis")
  options$dependent  <- "contNormal"
  options$wlsWeights <- "contGamma"
  methods <- metaAnalysisMethods
  options$plotResidualsDependent <- TRUE
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, " ", dir=" ")
})

test_that("Analysis handles errors", {
  #Negative Effect Size Standard Error values
  options <- jasptools::analysisOptions("ClassicalMetaAnalysis")
  options$dependent  <- "contNormal"
  options$wlsWeights <- "contcor1"
  results <- jasptools::run("ClassicalMetaAnalysis", "test.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
  
  #<2 Observations
  options <- jasptools::analysisOptions("ClassicalMetaAnalysis")
  options$dependent  <- "debMiss99"
  options$wlsWeights <- "contGamma"
  results <- jasptools::run("ClassicalMetaAnalysis", "test.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
  
  #Infinity values
  options <- jasptools::analysisOptions("ClassicalMetaAnalysis")
  options$dependent  <- "debInf"
  options$wlsWeights <- "contGamma"
  results <- jasptools::run("ClassicalMetaAnalysis", "test.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
})

jasptools::run("ClassicalMetaAnalysis", "test.csv",options)
options$regressionCoefficientsCovarianceMatrix <- TRUE
options$forestPlot <- TRUE
options$factors <- "facExperim"
options$modelTerms <- list(
  list(components="facExperim")
)
options$wlsWeights <- "debCollin1"
options$modelTerms
options$plotResidualsPredicted <- TRUE
options$funnelPlot <- TRUE
options$trimFillPlot <- TRUE
options$plotResidualsDependent <- TRUE
results <- jasptools::run("ClassicalMetaAnalysis", "test.csv", options)
table <- 
  expect_equal_tables(table,)