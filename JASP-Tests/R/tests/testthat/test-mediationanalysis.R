context("Mediation Analysis")

test_that("Simple mediation analysis works", {
  options <- jaspTools::analysisOptions("MediationAnalysis")
  options$predictor <- "contcor1"
  options$mediators <- "contcor2"
  options$dependent <- "contNormal"
  options$mimic     <- "lavaan"
  options$estimator <- "ML"
  options$se        <- "standard"
  options$missing   <- "FIML"
  results <- jaspTools::run("MediationAnalysis","test.csv", options)
  
  dir_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_dir"]][["data"]]
  ind_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_ind"]][["data"]]
  tot_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_tot"]][["data"]]
  
  expect_equal_tables(dir_tab, list(
    -0.00931725107194831, 0.524832903921214, 0.257757826424633, "contcor1",
    "<unicode><unicode><unicode>", 0.0585458735974023, "contNormal",
    0.136265298547951, 1.89158816787041
  ))
  
  expect_equal_tables(ind_tab, list(
    -0.265930503999881, 0.0873033053757795, -0.0893135993120506, "contcor2",
    "<unicode><unicode><unicode>", "<unicode><unicode><unicode>",
    0.321618995211592, 0.0901123214921099, "contcor1", "contNormal",
    -0.991136371066311
  ))
  expect_equal_tables(tot_tab, list(
    -0.0338982391107447, 0.37078669333591, 0.168444227112582, "contcor1",
    "<unicode><unicode><unicode>", 0.10276101683937, "contNormal",
    0.103237849174464, 1.63161309984214
  ))
})

test_that("Categorical confounders work", {
  options <- jaspTools::analysisOptions("MediationAnalysis")
  options$predictor <- "contcor1"
  options$mediators <- "contcor2"
  options$dependent <- "contNormal"
  options$confounds <- c("facGender", "facExperim")
  options$mimic     <- "lavaan"
  options$estimator <- "ML"
  options$se        <- "standard"
  options$missing   <- "FIML"
  results <- jaspTools::run("MediationAnalysis","test.csv", options)
  
  ind_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_ind"]][["data"]]

  expect_equal_tables(ind_tab, list(
    -0.231781905561682, 0.111397204712701, -0.0601923504244907, "contcor2",
    "<unicode><unicode><unicode>", "<unicode><unicode><unicode>",
    0.491741929653112, 0.0875473000987102, "contcor1", "contNormal",
    -0.68754091053206
  ))
})

test_that("Multiple mediation with missing values works", {
  options <- jaspTools::analysisOptions("MediationAnalysis")
  options$predictor <- c("contcor1", "contOutlier")
  options$mediators <- c("contcor2", "debMiss1")
  options$dependent <- c("contNormal", "debMiss30")
  options$mimic     <- "lavaan"
  options$estimator <- "ML"
  options$se        <- "standard"
  options$missing   <- "FIML"
  results <- jaspTools::run("MediationAnalysis","test.csv", options)
  
  ind_tab <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_parest"]][["collection"]][["modelContainer_parest_ind"]][["data"]]
 
  expect_equal_tables(ind_tab, list(
    -0.286287438147744, 0.0716044322927397, -0.107341502927502, "contcor2",
    "<unicode><unicode><unicode>", "<unicode><unicode><unicode>",
    0.239717584976287, 0.0913006242113347, "contcor1", "contNormal",
    -1.17569297970009, -0.0934068738831913, 0.0235897423163003,
    -0.0349085657834455, "debMiss1", "<unicode><unicode><unicode>",
    "<unicode><unicode><unicode>", 0.242162593096285, 0.0298466239998148,
    "contcor1", "contNormal", -1.16959847062308, -0.0253089791568423,
    0.00781462953911848, -0.00874717480886191, "contcor2", "<unicode><unicode><unicode>",
    "<unicode><unicode><unicode>", 0.300593344204476, 0.00845005544929284,
    "contcor1", "debMiss30", -1.03516182365335, -0.0113560645428284,
    0.0267224578923567, 0.00768319667476416, "debMiss1", "<unicode><unicode><unicode>",
    "<unicode><unicode><unicode>", 0.428982821114215, 0.00971408728311937,
    "contcor1", "debMiss30", 0.790933460945488, -3.22066204798646,
    7.66726356440007, 2.2233007582068, "contcor2", "<unicode><unicode><unicode>",
    "<unicode><unicode><unicode>", 0.423453378826046, 2.77758308271711,
    "contOutlier", "contNormal", 0.800444376278354, -0.950829436570268,
    0.588622855557618, -0.181103290506325, "debMiss1", "<unicode><unicode><unicode>",
    "<unicode><unicode><unicode>", 0.644694053353005, 0.392724637868576,
    "contOutlier", "contNormal", -0.461145731750425, -0.29132070016713,
    0.653670767090359, 0.181175033461614, "contcor2", "<unicode><unicode><unicode>",
    "<unicode><unicode><unicode>", 0.452331433562723, 0.241073681636872,
    "contOutlier", "debMiss30", 0.751533855672048, -0.143920756388788,
    0.223640571178022, 0.0398599073946173, "debMiss1", "<unicode><unicode><unicode>",
    "<unicode><unicode><unicode>", 0.670768447414181, 0.0937673677848388,
    "contOutlier", "debMiss30", 0.425093594245718
  ))
})
