context("Mediation Analysis")

test_that("Simple mediation analysis works", {
  options <- jasptools::analysisOptions("MediationAnalysis")
  options$predictor <- "contcor1"
  options$mediators <- "contcor2"
  options$dependent <- "contNormal"
  options$mimic     <- "lavaan"
  options$estimator <- "ML"
  options$se        <- "default"
  results <- jasptools::run("MediationAnalysis","test.csv", options, 
                            view = FALSE, quiet = TRUE)
  
  dir_tab <- results[["results"]][["parest"]][["collection"]][["parest_dir"]][["data"]]
  ind_tab <- results[["results"]][["parest"]][["collection"]][["parest_ind"]][["data"]]
  tot_tab <- results[["results"]][["parest"]][["collection"]][["parest_tot"]][["data"]]
  
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
  options <- jasptools::analysisOptions("MediationAnalysis")
  options$predictor <- "contcor1"
  options$mediators <- "contcor2"
  options$dependent <- "contNormal"
  options$confounds <- c("facGender", "facExperim")
  options$mimic     <- "lavaan"
  options$estimator <- "ML"
  options$se        <- "default"
  results <- jasptools::run("MediationAnalysis","test.csv", options, 
                            view = FALSE, quiet = TRUE)
  
  ind_tab <- results[["results"]][["parest"]][["collection"]][["parest_ind"]][["data"]]

  expect_equal_tables(ind_tab, list(
    -0.231781905561682, 0.111397204712701, -0.0601923504244907, "contcor2",
    "<unicode><unicode><unicode>", "<unicode><unicode><unicode>",
    0.491741929653112, 0.0875473000987102, "contcor1", "contNormal",
    -0.68754091053206
  ))
})

test_that("Multiple mediation with missing values works", {
  options <- jasptools::analysisOptions("MediationAnalysis")
  options$predictor <- c("contcor1", "contOutlier")
  options$mediators <- c("contcor2", "debMiss1")
  options$dependent <- c("contNormal", "debMiss30")
  options$mimic     <- "lavaan"
  options$estimator <- "ML"
  options$se        <- "default"
  results <- jasptools::run("MediationAnalysis","test.csv", options, 
                            view = FALSE, quiet = TRUE)
  
  ind_tab <- results[["results"]][["parest"]][["collection"]][["parest_ind"]][["data"]]
 
  expect_equal_tables(ind_tab, list(
    -0.296304106802847, 0.243189511202716, -0.0265572978000652, "contcor2",
    "<unicode><unicode><unicode>", "<unicode><unicode><unicode>",
    0.846987382777977, 0.137628451915703, "contcor1", "contNormal",
    -0.192963718114997, -0.0768889421567336, 0.0611610412186728,
    -0.0078639504690304, "debMiss1", "<unicode><unicode><unicode>",
    "<unicode><unicode><unicode>", 0.823304518398917, 0.0352174796231786,
    "contcor1", "contNormal", -0.223296799009283, -0.0220636167090898,
    0.0181228445399456, -0.00197038608457214, "contcor2", "<unicode><unicode><unicode>",
    "<unicode><unicode><unicode>", 0.847586838739501, 0.0102518366577195,
    "contcor1", "debMiss30", -0.192198349462431, -0.00958773524468217,
    0.0394738026097792, 0.0149430336825485, "debMiss1", "<unicode><unicode><unicode>",
    "<unicode><unicode><unicode>", 0.232508749748695, 0.0125159284153823,
    "contcor1", "debMiss30", 1.19392131263577, -3.79189688957024,
    7.89972500677269, 2.05391405860122, "contcor2", "<unicode><unicode><unicode>",
    "<unicode><unicode><unicode>", 0.491056482909909, 2.98261141239455,
    "contOutlier", "contNormal", 0.688629450710867, -0.391583831958197,
    0.318635662446574, -0.0364740847558115, "debMiss1", "<unicode><unicode><unicode>",
    "<unicode><unicode><unicode>", 0.84045453399334, 0.181181771707769,
    "contOutlier", "contNormal", -0.201312109998798, -0.302842168425176,
    0.60761742910437, 0.152387630339597, "contcor2", "<unicode><unicode><unicode>",
    "<unicode><unicode><unicode>", 0.511762616441047, 0.232264369322889,
    "contOutlier", "debMiss30", 0.656095598235091, -0.244044319284989,
    0.382660013570617, 0.069307847142814, "debMiss1", "<unicode><unicode><unicode>",
    "<unicode><unicode><unicode>", 0.664645261089264, 0.159876492067959,
    "contOutlier", "debMiss30", 0.433508680646766
  ))
})
