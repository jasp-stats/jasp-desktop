context("Log-Linear Regression")

# does not test
# - error handling

test_that("Main table results match", {
  options <- jasptools::analysisOptions("RegressionLogLinear")
  options$counts <- "facFifty"
  options$factors <- c("contBinom", "facGender")
  options$modelTerms <- list(
    list(components="contBinom"),
    list(components="facGender"),
    list(components=c("contBinom", "facGender"))
  )
  results <- jasptools::run("RegressionLogLinear", "test.csv", options)
  table <- results[["results"]][["Container"]][["collection"]][["Container_AnovaTable"]][["data"]]
  expect_equal_tables(table,
    list(" ", " ", "NULL", " ", 936.356249443911, 99, 9.73545292814663,
         1, "contBinom", 0.00180747470901472, 926.620796515764, 98, 7.02546792150429,
         1, "facGender", 0.00803584702758609, 919.59532859426, 97, 0.769509416901883,
         1, "contBinom<unicode><unicode><unicode><unicode><unicode><unicode><unicode><unicode><unicode>facGender",
         0.380368860922424, 918.825819177358, 96)
  )
})

test_that("Coefficients table matches", {
  options <- jasptools::analysisOptions("RegressionLogLinear")
  options$counts <- "facFifty"
  options$factors <- c("contBinom", "facFive")
  options$modelTerms <- list(
    list(components="contBinom"),
    list(components="facFive"),
    list(components=c("contBinom", "facFive"))
  )
  options$regressionCoefficientsEstimates <- TRUE
  options$regressionCoefficientsConfidenceIntervals <- TRUE
  options$regressionCoefficientsConfidenceIntervalsInterval <- 0.95
  results <- jasptools::run("RegressionLogLinear", "test.csv", options)
  table <- results[["results"]][["Container"]][["collection"]][["Container_CoefficientsTable"]][["data"]]
  expect_equal_tables(table,
    list(3.36441813015886, 0.0536828127084252, 62.6721656414034, 3.25920175066154,
    "(Intercept)", 0, 3.46963450965618, -0.63167531645938, 0.104937579028612,
    -6.01953392013314, -0.837349191980285, "contBinom = 1", 1.74920000092307e-09,
    -0.426001440938475, -0.379859157202991, 0.0921944777734458,
    -4.1201942499901, -0.560557013212423, "facFive = 2", 3.78553110594005e-05,
    -0.199161301193559, -0.343993244014492, 0.0833479734033365,
    -4.1271938592897, -0.507352270069434, "facFive = 3", 3.67216813648207e-05,
    -0.18063421795955, -0.250902820948484, 0.085593742903271, -2.93132199198285,
    -0.418663474340876, "facFive = 4", 0.00337522739489267, -0.0831421675560923,
    -0.0685812641545293, 0.0731503942633908, -0.937537860802095,
    -0.211953402365681, "facFive = 5", 0.348481958057894, 0.0747908740566221,
    0.992224258514625, 0.140836788661735, 7.04520649713029, 0.716189225039344,
    "contBinom = 1*facFive = 2", 1.85186666239764e-12, 1.2682592919899,
    1.10775799178149, 0.137349243064596, 8.06526462807306, 0.838558422071047,
    "contBinom = 1*facFive = 3", 7.30773999647178e-16, 1.37695756149194,
    0.935886690862371, 0.136875960028963, 6.83748037759394, 0.667614738856258,
    "contBinom = 1*facFive = 4", 8.05981239553115e-12, 1.20415864286848,
    0.703134280441524, 0.142750071638859, 4.92563171681182, 0.423349281238848,
    "contBinom = 1*facFive = 5", 8.40882439228307e-07, 0.982919279644201)
  )
})
