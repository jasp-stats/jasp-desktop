context("Linear Mixed Models")

# the tests are grouped by test model terms methods - they lead to differently structured types models structures
# the library("afex") call is needed to prevent crashing afex after being called for a second time using JASP
library("afex")

### default, all selected output using Satterwhite method
{
  options <- jasptools::analysisOptions("MixedModelsLMM")
  options$.meta <-
    list(
      Contrasts = list(
        list(levels = list(containsColumn = TRUE)),
        list(levels = list(containsColumn = TRUE)),
        list(levels = list(containsColumn = TRUE)),
        list(levels = list(containsColumn = TRUE))
      ),
      dependentVariable = list(containsColumn = TRUE),
      fixedEffects = list(list(), list(), list()),
      fixedVariables = list(containsColumn = TRUE),
      marginalMeans = list(list(), list()),
      plotsAgregatedOver = list(containsColumn = TRUE),
      plotsX = list(list()),
      randomEffects = list(list(
        randomComponents = list(list(), list(), list()),
        value = list(containsColumn = TRUE)
      )),
      randomVariables = list(containsColumn = TRUE),
      trendsContrasts = list(list(levels = list(containsColumn = TRUE)), list(levels = list(containsColumn = TRUE))),
      trendsTrend = list(list()),
      trendsVariables = list(list())
    )
  options$Contrasts <-
    list(
      list(
        isContrast = FALSE,
        levels = c("2", "3", "4", "5",
                   "6", "7"),
        name = "contGamma",
        values = c("-1", "0", "1", "-1",
                   "0", "1")
      ),
      list(
        isContrast = FALSE,
        levels = c("2", "3", "4",
                   "5", "6", "7"),
        name = "contBinom",
        values = c("0", "0", "0",
                   "1", "1", "1")
      ),
      list(
        isContrast = TRUE,
        levels = c("2", "3",
                   "4", "5", "6", "7"),
        name = "Contrast 1",
        values = c("1", "-1",
                   "0", "0", "0", "0")
      ),
      list(
        isContrast = TRUE,
        levels = c("2",
                   "3", "4", "5", "6", "7"),
        name = "Contrast 2",
        values = c("0",
                   "0", "1", "-1", "0", "0")
      )
    )
  options$dependentVariable <- "contNormal"
  options$fixedEffects <-
    list(
      list(components = "contGamma"),
      list(components = "contBinom"),
      list(components = c("contGamma", "contBinom"))
    )
  options$fixedVariables <- c("contGamma", "contBinom")
  options$marginalMeans <-
    list(list(variable = "contGamma"), list(variable = "contBinom"))
  options$marginalMeansCompare <- TRUE
  options$marginalMeansContrast <- TRUE
  options$plotsAgregatedOver <- "facFive"
  options$plotsX <- list(list(variable = "contBinom"))
  options$pvalVS <- TRUE
  options$randomEffects <-
    list(list(
      correlations = TRUE,
      randomComponents = list(
        list(randomSlopes = TRUE,
             value = "contGamma"),
        list(randomSlopes = TRUE, value = "contBinom"),
        list(
          randomSlopes = TRUE,
          value = c("contGamma", "contBinom")
        )
      ),
      value = "facFive"
    ))
  options$randomVariables <- "facFive"
  options$showFE <- TRUE
  options$showRE <- TRUE
  options$trendsCompare <- TRUE
  options$trendsContrast <- TRUE
  options$trendsContrasts <-
    list(
      list(
        isContrast = FALSE,
        levels = c("2", "3"),
        name = "contBinom",
        values = c("0", "1")
      ),
      list(
        isContrast = TRUE,
        levels = c("2",
                   "3"),
        name = "Contrast 1",
        values = c("1", "-1")
      )
    )
  options$trendsTrend <- list(list(variable = "contGamma"))
  options$trendsVariables <- list(list(variable = "contBinom"))
  set.seed(1)
  results <- jasptools::run("MixedModelsLMM", "debug.csv", options)
  
  
  test_that("ANOVA Summary table results match", {
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "1, 90.22",
        "contGamma",
        0.22884145049434,
        1.09008290032611,
        1.46789722467309,
        "1, 92.53",
        "contBinom",
        0.103517073393314,
        1.56692026770945,
        2.7035761613605,
        "1, 12.62",
        "contGamma * contBinom",
        0.180131523429278,
        1.19148271575718,
        2.01344486055981
      )
    )
  })
  
  test_that("Estimated Marginal Means table results match", {
    table <- results[["results"]][["EMMsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0,
        0.500549669999563,
        "<unicode><unicode><unicode>",
        0.0315977888593943,-0.378305758897991,
        1,
        0.879908182590682,
        1,
        0.209138306106976,
        0.151085611467235,
        0.44150133661678,
        0,
        2.03296079621,
        "<unicode><unicode><unicode>",-0.119989135548338,
        -0.43299337590836,
        2,
        0.452444643849674,
        1,
        0.159698975506162,
        -0.751345681260856,
        0.193015104811683,
        0,
        3.56537192242044,
        "<unicode><unicode><unicode>",
        -0.27157605995607,-0.656532520795187,
        3,
        0.166756873371831,
        1.23161009100725,
        0.196409966650206,
        -1.38269999525905,
        0.113380400883047,
        1,
        0.500549669999563,
        "<unicode><unicode><unicode>",
        -0.46391367708327,-0.958965681406266,
        4,
        0.0662565575749397,
        2.04565065823451,
        0.252582194483115,
        -1.83668400709173,
        0.031138327239726,
        1,
        2.03296079621,
        "<unicode><unicode><unicode>",
        -0.260176417478306,-0.582419559889269,
        5,
        0.113544933289102,
        1.48924916621322,
        0.164412787659761,
        -1.58245852516485,
        0.0620667249326567,
        1,
        3.56537192242044,
        "<unicode><unicode><unicode>",
        -0.056439157873342,-0.625996197497902,
        6,
        0.846004518907751,
        1,
        0.290595666102619,-0.19421885615256,
        0.513117881751218
      )
    )
  })
  
  test_that("Fixed Effects Estimates table results match", {
    table <- results[["results"]][["FEsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        8.85556813317639,
        0.0811124286911853,
        0.740830967234119,
        1,
        0.237652397340321,
        0.341307008046005,
        "Intercept",
        90.2220038108491,
        -0.0989205323656177,
        0.22884145049434,
        1.09008290032611,
        0.0816466970263889,
        -1.21156808503406,
        "contGamma",
        92.5266737437502,
        -0.611575229777537,
        0.103517073393314,
        1.56692026770945,
        0.371946591275627,
        -1.64425550367347,
        "contBinom",
        12.6215092915868,
        0.231872620822972,
        0.180131523429278,
        1.19148271575718,
        0.163410365158442,
        1.4189590764218,
        "contGamma * contBinom"
      )
    )
  })
  
  test_that("facFive: Correlation Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_CE1"]][["data"]]
    expect_equal_tables(
      table,
      list(
        1,
        "Intercept",
        1,
        1,
        "contGamma",
        0.999923179018341,
        0.999923179018341,
        1,
        "contBinom",
        -0.999999569773461,
        -0.999999569773461,
        -0.999933712251631,
        1,
        "contGamma * contBinom"
      )
    )
  })
  
  test_that("Residual Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_RES1"]][["data"]]
    expect_equal_tables(table,
                        list(1.04779439621782, 1.09787309674547))
  })
  
  test_that("facFive: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0.17681933405089,
        0.0312650768942002,
        "Intercept",
        0.000495771594957875,
        2.45789474367076e-07,
        "contGamma",
        0.0202350213850555,
        0.000409456090453653,
        "contBinom",
        0.100434818366022,
        0.0100871527402158,
        "contGamma * contBinom"
      )
    )
  })
  
  test_that("Contrasts table results match", {
    table <- results[["results"]][["contrasts_Means"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "Contrast 1",
        "<unicode><unicode><unicode>",
        0.151586924407732,
        0.451355515064526,
        1,
        0.125116306941571,
        1.21156808503406,
        "Contrast 2",
        "<unicode><unicode><unicode>",
        0.1923376171272,
        0.526393437270812,
        1,
        0.303600987358483,
        0.633521052749719
      )
    )
  })
  
  test_that("Contrasts table results match", {
    table <- results[["results"]][["contrasts_Trends"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "Contrast 1",
        "<unicode><unicode><unicode>",
        -0.231872620822973,
        0.155910947344017,
        1.26961862883028,
        0.163410365158442,
        -1.4189590764218
      )
    )
  })
  
  test_that("Plot matches", {
    plotName <- results[["results"]][["plots"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot", dir = "MixedModelsLMM")
  })
  
  test_that("Estimated Trends table results match", {
    table <- results[["results"]][["trendsSummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0,
        "<unicode><unicode><unicode>",
        -0.258945117993994,
        1,
        0.225677757532263,
        1.09502741021828,
        0.0816466970263891,
        -0.0989205323656181,
        -1.21156808503406,
        0.0611040532627581,
        1,
        "<unicode><unicode><unicode>",
        -0.144595998758653,
        2,
        0.347797988707533,
        1.00152005423767,
        0.141608769041305,
        0.132952088457355,
        0.938869035847456,
        0.410500175673363
      )
    )
  })
}
### no correlations between random effects, Kernwald Roggers method, custom values
{
  options <- jasptools::analysisOptions("MixedModelsLMM")
  options$.meta <-
    list(
      Contrasts = list(
        list(levels = list(containsColumn = TRUE)),
        list(levels = list(containsColumn = TRUE)),
        list(levels = list(containsColumn = TRUE)),
        list(levels = list(containsColumn = TRUE)),
        list(levels = list(containsColumn = TRUE))
      ),
      dependentVariable = list(containsColumn = TRUE),
      fixedEffects = list(list(), list(),
                          list()),
      fixedVariables = list(containsColumn = TRUE),
      marginalMeans = list(list(), list()),
      plotsAgregatedOver = list(containsColumn = TRUE),
      plotsX = list(list()),
      randomEffects = list(list(
        randomComponents = list(list(), list(), list()),
        value = list(containsColumn = TRUE)
      )),
      randomVariables = list(containsColumn = TRUE),
      trendsContrasts = list(
        list(levels = list(containsColumn = TRUE)),
        list(levels = list(containsColumn = TRUE)),
        list(levels = list(containsColumn = TRUE)),
        list(levels = list(containsColumn = TRUE))
      ),
      trendsTrend = list(list()),
      trendsVariables = list(list())
    )
  options$Contrasts <-
    list(
      list(
        isContrast = FALSE,
        levels = c("2", "3", "4", "5",
                   "6", "7"),
        name = "contGamma",
        values = c("-1.5", "0", "1.5",
                   "-1.5", "0", "1.5")
      ),
      list(
        isContrast = FALSE,
        levels = c("2",
                   "3", "4", "5", "6", "7"),
        name = "contBinom",
        values = c("0",
                   "0", "0", "1", "1", "1")
      ),
      list(
        isContrast = TRUE,
        levels = c("2",
                   "3", "4", "5", "6", "7"),
        name = "Contrast 1",
        values = c("1",
                   "-1", "0", "0", "0", "0")
      ),
      list(
        isContrast = TRUE,
        levels = c("2",
                   "3", "4", "5", "6", "7"),
        name = "Contrast 2",
        values = c("0",
                   "1", "0", "0", "0", "-1")
      ),
      list(
        isContrast = TRUE,
        levels = c("2",
                   "3", "4", "5", "6", "7"),
        name = "Contrast 3",
        values = c("0",
                   "0", "0", "0", "0", "0")
      )
    )
  options$dependentVariable <- "contNormal"
  options$fixedEffects <-
    list(
      list(components = "contGamma"),
      list(components = "contBinom"),
      list(components = c("contGamma", "contBinom"))
    )
  options$fixedVariables <- c("contGamma", "contBinom")
  options$marginalMeans <-
    list(list(variable = "contGamma"), list(variable = "contBinom"))
  options$marginalMeansAdjustment <- "mvt"
  options$marginalMeansCIwidth <- 0.1
  options$marginalMeansCompare <- TRUE
  options$marginalMeansCompareTo <- 0.054
  options$marginalMeansContrast <- TRUE
  options$marginalMeansDf <- "satterthwaite"
  options$marginalMeansSD <- 1.5
  options$method <- "KR"
  options$plotLegendPosition <- "bottom"
  options$plotsAgregatedOver <- "facFive"
  options$plotsBackgroundColor <- "none"
  options$plotsCImethod <- "none"
  options$plotsEstimatesTable <- TRUE
  options$plotsGeom <- "geom_violin"
  options$plotsMappingColor <- TRUE
  options$plotsMappingFill <- TRUE
  options$plotsMappingLineType <- FALSE
  options$plotsMappingShape <- FALSE
  options$plotsTheme <- "theme_bw"
  options$plotsX <- list(list(variable = "contBinom"))
  options$pvalVS <- TRUE
  options$randomEffects <-
    list(list(
      correlations = FALSE,
      randomComponents = list(
        list(randomSlopes = TRUE, value = "contGamma"),
        list(randomSlopes = TRUE,
             value = "contBinom"),
        list(
          randomSlopes = TRUE,
          value = c("contGamma",
                    "contBinom")
        )
      ),
      value = "facFive"
    ))
  options$randomVariables <- "facFive"
  options$showFE <- TRUE
  options$showRE <- TRUE
  options$trendsAdjustment <- "hommel"
  options$trendsCIwidth <- 0.5
  options$trendsCompare <- TRUE
  options$trendsCompareTo <- -0.107
  options$trendsContrast <- TRUE
  options$trendsContrasts <-
    list(
      list(
        isContrast = FALSE,
        levels = c("2", "3"),
        name = "contBinom",
        values = c("0", "1")
      ),
      list(
        isContrast = TRUE,
        levels = c("2",
                   "3"),
        name = "Contrast 1",
        values = c("1", "-1")
      ),
      list(
        isContrast = TRUE,
        levels = c("2", "3"),
        name = "Contrast 2",
        values = c("0",
                   "0")
      ),
      list(
        isContrast = TRUE,
        levels = c("2", "3"),
        name = "Contrast 3",
        values = c("2", "-2")
      )
    )
  options$trendsTrend <- list(list(variable = "contGamma"))
  options$trendsVariables <- list(list(variable = "contBinom"))
  set.seed(1)
  results <- jasptools::run("MixedModelsLMM", "debug.csv", options)
  
  
  test_that("ANOVA Summary table results match", {
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "1, 15.81",
        "contGamma",
        0.24295874110041,
        1.07018387657006,
        1.47127602634511,
        "1, 29.47",
        "contBinom",
        0.130993545549791,
        1.38166308833348,
        2.41315890983728,
        "1, 15.49",
        "contGamma * contBinom",
        0.200859568158031,
        1.14103131847054,
        1.78483412437607
      )
    )
  })
  
  test_that(
    "Estimated Marginal Means table results match (non zero compare to, SD scaling factor, CI)",
    {
      table <- results[["results"]][["EMMsummary"]][["data"]]
      expect_equal_tables(
        table,
        list(
          0,
          -0.265655893105655,
          96.0000000010674,
          0.143893982843771,
          0.113540650228316,
          1,
          0.709864972516005,
          1,
          0.240910527841596,
          0.373142608789922,
          0.174247315459225,
          0,
          2.03296079621,
          96.0000000017463,
          -0.106804244278956,-0.124336804433216,
          2,
          0.250717999626521,
          1.06063003209159,
          0.139153692765411,
          -1.15558732997509,
          -0.0892716841246962,
          0,
          4.33157748552565,
          96.0000000022494,
          -0.357502471401682,
          -0.385844836966173,
          3,
          0.0704575626327167,
          1.96825991287343,
          0.224949738960286,-1.82930850821895,
          -0.329160105837191,
          1,
          -0.265655893105655,
          96.0000000012664,
          -0.551972196866067,
          -0.593565816686763,
          4,
          0.0695135468031274,
          1.9848965110407,
          0.330123253113387,
          -1.83559380065219,-0.510378577045371,
          1,
          2.03296079621,
          95.999999997209,
          -0.262222077646192,-0.282971512311535,
          5,
          0.0578065338007162,
          2.23246244814319,
          0.164685615282215,
          -1.92015603247616,
          -0.24147264298085,
          1,
          4.33157748552565,
          95.9999999990992,
          0.0275280415736817,
          -0.0191351539906524,
          6,
          0.943167364689904,
          1,
          0.370359828905738,
          -0.0714763221068877,
          0.0741912371380158
        )
      )
    }
  )
  
  test_that("Estimated Means and Confidence Intervals table (from the Figure) results match",
            {
              table <- results[["results"]][["EstimatesTable"]][["data"]]
              expect_equal_tables(table,
                                  list(0,-0.106804244278956, 1,-0.262222077646192))
            })
  
  test_that("Fixed Effects Estimates table results match", {
    table <- results[["results"]][["FEsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        96.0000000010635,
        0.114920269333442,
        0.608458244717892,
        1,
        0.22359597541643,
        0.513963943757984,
        "Intercept",
        96.0000000015469,
        -0.109064825069796,
        0.183125458399537,
        1.18338367055506,
        0.0813385998424248,
        -1.3408741394748,
        "contGamma",
        96.0000000011338,
        -0.633405444610997,
        0.0933901140243669,
        1.66141678204042,
        0.373775316767901,
        -1.69461549812374,
        "contBinom",
        96.0000000006581,
        0.235118951695902,
        0.138528059496446,
        1.34347898289837,
        0.157404830037889,
        1.49372132760669,
        "contGamma * contBinom"
      )
    )
  })
  
  test_that("Residual Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_RES4"]][["data"]]
    expect_equal_tables(table,
                        list(1.05705451759457, 1.1173642531671))
  })
  
  test_that("facFive: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
    expect_equal_tables(table,
                        list(0, 0, "Intercept"))
  })
  
  test_that("facFive.1: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE2"]][["data"]]
    expect_equal_tables(table,
                        list(0, 0, "contGamma"))
  })
  
  test_that("facFive.2: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE3"]][["data"]]
    expect_equal_tables(table,
                        list(0, 0, "contBinom"))
  })
  
  test_that("facFive.3: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE4"]][["data"]]
    expect_equal_tables(table,
                        list(0, 0, "contGamma * contBinom"))
  })
  
  test_that("Contrasts table results match(3rd row empty, p-val adjustment)",
            {
              table <- results[["results"]][["contrasts_Means"]][["data"]]
              expect_equal_tables(
                table,
                list(
                  "Contrast 1",
                  96.0000000015469,
                  0.250698227122726,
                  0.331656123654551,
                  1.00504041339715,
                  0.186966263083365,
                  1.3408741394748,
                  "Contrast 2",
                  95.9999999988918,
                  -0.134332285852638,
                  0.929388463472442,
                  1,
                  0.395638917546464,
                  -0.339532538117567
                )
              )
            })
  
  test_that("Contrasts table results match (first and 3rd row, p-val adjustment)",
            {
              table <- results[["results"]][["contrasts_Trends"]][["data"]]
              expect_equal_tables(
                table,
                list(
                  "Contrast 1",
                  "<unicode><unicode><unicode>",
                  -0.235118951695902,
                  0.135248471691765,
                  1.35957710813585,
                  0.15740483003789,
                  -1.49372132760669,
                  "Contrast 3",
                  "<unicode><unicode><unicode>",
                  -0.470237903391805,
                  0.135248471691765,
                  1.35957710813585,
                  0.31480966007578,
                  -1.49372132760669
                )
              )
            })
  
  test_that("Plot matches (black & white style, bottom legend, no CI, fill & color)",
            {
              plotName <- results[["results"]][["plots"]][["data"]]
              testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
              expect_equal_plots(testPlot, "plot", dir = "MixedModelsLMM")
            })
  
  test_that("Estimated Trends table results match (custom compare to, CI)", {
    table <- results[["results"]][["trendsSummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0,
        "<unicode><unicode><unicode>",
        -0.163926876958813,
        1,
        0.979747437054209,
        1,
        0.0813385998424251,
        -0.109064825069797,
        -0.0253855497119066,-0.0542027731807804,
        1,
        "<unicode><unicode><unicode>",
        0.0351597500114612,
        2,
        0.0837377568333247,
        1.77141810154857,
        0.134760204418555,
        0.126054126626106,
        1.729398731856,
        0.21694850324075
      )
    )
  })
}
### type II, LRT + intercept, 2 factors
{
  options <- jasptools::analysisOptions("MixedModelsLMM")
  options$.meta <-
    list(
      Contrasts = list(list(levels = list(containsColumn = TRUE)),
                       list(levels = list(containsColumn = TRUE)), list(levels = list(containsColumn = TRUE))),
      dependentVariable = list(containsColumn = TRUE),
      fixedEffects = list(list(), list(), list()),
      fixedVariables = list(containsColumn = TRUE),
      marginalMeans = list(list(),
                           list()),
      plotsAgregatedOver = list(containsColumn = TRUE),
      plotsTrace = list(list()),
      plotsX = list(list()),
      randomEffects = list(list(
        randomComponents = list(list(), list(), list()),
        value = list(containsColumn = TRUE)
      )),
      randomVariables = list(containsColumn = TRUE)
    )
  options$Contrasts <-
    list(
      list(
        isContrast = FALSE,
        levels = c("2", "3", "4", "5"),
        name = "contBinom",
        values = c("0", "1", "0", "1")
      ),
      list(
        isContrast = FALSE,
        levels = c("2", "3", "4", "5"),
        name = "facGender",
        values = c("f", "f", "m", "m")
      ),
      list(
        isContrast = TRUE,
        levels = c("2", "3", "4", "5"),
        name = "Contrast 1",
        values = c("0",
                   "0", "0", "0")
      )
    )
  options$dependentVariable <- "contNormal"
  options$fixedEffects <-
    list(
      list(components = "contBinom"),
      list(components = "facGender"),
      list(components = c("contBinom", "facGender"))
    )
  options$fixedVariables <- c("contBinom", "facGender")
  options$marginalMeans <-
    list(list(variable = "contBinom"), list(variable = "facGender"))
  options$marginalMeansDf <- "kenward-roger"
  options$method <- "LRT"
  options$plotAlpha <- 0.3
  options$plotDodge <- 0.5
  options$plotGeomWidth <- 0.5
  options$plotLegendPosition <- "left"
  options$plotRelativeSize <- 3
  options$plotsAgregatedOver <- "facFive"
  options$plotsBackgroundColor <- "red"
  options$plotsCImethod <- "mean"
  options$plotsEstimatesTable <- TRUE
  options$plotsGeom <- "geom_boxplot"
  options$plotsTrace <- list(list(variable = "facGender"))
  options$plotsX <- list(list(variable = "contBinom"))
  options$randomEffects <-
    list(list(
      correlations = TRUE,
      randomComponents = list(
        list(randomSlopes = TRUE,
             value = "contBinom"),
        list(randomSlopes = TRUE, value = "facGender"),
        list(
          randomSlopes = TRUE,
          value = c("contBinom", "facGender")
        )
      ),
      value = "facFive"
    ))
  options$randomVariables <- "facFive"
  options$showFE <- TRUE
  options$showRE <- TRUE
  options$test_intercept <- TRUE
  options$trendsContrasts <- list()
  options$trendsTrend <- list()
  options$type <- "2"
  set.seed(1)
  results <- jasptools::run("MixedModelsLMM", "debug.csv", options)
  
  
  test_that("ANOVA Summary table results match", {
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        1,
        "contBinom",
        0.594259549672169,
        0.283741155549308,
        1,
        "facGender",
        0.0479894980535518,
        3.91035364013146,
        1,
        "contBinom * facGender",
        0.407493772472993,
        0.686100933704324
      )
    )
  })
  
  test_that("Estimated Marginal Means table results match", {
    table <- results[["results"]][["EMMsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0,
        5.22380621312413,
        -0.284398533204062,
        "f",
        -0.861547572890637,
        0.227420385818257,
        0.292750506482514,
        1,
        3.95727907369226,
        -0.565077215422171,
        "f",
        -1.16558432613444,
        0.215366288629511,
        0.035429895290096,
        0,
        5.90534738559078,
        -0.0254223747670139,
        "m",
        -0.748708073581769,
        0.294443442637268,
        0.697863324047741,
        1,
        3.68570821909927,
        0.0938003506789995,
        "m",
        -0.681480405822668,
        0.269911046165921,
        0.869081107180667
      )
    )
  })
  
  test_that("Estimated Means and Confidence Intervals table results match",
            {
              table <- results[["results"]][["EstimatesTable"]][["data"]]
              expect_equal_tables(
                table,
                list(
                  0,
                  "f",
                  -0.829153266876443,
                  -0.284398533204062,
                  0.260356200468319,
                  1,
                  "f",
                  -0.715686985981864,
                  -0.565077215422171,
                  -0.414467444862479,
                  0,
                  "m",
                  -0.782327322440419,
                  -0.0254223747670139,
                  0.731482572906392,
                  1,
                  "m",
                  -0.229453171298025,
                  0.0938003506789995,
                  0.417053872656024
                )
              )
            })
  
  test_that("Fixed Effects Estimates table results match", {
    table <- results[["results"]][["FEsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        10.6747130383861,
        -0.154910453985538,
        0.304816027593772,
        0.143709513201973,-1.07794154008317,
        "Intercept",
        29.9033843330254,
        -0.080727978386048,
        0.704931861598136,
        0.211155921509588,
        -0.382314537091408,
        "contBinom",
        5.15987176298244,
        -0.129488079218524,
        0.506753904579425,
        0.181595535861779,-0.713057612369302,
        "facGender (1)",
        9.38141883599472,
        -0.199950703832061,
        0.412773348389811,
        0.233300370575153,
        -0.857052662793142,
        "contBinom * facGender (1)"
      )
    )
  })
  
  test_that("facFive: Correlation Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_CE1"]][["data"]]
    expect_equal_tables(
      table,
      list(
        1,
        "Intercept",
        -0.999999958391774,
        1,
        "contBinom",
        -0.999999972442334,
        0.999999863110391,
        1,
        "facGender (1)",
        0.999999987109231,
        -0.999999903707837,-0.999999993563937,
        1,
        "contBinom * facGender (1)"
      )
    )
  })
  
  test_that("Residual Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_RES1"]][["data"]]
    expect_equal_tables(table,
                        list(0.993799279678289, 0.99689481876389))
  })
  
  test_that("facFive: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0.117559865145434,
        0.0138203218930127,
        "Intercept",
        0.115308019488353,
        0.0132959393583263,
        "contBinom",
        0.276630286679234,
        0.0765243155082351,
        "facGender (1)",
        0.251611769807223,
        0.0633084827055231,
        "contBinom * facGender (1)"
      )
    )
  })
  
  test_that("Plot matches (trace & line, lot of customization)", {
    plotName <- results[["results"]][["plots"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot", dir = "MixedModelsLMM")
  })
}
### parametric bootstrap
{
  options <- jasptools::analysisOptions("MixedModelsLMM")
  options$.meta <-
    list(
      Contrasts = list(list(levels = list(containsColumn = TRUE)),
                       list(levels = list(containsColumn = TRUE)), list(levels = list(containsColumn = TRUE))),
      dependentVariable = list(containsColumn = TRUE),
      fixedEffects = list(list(), list(), list()),
      fixedVariables = list(containsColumn = TRUE),
      marginalMeans = list(list(),
                           list()),
      plotsAgregatedOver = list(containsColumn = TRUE),
      plotsPanel = list(list()),
      plotsX = list(list()),
      randomEffects = list(list(
        randomComponents = list(list(), list(), list()),
        value = list(containsColumn = TRUE)
      )),
      randomVariables = list(containsColumn = TRUE),
      trendsContrasts = list(list(levels = list(containsColumn = TRUE)))
    )
  options$Contrasts <-
    list(
      list(
        isContrast = FALSE,
        levels = c("2", "3", "4", "5"),
        name = "facExperim",
        values = c("control", "experimental",
                   "control", "experimental")
      ),
      list(
        isContrast = FALSE,
        levels = c("2",
                   "3", "4", "5"),
        name = "facGender",
        values = c("f", "f", "m",
                   "m")
      ),
      list(
        isContrast = TRUE,
        levels = c("2", "3", "4", "5"),
        name = "Contrast 1",
        values = c("0", "0", "0", "0")
      )
    )
  options$bootstrap_samples <- 3
  options$dependentVariable <- "contNormal"
  options$fixedEffects <-
    list(
      list(components = "facExperim"),
      list(components = "facGender"),
      list(components = c("facExperim", "facGender"))
    )
  options$fixedVariables <- c("facExperim", "facGender")
  options$marginalMeans <-
    list(list(variable = "facExperim"), list(variable = "facGender"))
  options$method <- "PB"
  options$plotLegendPosition <- "top"
  options$plotsAgregatedOver <- "facFive"
  options$plotsGeom <- "geom_count"
  options$plotsPanel <- list(list(variable = "facGender"))
  options$plotsTheme <- "jtools::theme_apa"
  options$plotsX <- list(list(variable = "facExperim"))
  options$randomEffects <-
    list(list(
      correlations = TRUE,
      randomComponents = list(
        list(randomSlopes = TRUE,
             value = "facExperim"),
        list(randomSlopes = FALSE, value = "facGender"),
        list(
          randomSlopes = FALSE,
          value = c("facExperim", "facGender")
        )
      ),
      value = "facFive"
    ))
  options$randomVariables <- "facFive"
  options$showFE <- TRUE
  options$showRE <- TRUE
  options$trendsContrasts <-
    list(list(
      isContrast = TRUE,
      levels = list(),
      name = "Contrast 1",
      values = list()
    ))
  options$trendsTrend <- list()
  set.seed(1)
  results <- jasptools::run("MixedModelsLMM", "debug.csv", options)
  
  
  test_that("ANOVA Summary table results match", {
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        1,
        "facExperim",
        0.987006987343789,
        1,
        0.000265202730588499,
        1,
        "facGender",
        0.0212909570439191,
        0.25,
        5.30281759935127,
        1,
        "facExperim * facGender",
        0.519959702074162,
        1,
        0.413973242932229
      )
    )
  })
  
  test_that("Estimated Marginal Means table results match", {
    table <- results[["results"]][["EMMsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "<unicode><unicode><unicode>",
        -0.501117515024744,
        "control",
        "f",
        -0.903021075176726,
        0.205056604775469,
        -0.0992139548727615,
        "<unicode><unicode><unicode>",
        -0.333493266062358,
        "experimental",
        "f",
        -0.794798623615014,
        0.235364201174804,
        0.127812091490298,
        "<unicode><unicode><unicode>",
        0.155416154415123,
        "control",
        "m",
        -0.306282676200741,
        0.23556495642659,
        0.617114985030986,
        "<unicode><unicode><unicode>",
        -0.0207872921962234,
        "experimental",
        "m",
        -0.422453858273081,
        0.204935687209128,
        0.380879273880634
      )
    )
  })
  
  test_that("Fixed Effects Estimates table results match", {
    table <- results[["results"]][["FEsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        94.9540684343928,
        -0.174995479717051,
        0.0897367191570443,
        0.102080062246313,-1.71429636567811,
        "Intercept",
        5.06012086512521,
        0.00214479941224004,
        0.987629022167055,
        0.13169726781181,
        0.0162858307379988,
        "facExperim (1)",
        96.0669878122869,
        -0.2423099108265,
        0.0198456107652341,
        0.102290848654522,-2.3688327354178,
        "facGender (1)",
        97.0329518229423,
        -0.085956923893433,
        0.403737536752817,
        0.102496700241713,
        -0.838631133399661,
        "facExperim (1) * facGender (1)"
      )
    )
  })
  
  test_that("facFive: Correlation Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_CE1"]][["data"]]
    expect_equal_tables(table,
                        list(1, "Intercept", "NaN", 1, "facExperim (1)"))
  })
  
  test_that("Residual Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_RES1"]][["data"]]
    expect_equal_tables(table,
                        list(1.00754138500927, 1.0151396425064))
  })
  
  test_that("facFive: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0,
        0,
        "Intercept",
        0.186076737267706,
        0.0346245521521951,
        "facExperim (1)"
      )
    )
  })
  
  test_that("Plot matches (trace + separate plots, count)", {
    plotName <- results[["results"]][["plots"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot", dir = "MixedModelsLMM")
  })
}