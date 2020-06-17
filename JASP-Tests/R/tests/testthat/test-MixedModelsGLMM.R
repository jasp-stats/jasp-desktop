context("Generalized Linear Mixed Models")

# the tests do not cover all links / families
# the tests are grouped by test model terms methods - they lead to differently structured types models structures
# the library("afex") call is needed to prevent crashing afex after being called for a second time using JASP
library("afex")

### binomial + logit, default, all selected output using LRT method
{
  options <- jasptools::analysisOptions("MixedModelsGLMM")
  options$.meta <-
    list(
      Contrasts = list(list(levels = list(containsColumn = TRUE)),
                       list(levels = list(containsColumn = TRUE)), list(levels = list(containsColumn = TRUE))),
      dependentVariable = list(containsColumn = TRUE),
      dependentVariableAggregation = list(containsColumn = TRUE),
      fixedEffects = list(list(), list(), list()),
      fixedVariables = list(containsColumn = TRUE),
      marginalMeans = list(list(),
                           list()),
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
        name = "contNormal",
        values = c("-1", "0", "1", "-1",
                   "0", "1")
      ),
      list(
        isContrast = FALSE,
        levels = c("2", "3", "4",
                   "5", "6", "7"),
        name = "facGender",
        values = c("f", "f", "f",
                   "m", "m", "m")
      ),
      list(
        isContrast = TRUE,
        levels = c("2", "3",
                   "4", "5", "6", "7"),
        name = "Contrast 1",
        values = c("-1", "1",
                   "0", "0", "0", "0")
      )
    )
  options$dependentVariable <- "contBinom"
  options$fixedEffects <-
    list(
      list(components = "contNormal"),
      list(components = "facGender"),
      list(components = c("contNormal", "facGender"))
    )
  options$fixedVariables <- c("contNormal", "facGender")
  options$marginalMeans <-
    list(list(variable = "contNormal"), list(variable = "facGender"))
  options$marginalMeansCompare <- TRUE
  options$marginalMeansContrast <- TRUE
  options$plotsAgregatedOver <- "facFive"
  options$plotsEstimatesTable <- TRUE
  options$plotsX <- list(list(variable = "facGender"))
  options$pvalVS <- TRUE
  options$randomEffects <-
    list(list(
      correlations = TRUE,
      randomComponents = list(
        list(randomSlopes = TRUE,
             value = "contNormal"),
        list(randomSlopes = TRUE, value = "facGender"),
        list(
          randomSlopes = TRUE,
          value = c("contNormal", "facGender")
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
        name = "facGender",
        values = c("f", "m")
      ),
      list(
        isContrast = TRUE,
        levels = c("2",
                   "3"),
        name = "Contrast 1",
        values = c("-1", "1")
      )
    )
  options$trendsTrend <- list(list(variable = "contNormal"))
  options$trendsVariables <- list(list(variable = "facGender"))
  set.seed(1)
  results <- jasptools::run("MixedModelsGLMM", "debug.csv", options)
  
  
  test_that("ANOVA Summary table results match", {
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        1,
        "contNormal",
        0.338192417441406,
        1.00335860195685,
        0.917271466382658,
        1,
        "facGender",
        0.282211189851148,
        1.03040171756559,
        1.15641063028642,
        1,
        "contNormal * facGender",
        0.234356656714998,
        1.0819007106942,
        1.41422274372206
      )
    )
  })
  
  test_that("Estimated Marginal Means table results match", {
    table <- results[["results"]][["EMMsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        -1.24716219673316,
        "<unicode><unicode><unicode>",
        0.548980002230091,
        "f",
        0.359266674805453,
        1,
        0.619182221919005,
        1,
        0.0979177129958996,
        0.497009684592554,
        0.725448070897048,-0.18874858754,
        "<unicode><unicode><unicode>",
        0.457645840995902,
        "f",
        0.317162773357282,
        2,
        0.577173573677129,
        1,
        0.0756052005918349,-0.557517986317609,
        0.605203464795789,
        0.869665021653162,
        "<unicode><unicode><unicode>",
        0.369072572161133,
        "f",
        0.172245883885305,
        3,
        0.309263365357224,
        1.01361022631892,
        0.122798690344828,-1.01676893102908,
        0.621847666607969,-1.24716219673316,
        "<unicode><unicode><unicode>",
        0.326824366323759,
        "m",
        0.159666179565096,
        4,
        0.131135898133824,
        1.38090112997943,
        0.105307192129248,-1.50963769193164,
        0.55367914707792,-0.18874858754,
        "<unicode><unicode><unicode>",
        0.361230317280148,
        "m",
        0.220448118954088,
        5,
        0.106939162958909,
        1.53884543482584,
        0.0815889967831485,-1.61210467797629,
        0.530711404805015,
        0.869665021653162,
        "<unicode><unicode><unicode>",
        0.39712158280474,
        "m",
        0.197013437306204,
        6,
        0.407380876646393,
        1,
        0.120637723373079,
        -0.828511514580678,
        0.638790847032052
      )
    )
  })
  
  test_that("Estimated Means and Confidence Intervals table results match",
            {
              table <- results[["results"]][["EstimatesTable"]][["data"]]
              expect_equal_tables(
                table,
                list(
                  "f",
                  0.317162773357282,
                  0.457645840995902,
                  0.605203464795789,
                  "m",
                  0.220448118954088,
                  0.361230317280148,
                  0.530711404805015
                )
              )
            })
  
  test_that("Fixed Effects Estimates table results match", {
    table <- results[["results"]][["FEsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        -0.388991227896234,
        0.123567020121818,
        1.42381908821809,
        0.252596192372349,
        -1.5399726505886,
        "Intercept",-0.101009093160074,
        0.66130176857207,
        1,
        0.230552997518184,-0.438116590317188,
        "contNormal",
        0.153831572502025,
        0.513634671798847,
        1,
        0.235508645851556,
        0.653188641740935,
        "facGender (1)",-0.2451447528175,
        0.293160000721356,
        1.02268818093942,
        0.233201416066435,-1.0512146836521,
        "contNormal * facGender (1)"
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
        0.999942822682058,
        1,
        "contNormal",-0.99997620082427,
        -0.999898648917715,
        1,
        "facGender (1)",-0.999951851698203,
        -0.999928770367823,
        0.999929670653158,
        1,
        "contNormal * facGender (1)"
      )
    )
  })
  
  test_that("Residual Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_RES1"]][["data"]]
    expect_equal_tables(table,
                        list(1, 1))
  })
  
  test_that("facFive: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0.260557712569343,
        0.0678903215793681,
        "Intercept",
        0.110637222348517,
        0.0122405949689952,
        "contNormal",
        0.157061099321917,
        0.024668188920209,
        "facGender (1)",
        0.114586125611911,
        0.0131299801827487,
        "contNormal * facGender (1)"
      )
    )
  })
  
  test_that("Contrasts table results match", {
    table <- results[["results"]][["contrasts_Means"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "Contrast 1",
        "<unicode><unicode><unicode>",-0.0913341612341894,
        0.296040158859129,
        1.02087244264879,
        0.0874042219204176,-1.04496280874567
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
        0.490289505634998,
        0.293160000721356,
        1.02268818093942,
        0.466402832132868,
        1.0512146836521
      )
    )
  })
  
  test_that("Plot matches", {
    plotName <- results[["results"]][["plots"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot1", dir = "MixedModelsGLMM")
  })
  
  test_that("Estimated Trends table results match", {
    table <- results[["results"]][["trendsSummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "<unicode><unicode><unicode>",
        "f",-0.999453542329123,
        1,
        0.299039232332829,
        1.0190726276443,
        0.333322296483351,-0.346153845977573,-1.03849592310385,
        0.307145850373978,
        "<unicode><unicode><unicode>",
        "m",-0.487846869039201,
        2,
        0.654869905635109,
        1,
        0.322445990682291,
        0.144135659657426,
        0.44700713862944,
        0.776118188354053
      )
    )
  })
}
### binomial + probit, type II with LRT, no random slopes, custom options
{
  options <- jasptools::analysisOptions("MixedModelsGLMM")
  options$.meta <-
    list(
      Contrasts = list(
        list(levels = list(containsColumn = TRUE)),
        list(levels = list(containsColumn = TRUE)),
        list(levels = list(containsColumn = TRUE)),
        list(levels = list(containsColumn = TRUE))
      ),
      dependentVariable = list(containsColumn = TRUE),
      dependentVariableAggregation = list(containsColumn = TRUE),
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
        name = "contNormal",
        values = c("-0.99", "0", "0.99",
                   "-0.99", "0", "0.99")
      ),
      list(
        isContrast = FALSE,
        levels = c("2",
                   "3", "4", "5", "6", "7"),
        name = "facGender",
        values = c("f",
                   "f", "f", "m", "m", "m")
      ),
      list(
        isContrast = TRUE,
        levels = c("2",
                   "3", "4", "5", "6", "7"),
        name = "Contrast 1",
        values = c("1",
                   "1", "1", "0", "0", "0")
      ),
      list(
        isContrast = TRUE,
        levels = c("2",
                   "3", "4", "5", "6", "7"),
        name = "Contrast 2",
        values = c("0",
                   "1", "-1", "0", "0", "0")
      )
    )
  options$dependentVariable <- "contBinom"
  options$fixedEffects <-
    list(
      list(components = "contNormal"),
      list(components = "facGender"),
      list(components = c("contNormal", "facGender"))
    )
  options$fixedVariables <- c("contNormal", "facGender")
  options$link <- "probit"
  options$marginalMeans <-
    list(list(variable = "contNormal"), list(variable = "facGender"))
  options$marginalMeansAdjustment <- "mvt"
  options$marginalMeansCompare <- TRUE
  options$marginalMeansCompareTo <- 0.122
  options$marginalMeansContrast <- TRUE
  options$marginalMeansResponse <- FALSE
  options$marginalMeansSD <- 0.99
  options$plotsAgregatedOver <- "facFive"
  options$plotsBackgroundColor <- "none"
  options$plotsCImethod <- "between"
  options$plotsEstimatesTable <- TRUE
  options$plotsGeom <- "geom_boxjitter"
  options$plotsMappingColor <- TRUE
  options$plotsTheme <- "theme_minimal"
  options$plotsX <- list(list(variable = "facGender"))
  options$pvalVS <- TRUE
  options$randomEffects <-
    list(list(
      correlations = TRUE,
      randomComponents = list(
        list(randomSlopes = FALSE,
             value = "contNormal"),
        list(randomSlopes = FALSE, value = "facGender"),
        list(
          randomSlopes = FALSE,
          value = c("contNormal", "facGender")
        )
      ),
      value = "facFive"
    ))
  options$randomVariables <- "facFive"
  options$showFE <- TRUE
  options$showRE <- TRUE
  options$trendsAdjustment <- "none"
  options$trendsCompare <- TRUE
  options$trendsCompareTo <- 0.035
  options$trendsContrast <- TRUE
  options$trendsContrasts <-
    list(
      list(
        isContrast = FALSE,
        levels = c("2", "3"),
        name = "facGender",
        values = c("f", "m")
      ),
      list(
        isContrast = TRUE,
        levels = c("2",
                   "3"),
        name = "Contrast 1",
        values = c("-1", "1")
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
        values = c("1", "-1")
      )
    )
  options$trendsTrend <- list(list(variable = "contNormal"))
  options$trendsVariables <- list(list(variable = "facGender"))
  options$type <- "2"
  set.seed(1)
  results <- jasptools::run("MixedModelsGLMM", "debug.csv", options)
  
  
  test_that("ANOVA Summary table results match", {
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        1,
        "contNormal",
        0.609594107923892,
        1,
        0.260766411860686,
        1,
        "facGender",
        0.284327625868667,
        1.0288077546893,
        1.14628764586072,
        1,
        "contNormal * facGender",
        0.334577761935427,
        1.00424473123695,
        0.931098042304626
      )
    )
  })
  
  test_that("Estimated Marginal Means table results match  (not on the response scale)",
            {
              table <- results[["results"]][["EMMsummary"]][["data"]]
              expect_equal_tables(
                table,
                list(
                  -1.23657806064123,
                  "<unicode><unicode><unicode>",
                  0.122014229765717,
                  "f",-0.350848381425716,
                  1,
                  0.999952940108025,
                  1,
                  0.2412608675064,
                  5.89808279474833e-05,
                  0.59487684095715,-0.18874858754,
                  "<unicode><unicode><unicode>",
                  -0.102631617037894,
                  "f",-0.467214155012724,
                  2,
                  0.227201152552225,
                  1.09262307856175,
                  0.186014917034502,-1.20760001734823,
                  0.261950920936935,
                  0.859080885561231,
                  "<unicode><unicode><unicode>",-0.327277463841505,
                  "f",-0.956373879197792,
                  3,
                  0.161592985291706,
                  1.24903292376792,
                  0.320973456817839,-1.39973401008197,
                  0.301818951514782,-1.23657806064123,
                  "<unicode><unicode><unicode>",-0.403545944379684,
                  "m",-0.945258662957299,
                  4,
                  0.0572403236944089,
                  2.24678745672641,
                  0.276389118805537,
                  -1.90147118182844,
                  0.13816677419793,-0.18874858754,
                  "<unicode><unicode><unicode>",
                  -0.366884673848823,
                  "m",-0.730935587943824,
                  5,
                  0.00848740169016926,
                  9.08840646780043,
                  0.185743675376991,-2.63203941052942,-0.00283375975382183,
                  0.859080885561231,
                  "<unicode><unicode><unicode>",-0.330223403317961,
                  "m",-0.767212798610842,
                  6,
                  0.0425305742745752,
                  2.73940662296884,
                  0.222957870011795,-2.02829083043374,
                  0.10676599197492
                )
              )
            })
  
  test_that("Estimated Means and Confidence Intervals table results match",
            {
              table <- results[["results"]][["EstimatesTable"]][["data"]]
              expect_equal_tables(
                table,
                list(
                  "f",
                  0.375517529884352,
                  0.459127674282166,
                  0.54273781867998,
                  "m",
                  0.129912911781635,
                  0.356852521999701,
                  0.583792132217766
                )
              )
            })
  
  test_that("Fixed Effects Estimates table results match", {
    table <- results[["results"]][["FEsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        -0.251689251201327,
        0.0627285967943076,
        2.11800381519144,
        0.135236071846752,
        -1.86111033664552,
        "Intercept",-0.0897018938188371,
        0.491183326203148,
        1,
        0.130299610105594,-0.688427952671103,
        "contNormal",
        0.108591520764834,
        0.421987737684679,
        1,
        0.135236068683755,
        0.802977503130257,
        "facGender (1)",-0.124689715284057,
        0.338594418466376,
        1.00326670085556,
        0.130299610691309,-0.956946184432264,
        "contNormal * facGender (1)"
      )
    )
  })
  
  test_that("Residual Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_RES1"]][["data"]]
    expect_equal_tables(table,
                        list(1, 1))
  })
  
  test_that("facFive: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
    expect_equal_tables(table,
                        list(0, 0, "Intercept"))
  })
  
  test_that("Contrasts table results match (not on the response scale)", {
    table <- results[["results"]][["contrasts_Means"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "Contrast 1",
        "<unicode><unicode><unicode>",-0.307894851113682,
        0.818626778359989,
        1,
        0.558044751103505,-0.551738638352634,
        "Contrast 2",
        "<unicode><unicode><unicode>",
        0.224645846803611,
        0.493531265603694,
        1,
        0.214508353511966,
        1.04725920051911
      )
    )
  })
  
  test_that("Contrasts table results match (row 2 empty)", {
    table <- results[["results"]][["contrasts_Trends"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "Contrast 1",
        "<unicode><unicode><unicode>",
        0.249379430568114,
        0.338594418466376,
        1.00326670085556,
        0.260599221382617,
        0.956946184432264,
        "Contrast 3",
        "<unicode><unicode><unicode>",-0.249379430568114,
        0.338594418466376,
        1.00326670085556,
        0.260599221382617,-0.956946184432264
      )
    )
  })
  
  test_that("Plot matches (no background color - should match the factor levels color)",
            {
              plotName <- results[["results"]][["plots"]][["data"]]
              testPlot <-
                results[["state"]][["figures"]][[plotName]][["obj"]]
              expect_equal_plots(testPlot, "plot2", dir = "MixedModelsGLMM")
            })
  
  test_that("Estimated Trends table results match (compare to not null)", {
    table <- results[["results"]][["trendsSummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "<unicode><unicode><unicode>",
        "f",-0.615629270439244,
        1,
        0.223137703625611,
        1.09913512787484,
        0.204716854238783,-0.214391609102894,-1.21822704842857,
        0.186846052233456,
        "<unicode><unicode><unicode>",
        "m",-0.281064885196269,
        2,
        0.999939740755991,
        1,
        0.161254344036152,
        0.0349878214652202,
        -7.55237624920116e-05,
        0.351040528126709
      )
    )
  })
}
### gamma + log, parametric bootsrap, no correlation
{
  options <- jasptools::analysisOptions("MixedModelsGLMM")
  options$.meta <-
    list(
      Contrasts = list(list(levels = list(containsColumn = TRUE)),
                       list(levels = list(containsColumn = TRUE)), list(levels = list(containsColumn = TRUE))),
      dependentVariable = list(containsColumn = TRUE),
      dependentVariableAggregation = list(containsColumn = TRUE),
      fixedEffects = list(list(), list(), list()),
      fixedVariables = list(containsColumn = TRUE),
      marginalMeans = list(list()),
      plotsAgregatedOver = list(containsColumn = TRUE),
      plotsTrace = list(list()),
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
      trendsVariables = list(list())
    )
  options$Contrasts <-
    list(
      list(
        isContrast = FALSE,
        levels = c("2", "3"),
        name = "facGender",
        values = c("f", "m")
      ),
      list(
        isContrast = TRUE,
        levels = c("2",
                   "3"),
        name = "Contrast 1",
        values = c("1", "0")
      ),
      list(
        isContrast = TRUE,
        levels = c("2", "3"),
        name = "Contrast 2",
        values = c("0",
                   "0")
      )
    )
  options$bootstrap_samples <- 3
  options$dependentVariable <- "contGamma"
  options$family <- "Gamma"
  options$fixedEffects <-
    list(
      list(components = "facGender"),
      list(components = "contBinom"),
      list(components = c("facGender", "contBinom"))
    )
  options$fixedVariables <- c("facGender", "contBinom")
  options$link <- "log"
  options$marginalMeans <- list(list(variable = "facGender"))
  options$marginalMeansCompareTo <- 0.122
  options$marginalMeansSD <- 0.99
  options$method <- "PB"
  options$plotLegendPosition <- "right"
  options$plotsAgregatedOver <- "facFive"
  options$plotsBackgroundColor <- "black"
  options$plotsCImethod <- "between"
  options$plotsGeom <- "geom_violin"
  options$plotsMappingColor <- TRUE
  options$plotsMappingFill <- TRUE
  options$plotsTheme <- "theme_minimal"
  options$plotsTrace <- list(list(variable = "contBinom"))
  options$plotsX <- list(list(variable = "facGender"))
  options$randomEffects <-
    list(list(
      correlations = FALSE,
      randomComponents = list(
        list(randomSlopes = FALSE, value = "facGender"),
        list(randomSlopes = TRUE,
             value = "contBinom"),
        list(
          randomSlopes = TRUE,
          value = c("facGender",
                    "contBinom")
        )
      ),
      value = "facFive"
    ))
  options$randomVariables <- "facFive"
  options$showFE <- TRUE
  options$showRE <- TRUE
  options$test_intercept <- TRUE
  options$trendsAdjustment <- "none"
  options$trendsCompareTo <- 0.035
  options$trendsContrasts <-
    list(
      list(
        isContrast = FALSE,
        levels = c("2", "3"),
        name = "facGender",
        values = c("f", "m")
      ),
      list(
        isContrast = TRUE,
        levels = c("2",
                   "3"),
        name = "Contrast 1",
        values = c("-1", "1")
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
        values = c("1", "-1")
      )
    )
  options$trendsTrend <- list()
  options$trendsVariables <- list(list(variable = "facGender"))
  set.seed(1)
  results <- jasptools::run("MixedModelsGLMM", "debug.csv", options)
  
  
  test_that("ANOVA Summary table results match", {
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        1,
        "Intercept",
        0.0013877183345834,
        0.25,
        10.2220458524067,
        1,
        "facGender",
        0.687415043869772,
        1,
        0.161898009277024,
        1,
        "contBinom",
        0.377048727120632,
        0.666666666666667,
        0.780302216698828,
        1,
        "facGender * contBinom",
        0.62620727849907,
        1,
        0.237239173930675
      )
    )
  })
  
  test_that("Estimated Marginal Means table results match", {
    table <- results[["results"]][["EMMsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "<unicode><unicode><unicode>",
        1.7872811431656,
        "f",
        1.42773108910807,
        0.204819815398267,
        2.23737782911972,
        "<unicode><unicode><unicode>",
        2.06847415166258,
        "m",
        1.44738719614938,
        0.376818763752598,
        2.95607514525414
      )
    )
  })
  
  test_that("Fixed Effects Estimates table results match", {
    table <- results[["results"]][["FEsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0.732040338231203,
        8.43369931802604e-09,
        0.127100200764652,
        5.75955296551187,
        "Intercept",-0.0398848893962455,
        0.709315586393992,
        0.10699441415504,
        -0.372775436093799,
        "facGender (1)",-0.156573915095795,
        0.390029766375323,
        0.182155170886762,-0.859563383974045,
        "contBinom",-0.0663458811739536,
        0.677839113491156,
        0.159710401088987,-0.415413653222167,
        "facGender (1) * contBinom"
      )
    )
  })
  
  test_that("facFive.1: Correlation Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_CE2"]][["data"]]
    expect_equal_tables(table,
                        list(1, "facGender (f)", 0.691720915042447, 1, "facGender (m)"))
  })
  
  test_that("facFive.3: Correlation Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_CE4"]][["data"]]
    expect_equal_tables(
      table,
      list(
        1,
        "facGender (f) * contBinom",
        0.964084647766179,
        1,
        "facGender (m) * contBinom"
      )
    )
  })
  
  test_that("Residual Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_RES4"]][["data"]]
    expect_equal_tables(table,
                        list(0.693571509355682, 0.481041438589919))
  })
  
  test_that("facFive: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
    expect_equal_tables(table,
                        list(0.0506454268598145, 0.00256495926181282, "Intercept"))
  })
  
  test_that("facFive.1: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE2"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0.0815586104017389,
        0.00665180693066264,
        "facGender (f)",
        0.207560327706129,
        0.0430812896374757,
        "facGender (m)"
      )
    )
  })
  
  test_that("facFive.2: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE3"]][["data"]]
    expect_equal_tables(table,
                        list(0.018316119397551, 0.000335480229785343, "contBinom"))
  })
  
  test_that("facFive.3: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE4"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0.0546952054041368,
        0.00299156549420072,
        "facGender (f) * contBinom",
        0.24832485530695,
        0.0616652337632174,
        "facGender (m) * contBinom"
      )
    )
  })
  
  test_that("Plot matches", {
    plotName <- results[["results"]][["plots"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot3", dir = "MixedModelsGLMM")
  })
}
### inverse gaussian + log, type II parametric bootsrap
{
  options <- jasptools::analysisOptions("MixedModelsGLMM")
  options$.meta <-
    list(
      Contrasts = list(list(levels = list(containsColumn = TRUE)),
                       list(levels = list(containsColumn = TRUE)), list(levels = list(containsColumn = TRUE))),
      dependentVariable = list(containsColumn = TRUE),
      dependentVariableAggregation = list(containsColumn = TRUE),
      fixedEffects = list(list()),
      fixedVariables = list(containsColumn = TRUE),
      marginalMeans = list(list()),
      plotsAgregatedOver = list(containsColumn = TRUE),
      plotsX = list(list()),
      randomEffects = list(list(
        randomComponents = list(list()),
        value = list(containsColumn = TRUE)
      )),
      randomVariables = list(containsColumn = TRUE),
      trendsContrasts = list(list(levels = list(containsColumn = TRUE)), list(levels = list(containsColumn = TRUE)),
                             list(levels = list(containsColumn = TRUE)))
    )
  options$Contrasts <-
    list(
      list(
        isContrast = FALSE,
        levels = c("2", "3"),
        name = "facGender",
        values = c("f", "m")
      ),
      list(
        isContrast = TRUE,
        levels = c("2",
                   "3"),
        name = "Contrast 1",
        values = c("0", "0")
      ),
      list(
        isContrast = TRUE,
        levels = c("2", "3"),
        name = "Contrast 2",
        values = c("0",
                   "0")
      )
    )
  options$bootstrap_samples <- 3
  options$dependentVariable <- "contGamma"
  options$family <- "inverse.gaussian"
  options$fixedEffects <- list(list(components = "facGender"))
  options$fixedVariables <- "facGender"
  options$link <- "log"
  options$marginalMeans <- list(list(variable = "facGender"))
  options$marginalMeansCompareTo <- 0.122
  options$marginalMeansResponse <- FALSE
  options$marginalMeansSD <- 0.99
  options$method <- "PB"
  options$plotLegendPosition <- "right"
  options$plotsAgregatedOver <- "facFive"
  options$plotsBackgroundColor <- "black"
  options$plotsCImethod <- "none"
  options$plotsGeom <- "geom_boxplot"
  options$plotsMappingFill <- TRUE
  options$plotsMappingLineType <- FALSE
  options$plotsMappingShape <- FALSE
  options$plotsTheme <- "ggpubr::theme_pubr"
  options$plotsX <- list(list(variable = "facGender"))
  options$randomEffects <-
    list(list(
      correlations = FALSE,
      randomComponents = list(list(
        randomSlopes = FALSE, value = "facGender"
      )),
      value = "facFive"
    ))
  options$randomVariables <- "facFive"
  options$showFE <- TRUE
  options$showRE <- TRUE
  options$trendsAdjustment <- "none"
  options$trendsCompareTo <- 0.035
  options$trendsContrasts <-
    list(
      list(
        isContrast = TRUE,
        levels = list(),
        name = "Contrast 1",
        values = list()
      ),
      list(
        isContrast = TRUE,
        levels = list(),
        name = "Contrast 2",
        values = list()
      ),
      list(
        isContrast = TRUE,
        levels = list(),
        name = "Contrast 3",
        values = list()
      )
    )
  options$trendsTrend <- list()
  options$type <- "2"
  set.seed(1)
  results <- jasptools::run("MixedModelsGLMM", "debug.csv", options)
  
  
  test_that("ANOVA Summary table results match", {
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list(1, "facGender", 0.294513703936202, 0.25, 1.09887237564914))
  })
  
  test_that("Estimated Marginal Means table results match", {
    table <- results[["results"]][["EMMsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "<unicode><unicode><unicode>",
        0.593497166174035,
        "f",
        0.548955859240753,
        0.022725574186372,
        0.638038473107318,
        "<unicode><unicode><unicode>",
        0.788569126981278,
        "m",
        0.744653554961215,
        0.02240631581318,
        0.832484699001341
      )
    )
  })
  
  test_that("Fixed Effects Estimates table results match", {
    table <- results[["results"]][["FEsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0.691033146577657,
        0,
        0.0159669120572628,
        43.2790726284065,
        "Intercept",
        -0.0975359804036214,
        9.57851225519835e-10,
        0.0159469456186997,
        -6.11627974006816,
        "facGender (1)"
      )
    )
  })
  
  test_that("Residual Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_RES1"]][["data"]]
    expect_equal_tables(table,
                        list(0.512833445264655, 0.716123903570224))
  })
  
  test_that("facFive: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
    expect_equal_tables(table,
                        list(0.0771013537793761, 0.00594461875461251, "Intercept"))
  })
  
  test_that("Plot matches", {
    plotName <- results[["results"]][["plots"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot4", dir = "MixedModelsGLMM")
  })
}
### poisson + sqrt
{
  test_that("ANOVA Summary table results match", {
    options <- jasptools::analysisOptions("MixedModelsGLMM")
    options$.meta <-
      list(
        dependentVariable = list(containsColumn = TRUE),
        dependentVariableAggregation = list(containsColumn = TRUE),
        fixedEffects = list(list()),
        fixedVariables = list(containsColumn = TRUE),
        plotsAgregatedOver = list(containsColumn = TRUE),
        randomEffects = list(list(
          randomComponents = list(list()),
          value = list(containsColumn = TRUE)
        )),
        randomVariables = list(containsColumn = TRUE)
      )
    options$Contrasts <- list()
    options$dependentVariable <- "facFive"
    options$family <- "poisson"
    options$fixedEffects <- list(list(components = "contBinom"))
    options$fixedVariables <- "contBinom"
    options$link <- "sqrt"
    options$plotsAgregatedOver <- "facGender"
    options$randomEffects <-
      list(list(
        correlations = TRUE,
        randomComponents = list(list(
          randomSlopes = TRUE,
          value = "contBinom"
        )),
        value = "facGender"
      ))
    options$randomVariables <- "facGender"
    options$trendsContrasts <- list()
    options$trendsTrend <- list()
    set.seed(1)
    results <-
      jasptools::run("MixedModelsGLMM", "debug.csv", options)
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(table,
                        list(1, "contBinom", 0.411589553280221, 0.674204508661546))
  })
}
### aggregated binomial
{
  options <- jasptools::analysisOptions("MixedModelsGLMM")
  options$.meta <-
    list(
      Contrasts = list(list(levels = list(containsColumn = TRUE)),
                       list(levels = list(containsColumn = TRUE)), list(levels = list(containsColumn = TRUE))),
      dependentVariable = list(containsColumn = TRUE),
      dependentVariableAggregation = list(containsColumn = TRUE),
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
      randomVariables = list(containsColumn = TRUE),
      trendsContrasts = list(list(levels = list(containsColumn = TRUE)))
    )
  options$Contrasts <-
    list(
      list(
        isContrast = FALSE,
        levels = c("2", "3", "4", "5",
                   "6", "7"),
        name = "cA",
        values = c("1", "2", "1", "2", "1", "2")
      ),
      list(
        isContrast = FALSE,
        levels = c("2", "3", "4", "5", "6",
                   "7"),
        name = "cB",
        values = c("1", "1", "2", "2", "3", "3")
      ),
      list(
        isContrast = TRUE,
        levels = c("2", "3", "4", "5", "6",
                   "7"),
        name = "Contrast 1",
        values = c("0", "0", "0", "0",
                   "0", "0")
      )
    )
  options$dependentVariable <- "binom_mean"
  options$dependentVariableAggregation <- "rep"
  options$family <- "binomial_agg"
  options$fixedEffects <-
    list(list(components = "cA"),
         list(components = "cB"),
         list(components = c("cA",
                             "cB")))
  options$fixedVariables <- c("cA", "cB")
  options$marginalMeans <-
    list(list(variable = "cA"), list(variable = "cB"))
  options$plotsAgregatedOver <- "id"
  options$plotsTrace <- list(list(variable = "cB"))
  options$plotsX <- list(list(variable = "cA"))
  options$randomEffects <-
    list(list(
      correlations = TRUE,
      randomComponents = list(
        list(randomSlopes = FALSE,
             value = "cA"),
        list(randomSlopes = FALSE, value = "cB"),
        list(randomSlopes = FALSE, value = c("cA", "cB"))
      ),
      value = "id"
    ))
  options$randomVariables <- "id"
  options$trendsContrasts <-
    list(list(
      isContrast = TRUE,
      levels = list(),
      name = "Contrast 1",
      values = list()
    ))
  options$trendsTrend <- list()
  set.seed(1)
  dataset <-
    structure(
      list(
        id = c(
          1L,
          2L,
          3L,
          4L,
          5L,
          6L,
          7L,
          8L,
          9L,
          10L,
          11L,
          12L,
          13L,
          14L,
          15L,
          16L,
          17L,
          18L,
          19L,
          20L,
          21L,
          22L,
          23L,
          24L,
          25L,
          1L,
          2L,
          3L,
          4L,
          5L,
          6L,
          7L,
          8L,
          9L,
          10L,
          11L,
          12L,
          13L,
          14L,
          15L,
          16L,
          17L,
          18L,
          19L,
          20L,
          21L,
          22L,
          23L,
          24L,
          25L,
          1L,
          2L,
          3L,
          4L,
          5L,
          6L,
          7L,
          8L,
          9L,
          10L,
          11L,
          12L,
          13L,
          14L,
          15L,
          16L,
          17L,
          18L,
          19L,
          20L,
          21L,
          22L,
          23L,
          24L,
          25L,
          1L,
          2L,
          3L,
          4L,
          5L,
          6L,
          7L,
          8L,
          9L,
          10L,
          11L,
          12L,
          13L,
          14L,
          15L,
          16L,
          17L,
          18L,
          19L,
          20L,
          21L,
          22L,
          23L,
          24L,
          25L,
          1L,
          2L,
          3L,
          4L,
          5L,
          6L,
          7L,
          8L,
          9L,
          10L,
          11L,
          12L,
          13L,
          14L,
          15L,
          16L,
          17L,
          18L,
          19L,
          20L,
          21L,
          22L,
          23L,
          24L,
          25L,
          1L,
          2L,
          3L,
          4L,
          5L,
          6L,
          7L,
          8L,
          9L,
          10L,
          11L,
          12L,
          13L,
          14L,
          15L,
          16L,
          17L,
          18L,
          19L,
          20L,
          21L,
          22L,
          23L,
          24L,
          25L
        ),
        cA = structure(
          c(
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L
          ),
          .Label = c("1", "2"),
          class = "factor"
        ),
        cB = structure(
          c(
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L,
            3L
          ),
          .Label = c("1", "2", "3"),
          class = "factor"
        ),
        binom_mean = c(
          0.6,
          1,
          0.6,
          0.8,
          0.8,
          0.6,
          0.2,
          0.2,
          0.4,
          0.6,
          0.4,
          0.6,
          0.6,
          0.6,
          0,
          0.6,
          0.8,
          0.4,
          0.6,
          0.2,
          0.6,
          0.6,
          1,
          0.6,
          0.4,
          0.6,
          0.6,
          0,
          0.6,
          0.8,
          1,
          1,
          0.4,
          0.8,
          0.6,
          0.6,
          0.4,
          0.8,
          0.4,
          0,
          0.4,
          0.2,
          0.4,
          0.8,
          0.6,
          0.8,
          0.6,
          0.8,
          1,
          0.8,
          0.6,
          0.6,
          0.2,
          0.8,
          0.6,
          1,
          0.8,
          0.2,
          0.4,
          0.6,
          0.6,
          0.4,
          0.4,
          0.2,
          0.2,
          0.4,
          0.4,
          0.4,
          0.6,
          0.2,
          0.6,
          0.6,
          0.6,
          0.4,
          0.6,
          0.2,
          0.8,
          0.2,
          0.6,
          0.4,
          0.8,
          0.6,
          0.6,
          0.8,
          0.4,
          0.6,
          0.8,
          0.6,
          0.4,
          0.6,
          0.4,
          0.8,
          0.8,
          0.8,
          0.6,
          0.4,
          1,
          0.6,
          0.6,
          0.4,
          0,
          0.4,
          0.4,
          0,
          0.8,
          0.8,
          0.4,
          0.4,
          0.2,
          0.6,
          0.4,
          0.8,
          0.4,
          0.4,
          0.6,
          0.6,
          0.4,
          0.2,
          0.4,
          0.2,
          0.6,
          0.6,
          0.2,
          0,
          0.2,
          0.4,
          0.4,
          0.4,
          0.4,
          0.8,
          0.2,
          0.8,
          0.2,
          0.6,
          0.2,
          0.6,
          0.6,
          0.4,
          0.4,
          0,
          0.2,
          0.6,
          0.4,
          0.8,
          0.6,
          0.6,
          1,
          0.8,
          0.8,
          0
        ),
        rep = c(
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L
        )
      ),
      row.names = c(NA,-150L),
      class = "data.frame"
    )
  results <- jasptools::run("MixedModelsGLMM", dataset, options)
  
  
  test_that("ANOVA Summary table results match", {
    table <- results[["results"]][["ANOVAsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        1,
        "cA",
        0.0295532081298837,
        4.73509545917489,
        2,
        "cB",
        0.00685488903662205,
        9.96558630663333,
        2,
        "cA * cB",
        0.84494437073476,
        0.336968974484137
      )
    )
  })
  
  test_that("Estimated Marginal Means table results match", {
    table <- results[["results"]][["EMMsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        1,
        1,
        "<unicode><unicode><unicode>",
        0.554090847944161,
        0.455421639718314,
        0.0499201875723043,
        0.648673842021157,
        2,
        1,
        "<unicode><unicode><unicode>",
        0.604038462746151,
        0.505345606874215,
        0.0489267357942983,
        0.694927559908738,
        1,
        2,
        "<unicode><unicode><unicode>",
        0.495680921823269,
        0.398547397034172,
        0.0502834679006801,
        0.593141570233039,
        2,
        2,
        "<unicode><unicode><unicode>",
        0.595723595920685,
        0.496949842639429,
        0.04913641248564,
        0.687306475586482,
        1,
        3,
        "<unicode><unicode><unicode>",
        0.395714994821718,
        0.304848962284826,
        0.0489190880133315,
        0.494402553657808,
        2,
        3,
        "<unicode><unicode><unicode>",
        0.487339396483339,
        0.390554433755448,
        0.0502657464118357,
        0.585083141048968
      )
    )
  })
  
  test_that("Plot matches", {
    plotName <- results[["results"]][["plots"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot5", dir = "MixedModelsGLMM")
  })
}
