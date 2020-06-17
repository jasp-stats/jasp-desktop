context("Bayesian Linear Mixed Models")

# quite large overlap with the functionality of LMMs
skip("rstan reproducibility issues...")
### 1 chain, default, all selected output
{
  options <- jasptools::analysisOptions("MixedModelsBLMM")
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
      plotsX = list(list()),
      randomEffects = list(list(
        randomComponents = list(list(), list(), list()),
        value = list(containsColumn = TRUE)
      )),
      randomVariables = list(containsColumn = TRUE),
      samplingVariable1 = list(list()),
      trendsContrasts = list(list(levels = list(containsColumn = TRUE)),
                             list(levels = list(containsColumn = TRUE))),
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
        name = "facGender",
        values = c("f", "f", "f",
                   "m", "m", "m")
      ),
      list(
        isContrast = TRUE,
        levels = c("2", "3",
                   "4", "5", "6", "7"),
        name = "Contrast 1",
        values = c("1", "-1",
                   "0", "0", "0", "0")
      )
    )
  options$chains <- 1
  options$dependentVariable <- "contNormal"
  options$fixedEffects <-
    list(
      list(components = "contGamma"),
      list(components = "facGender"),
      list(components = c("contGamma", "facGender"))
    )
  options$fixedVariables <- c("contGamma", "facGender")
  options$iteration <- 1000
  options$marginalMeans <-
    list(list(variable = "contGamma"), list(variable = "facGender"))
  options$marginalMeansContrast <- TRUE
  options$plotsAgregatedOver <- "facFive"
  options$plotsX <- list(list(variable = "facGender"))
  options$randomEffects <-
    list(list(
      correlations = TRUE,
      randomComponents = list(
        list(randomSlopes = TRUE,
             value = "contGamma"),
        list(randomSlopes = TRUE, value = "facGender"),
        list(
          randomSlopes = TRUE,
          value = c("contGamma", "facGender")
        )
      ),
      value = "facFive"
    ))
  options$randomVariables <- "facFive"
  options$samplingVariable1 <- list(list(variable = "contGamma"))
  options$samplingVariable2 <- list()
  options$showFE <- TRUE
  options$showRE <- TRUE
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
        values = c("1", "-1")
      )
    )
  options$trendsTrend <- list(list(variable = "contGamma"))
  options$trendsVariables <- list(list(variable = "facGender"))
  options$warmup <- 500
  set.seed(1)
  results <- jasptools::run("MixedModelsBLMM", "debug.csv", options)
  
  
  test_that("Estimated Marginal Means table results match", {
    table <- results[["results"]][["EMMsummary"]][["data"]]
    expect_equal_tables(
      unlist(table),
      list(
        0.500549669999563,
        -0.515040062005683,
        "f",
        -0.976024956316189,
        1,
        -0.113001752820349,
        2.03296079621,
        -0.413232605497719,
        "f",-0.733811501981486,
        2,
        -0.0878985674063757,
        3.56537192242044,-0.298439516815009,
        "f",
        -0.810270340314971,
        3,
        0.377029141629359,
        0.500549669999563,
        0.250890325797991,
        "m",
        -0.12260947345701,
        4,
        0.727398563018588,
        2.03296079621,
        0.0917038680701213,
        "m",-0.210981838731247,
        5,
        0.467640643722235,
        3.56537192242044,-0.0865454876855547,
        "m",
        -0.513060889170545,
        6,
        0.321027998835378
      )
    )
  })
  
  test_that("Fixed Effects Estimates table results match", {
    table <- results[["results"]][["FEsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        -0.120853757395694,
        -0.481452460275346,
        1039.63176527362,
        0.998378685804121,
        0.185429631465067,
        "Intercept",
        0.232228540173356,
        -0.0208141137243334,-0.172276464791562,
        911.438538272589,
        0.998146110251807,
        0.0813148180428108,
        "contGamma",
        0.149045778033579,
        0.623327849998275,
        0.168562990423968,
        411.920660474736,
        0.999420733071299,
        0.24913346465672,
        "facGender (1)",
        1.1017883008468,
        -0.132450205989665,
        -0.340379209392789,
        355.565135263594,
        0.998171298666418,
        0.111530100784859,
        "contGamma * facGender (1)",
        0.0803267525811126
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
        -0.0246383376433051,
        1,
        "contGamma",
        0.0753630887148735,
        0.0138101861962475,
        1,
        "facGender (1)",
        0.0459673356945536,
        0.0641597917675106,
        -0.140824188091717,
        1,
        "contGamma * facGender (1)"
      )
    )
  })
  
  test_that("Residual Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_RES"]][["data"]]
    expect_equal_tables(table,
                        list(1.03467427950276, 1.07055086466456))
  })
  
  test_that("facFive: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0.0918130162434662,
        0.00842962995172299,
        "Intercept",
        0.0548736632993869,
        0.00301111892389448,
        "contGamma",
        0.067639752927631,
        0.00457513617611096,
        "facGender (1)",
        0.0751876948687666,
        0.00565318945967875,
        "contGamma * facGender (1)"
      )
    )
  })
  
  test_that("Intercept table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
    expect_equal_tables(
      table,
      list(
        1109.97338120067,
        389.190703996611,
        -0.120853757395694,
        -0.481452460275346,
        1.00153578056658,
        0.192808229710563,
        0.232228540173356
      )
    )
  })
  
  test_that("contGamma (difference from intercept) table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
    expect_equal_tables(
      table,
      list(
        812.500863691602,
        408.339449840951,
        -0.0208141137243334,
        -0.172276464791562,
        1.00573647695248,
        0.0800482618990967,
        0.149045778033579
      )
    )
  })
  
  test_that("facGender (difference from intercept) table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_3"]][["data"]]
    expect_equal_tables(
      table,
      list(
        868.789904898283,
        400.787494968261,
        -0.292673758402082,
        "f",
        -0.733056162384164,
        0.998098334762497,
        0.223175688620375,
        0.123006483513803,
        623.851212529452,
        473.405073255672,
        0.20804520398323,
        "m",
        -0.140972461314032,
        0.999034773686775,
        0.184334912316998,
        0.570830830690595
      )
    )
  })
  
  test_that("contGamma*facGender (difference from intercept) table results match",
            {
              table <-
                results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_4"]][["data"]]
              expect_equal_tables(
                table,
                list(
                  555.233589087928,
                  399.552428949784,
                  0.0728423251005156,
                  "f",
                  -0.161441771223135,
                  0.998527735565998,
                  0.134940941688205,
                  0.341643671158452,
                  475.652346028151,
                  315.585308802146,
                  -0.114470552549182,
                  "m",
                  -0.30073004294566,
                  0.9980426461534,
                  0.0893118380096413,
                  0.0706454574581624
                )
              )
            })
  
  test_that("Contrasts table results match", {
    table <- results[["results"]][["contrasts_Means"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "Contrast 1",
        -0.106267946312568,
        -0.503847097069113,
        0.265033730502364
      )
    )
  })
  
  test_that("Contrasts table results match", {
    table <- results[["results"]][["contrasts_Trends"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "Contrast 1",
        0.18614797246104,
        -0.108127148628483,
        0.487128195926725
      )
    )
  })
  
  test_that("MCMC diagnostics plot matches (traceplot)", {
    plotName <-
      results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_contGamma"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot1", dir = "MixedModelsBLMM")
  })
  
  test_that("Plot matches", {
    plotName <- results[["results"]][["plots"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot2", dir = "MixedModelsBLMM")
  })
  
  test_that("Estimated Trends table results match", {
    table <- results[["results"]][["trendsSummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "f",
        -0.172952105325533,
        1,
        0.0693468903318167,
        0.328793682355403,
        "m",
        -0.285048400844553,
        2,
        -0.111129080073516,
        0.0788259074891166
      )
    )
  })
}
### 1 chain, difference from mean output, custom options
{
  options <- jasptools::analysisOptions("MixedModelsBLMM")
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
      samplingVariable1 = list(list()),
      trendsContrasts = list(list(levels = list(containsColumn = TRUE)),
                             list(levels = list(containsColumn = TRUE))),
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
        values = c("-2", "0", "2", "-2",
                   "0", "2")
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
        values = c("1", "-1",
                   "0", "0", "0", "0")
      ),
      list(
        isContrast = TRUE,
        levels = c("2",
                   "3", "4", "5", "6", "7"),
        name = "Contrast 2",
        values = c("0",
                   "0", "0", "1", "-1", "0")
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
  options$chains <- 1
  options$dependentVariable <- "contNormal"
  options$fixedEffects <-
    list(
      list(components = "contGamma"),
      list(components = "facGender"),
      list(components = c("contGamma", "facGender"))
    )
  options$fixedVariables <- c("contGamma", "facGender")
  options$iteration <- 1000
  options$marginalMeans <-
    list(list(variable = "contGamma"), list(variable = "facGender"))
  options$marginalMeansCIwidth <- 0.666
  options$marginalMeansContrast <- TRUE
  options$marginalMeansSD <- 2
  options$plotLegendPosition <- "bottom"
  options$plotsAgregatedOver <- "facFive"
  options$plotsCImethod <- "mean"
  options$plotsGeom <- "geom_boxjitter"
  options$plotsMappingColor <- TRUE
  options$plotsTheme <- "theme_bw"
  options$plotsX <- list(list(variable = "facGender"))
  options$randomEffects <-
    list(list(
      correlations = TRUE,
      randomComponents = list(
        list(randomSlopes = TRUE,
             value = "contGamma"),
        list(randomSlopes = TRUE, value = "facGender"),
        list(
          randomSlopes = TRUE,
          value = c("contGamma", "facGender")
        )
      ),
      value = "facFive"
    ))
  options$randomVariables <- "facFive"
  options$samplingPlot <- "stan_hist"
  options$samplingVariable1 <- list(list(variable = "facGender"))
  options$samplingVariable2 <- list()
  options$show <- "mmeans"
  options$showFE <- TRUE
  options$showRE <- TRUE
  options$trendsCIwidth <- 0.15
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
        values = c("1", "-1")
      )
    )
  options$trendsTrend <- list(list(variable = "contGamma"))
  options$trendsVariables <- list(list(variable = "facGender"))
  options$warmup <- 500
  set.seed(1)
  results <- jasptools::run("MixedModelsBLMM", "debug.csv", options)
  
  
  test_that("Estimated Marginal Means table results match (CI changed)", {
    table <- results[["results"]][["EMMsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        -1.03186145621087,
        -0.633590053921018,
        "f",
        -0.952829683064346,
        1,
        -0.228691490020486,
        2.03296079621,
        -0.413232605497719,
        "f",-0.576673467588814,
        2,
        -0.270149284811175,
        5.09778304863087,-0.211990345593071,
        "f",
        -0.565638932728145,
        3,
        0.3129713845746,-1.03186145621087,
        0.429704434443064,
        "m",
        0.0261249820990926,
        4,
        0.662560166144668,
        2.03296079621,
        0.0917038680701213,
        "m",-0.0836073265571009,
        5,
        0.230978709253365,
        5.09778304863087,-0.249273452572074,
        "m",
        -0.575467333758803,
        6,
        0.0445773512612324
      )
    )
  })
  
  test_that("Fixed Effects Estimates table results match", {
    table <- results[["results"]][["FEsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        -0.120853757395694,
        -0.481452460275346,
        1039.63176527362,
        0.998378685804121,
        0.185429631465067,
        "Intercept",
        0.232228540173356,
        -0.0208141137243334,-0.172276464791562,
        911.438538272589,
        0.998146110251807,
        0.0813148180428108,
        "contGamma",
        0.149045778033579,
        0.623327849998275,
        0.168562990423968,
        411.920660474736,
        0.999420733071299,
        0.24913346465672,
        "facGender (1)",
        1.1017883008468,
        -0.132450205989665,
        -0.340379209392789,
        355.565135263594,
        0.998171298666418,
        0.111530100784859,
        "contGamma * facGender (1)",
        0.0803267525811126
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
        -0.0246383376433051,
        1,
        "contGamma",
        0.0753630887148735,
        0.0138101861962475,
        1,
        "facGender (1)",
        0.0459673356945536,
        0.0641597917675106,
        -0.140824188091717,
        1,
        "contGamma * facGender (1)"
      )
    )
  })
  
  test_that("Residual Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_RES"]][["data"]]
    expect_equal_tables(table,
                        list(1.03467427950276, 1.07055086466456))
  })
  
  test_that("facFive: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0.0918130162434662,
        0.00842962995172299,
        "Intercept",
        0.0548736632993869,
        0.00301111892389448,
        "contGamma",
        0.067639752927631,
        0.00457513617611096,
        "facGender (1)",
        0.0751876948687666,
        0.00565318945967875,
        "contGamma * facGender (1)"
      )
    )
  })
  
  test_that("Intercept table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
    expect_equal_tables(
      table,
      list(
        1109.97338120067,
        389.190703996611,
        -0.120853757395694,
        -0.481452460275346,
        1.00153578056658,
        0.192808229710563,
        0.232228540173356
      )
    )
  })
  
  test_that("contGamma (trend) table results match (CI change)", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
    expect_equal_tables(
      table,
      list(
        812.500863691602,
        408.339449840951,
        -0.0208141137243334,
        -0.172276464791562,
        1.00573647695248,
        0.0800482618990967,
        0.149045778033579
      )
    )
  })
  
  test_that("facGender (marginal means) table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_3"]][["data"]]
    expect_equal_tables(
      table,
      list(
        548.356237382957,
        335.103375630988,
        -0.413527515797776,
        "f",
        -0.73148996478623,
        0.999724134209894,
        0.156523391576831,
        -0.0877264066665114,
        608.356989055533,
        424.997967683715,
        0.0871914465875363,
        "m",
        -0.251125834720031,
        1.00139251305296,
        0.166779854879117,
        0.434754084935457
      )
    )
  })
  
  test_that("contGamma*facGender (marginal means) table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_4"]][["data"]]
    expect_equal_tables(
      table,
      list(
        555.233589087928,
        399.552428949784,
        0.0728423251005156,
        "f",
        -0.161441771223135,
        0.998527735565998,
        0.134940941688205,
        0.341643671158452,
        475.652346028151,
        315.585308802146,
        -0.114470552549182,
        "m",
        -0.30073004294566,
        0.9980426461534,
        0.0893118380096413,
        0.0706454574581624
      )
    )
  })
  
  test_that("Contrasts table results match (3rd row is empty)", {
    table <- results[["results"]][["contrasts_Means"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "Contrast 1",
        -0.212535892625136,
        -0.613940982426083,
        0.147538429422545,
        "Contrast 2",
        0.340590877500365,
        0.132723213167432,
        0.668329810558322
      )
    )
  })
  
  test_that("Contrasts table results match", {
    table <- results[["results"]][["contrasts_Trends"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "Contrast 1",
        0.18614797246104,
        0.148982922813084,
        0.198917602352423
      )
    )
  })
  
  test_that("MCMC diagnostics plot matches [1] (densities)", {
    plotName <-
      results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_facGender f"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot3", dir = "MixedModelsBLMM")
  })
  
  test_that("MCMC diagnostics plot matches [2] (densities)", {
    plotName <-
      results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_facGender m"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot4", dir = "MixedModelsBLMM")
  })
  
  test_that("Plot matches", {
    plotName <- results[["results"]][["plots"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot5", dir = "MixedModelsBLMM")
  })
  
  test_that("Estimated Trends table results match", {
    table <- results[["results"]][["trendsSummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "f",
        0.115363245231648,
        1,
        0.0693468903318167,
        0.163526214578484,
        "m",
        -0.103831042994355,
        2,
        -0.111129080073516,
        -0.0726266155840078
      )
    )
  })
}
### 2 chains, no correlation, additional settings
{
  options <- jasptools::analysisOptions("MixedModelsBLMM")
  options$.meta <-
    list(
      Contrasts = list(list(levels = list(containsColumn = TRUE))),
      dependentVariable = list(containsColumn = TRUE),
      fixedEffects = list(list(), list(), list()),
      fixedVariables = list(containsColumn = TRUE),
      plotsAgregatedOver = list(containsColumn = TRUE),
      plotsX = list(list()),
      randomEffects = list(list(
        randomComponents = list(list(), list(), list()),
        value = list(containsColumn = TRUE)
      )),
      randomVariables = list(containsColumn = TRUE),
      samplingVariable1 = list(list()),
      samplingVariable2 = list(list()),
      trendsContrasts = list(list(levels = list(containsColumn = TRUE)))
    )
  options$Contrasts <-
    list(list(
      isContrast = TRUE,
      levels = list(),
      name = "Contrast 1",
      values = list()
    ))
  options$chains <- 2
  options$dependentVariable <- "contNormal"
  options$fixedEffects <-
    list(
      list(components = "facGender"),
      list(components = "facExperim"),
      list(components = c("facGender", "facExperim"))
    )
  options$fixedVariables <- c("facGender", "facExperim")
  options$iteration <- 500
  options$plotsAgregatedOver <- "facFive"
  options$plotsCImethod <- "none"
  options$plotsGeom <- "geom_violin"
  options$plotsX <- list(list(variable = "facGender"))
  options$randomEffects <-
    list(list(
      correlations = FALSE,
      randomComponents = list(
        list(randomSlopes = TRUE, value = "facGender"),
        list(randomSlopes = TRUE,
             value = "facExperim"),
        list(
          randomSlopes = TRUE,
          value = c("facGender",
                    "facExperim")
        )
      ),
      value = "facFive"
    ))
  options$randomVariables <- "facFive"
  options$samplingPlot <- "stan_scat"
  options$samplingVariable1 <- list(list(variable = "facGender"))
  options$samplingVariable2 <- list(list(variable = "facExperim"))
  options$showRE <- TRUE
  options$summaryCI <- 0.05
  options$trendsContrasts <-
    list(list(
      isContrast = TRUE,
      levels = list(),
      name = "Contrast 1",
      values = list()
    ))
  options$trendsTrend <- list()
  options$warmup <- 200
  set.seed(1)
  results <- jasptools::run("MixedModelsBLMM", "debug.csv", options)
  
  
  test_that("facFive: Correlation Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_CE2"]][["data"]]
    expect_equal_tables(table,
                        list(1, "facGender (f)",-0.0688435665717504, 1, "facGender (m)"))
  })
  
  test_that("facFive: Correlation Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_CE3"]][["data"]]
    expect_equal_tables(
      table,
      list(
        1,
        "facExperim (control)",
        -0.0639492570617858,
        1,
        "facExperim (experimental)"
      )
    )
  })
  
  test_that("facFive: Correlation Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_CE4"]][["data"]]
    expect_equal_tables(
      table,
      list(
        1,
        "facGender (f) * facExperim (control)",
        0.058800661870891,
        1,
        "facGender (m) * facExperim (control)",
        -0.0135160872741998,-0.255875189196793,
        1,
        "facGender (f) * facExperim (experimental)",
        0.00725314110872252,
        0.00648950953663931,
        0.0703427317963286,
        1,
        "facGender (m) * facExperim (experimental)"
      )
    )
  })
  
  test_that("Residual Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_RES"]][["data"]]
    expect_equal_tables(table,
                        list(0.99952582071103, 0.999051866268058))
  })
  
  test_that("facFive: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
    expect_equal_tables(table,
                        list(0.315999475926311, 0.0998556687857031, "Intercept"))
  })
  
  test_that("facFive: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE2"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0.290860485722489,
        0.0845998221547222,
        "facGender (f)",
        0.322189959700134,
        0.103806370131574,
        "facGender (m)"
      )
    )
  })
  
  test_that("facFive: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE3"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0.365281845094409,
        0.133430826355576,
        "facExperim (control)",
        0.312200637045963,
        0.097469237771905,
        "facExperim (experimental)"
      )
    )
  })
  
  test_that("facFive: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE4"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0.259373308343099,
        0.0672745130808443,
        "facGender (f) * facExperim (control)",
        0.309438693120327,
        0.0957523048000156,
        "facGender (m) * facExperim (control)",
        0.263264775408116,
        0.0693083419706859,
        "facGender (f) * facExperim (experimental)",
        0.250273343120058,
        0.0626367462764904,
        "facGender (m) * facExperim (experimental)"
      )
    )
  })
  
  test_that("Intercept table results match (different CI)", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
    expect_equal_tables(
      table,
      list(
        271.562287874207,
        213.273060052572,
        -0.177431279181268,
        -0.201686827324417,
        1.00707445837085,
        0.206120808950985,
        -0.177805731372483
      )
    )
  })
  
  test_that("facGender (difference from intercept) table results match(different CI)",
            {
              table <-
                results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
              expect_equal_tables(
                table,
                list(
                  310.003780904477,
                  137.402454129655,
                  -0.244932986437503,
                  "f",
                  -0.252475957405322,
                  1.01263748540014,
                  0.156462294093938,
                  -0.233218878082006,
                  310.003780904477,
                  137.402454129655,
                  0.244932986437503,
                  "m",
                  0.233218878082006,
                  1.01263748540014,
                  0.156462294093938,
                  0.252475957405322
                )
              )
            })
  
  test_that("facExperim (difference from intercept) table results match (different CI)",
            {
              table <-
                results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_3"]][["data"]]
              expect_equal_tables(
                table,
                list(
                  360.640537532038,
                  357.93706988785,
                  0.0104166466606312,
                  "control",-0.000978275497462992,
                  1.00816085828735,
                  0.126776197429132,
                  0.0185174555204936,
                  360.640537532038,
                  357.93706988785,
                  -0.0104166466606312,
                  "experimental",
                  -0.0185174555204936,
                  1.00816085828735,
                  0.126776197429132,
                  0.000978275497462992
                )
              )
            })
  
  test_that(
    "facGender*facExperim (difference from intercept) table results match (different CI)",
    {
      table <-
        results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_4"]][["data"]]
      expect_equal_tables(
        table,
        list(
          305.569326004817,
          194.325451453504,
          -0.333803139972707,
          "f:control",-0.347584963200009,
          1.00594284050429,
          0.23477727666005,
          -0.309852211186555,
          381.319682838549,
          349.136265203055,
          0.35463643329397,
          "m:control",
          0.328636601856533,
          1.01014637377777,
          0.254889329372787,
          0.362747588543464,
          323.938473811055,
          395.671086457319,
          -0.156062832902298,
          "f:experimental",-0.176146729888885,
          1.02063680226858,
          0.241722770576761,
          -0.139784835913999,
          444.02578523926,
          243.85732875965,
          0.135229539581036,
          "m:experimental",
          0.122433571476064,
          1.00175485649712,
          0.245486278605989,
          0.15167488492735
        )
      )
    }
  )
  
  test_that("MCMC diagnostics plot matches [1] (scatterplot)", {
    plotName <-
      results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_facGender (f):facExperim (control)"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot6", dir = "MixedModelsBLMM")
  })
  
  test_that("MCMC diagnostics plot matches [2] (scatterplot)", {
    plotName <-
      results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_facGender (f):facExperim (experimental)"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot7", dir = "MixedModelsBLMM")
  })
  
  test_that("MCMC diagnostics plot matches [3] (scatterplot)", {
    plotName <-
      results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_facGender (m):facExperim (control)"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot8", dir = "MixedModelsBLMM")
  })
  
  test_that("MCMC diagnostics plot matches [4] (scatterplot)", {
    plotName <-
      results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_facGender (m):facExperim (experimental)"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot9", dir = "MixedModelsBLMM")
  })
}
### remaining MCMC diagnostics plots
{
  options <- jasptools::analysisOptions("MixedModelsBLMM")
  options$.meta <-
    list(
      Contrasts = list(list(levels = list(containsColumn = TRUE))),
      dependentVariable = list(containsColumn = TRUE),
      fixedEffects = list(list(), list(), list()),
      fixedVariables = list(containsColumn = TRUE),
      plotsAgregatedOver = list(containsColumn = TRUE),
      randomEffects = list(list(
        randomComponents = list(list(), list(), list()),
        value = list(containsColumn = TRUE)
      )),
      randomVariables = list(containsColumn = TRUE),
      samplingVariable1 = list(list()),
      trendsContrasts = list(list(levels = list(containsColumn = TRUE)))
    )
  options$Contrasts <-
    list(list(
      isContrast = TRUE,
      levels = list(),
      name = "Contrast 1",
      values = list()
    ))
  options$chains <- 2
  options$dependentVariable <- "contNormal"
  options$fixedEffects <-
    list(
      list(components = "facGender"),
      list(components = "facExperim"),
      list(components = c("facGender", "facExperim"))
    )
  options$fixedVariables <- c("facGender", "facExperim")
  options$iteration <- 500
  options$plotsAgregatedOver <- "facFive"
  options$plotsCImethod <- "none"
  options$plotsGeom <- "geom_violin"
  options$randomEffects <-
    list(list(
      correlations = FALSE,
      randomComponents = list(
        list(randomSlopes = TRUE, value = "facGender"),
        list(randomSlopes = TRUE,
             value = "facExperim"),
        list(
          randomSlopes = TRUE,
          value = c("facGender",
                    "facExperim")
        )
      ),
      value = "facFive"
    ))
  options$randomVariables <- "facFive"
  options$samplingPlot <- "stan_dens"
  options$samplingVariable1 <-
    list(list(variable = c("facGender", "facExperim")))
  options$samplingVariable2 <- list()
  options$summaryCI <- 0.05
  options$trendsContrasts <-
    list(list(
      isContrast = TRUE,
      levels = list(),
      name = "Contrast 1",
      values = list()
    ))
  options$trendsTrend <- list()
  options$warmup <- 200
  set.seed(1)
  results <- jasptools::run("MixedModelsBLMM", "debug.csv", options)
  
  test_that("MCMC diagnostics plot matches densities [1] (with interaction)",
            {
              plotName <-
                results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_facGender f, facExperim control"]][["data"]]
              testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
              expect_equal_plots(testPlot, "plot10", dir =
                                   "MixedModelsBLMM")
            })
  
  test_that("MCMC diagnostics plot matches densities [2] (with interaction)",
            {
              plotName <-
                results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_facGender f, facExperim experimental"]][["data"]]
              testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
              expect_equal_plots(testPlot, "plot11", dir =
                                   "MixedModelsBLMM")
            })
  
  test_that("MCMC diagnostics plot matches densities [3] (with interaction)",
            {
              plotName <-
                results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_facGender m, facExperim control"]][["data"]]
              testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
              expect_equal_plots(testPlot, "plot12", dir =
                                   "MixedModelsBLMM")
            })
  
  test_that("MCMC diagnostics plot matches densities [4] (with interaction)",
            {
              plotName <-
                results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_facGender m, facExperim experimental"]][["data"]]
              testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
              expect_equal_plots(testPlot, "plot13", dir =
                                   "MixedModelsBLMM")
            })
}
### remaining MCMC diagnostics plots
{
  options <- jasptools::analysisOptions("MixedModelsBLMM")
  options$.meta <-
    list(
      Contrasts = list(list(levels = list(containsColumn = TRUE))),
      dependentVariable = list(containsColumn = TRUE),
      fixedEffects = list(list(), list(), list()),
      fixedVariables = list(containsColumn = TRUE),
      plotsAgregatedOver = list(containsColumn = TRUE),
      randomEffects = list(list(
        randomComponents = list(list(), list(), list()),
        value = list(containsColumn = TRUE)
      )),
      randomVariables = list(containsColumn = TRUE),
      samplingVariable1 = list(list()),
      trendsContrasts = list(list(levels = list(containsColumn = TRUE)))
    )
  options$Contrasts <-
    list(list(
      isContrast = TRUE,
      levels = list(),
      name = "Contrast 1",
      values = list()
    ))
  options$chains <- 1
  options$dependentVariable <- "contNormal"
  options$fixedEffects <-
    list(
      list(components = "facGender"),
      list(components = "facExperim"),
      list(components = c("facGender", "facExperim"))
    )
  options$fixedVariables <- c("facGender", "facExperim")
  options$iteration <- 500
  options$plotsAgregatedOver <- "facFive"
  options$plotsCImethod <- "none"
  options$plotsGeom <- "geom_violin"
  options$randomEffects <-
    list(list(
      correlations = FALSE,
      randomComponents = list(
        list(randomSlopes = TRUE, value = "facGender"),
        list(randomSlopes = TRUE,
             value = "facExperim"),
        list(
          randomSlopes = TRUE,
          value = c("facGender",
                    "facExperim")
        )
      ),
      value = "facFive"
    ))
  options$randomVariables <- "facFive"
  options$samplingPlot <- "stan_ac"
  options$samplingVariable1 <- list(list(variable = "facGender"))
  options$samplingVariable2 <- list()
  options$summaryCI <- 0.05
  options$trendsContrasts <-
    list(list(
      isContrast = TRUE,
      levels = list(),
      name = "Contrast 1",
      values = list()
    ))
  options$trendsTrend <- list()
  options$warmup <- 200
  set.seed(1)
  results <- jasptools::run("MixedModelsBLMM", "debug.csv", options)
  
  
  test_that("MCMC diagnostics plot matches autocorrelations [1]", {
    plotName <-
      results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_facGender f"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot14", dir = "MixedModelsBLMM")
  })
  
  test_that("MCMC diagnostics plot matches autocorrelations [2]", {
    plotName <-
      results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_facGender m"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot15", dir = "MixedModelsBLMM")
  })
}