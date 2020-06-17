context("Bayesian Generalized Linear Mixed Models")

# the tests do not cover all links / families
# quite large overlap with the functionality of LMMs

### binomial + logit, default all selected output
{
  options <- jasptools::analysisOptions("MixedModelsBGLMM")
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
  options$chains <- 1
  options$dependentVariable <- "contBinom"
  options$fixedEffects <-
    list(
      list(components = "contNormal"),
      list(components = "facGender"),
      list(components = c("contNormal", "facGender"))
    )
  options$fixedVariables <- c("contNormal", "facGender")
  options$iteration <- 1000
  options$marginalMeans <-
    list(list(variable = "contNormal"), list(variable = "facGender"))
  options$marginalMeansContrast <- TRUE
  options$plotsAgregatedOver <- "facFive"
  options$plotsX <- list(list(variable = "facGender"))
  options$randomEffects <-
    list(list(
      correlations = TRUE,
      randomComponents = list(
        list(randomSlopes = TRUE,
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
  options$samplingVariable1 <- list(list(variable = "contNormal"))
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
        values = c("-1", "2")
      )
    )
  options$trendsTrend <- list(list(variable = "contNormal"))
  options$trendsVariables <- list(list(variable = "facGender"))
  options$warmup <- 500
  set.seed(1)
  results <-
    jasptools::run("MixedModelsBGLMM", "debug.csv", options)
  
  
  test_that("Estimated Marginal Means table results match", {
    table <- results[["results"]][["EMMsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        -1.24716219673316,
        0.548027610663294,
        "f",
        0.366564506559536,
        1,
        0.751642892078574,
        -0.18874858754,
        0.445285981714205,
        "f",
        0.300375823871317,
        2,
        0.618473560757655,
        0.869665021653162,
        0.355564604525495,
        "f",
        0.142822031702842,
        3,
        0.595784716446283,-1.24716219673316,
        0.334731681665119,
        "m",
        0.132614701703772,
        4,
        0.554887490496956,
        -0.18874858754,
        0.357034345622598,
        "m",
        0.205504723637473,
        5,
        0.499559697984924,
        0.869665021653162,
        0.378690101977103,
        "m",
        0.181982714420204,
        6,
        0.548685298413663
      )
    )
  })
  
  test_that("Fixed Effects Estimates table results match", {
    table <- results[["results"]][["FEsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        -0.430724092910362,
        -0.980370891160471,
        397.13799523368,
        0.998008558414307,
        0.273055312571228,
        "Intercept",
        0.105532807138659,
        -0.148999151420159,-0.592360688661261,
        438.17792154575,
        0.99908796368998,
        0.242626093469842,
        "contNormal",
        0.315823838386511,
        -0.225154136094372,
        -0.828246151132417,
        650.062872323771,
        0.998270815863813,
        0.3133315768379,
        "facGender (1)",
        0.382012821613771,
        0.34166837064958,
        -0.29030575731141,
        685.896242744869,
        0.998514769596295,
        0.332299652671739,
        "contNormal * facGender (1)",
        0.97770632187143
      )
    )
  })
  
  test_that("facFive: Correlation Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_CE1"]][["data"]]
    expect_equal_tables(table,
                        list(1, "Intercept", 0.222676085936891, 1, "contNormal"))
  })
  
  test_that("Residual Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_RES"]][["data"]]
    expect_equal_tables(table,
                        list(1, 1))
  })
  
  test_that("facFive: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0.343942370048844,
        0.118296353914816,
        "Intercept",
        0.293185616691077,
        0.0859578058345271,
        "contNormal"
      )
    )
  })
  
  test_that("Intercept table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
    expect_equal_tables(
      table,
      list(
        393.337864987284,
        251.336194215781,
        -0.430724092910362,
        -0.980370891160471,
        0.997998738722209,
        0.252432892438763,
        0.105532807138659
      )
    )
  })
  
  test_that("contNormal (difference from intercept) table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
    expect_equal_tables(
      table,
      list(
        439.846701090452,
        349.52181650278,
        -0.148999151420159,
        -0.592360688661261,
        1.00211212840535,
        0.233108475621976,
        0.315823838386511
      )
    )
  })
  
  test_that("facGender (difference from intercept) table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_3"]][["data"]]
    expect_equal_tables(
      table,
      list(
        650.911529903948,
        408.339449840951,
        0.232932303690394,
        "f",
        -0.179178288885472,
        1.00396915637737,
        0.199506850941487,
        0.673963589765923,
        648.337452181427,
        347.886938189187,
        -0.176685544939967,
        "m",
        -0.625145150201283,
        1.00722079277498,
        0.214313594872765,
        0.281578146883535
      )
    )
  })
  
  test_that("contNormal*facGender (difference from intercept) table results match",
            {
              table <-
                results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_4"]][["data"]]
              expect_equal_tables(
                table,
                list(
                  526.912463902363,
                  257.234308474403,
                  -0.390595173223436,
                  "f",
                  -1.0947663725809,
                  1.00271657869176,
                  0.3439394387419,
                  0.321799006041777,
                  559.644877353362,
                  289.926927436098,
                  0.0925968703831189,
                  "m",
                  -0.513415734022437,
                  1.00455519623868,
                  0.303797000580579,
                  0.761828673225709
                )
              )
            })
  
  test_that("Contrasts table results match", {
    table <- results[["results"]][["contrasts_Means"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "Contrast 1",
        -0.0979810833155545,
        -0.26677887447204,
        0.081889471028108
      )
    )
  })
  
  test_that("Contrasts table results match", {
    table <- results[["results"]][["contrasts_Trends"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "Contrast 1",
        0.532732556296047,
        -0.865051088234528,
        1.86599795324889
      )
    )
  })
  
  test_that("contNormal plot matches", {
    plotName <-
      results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_contNormal"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "contnormal", dir = "MixedModelsBGLMM")
  })
  
  test_that("Plot matches", {
    plotName <- results[["results"]][["plots"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot", dir = "MixedModelsBGLMM")
  })
  
  test_that("Estimated Trends table results match", {
    table <- results[["results"]][["trendsSummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "f",
        -1.02317353106737,
        1,
        -0.383711986018588,
        0.340273180096766,
        "m",
        -0.536893909033603,
        2,
        0.0863836683474684,
        0.693892022520532
      )
    )
  })
}
### binomial + cauchit, difference from mean, custom options
{
  options <- jasptools::analysisOptions("MixedModelsBGLMM")
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
      dependentVariableAggregation = list(containsColumn = TRUE),
      fixedEffects = list(list(), list(),
                          list()),
      fixedVariables = list(containsColumn = TRUE),
      marginalMeans = list(list(), list()),
      plotsAgregatedOver = list(containsColumn = TRUE),
      randomEffects = list(list(
        randomComponents = list(list(),
                                list(), list()),
        value = list(containsColumn = TRUE)
      )),
      randomVariables = list(containsColumn = TRUE),
      samplingVariable1 = list(list()),
      samplingVariable2 = list(list()),
      trendsContrasts = list(list(levels = list(containsColumn = TRUE)), list(levels = list(containsColumn = TRUE)), list(levels = list(containsColumn = TRUE))),
      trendsTrend = list(list()),
      trendsVariables = list(list())
    )
  options$Contrasts <-
    list(
      list(
        isContrast = FALSE,
        levels = c("2", "3", "4", "5",
                   "6", "7"),
        name = "facGender",
        values = c("f", "m", "f", "m",
                   "f", "m")
      ),
      list(
        isContrast = FALSE,
        levels = c("2", "3", "4",
                   "5", "6", "7"),
        name = "contNormal",
        values = c("-2", "-2", "0",
                   "0", "2", "2")
      ),
      list(
        isContrast = TRUE,
        levels = c("2", "3",
                   "4", "5", "6", "7"),
        name = "Contrast 1",
        values = c("-1", "0",
                   "-1", "0", "-1", "0")
      ),
      list(
        isContrast = TRUE,
        levels = c("2",
                   "3", "4", "5", "6", "7"),
        name = "Contrast 2",
        values = c("0",
                   "0", "0", "0", "0", "0")
      ),
      list(
        isContrast = TRUE,
        levels = c("2",
                   "3", "4", "5", "6", "7"),
        name = "Contrast 3",
        values = c("1",
                   "0", "1", "0", "1", "0")
      )
    )
  options$chains <- 1
  options$dependentVariable <- "contBinom"
  options$fixedEffects <-
    list(
      list(components = "contNormal"),
      list(components = "facGender"),
      list(components = c("contNormal", "facGender"))
    )
  options$fixedVariables <- c("contNormal", "facGender")
  options$iteration <- 1000
  options$link <- "cauchit"
  options$marginalMeans <-
    list(list(variable = "facGender"), list(variable = "contNormal"))
  options$marginalMeansCIwidth <- 0.42
  options$marginalMeansContrast <- TRUE
  options$marginalMeansSD <- 2
  options$plotsAgregatedOver <- "facFive"
  options$randomEffects <-
    list(list(
      correlations = TRUE,
      randomComponents = list(
        list(randomSlopes = TRUE,
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
  options$samplingPlot <- "stan_scat"
  options$samplingVariable1 <- list(list(variable = "contNormal"))
  options$samplingVariable2 <- list(list(variable = "facGender"))
  options$show <- "mmeans"
  options$showFE <- TRUE
  options$showRE <- TRUE
  options$trendsCIwidth <- 0.05
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
        values = c("0", "0")
      ),
      list(
        isContrast = TRUE,
        levels = c("2", "3"),
        name = "Contrast 2",
        values = c("1",
                   "-1")
      )
    )
  options$trendsTrend <- list(list(variable = "contNormal"))
  options$trendsVariables <- list(list(variable = "facGender"))
  options$warmup <- 500
  set.seed(1)
  results <-
    jasptools::run("MixedModelsBGLMM", "debug.csv", options)
  
  
  test_that("Estimated Marginal Means table results match (different CI, SD factor covariate)",
            {
              table <- results[["results"]][["EMMsummary"]][["data"]]
              expect_equal_tables(
                table,
                list(
                  -2.30557580592632,
                  0.700502048572065,
                  "f",
                  0.696001503143357,
                  1,
                  0.847638446937874,
                  -2.30557580592632,
                  0.297212430625089,
                  "m",
                  0.152586263169453,
                  2,
                  0.308283354156753,
                  -0.18874858754,
                  0.441939030013511,
                  "f",
                  0.406467328349936,
                  3,
                  0.509150970326313,-0.18874858754,
                  0.331002749269294,
                  "m",
                  0.280604139863024,
                  4,
                  0.365274754612751,
                  1.92807863084632,
                  0.237533819597634,
                  "f",
                  0.127966504849371,
                  5,
                  0.2522715110857,
                  1.92807863084632,
                  0.387439656673258,
                  "m",
                  0.169789403356014,
                  6,
                  0.372145511746846
                )
              )
            })
  
  test_that("Fixed Effects Estimates table results match", {
    table <- results[["results"]][["FEsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        -0.437272653158516,
        -1.13644108597297,
        441.77216982685,
        0.999057276702191,
        0.303219260243093,
        "Intercept",
        0.156465242743251,
        -0.180937849517816,-0.866078488121571,
        384.59507750864,
        0.998599733190688,
        0.306722165643889,
        "contNormal",
        0.397916927811301,
        -0.225435892922036,
        -0.784864468858935,
        307.577424617405,
        0.999019793005256,
        0.311012742351749,
        "facGender (1)",
        0.371055233863593,
        0.402469698391243,
        -0.367541936709876,
        247.637619323941,
        1.00514872492117,
        0.407880362463913,
        "contNormal * facGender (1)",
        1.26019658602446
      )
    )
  })
  
  test_that("facFive: Correlation Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_CE1"]][["data"]]
    expect_equal_tables(table,
                        list(1, "Intercept", 0.218960786843392, 1, "contNormal"))
  })
  
  test_that("Residual Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_RES"]][["data"]]
    expect_equal_tables(table,
                        list(1, 1))
  })
  
  test_that("facFive: Variance Estimates table results match", {
    table <-
      results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0.369801893643115,
        0.136753440542034,
        "Intercept",
        0.329993285463961,
        0.108895568451299,
        "contNormal"
      )
    )
  })
  
  test_that("Intercept table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
    expect_equal_tables(
      table,
      list(
        498.203420327643,
        255.755140652864,
        -0.437272653158516,
        -1.13644108597297,
        0.998865096041264,
        0.266113062548732,
        0.156465242743251
      )
    )
  })
  
  test_that("contNormal (trend) table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
    expect_equal_tables(
      table,
      list(
        389.534023350883,
        300.948026322939,
        -0.180937849517816,
        -0.866078488121571,
        1.01611268355032,
        0.28002251801465,
        0.397916927811301
      )
    )
  })
  
  test_that("facGender (marginal means) table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_3"]][["data"]]
    expect_equal_tables(
      table,
      list(
        534.99329613053,
        328.472467071214,
        -0.189997859246969,
        "f",
        -0.907781821222038,
        1.00051431832294,
        0.312218616311399,
        0.450785958810489,
        387.50868888973,
        267.186090145268,
        -0.616243920012037,
        "m",
        -1.45595089500095,
        0.998678452406444,
        0.335940808136765,
        0.0748928055753995
      )
    )
  })
  
  test_that("contNormal*facGender (marginal means) table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_4"]][["data"]]
    expect_equal_tables(
      table,
      list(
        278.36973995684,
        247.817882199961,
        -0.465526902472369,
        "f",
        -1.56910781661442,
        1.00707361254604,
        0.392376021762154,
        0.336420663307344,
        395.058470699329,
        296.783786620576,
        0.103651203436737,
        "m",
        -0.671977481481596,
        1.00145917837164,
        0.328585417996115,
        0.973928421111936
      )
    )
  })
  
  test_that("Contrasts table results match (different CI, no second row)", {
    table <- results[["results"]][["contrasts_Means"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "Contrast 1",
        -1.38945225097879,
        -1.49898874414783,
        -1.29996754810026,
        "Contrast 3",
        1.38945225097879,
        1.29996754810026,
        1.49898874414783
      )
    )
  })
  
  test_that("Contrasts table results match (different CI, no first row)", {
    table <- results[["results"]][["contrasts_Trends"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "Contrast 2",
        -0.566112914973356,
        -0.206863934362976,
        -0.161910009785822
      )
    )
  })
  
  test_that("MCMC diagnostics plot matches scatterplot [1]", {
    plotName <-
      results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_contNormal:facGender (f)"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "contnormal-by-facgender", dir = "MixedModelsBGLMM")
  })
  
  test_that("MCMC diagnostics plot matches scatterplot [2]", {
    plotName <-
      results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_contNormal:facGender (m)"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "contnormal-by-facgender", dir = "MixedModelsBGLMM")
  })
  
  test_that("Estimated Trends table results match (different CI)", {
    table <- results[["results"]][["trendsSummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "f",
        -0.495223720491627,
        1,
        -0.432166504755183,
        -0.459484310947938,
        "m",
        0.0979039774362758,
        2,
        0.111007603811808,
        0.126944361172563
      )
    )
  })
}
### gamma + inverse, other options
{
  options <- jasptools::analysisOptions("MixedModelsBGLMM")
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
      randomEffects = list(list(
        randomComponents = list(list(),
                                list(), list()),
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
        name = "facGender",
        values = c("f", "m", "f", "m",
                   "f", "m")
      ),
      list(
        isContrast = FALSE,
        levels = c("2", "3", "4",
                   "5", "6", "7"),
        name = "contNormal",
        values = c("-1", "-1", "0",
                   "0", "1", "1")
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
  options$dependentVariable <- "contGamma"
  options$family <- "Gamma"
  options$fixedEffects <-
    list(
      list(components = "facGender"),
      list(components = "contNormal"),
      list(components = c("facGender", "contNormal"))
    )
  options$fixedVariables <- c("facGender", "contNormal")
  options$iteration <- 1000
  options$link <- "inverse"
  options$marginalMeans <-
    list(list(variable = "facGender"), list(variable = "contNormal"))
  options$marginalMeansContrast <- TRUE
  options$marginalMeansResponse <- FALSE
  options$plotsAgregatedOver <- "facFive"
  options$randomEffects <-
    list(list(
      correlations = TRUE,
      randomComponents = list(
        list(randomSlopes = FALSE,
             value = "facGender"),
        list(randomSlopes = FALSE, value = "contNormal"),
        list(
          randomSlopes = FALSE,
          value = c("facGender", "contNormal")
        )
      ),
      value = "facFive"
    ))
  options$randomVariables <- "facFive"
  options$samplingPlot <- "stan_hist"
  options$samplingVariable1 <- list(list(variable = "facGender"))
  options$samplingVariable2 <- list()
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
        values = c("1", "1")
      )
    )
  options$trendsTrend <- list(list(variable = "contNormal"))
  options$trendsVariables <- list(list(variable = "facGender"))
  options$warmup <- 500
  set.seed(1)
  results <-
    jasptools::run("MixedModelsBGLMM", "debug.csv", options)
  
  
  test_that("Estimated Marginal Means table results match (original scale)",
            {
              table <- results[["results"]][["EMMsummary"]][["data"]]
              expect_equal_tables(
                table,
                list(
                  -1.24716219673316,
                  0.59180360519958,
                  "f",
                  0.406419682089171,
                  1,
                  0.800554634159138,
                  -1.24716219673316,
                  0.395675411805664,
                  "m",
                  0.230570700136182,
                  2,
                  0.646279637680278,
                  -0.18874858754,
                  0.56051457343804,
                  "f",
                  0.42821955638553,
                  3,
                  0.769592422920287,
                  -0.18874858754,
                  0.463254545958007,
                  "m",
                  0.294045111095305,
                  4,
                  0.656268774633676,
                  0.869665021653162,
                  0.527034685619149,
                  "f",
                  0.325348737274731,
                  5,
                  0.754400795379594,
                  0.869665021653162,
                  0.524583140366059,
                  "m",
                  0.355931061314363,
                  6,
                  0.736933242440346
                )
              )
            })
  
  test_that("Intercept table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
    expect_equal_tables(
      table,
      list(
        195.129496752214,
        138.417380699206,
        0.520425711095159,
        0.378163071160719,
        1.02469335381077,
        0.0598431280859235,
        0.706019514361253
      )
    )
  })
  
  test_that("facGender (difference from intercept) table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
    expect_equal_tables(
      table,
      list(
        266.658297842272,
        366.979008565971,
        0.0469411043373429,
        "f",
        -0.0246006687154284,
        1.00400280756772,
        0.0404354947000362,
        0.114988539895517,
        177.727862250672,
        129.643020832085,
        -0.0516575166374559,
        "m",
        -0.123694763108243,
        1.00462796067653,
        0.046505098386436,
        0.029187925967158
      )
    )
  })
  
  test_that("contNormal (difference from intercept) table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_3"]][["data"]]
    expect_equal_tables(
      table,
      list(
        152.020458145983,
        97.674267937551,
        0.0124939009122744,
        -0.0628009147513921,
        0.999631190218145,
        0.0368659083756619,
        0.0858692659377892
      )
    )
  })
  
  test_that("facGender*contNormal (difference from intercept) table results match",
            {
              table <-
                results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_4"]][["data"]]
              expect_equal_tables(
                table,
                list(
                  145.569783409758,
                  124.299808369136,
                  -0.0297673478344751,
                  "f",-0.138816011009079,
                  0.999911792807736,
                  0.0589115260425204,
                  0.0955387309465729,
                  291.156971884378,
                  231.275467931831,
                  0.0547551496590239,
                  "m",-0.0300873274089139,
                  0.998056570722658,
                  0.0495566817356151,
                  0.135731050320163
                )
              )
            })
  
  test_that("Contrasts table results match", {
    table <- results[["results"]][["contrasts_Means"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "Contrast 1",
        0.18385474480381,
        0.00508411516274271,
        0.390334032493681
      )
    )
  })
  
  test_that("Contrasts table results match", {
    table <- results[["results"]][["contrasts_Trends"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "Contrast 1",
        0.0282884405833906,
        -0.131959972418838,
        0.156211300657964
      )
    )
  })
  
  test_that("MCMC diagnostics plot matches histogram [1]", {
    plotName <-
      results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_facGender f"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "facgender-f-", dir = "MixedModelsBGLMM")
  })
  
  test_that("MCMC diagnostics plot matches histogram [2]", {
    plotName <-
      results[["results"]][["diagnosticPlots"]][["collection"]][["diagnosticPlots_facGender m"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "facgender-m-", dir = "MixedModelsBGLMM")
  })
  
  test_that("Estimated Trends table results match", {
    table <- results[["results"]][["trendsSummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        "f",
        -0.145447593153322,
        1,
        -0.0324530920862224,
        0.0847136889474732,
        "m",
        -0.0326079592917737,
        2,
        0.0571994857388874,
        0.131787088625213
      )
    )
  })
}
### inverse gaussian + log
{
  options <- jasptools::analysisOptions("MixedModelsBGLMM")
  options$.meta <-
    list(
      Contrasts = list(list(levels = list(containsColumn = TRUE)),
                       list(levels = list(containsColumn = TRUE))),
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
      randomVariables = list(containsColumn = TRUE)
    )
  options$Contrasts <-
    list(
      list(
        isContrast = FALSE,
        levels = c("2", "3"),
        name = "fA",
        values = c("1", "2")
      ),
      list(
        isContrast = TRUE,
        levels = c("2",
                   "3"),
        name = "Contrast 1",
        values = c("0", "0")
      )
    )
  options$chains <- 1
  options$dependentVariable <- "y_beta"
  options$family <- "inverse.gaussian"
  options$fixedEffects <- list(list(components = "fA"))
  options$fixedVariables <- "fA"
  options$iteration <- 500
  options$link <- "log"
  options$marginalMeans <- list(list(variable = "fA"))
  options$plotsAgregatedOver <- "id"
  options$plotsGeom <- "geom_count"
  options$plotsX <- list(list(variable = "fA"))
  options$randomEffects <-
    list(list(
      correlations = TRUE,
      randomComponents = list(list(
        randomSlopes = TRUE,
        value = "fA"
      )),
      value = "id"
    ))
  options$randomVariables <- "id"
  options$samplingVariable1 <- list()
  options$samplingVariable2 <- list()
  options$trendsContrasts <- list()
  options$trendsTrend <- list()
  options$warmup <- 250
  set.seed(1)
  dataset <-
    structure(
      list(
        y_beta = c(
          0.576976447894837,
          0.445157187441822,
          0.519322517218143,
          0.571801958312222,
          0.302569917821024,
          0.817057385905033,
          0.411334574604233,
          0.578949745933594,
          0.628806632120146,
          0.371088630757214,
          0.456910584151786,
          0.612535611188177,
          0.535866597798595,
          0.312739790057611,
          0.333356896948679,
          0.766298475707685,
          0.421693814686297,
          0.191732804336131,
          0.43552512222489,
          0.511290545665442
        ),
        y_pois = c(
          4L,
          3L,
          5L,
          1L,
          2L,
          3L,
          1L,
          4L,
          3L,
          3L,
          1L,
          3L,
          4L,
          2L,
          4L,
          2L,
          3L,
          2L,
          2L,
          7L
        ),
        fA = c(
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L
        ),
        fB = c(
          1L,
          2L,
          3L,
          4L,
          1L,
          2L,
          3L,
          4L,
          1L,
          2L,
          3L,
          4L,
          1L,
          2L,
          3L,
          4L,
          1L,
          2L,
          3L,
          4L
        ),
        id = c(
          1L,
          2L,
          3L,
          4L,
          5L,
          1L,
          2L,
          3L,
          4L,
          5L,
          1L,
          2L,
          3L,
          4L,
          5L,
          1L,
          2L,
          3L,
          4L,
          5L
        )
      ),
      class = "data.frame",
      row.names = c(NA,-20L)
    )
  results <- jasptools::run("MixedModelsBGLMM", dataset, options)
  
  
  test_that("Estimated Marginal Means table results match", {
    table <- results[["results"]][["EMMsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0.481609566422193,
        1,
        0.362219908273037,
        0.677771481043281,
        0.536414180662079,
        2,
        0.3218871925481,
        0.815883069993223
      )
    )
  })
  
  test_that("Intercept table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
    expect_equal_tables(
      table,
      list(
        191.494008521153,
        95.268453651647,
        -0.822455881974452,
        -1.64855227743983,
        0.999449706400169,
        0.308220908254395,
        -0.055507827875067
      )
    )
  })
  
  test_that("fA (difference from intercept) table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
    expect_equal_tables(
      table,
      list(
        199.57606753727,
        137.417773689005,
        0.103046546581088,
        -0.297371419702504,
        1.00154404197415,
        0.227003120655153,
        0.62393545429813
      )
    )
  })
  
  test_that("Plot matches", {
    plotName <- results[["results"]][["plots"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot", dir = "MixedModelsBGLMM")
  })
  
}
### beta
{
  options <- jasptools::analysisOptions("MixedModelsBGLMM")
  options$.meta <-
    list(
      Contrasts = list(list(levels = list(containsColumn = TRUE)),
                       list(levels = list(containsColumn = TRUE))),
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
      randomVariables = list(containsColumn = TRUE)
    )
  options$Contrasts <-
    list(
      list(
        isContrast = FALSE,
        levels = c("2", "3"),
        name = "fA",
        values = c("1", "2")
      ),
      list(
        isContrast = TRUE,
        levels = c("2",
                   "3"),
        name = "Contrast 1",
        values = c("0", "0")
      )
    )
  options$chains <- 1
  options$dependentVariable <- "y_beta"
  options$family <- "betar"
  options$fixedEffects <- list(list(components = "fA"))
  options$fixedVariables <- "fA"
  options$iteration <- 500
  options$marginalMeans <- list(list(variable = "fA"))
  options$plotsAgregatedOver <- "id"
  options$plotsX <- list(list(variable = "fA"))
  options$randomEffects <-
    list(list(
      correlations = TRUE,
      randomComponents = list(list(
        randomSlopes = TRUE,
        value = "fA"
      )),
      value = "id"
    ))
  options$randomVariables <- "id"
  options$samplingVariable1 <- list()
  options$samplingVariable2 <- list()
  options$trendsContrasts <- list()
  options$trendsTrend <- list()
  options$warmup <- 250
  set.seed(1)
  dataset <-
    structure(
      list(
        y_beta = c(
          0.576976447894837,
          0.445157187441822,
          0.519322517218143,
          0.571801958312222,
          0.302569917821024,
          0.817057385905033,
          0.411334574604233,
          0.578949745933594,
          0.628806632120146,
          0.371088630757214,
          0.456910584151786,
          0.612535611188177,
          0.535866597798595,
          0.312739790057611,
          0.333356896948679,
          0.766298475707685,
          0.421693814686297,
          0.191732804336131,
          0.43552512222489,
          0.511290545665442
        ),
        y_pois = c(
          4L,
          3L,
          5L,
          1L,
          2L,
          3L,
          1L,
          4L,
          3L,
          3L,
          1L,
          3L,
          4L,
          2L,
          4L,
          2L,
          3L,
          2L,
          2L,
          7L
        ),
        fA = c(
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L
        ),
        fB = c(
          1L,
          2L,
          3L,
          4L,
          1L,
          2L,
          3L,
          4L,
          1L,
          2L,
          3L,
          4L,
          1L,
          2L,
          3L,
          4L,
          1L,
          2L,
          3L,
          4L
        ),
        id = c(
          1L,
          2L,
          3L,
          4L,
          5L,
          1L,
          2L,
          3L,
          4L,
          5L,
          1L,
          2L,
          3L,
          4L,
          5L,
          1L,
          2L,
          3L,
          4L,
          5L
        )
      ),
      class = "data.frame",
      row.names = c(NA,-20L)
    )
  results <- jasptools::run("MixedModelsBGLMM", dataset, options)
  
  
  test_that("Estimated Marginal Means table results match", {
    table <- results[["results"]][["EMMsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        0.470416089209965,
        1,
        0.337687718566763,
        0.642231258900355,
        0.538455612881716,
        2,
        0.393733081470019,
        0.724563782014576
      )
    )
  })
  
  test_that("Intercept table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
    expect_equal_tables(
      table,
      list(
        153.591954371123,
        62.7088760485386,
        -0.330692304665802,
        -1.41254497531971,
        1.02190931912402,
        0.569845063097798,
        1.03119706718526
      )
    )
  })
  
  test_that("fA (difference from intercept) table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
    expect_equal_tables(
      table,
      list(
        220.004940459319,
        174.770143840464,
        0.24317233286898,
        -0.520404904387767,
        1.00741357537031,
        0.392559131673451,
        0.938841988193603
      )
    )
  })
  
  test_that("Plot matches", {
    plotName <- results[["results"]][["plots"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot", dir = "MixedModelsBGLMM")
  })
}
### negative binomial
{
  options <- jasptools::analysisOptions("MixedModelsBGLMM")
  options$.meta <-
    list(
      Contrasts = list(list(levels = list(containsColumn = TRUE)),
                       list(levels = list(containsColumn = TRUE))),
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
      randomVariables = list(containsColumn = TRUE)
    )
  options$Contrasts <-
    list(
      list(
        isContrast = FALSE,
        levels = c("2", "3"),
        name = "fA",
        values = c("1", "2")
      ),
      list(
        isContrast = TRUE,
        levels = c("2",
                   "3"),
        name = "Contrast 1",
        values = c("0", "0")
      )
    )
  options$chains <- 1
  options$dependentVariable <- "y_pois"
  options$family <- "neg_binomial_2"
  options$fixedEffects <- list(list(components = "fA"))
  options$fixedVariables <- "fA"
  options$iteration <- 500
  options$link <- "identity"
  options$marginalMeans <- list(list(variable = "fA"))
  options$plotsAgregatedOver <- "id"
  options$plotsGeom <- "geom_count"
  options$plotsX <- list(list(variable = "fA"))
  options$randomEffects <-
    list(list(
      correlations = TRUE,
      randomComponents = list(list(
        randomSlopes = TRUE,
        value = "fA"
      )),
      value = "id"
    ))
  options$randomVariables <- "id"
  options$samplingVariable1 <- list()
  options$samplingVariable2 <- list()
  options$trendsContrasts <- list()
  options$trendsTrend <- list()
  options$warmup <- 250
  set.seed(1)
  dataset <-
    structure(
      list(
        y_beta = c(
          0.576976447894837,
          0.445157187441822,
          0.519322517218143,
          0.571801958312222,
          0.302569917821024,
          0.817057385905033,
          0.411334574604233,
          0.578949745933594,
          0.628806632120146,
          0.371088630757214,
          0.456910584151786,
          0.612535611188177,
          0.535866597798595,
          0.312739790057611,
          0.333356896948679,
          0.766298475707685,
          0.421693814686297,
          0.191732804336131,
          0.43552512222489,
          0.511290545665442
        ),
        y_pois = c(
          4L,
          3L,
          5L,
          1L,
          2L,
          3L,
          1L,
          4L,
          3L,
          3L,
          1L,
          3L,
          4L,
          2L,
          4L,
          2L,
          3L,
          2L,
          2L,
          7L
        ),
        fA = c(
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L
        ),
        fB = c(
          1L,
          2L,
          3L,
          4L,
          1L,
          2L,
          3L,
          4L,
          1L,
          2L,
          3L,
          4L,
          1L,
          2L,
          3L,
          4L,
          1L,
          2L,
          3L,
          4L
        ),
        id = c(
          1L,
          2L,
          3L,
          4L,
          5L,
          1L,
          2L,
          3L,
          4L,
          5L,
          1L,
          2L,
          3L,
          4L,
          5L,
          1L,
          2L,
          3L,
          4L,
          5L
        )
      ),
      class = "data.frame",
      row.names = c(NA,-20L)
    )
  results <- jasptools::run("MixedModelsBGLMM", dataset, options)
  
  
  test_that("Estimated Marginal Means table results match", {
    table <- results[["results"]][["EMMsummary"]][["data"]]
    expect_equal_tables(
      table,
      list(
        3.09208478977381,
        1,
        1.60802612917851,
        5.02835182803261,
        3.37751539963834,
        2,
        1.98101829464952,
        5.70717856144971
      )
    )
  })
  
  test_that("Intercept table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
    expect_equal_tables(
      table,
      list(
        131.802348636944,
        82.5302105310202,
        2.9246392817704,
        0.0314911387962763,
        1.04105485242279,
        1.62834910179864,
        6.88676784936048
      )
    )
  })
  
  test_that("fA (difference from intercept) table results match", {
    table <-
      results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
    expect_equal_tables(
      table,
      list(
        169.384442751773,
        94.3982168566097,
        0.27620295539782,
        -2.06223235299029,
        1.01601937074362,
        1.00500629586041,
        2.19318251199544
      )
    )
  })
  
  test_that("Plot matches", {
    plotName <- results[["results"]][["plots"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "plot", dir = "MixedModelsBGLMM")
  })
}