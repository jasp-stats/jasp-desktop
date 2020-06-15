context("Bayesian Generalized Linear Mixed Models")

# the testing doesn't cover all links / families

#### main summary for different families / links ###
{
  # binomial / logit
  {
    options <- jasptools::analysisOptions("MixedModelsBGLMM")
    options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                          dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                            containsColumn = TRUE), fixedEffects = list(list()), 
                          fixedVariables = list(containsColumn = TRUE), plotsAgregatedOver = list(
                            containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                              list(value = list(containsColumn = TRUE))), value = list(
                                containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                          trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
    options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                   values = list()))
    options$chains <- 1
    options$dependentVariable <- "contBinom"
    options$fixedEffects <- list(list(components = "contNormal"))
    options$fixedVariables <- "contNormal"
    options$iteration <- 1000
    options$plotsAgregatedOver <- "facFive"
    options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                         value = "contNormal")), value = "facFive"))
    options$randomVariables <- "facFive"
    options$samplingVariable1 <- list()
    options$samplingVariable2 <- list()
    options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                         values = list()))
    options$trendsTrend <- list()
    options$warmup <- 500
    set.seed(1)
    results <- jasptools::run("MixedModelsBGLMM", "debug.csv", options)
    
    
    test_that("Intercept table results match", {
      table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
      expect_equal_tables(table,
                          list(445.986711288276, 243.021310777226, -0.367945697032323, -0.8939860663056,
                               1.01423597652283, 0.241287709260238, 0.081417383256481))
    })
    
    test_that("contNormal (difference from intercept) table results match", {
      table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
      expect_equal_tables(table,
                          list(293.877552955164, 82.1495752783345, -0.153554762814738, -0.788106400736205,
                               1.00728623769667, 0.234499752760393, 0.272008764353946))
    })
  }
  # binomial / probit
  {
    options <- jasptools::analysisOptions("MixedModelsBGLMM")
    options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                          dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                            containsColumn = TRUE), fixedEffects = list(list()), 
                          fixedVariables = list(containsColumn = TRUE), plotsAgregatedOver = list(
                            containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                              list(value = list(containsColumn = TRUE))), value = list(
                                containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                          trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
    options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                   values = list()))
    options$chains <- 1
    options$dependentVariable <- "contBinom"
    options$fixedEffects <- list(list(components = "contNormal"))
    options$fixedVariables <- "contNormal"
    options$iteration <- 1000
    options$link <- "probit"
    options$plotsAgregatedOver <- "facFive"
    options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                         value = "contNormal")), value = "facFive"))
    options$randomVariables <- "facFive"
    options$samplingVariable1 <- list()
    options$samplingVariable2 <- list()
    options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                         values = list()))
    options$trendsTrend <- list()
    options$warmup <- 500
    set.seed(1)
    results <- jasptools::run("MixedModelsBGLMM", "debug.csv", options)
    
    
    test_that("Intercept table results match", {
      table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
      expect_equal_tables(table,
                          list(227.774430696025, 134.743958832805, -0.228921634308221, -0.58283673380392,
                               1.00746395630302, 0.164506008515831, 0.123199037645944))
    })
    
    test_that("contNormal (difference from intercept) table results match", {
      table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
      expect_equal_tables(table,
                          list(370.922245718373, 266.265744913265, -0.0954665089801237, -0.379166726010563,
                               0.999458872667041, 0.15601216641215, 0.194505100761341))
    })
  }
  # binomial / cauchit
  {
    options <- jasptools::analysisOptions("MixedModelsBGLMM")
    options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                          dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                            containsColumn = TRUE), fixedEffects = list(list()), 
                          fixedVariables = list(containsColumn = TRUE), plotsAgregatedOver = list(
                            containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                              list(value = list(containsColumn = TRUE))), value = list(
                                containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                          trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
    options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                   values = list()))
    options$chains <- 1
    options$dependentVariable <- "contBinom"
    options$fixedEffects <- list(list(components = "contNormal"))
    options$fixedVariables <- "contNormal"
    options$iteration <- 1000
    options$link <- "cauchit"
    options$plotsAgregatedOver <- "facFive"
    options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                         value = "contNormal")), value = "facFive"))
    options$randomVariables <- "facFive"
    options$samplingVariable1 <- list()
    options$samplingVariable2 <- list()
    options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                         values = list()))
    options$trendsTrend <- list()
    options$warmup <- 500
    set.seed(1)
    results <- jasptools::run("MixedModelsBGLMM", "debug.csv", options)
    
    
    test_that("Intercept table results match", {
      table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
      expect_equal_tables(table,
                          list(352.318702943033, 375.446589274476, -0.331048751110432, -0.767197212221807,
                               1.00196137958604, 0.213466590553267, 0.0748360582216316))
    })
    
    test_that("contNormal (difference from intercept) table results match", {
      table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
      expect_equal_tables(table,
                          list(288.765401562201, 165.607299704556, -0.151000277925848, -0.665960331069333,
                               1.00353912083651, 0.219004229826208, 0.301354795994995))
    }) 
  }
  # binomial / cloglog
  {
    options <- jasptools::analysisOptions("MixedModelsBGLMM")
    options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                          dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                            containsColumn = TRUE), fixedEffects = list(list()), 
                          fixedVariables = list(containsColumn = TRUE), plotsAgregatedOver = list(
                            containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                              list(value = list(containsColumn = TRUE))), value = list(
                                containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                          trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
    options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                   values = list()))
    options$chains <- 1
    options$dependentVariable <- "contBinom"
    options$fixedEffects <- list(list(components = "contNormal"))
    options$fixedVariables <- "contNormal"
    options$iteration <- 1000
    options$link <- "cloglog"
    options$plotsAgregatedOver <- "facFive"
    options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                         value = "contNormal")), value = "facFive"))
    options$randomVariables <- "facFive"
    options$samplingVariable1 <- list()
    options$samplingVariable2 <- list()
    options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                         values = list()))
    options$trendsTrend <- list()
    options$warmup <- 500
    set.seed(1)
    results <- jasptools::run("MixedModelsBGLMM", "debug.csv", options)
    
    
    test_that("Intercept table results match", {
      table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
      expect_equal_tables(table,
                          list(377.750548007873, 252.501287679477, -0.640503572113753, -1.01628507134149,
                               1.01863707885899, 0.198140609779535, -0.262284103788199))
    })
    
    test_that("contNormal (difference from intercept) table results match", {
      table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
      expect_equal_tables(table,
                          list(260.092930357529, 234.185336620634, -0.0903270980068153, -0.455443838691091,
                               0.998522941062149, 0.178716252918756, 0.268544815752488))
    })
  }
  # binomial / log
  {
    options <- jasptools::analysisOptions("MixedModelsBGLMM")
    options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                          dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                            containsColumn = TRUE), fixedEffects = list(list()), 
                          fixedVariables = list(containsColumn = TRUE), plotsAgregatedOver = list(
                            containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                              list(value = list(containsColumn = TRUE))), value = list(
                                containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                          trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
    options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                   values = list()))
    options$chains <- 1
    options$dependentVariable <- "contBinom"
    options$fixedEffects <- list(list(components = "contNormal"))
    options$fixedVariables <- "contNormal"
    options$iteration <- 1000
    options$link <- "log"
    options$plotsAgregatedOver <- "facFive"
    options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                         value = "contNormal")), value = "facFive"))
    options$randomVariables <- "facFive"
    options$samplingVariable1 <- list()
    options$samplingVariable2 <- list()
    options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                         values = list()))
    options$trendsTrend <- list()
    options$warmup <- 500
    set.seed(1)
    results <- jasptools::run("MixedModelsBGLMM", "debug.csv", options)
    
    
    test_that("Intercept table results match", {
      table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
      expect_equal_tables(table,
                          list(464.795502594327, 374.37844430355, -0.929377582365268, -1.22113127480402,
                               1.00204686228897, 0.151803846844622, -0.644670178511258))
    })
    
    test_that("contNormal (difference from intercept) table results match", {
      table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
      expect_equal_tables(table,
                          list(273.470700395767, 179.610781660361, -0.0633054676460669, -0.287509053795061,
                               0.99860451566656, 0.121344980061411, 0.186290956435622))
    })
  }
  # negative-binomial / identity
  {
    options <- jasptools::analysisOptions("MixedModelsBGLMM")
    options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                          dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                            containsColumn = TRUE), fixedEffects = list(list()), 
                          fixedVariables = list(containsColumn = TRUE), plotsAgregatedOver = list(
                            containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                              list(value = list(containsColumn = TRUE))), value = list(
                                containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                          trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
    options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                   values = list()))
    options$chains <- 1
    options$dependentVariable <- "V1"
    options$family <- "neg_binomial_2"
    options$fixedEffects <- list(list(components = "contBinom"))
    options$fixedVariables <- "contBinom"
    options$iteration <- 1000
    options$link <- "identity"
    options$plotsAgregatedOver <- "facFive"
    options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                         value = "contBinom")), value = "facFive"))
    options$randomVariables <- "facFive"
    options$samplingVariable1 <- list()
    options$samplingVariable2 <- list()
    options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                         values = list()))
    options$trendsTrend <- list()
    options$warmup <- 500
    set.seed(1)
    results <- jasptools::run("MixedModelsBGLMM", "debug.csv", options)
    
    
    test_that("Intercept table results match", {
      table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
      expect_equal_tables(table,
                          list(481.818453285672, 423.230216559696, 46.9981594701951, 39.2694984384181,
                               0.998406967373947, 3.92365574602942, 55.2819589660608))
    })
    
    test_that("contBinom (difference from intercept) table results match", {
      table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
      expect_equal_tables(table,
                          list(365.070711707983, 447.451153701175, 0.511865044007446, -4.01896943221816,
                               1.00676485614601, 2.27153332341802, 5.31134724613779))
    })
  }
  # beta / logit
  {
    options <- jasptools::analysisOptions("MixedModelsBGLMM")
    options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                          dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                            containsColumn = TRUE), fixedEffects = list(list()), 
                          fixedVariables = list(containsColumn = TRUE), plotsAgregatedOver = list(
                            containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                              list(value = list(containsColumn = TRUE))), value = list(
                                containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                          trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
    options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                   values = list()))
    options$chains <- 1
    options$dependentVariable <- "debCollin1"
    options$family <- "betar"
    options$fixedEffects <- list(list(components = "contBinom"))
    options$fixedVariables <- "contBinom"
    options$iteration <- 1000
    options$plotsAgregatedOver <- "facFive"
    options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                         value = "contBinom")), value = "facFive"))
    options$randomVariables <- "facFive"
    options$samplingVariable1 <- list()
    options$samplingVariable2 <- list()
    options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                         values = list()))
    options$trendsTrend <- list()
    options$warmup <- 500
    set.seed(1)
    results <- jasptools::run("MixedModelsBGLMM", "debug.csv", options)
    
    
    test_that("Intercept table results match", {
      table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
      expect_equal_tables(table,
                          list(710.31169631573, 324.256399529114, 0.648182737293342, 0.530391576119011,
                               1.0023408454047, 0.064932828996001, 0.76568852024984))
    })
    
    test_that("contBinom (difference from intercept) table results match", {
      table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
      expect_equal_tables(table,
                          list(466.697783247707, 293.760329653715, 0.0257928058192718, -0.166865848861074,
                               0.999446593547815, 0.104153474545017, 0.220001636546599))
    })
  }
}
#### overall summary ####
{
  options <- jasptools::analysisOptions("MixedModelsBGLMM")
  options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                        dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                          containsColumn = TRUE), fixedEffects = list(list()), 
                        fixedVariables = list(containsColumn = TRUE), plotsAgregatedOver = list(
                          containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                            list(value = list(containsColumn = TRUE))), value = list(
                              containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                        trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
  options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                 values = list()))
  options$chains <- 1
  options$dependentVariable <- "debCollin1"
  options$family <- "betar"
  options$fixedEffects <- list(list(components = "contBinom"))
  options$fixedVariables <- "contBinom"
  options$iteration <- 1000
  options$plotsAgregatedOver <- "facFive"
  options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                       value = "contBinom")), value = "facFive"))
  options$randomVariables <- "facFive"
  options$samplingVariable1 <- list()
  options$samplingVariable2 <- list()
  options$show <- "mmeans"
  options$showFE <- TRUE
  options$showRE <- TRUE
  options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                       values = list()))
  options$trendsTrend <- list()
  options$warmup <- 500
  set.seed(1)
  results <- jasptools::run("MixedModelsBGLMM", "debug.csv", options)
  
  
  test_that("Fixed Effects Estimates table results match", {
    table <- results[["results"]][["FEsummary"]][["data"]]
    expect_equal_tables(table,
                        list(0.648182737293342, 0.530391576119011, 747.352640957867, 1.00208220586709,
                             0.0630176800862631, "Intercept", 0.76568852024984, 0.0257928058192718,
                             -0.166865848861074, 472.305683168769, 0.999154208951953, 0.0991051542860211,
                             "contBinom", 0.220001636546599, 19.3676800811699, 14.6340011157705,
                             894.02941826223, 0.998051713836483, 2.64536784369731, "(phi))",
                             24.8955442101264))
  })
  
  test_that("facFive: Correlation Estimates table results match", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_CE1"]][["data"]]
    expect_equal_tables(table,
                        list(1, "Intercept", -0.208977434520931, 1, "contBinom"))
  })
  
  test_that("Residual Variance Estimates table results match", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_RES"]][["data"]]
    expect_equal_tables(table,
                        list(1, 1))
  })
  
  test_that("facFive: Variance Estimates table results match", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
    expect_equal_tables(table,
                        list(0.0720079688687572, 0.268343005999331, "Intercept", 0.0888820225950831,
                             0.298130881652812, "contBinom"))
  })
  
  test_that("Intercept table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
    expect_equal_tables(table,
                        list(710.31169631573, 324.256399529114, 0.648182737293342, 0.530391576119011,
                             1.0023408454047, 0.064932828996001, 0.76568852024984))
  })
  
  test_that("contBinom (trend) table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
    expect_equal_tables(table,
                        list(466.697783247707, 293.760329653715, 0.0257928058192718, -0.166865848861074,
                             0.999446593547815, 0.104153474545017, 0.220001636546599))
  })
  
  
  
  options <- jasptools::analysisOptions("MixedModelsBGLMM")
  options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE))), 
                        dependentVariable = list(containsColumn = TRUE), dependentVariableAggregation = list(
                          containsColumn = TRUE), fixedEffects = list(list()), 
                        fixedVariables = list(containsColumn = TRUE), plotsAgregatedOver = list(
                          containsColumn = TRUE), randomEffects = list(list(randomComponents = list(
                            list(value = list(containsColumn = TRUE))), value = list(
                              containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                        trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
  options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                 values = list()))
  options$chains <- 1
  options$dependentVariable <- "V1"
  options$family <- "poisson"
  options$fixedEffects <- list(list(components = "contBinom"))
  options$fixedVariables <- "contBinom"
  options$iteration <- 1000
  options$link <- "identity"
  options$plotsAgregatedOver <- "facFive"
  options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                       value = "contBinom")), value = "facFive"))
  options$randomVariables <- "facFive"
  options$samplingVariable1 <- list()
  options$samplingVariable2 <- list()
  options$showFE <- TRUE
  options$showRE <- TRUE
  options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                       values = list()))
  options$trendsTrend <- list()
  options$warmup <- 500
  set.seed(1)
  results <- jasptools::run("MixedModelsBGLMM", "debug.csv", options)
  
  
  test_that("Fixed Effects Estimates table results match", {
    table <- results[["results"]][["FEsummary"]][["data"]]
    expect_equal_tables(table,
                        list(48.1557775727442, 45.2713311905904, 315.365459249054, 0.998009196754918,
                             1.44048650699639, "Intercept", 51.1994880896209, 3.73616274873187,
                             -0.477222340512406, 181.939174879411, 1.00292349113031, 2.09433126524741,
                             "contBinom", 7.46692293804643))
  })
  
  test_that("facFive: Correlation Estimates table results match", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_CE1"]][["data"]]
    expect_equal_tables(table,
                        list(1, "Intercept", -0.451913838694585, 1, "contBinom"))
  })
  
  test_that("Residual Variance Estimates table results match", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_RES"]][["data"]]
    expect_equal_tables(table,
                        list(1, 1))
  })
  
  test_that("facFive: Variance Estimates table results match", {
    table <- results[["results"]][["REsummary"]][["collection"]][["REsummary_VE1"]][["data"]]
    expect_equal_tables(table,
                        list(2.79011757982138, 1.67036450507708, "Intercept", 5.26168989667027,
                             2.29383737363185, "contBinom"))
  })
  
  test_that("Intercept table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
    expect_equal_tables(table,
                        list(321.660298236393, 340.104372544917, 48.1557775727442, 45.2713311905904,
                             0.998110324808336, 1.24101405913827, 51.1994880896209))
  })
  
  test_that("contBinom (difference from intercept) table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
    expect_equal_tables(table,
                        list(190.444555913229, 247.792359026406, 3.73616274873187, -0.477222340512406,
                             1.00254385395416, 2.05674060551404, 7.46692293804643))
  })
}
#### estimated marginal means + contrasts #####
{
  options <- jasptools::analysisOptions("MixedModelsBGLMM")
  options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE)), 
                                         list(levels = list(containsColumn = TRUE)), list(levels = list(
                                           containsColumn = TRUE)), list(levels = list(containsColumn = TRUE)), 
                                         list(levels = list(containsColumn = TRUE))), dependentVariable = list(
                                           containsColumn = TRUE), dependentVariableAggregation = list(
                                             containsColumn = TRUE), fixedEffects = list(list(), list(), 
                                                                                         list()), fixedVariables = list(containsColumn = TRUE), marginalMeans = list(
                                                                                           list(), list()), plotsAgregatedOver = list(containsColumn = TRUE), 
                        randomEffects = list(list(randomComponents = list(list(value = list(
                          containsColumn = TRUE)), list(value = list(containsColumn = TRUE)), 
                          list(value = list(containsColumn = TRUE))), value = list(
                            containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                        trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
  options$Contrasts <- list(list(isContrast = FALSE, levels = c("Row 1", "Row 2", "Row 3", 
                                                                "Row 4", "Row 5", "Row 6"), name = "contNormal", values = c("-1", 
                                                                                                                            "0", "1", "-1", "0", "1")), list(isContrast = FALSE, levels = c("Row 1", 
                                                                                                                                                                                            "Row 2", "Row 3", "Row 4", "Row 5", "Row 6"), name = "facGender", 
                                                                                                                                                             values = c("f", "f", "f", "m", "m", "m")), list(isContrast = TRUE, 
                                                                                                                                                                                                             levels = c("Row 1", "Row 2", "Row 3", "Row 4", "Row 5", "Row 6"
                                                                                                                                                                                                             ), name = "Contrast 1", values = c("-1", "0", "1", "0", "0", 
                                                                                                                                                                                                                                                "0")), list(isContrast = TRUE, levels = c("Row 1", "Row 2", 
                                                                                                                                                                                                                                                                                          "Row 3", "Row 4", "Row 5", "Row 6"), name = "Contrast 2", values = c("0", 
                                                                                                                                                                                                                                                                                                                                                               "0", "0", "0", "0", "0")), list(isContrast = TRUE, levels = c("Row 1", 
                                                                                                                                                                                                                                                                                                                                                                                                                             "Row 2", "Row 3", "Row 4", "Row 5", "Row 6"), name = "Contrast 3", 
                                                                                                                                                                                                                                                                                                                                                                                               values = c("0", "1", "0", "-1", "0", "0")))
  options$chains <- 1
  options$dependentVariable <- "contBinom"
  options$fixedEffects <- list(list(components = "contNormal"), list(components = "facGender"), 
                               list(components = c("contNormal", "facGender")))
  options$fixedVariables <- c("\"contNormal\"", "\"facGender\"")
  options$iteration <- 1000
  options$marginalMeans <- list(list(variable = "contNormal"), list(variable = "facGender"))
  options$marginalMeansContrast <- TRUE
  options$plotsAgregatedOver <- "facFive"
  options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                       value = "contNormal"), list(randomSlopes = TRUE, value = "facGender"), 
                                                                                  list(randomSlopes = TRUE, value = "contNormal * facGender")), 
                                     value = "facFive"))
  options$randomVariables <- "facFive"
  options$samplingVariable1 <- list()
  options$samplingVariable2 <- list()
  options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                       values = list()))
  options$trendsTrend <- list()
  options$warmup <- 500
  set.seed(1)
  results <- jasptools::run("MixedModelsBGLMM", "debug.csv", options)
  
  
  test_that("Estimated Marginal Means table results match", {
    table <- results[["results"]][["EMMsummary"]][["data"]]
    expect_equal_tables(table,
                        list(-1.24716219673316, 0.562983944089333, "f", 0.349577425147478,
                             1, 0.771747264463438, -0.18874858754, 0.459695076915517, "f",
                             0.299046159695554, 2, 0.606902661417908, 0.869665021653162,
                             0.359099652650355, "f", 0.118895519248182, 3, 0.597709774777667,
                             -1.24716219673316, 0.332397226400491, "m", 0.135821881437974,
                             4, 0.570965718034376, -0.18874858754, 0.35026042381221, "m",
                             0.214353324433923, 5, 0.498703601120805, 0.869665021653162,
                             0.376853445044484, "m", 0.16625158772795, 6, 0.552166558898216
                        ))
  })
  
  test_that("Intercept table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
    expect_equal_tables(table,
                        list(427.491963217903, 183.291361065234, -0.42341265885672, -0.986756153171065,
                             0.999250149395247, 0.249786376694917, 0.0565001843324826))
  })
  
  test_that("contNormal (difference from intercept) table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
    expect_equal_tables(table,
                        list(555.426972265769, 399.141656983575, -0.164257995973173, -0.711104668808576,
                             1.00509712428423, 0.279152466624136, 0.355655655654459))
  })
  
  test_that("facGender (difference from intercept) table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_3"]][["data"]]
    expect_equal_tables(table,
                        list(602.360787422656, 373.281075992294, 0.248324014468914, "f", -0.177238028860057,
                             1.00008924672657, 0.213563794964421, 0.669238265038089, 583.462304435207,
                             381.419565074341, -0.186317085004739, "m", -0.615067412655818,
                             0.998921896041412, 0.242162325472575, 0.277175231662363))
  })
  
  test_that("contNormal*facGender (difference from intercept) table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_4"]][["data"]]
    expect_equal_tables(table,
                        list(418.566953860762, 256.437104134348, -0.419549853722738, "facGenderf",
                             -1.3180518405685, 1.00156144917888, 0.376001040484898, 0.329794191341252,
                             638.071244094195, 404.597837111795, 0.0910338617763935, "facGenderm",
                             -0.546954529893945, 1.00241343758194, 0.317620424473606, 0.804526692598267
                        ))
  })
  
  test_that("Contrasts table results match", {
    table <- results[["results"]][["contrasts_Means"]][["data"]]
    expect_equal_tables(table,
                        list("Contrast 1", -0.202058238369858, -0.531448421856406, 0.203370033852118,
                             "Contrast 3", 0.121851142407508, -0.136841129857329, 0.366444434917067
                        ))
  })
}
#### estimated marginal means + contrasts (non-response scale) #####
{
  options <- jasptools::analysisOptions("MixedModelsBGLMM")
  options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE)), 
                                         list(levels = list(containsColumn = TRUE)), list(levels = list(
                                           containsColumn = TRUE)), list(levels = list(containsColumn = TRUE)), 
                                         list(levels = list(containsColumn = TRUE))), dependentVariable = list(
                                           containsColumn = TRUE), dependentVariableAggregation = list(
                                             containsColumn = TRUE), fixedEffects = list(list(), list(), 
                                                                                         list()), fixedVariables = list(containsColumn = TRUE), marginalMeans = list(
                                                                                           list(), list()), plotsAgregatedOver = list(containsColumn = TRUE), 
                        randomEffects = list(list(randomComponents = list(list(value = list(
                          containsColumn = TRUE)), list(value = list(containsColumn = TRUE)), 
                          list(value = list(containsColumn = TRUE))), value = list(
                            containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                        trendsContrasts = list(list(levels = list(containsColumn = TRUE))))
  options$Contrasts <- list(list(isContrast = FALSE, levels = c("Row 1", "Row 2", "Row 3", 
                                                                "Row 4", "Row 5", "Row 6"), name = "contNormal", values = c("-2", 
                                                                                                                            "0", "2", "-2", "0", "2")), list(isContrast = FALSE, levels = c("Row 1", 
                                                                                                                                                                                            "Row 2", "Row 3", "Row 4", "Row 5", "Row 6"), name = "facGender", 
                                                                                                                                                             values = c("f", "f", "f", "m", "m", "m")), list(isContrast = TRUE, 
                                                                                                                                                                                                             levels = c("Row 1", "Row 2", "Row 3", "Row 4", "Row 5", "Row 6"
                                                                                                                                                                                                             ), name = "Contrast 1", values = c("-1", "0", "1", "0", "0", 
                                                                                                                                                                                                                                                "0")), list(isContrast = TRUE, levels = c("Row 1", "Row 2", 
                                                                                                                                                                                                                                                                                          "Row 3", "Row 4", "Row 5", "Row 6"), name = "Contrast 2", values = c("0", 
                                                                                                                                                                                                                                                                                                                                                               "0", "0", "0", "0", "0")), list(isContrast = TRUE, levels = c("Row 1", 
                                                                                                                                                                                                                                                                                                                                                                                                                             "Row 2", "Row 3", "Row 4", "Row 5", "Row 6"), name = "Contrast 3", 
                                                                                                                                                                                                                                                                                                                                                                                               values = c("0", "1", "0", "-1", "0", "0")))
  options$chains <- 1
  options$dependentVariable <- "contBinom"
  options$fixedEffects <- list(list(components = "contNormal"), list(components = "facGender"), 
                               list(components = c("contNormal", "facGender")))
  options$fixedVariables <- c("\"contNormal\"", "\"facGender\"")
  options$iteration <- 1000
  options$marginalMeans <- list(list(variable = "contNormal"), list(variable = "facGender"))
  options$marginalMeansCIwidth <- 0.2
  options$marginalMeansContrast <- TRUE
  options$marginalMeansResponse <- FALSE
  options$marginalMeansSD <- 2
  options$plotsAgregatedOver <- "facFive"
  options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                       value = "contNormal"), list(randomSlopes = TRUE, value = "facGender"), 
                                                                                  list(randomSlopes = TRUE, value = "contNormal * facGender")), 
                                     value = "facFive"))
  options$randomVariables <- "facFive"
  options$samplingVariable1 <- list()
  options$samplingVariable2 <- list()
  options$trendsContrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                       values = list()))
  options$trendsTrend <- list()
  options$warmup <- 500
  set.seed(1)
  results <- jasptools::run("MixedModelsBGLMM", "debug.csv", options)
  
  
  test_that("Estimated Marginal Means table results match", {
    table <- results[["results"]][["EMMsummary"]][["data"]]
    expect_equal_tables(table,
                        list(-2.30557580592632, 0.660489358090744, "f", 0.348503713467344,
                             1, 0.694125330095132, -0.18874858754, -0.161570259156788, "f",
                             -0.23399987995735, 2, -0.0787935580013043, 1.92807863084632,
                             -0.986614970886505, "f", -1.11870125603295, 3, -0.721319911444392,
                             -2.30557580592632, -0.749624917090976, "m", -0.816046913202669,
                             4, -0.439183390007487, -0.18874858754, -0.617895044294047, "m",
                             -0.71517442397752, 5, -0.556197769630177, 1.92807863084632,
                             -0.430206253890729, "m", -0.717408106291287, 6, -0.36463241918358
                        ))
  })
  
  test_that("Intercept table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
    expect_equal_tables(table,
                        list(427.491963217903, 183.291361065234, -0.42341265885672, -0.986756153171065,
                             0.999250149395247, 0.249786376694917, 0.0565001843324826))
  })
  
  test_that("contNormal (difference from intercept) table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
    expect_equal_tables(table,
                        list(555.426972265769, 399.141656983575, -0.164257995973173, -0.711104668808576,
                             1.00509712428423, 0.279152466624136, 0.355655655654459))
  })
  
  test_that("facGender (difference from intercept) table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_3"]][["data"]]
    expect_equal_tables(table,
                        list(602.360787422656, 373.281075992294, 0.248324014468914, "f", -0.177238028860057,
                             1.00008924672657, 0.213563794964421, 0.669238265038089, 583.462304435207,
                             381.419565074341, -0.186317085004739, "m", -0.615067412655818,
                             0.998921896041412, 0.242162325472575, 0.277175231662363))
  })
  
  test_that("contNormal*facGender (difference from intercept) table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_4"]][["data"]]
    expect_equal_tables(table,
                        list(418.566953860762, 256.437104134348, -0.419549853722738, "facGenderf",
                             -1.3180518405685, 1.00156144917888, 0.376001040484898, 0.329794191341252,
                             638.071244094195, 404.597837111795, 0.0910338617763935, "facGenderm",
                             -0.546954529893945, 1.00241343758194, 0.317620424473606, 0.804526692598267
                        ))
  })
  
  test_that("Contrasts table results match", {
    table <- results[["results"]][["contrasts_Means"]][["data"]]
    expect_equal_tables(table,
                        list("Contrast 1", -1.68826179252171, -2.02090512099624, -1.27566617766059,
                             "Contrast 3", 0.636916635004943, 0.381466747824437, 0.790674854738761
                        ))
  })
}
#### estimated trends + contrasts #####
{
  options <- jasptools::analysisOptions("MixedModelsBGLMM")
  options$.meta <- list(Contrasts = list(list(levels = list(containsColumn = TRUE)), 
                                         list(levels = list(containsColumn = TRUE)), list(levels = list(
                                           containsColumn = TRUE))), dependentVariable = list(containsColumn = TRUE), 
                        dependentVariableAggregation = list(containsColumn = TRUE), 
                        fixedEffects = list(list(), list(), list()), fixedVariables = list(
                          containsColumn = TRUE), plotsAgregatedOver = list(containsColumn = TRUE), 
                        randomEffects = list(list(randomComponents = list(list(value = list(
                          containsColumn = TRUE)), list(value = list(containsColumn = TRUE)), 
                          list(value = list(containsColumn = TRUE))), value = list(
                            containsColumn = TRUE))), randomVariables = list(containsColumn = TRUE), 
                        trendsContrasts = list(list(levels = list(containsColumn = TRUE)), 
                                               list(levels = list(containsColumn = TRUE))), trendsTrend = list(
                                                 list()), trendsVariables = list(list()))
  options$Contrasts <- list(list(isContrast = TRUE, levels = list(), name = "Contrast 1", 
                                 values = list()), list(isContrast = TRUE, levels = list(), 
                                                        name = "Contrast 2", values = list()), list(isContrast = TRUE, 
                                                                                                    levels = list(), name = "Contrast 3", values = list()))
  options$chains <- 1
  options$dependentVariable <- "contBinom"
  options$fixedEffects <- list(list(components = "contNormal"), list(components = "facGender"), 
                               list(components = c("contNormal", "facGender")))
  options$fixedVariables <- c("\"contNormal\"", "\"facGender\"")
  options$iteration <- 1000
  options$marginalMeansCIwidth <- 0.5
  options$marginalMeansContrast <- TRUE
  options$plotsAgregatedOver <- "facFive"
  options$randomEffects <- list(list(correlations = TRUE, randomComponents = list(list(randomSlopes = TRUE, 
                                                                                       value = "contNormal"), list(randomSlopes = TRUE, value = "facGender"), 
                                                                                  list(randomSlopes = TRUE, value = "contNormal * facGender")), 
                                     value = "facFive"))
  options$randomVariables <- "facFive"
  options$samplingVariable1 <- list()
  options$samplingVariable2 <- list()
  options$trendsCIwidth <- 0.5
  options$trendsContrast <- TRUE
  options$trendsContrasts <- list(list(isContrast = FALSE, levels = c("Row 1", "Row 2"), name = "facGender", 
                                       values = c("f", "m")), list(isContrast = TRUE, levels = c("Row 1", 
                                                                                                 "Row 2"), name = "Contrast 1", values = c("1", "-1")))
  options$trendsTrend <- list(list(variable = "contNormal"))
  options$trendsVariables <- list(list(variable = "facGender"))
  options$warmup <- 500
  set.seed(1)
  results <- jasptools::run("MixedModelsBGLMM", "debug.csv", options)
  
  
  test_that("Intercept table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_1"]][["data"]]
    expect_equal_tables(table,
                        list(427.491963217903, 183.291361065234, -0.42341265885672, -0.986756153171065,
                             0.999250149395247, 0.249786376694917, 0.0565001843324826))
  })
  
  test_that("contNormal (difference from intercept) table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_2"]][["data"]]
    expect_equal_tables(table,
                        list(555.426972265769, 399.141656983575, -0.164257995973173, -0.711104668808576,
                             1.00509712428423, 0.279152466624136, 0.355655655654459))
  })
  
  test_that("facGender (difference from intercept) table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_3"]][["data"]]
    expect_equal_tables(table,
                        list(602.360787422656, 373.281075992294, 0.248324014468914, "f", -0.177238028860057,
                             1.00008924672657, 0.213563794964421, 0.669238265038089, 583.462304435207,
                             381.419565074341, -0.186317085004739, "m", -0.615067412655818,
                             0.998921896041412, 0.242162325472575, 0.277175231662363))
  })
  
  test_that("contNormal*facGender (difference from intercept) table results match", {
    table <- results[["results"]][["STANOVAsummary"]][["collection"]][["STANOVAsummary_summary_4"]][["data"]]
    expect_equal_tables(table,
                        list(418.566953860762, 256.437104134348, -0.419549853722738, "facGenderf",
                             -1.3180518405685, 1.00156144917888, 0.376001040484898, 0.329794191341252,
                             638.071244094195, 404.597837111795, 0.0910338617763935, "facGenderm",
                             -0.546954529893945, 1.00241343758194, 0.317620424473606, 0.804526692598267
                        ))
  })
  
  test_that("Contrasts table results match", {
    table <- results[["results"]][["contrasts_Trends"]][["data"]]
    expect_equal_tables(table,
                        list("Contrast 1", -0.508189890940246, -0.731012641121986, -0.115689301741281
                        ))
  })
  
  test_that("Estimated Trends table results match", {
    table <- results[["results"]][["trendsSummary"]][["data"]]
    expect_equal_tables(table,
                        list("f", -0.595616876697386, 1, -0.398771750915196, -0.114182221755616,
                             "m", -0.111229084932071, 2, 0.0728326012604415, 0.304576357915503
                        ))
  })
}