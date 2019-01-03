context("ANOVA")

# does not test
# - if analysis handles too few observations

test_that("Main table results match", {
  options <- jasptools::analysisOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- c("facFive", "contBinom")
  options$wlsWeights <- "facFifty"
  options$modelTerms <- list(
    list(components="facFive"),
    list(components="contBinom"),
    list(components=c("facFive", "contBinom"))
  )
  options$contrasts <- list(
    list(contrast="none", variable="facFive"),
    list(contrast="none", variable="contBinom")
  )
  options$effectSizeEstimates <- TRUE
  options$effectSizeEtaSquared <- TRUE
  options$effectSizeOmegaSquared <- TRUE
  options$effectSizePartialEtaSquared <- TRUE
  options$VovkSellkeMPR <- TRUE

  refTables <- list(
    type1 = list("facFive", 181.151987151139, 4, 45.2879967877848, 1.82424792091397,
                 0.131040166930968, "TRUE", 0.0733818170125722, 0.0749970945389635,
                 0.0328259131508147, 1.3814133611929, "contBinom", 24.5000310074981,
                 1, 24.5000310074981, 0.98688689714382, 0.323167856084615, "FALSE",
                 0.00992457670748362, 0.0108464739348588, 0, 1.00776449783097,
                 "facFive <unicode> contBinom", 28.6688295533663, 4, 7.16720738834158,
                 0.288702616682483, 0.884618241388975, "FALSE", 0.011613291343553,
                 0.0126686727866262, 0, 1, "Residual", 2234.30141494065, 90,
                 24.8255712771184, "", "", "TRUE", "", "", "", ""),
    type2 = list("facFive", 174.090039449647, 4, 43.5225098624118, 1.75313225933803,
                 0.145323959824188, "TRUE", 0.0707234506472374, 0.0722847770997921,
                 0.0300789007673393, 1.31245187936232, "contBinom", 24.5000310074979,
                 1, 24.5000310074979, 0.986886897143812, 0.323167856084617, "FALSE",
                 0.00995304923413341, 0.0108464739348588, 0, 1.00776449783097,
                 "facFive <unicode> contBinom", 28.6688295533663, 4, 7.16720738834158,
                 0.288702616682483, 0.884618241388975, "FALSE", 0.0116466086080589,
                 0.0126686727866262, 0, 1, "Residual", 2234.30141494066, 90,
                 24.8255712771184, "", "", "TRUE", "", "", "", ""),
    type3 = list("facFive", 165.514600111922, 4, 41.3786500279805, 1.66677534088083,
                 0.164641950089634, "TRUE", 0.0675231783920824, 0.0689697039580327,
                 0.0267410840853991, 1.23860485419559, "contBinom", 22.7415942746311,
                 1, 22.7415942746311, 0.916055224702601, 0.341076850898726, "FALSE",
                 0.00927763910910521, 0.0100758355874387, 0, 1.00272840654715,
                 "facFive <unicode> contBinom", 28.6688295533663, 4, 7.16720738834158,
                 0.288702616682484, 0.884618241388975, "FALSE", 0.0116957083599583,
                 0.0126686727866262, 0, 1, "Residual", 2234.30141494065, 90,
                 24.8255712771184, "", "", "TRUE", "", "", "", "")
  )

  for (type in c("type1", "type2", "type3")) {
    options$sumOfSquares <- type
    results <- jasptools::run("Anova", "test.csv", options, view=FALSE, quiet=TRUE)
    table <- results[["results"]][["anova"]][["data"]]
    expect_equal_tables(table, refTables[[type]], label=paste("Table with SS", type))
  }
})

test_that("Homogeneity of Variances table results match", {
  options <- jasptools::analysisOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facExperim"
  options$modelTerms <- list(list(components="facExperim"))
  options$homogeneityTests <- TRUE
  options$VovkSellkeMPR <- TRUE
  results <- jasptools::run("Anova", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["assumptionsObj"]][["levene"]][["data"]]
  expect_equal_tables(table, list(3.1459013381035, 1, 98, 0.0792241296904395, 1.83142365040653, 1))
})

# Contrasts verified with SPSS
test_that("Contrasts table results match", {
  options <- jasptools::analysisOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(list(components="facFive"))

  refTables <- list(
    deviation = list("2 - 1, 2, 3, 4, 5", -0.19513913461, 0.211517937182488, -0.922565420263354,
                     0.35857088018682, "TRUE", "3 - 1, 2, 3, 4, 5", 0.33149855864,
                     0.211517937182488, 1.56723615526752, 0.12038454371847, "FALSE",
                     "4 - 1, 2, 3, 4, 5", -0.16911442746, 0.211517937182488, -0.799527594267789,
                     0.425979159402386, "FALSE", "5 - 1, 2, 3, 4, 5", 0.18254372644,
                     0.211517937182488, 0.86301771316212, 0.390301247101765, "FALSE"),
    simple = list("2 - 1", -0.0453504116000002, 0.334439223738541, -0.135601354090735,
                  0.892423350599012, "TRUE", "3 - 1", 0.48128728165, 0.334439223738541,
                  1.43908742601993, 0.153412521131058, "FALSE", "4 - 1", -0.0193257044500001,
                  0.334439223738541, -0.0577854003904418, 0.954040939034334, "FALSE",
                  "5 - 1", 0.33233244945, 0.334439223738541, 0.993700576550232,
                  0.322892982956191, "FALSE"),
    difference = list("2 - 1", -0.0453504116, 0.334439223738541, -0.135601354090734,
                      0.892423350599013, "TRUE", "3 - 1, 2", 0.50396248745, 0.289632863779524,
                      1.74000450388679, 0.0850966309951808, "FALSE", "4 - 1, 2, 3",
                      -0.164637994466667, 0.273068482710641, -0.602918333278054, 0.547999665617461,
                      "FALSE", "5 - 1, 2, 3, 4", 0.22817965805, 0.26439742147811,
                      0.86301771316212, 0.390301247101765, "FALSE"),
    Helmert = list("1 - 2, 3, 4, 5", -0.1872359037625, 0.26439742147811, -0.708160853898499,
                   0.480579313597282, "TRUE", "2 - 3, 4, 5", -0.31011508715, 0.273068482710641,
                   -1.13566781516348, 0.25895260195633, "FALSE", "3 - 4, 5", 0.32478390915,
                   0.289632863779524, 1.12136414670551, 0.264959410721321, "FALSE",
                   "4 - 5", -0.3516581539, 0.334439223738541, -1.05148597694067,
                   0.295702769469608, "FALSE"),
    repeated = list("1 - 2", 0.0453504115999998, 0.334439223738541, 0.135601354090733,
                    0.892423350599014, "TRUE", "2 - 3", -0.52663769325, 0.334439223738541,
                    -1.57468878011066, 0.118652819171739, "FALSE", "3 - 4", 0.5006129861,
                    0.334439223738541, 1.49687282641037, 0.137741463128169, "FALSE",
                    "4 - 5", -0.3516581539, 0.334439223738541, -1.05148597694067,
                    0.295702769469608, "FALSE"),
    polynomial = list("linear", 0.218415231132241, 0.236484243000287, 0.923593167820386,
                      0.358038105335522, "TRUE", "quadratic", -0.0623342877876619,
                      0.236484243000287, -0.263587488945664, 0.792668695639493, "FALSE",
                      "cubic", 0.0886332780579033, 0.236484243000287, 0.374795702806278,
                      0.70864779281998, "FALSE", "quartic", 0.415791419838834, 0.236484243000287,
                      1.75822039795831, 0.0819306308915546, "FALSE")
  )

  contrasts <- c("deviation", "simple", "difference", "Helmert", "repeated", "polynomial")
  for (contrast in contrasts) {
    options$contrasts <- list(list(contrast=contrast, variable="facFive"))
    results <- jasptools::run("Anova", "test.csv", options, view=FALSE, quiet=TRUE)
    table <- results[["results"]][["contrasts"]][["collection"]][[1]][["data"]]
    expect_equal_tables(table, refTables[[contrast]], label=paste("Table with contrast", contrast))
  }
})

test_that("Post Hoc table results match", {
  options <- jasptools::analysisOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "contBinom"
  options$modelTerms <- list(list(components="contBinom"))
  options$postHocTestEffectSize <- TRUE
  options$postHocTestsBonferroni <- TRUE
  options$postHocTestsHolm <- TRUE
  options$postHocTestsScheffe <- TRUE
  options$postHocTestsTukey <- TRUE
  options$postHocTestsVariables <- "contBinom"
  results <- jasptools::run("Anova", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["posthoc"]][["collection"]][[1]][["data"]]
  expect_equal_tables(table,
    list(0, 1, 0.163364220743842, 0.214904085649005, 0.760172707980336,
         0.15401876311258, 0.448976320466698, 0.448976320466698, 0.448976320466698,
         "", 0.448976320466698, "TRUE")
    )
})

test_that("Marginal Means table results match", {
  options <- jasptools::analysisOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "contBinom"
  options$modelTerms <- list(list(components="contBinom"))
  options$marginalMeansCompareMainEffects <- TRUE
  options$marginalMeansTerms <- "contBinom"

  refTables <- list(
    none = list(0, -0.120135614827586, 0.139273765411964, -0.396519869554477,
                0.156248639899304, -0.862586104943973, 0.390471041862811, "TRUE",
                1, -0.283499835571429, 0.163666075582597, -0.608289835972217,
                0.0412901648293597, -1.73218447721841, 0.0863869418751253, "FALSE"),
    Bonferroni = list(0, -0.120135614827586, 0.139273765411964, -0.396519869554477,
                      0.156248639899304, -0.862586104943973, 0.390471041862811, "TRUE",
                      1, -0.283499835571429, 0.163666075582597, -0.608289835972217,
                      0.0412901648293597, -1.73218447721841, 0.0863869418751253, "FALSE"),
    Sidak = list(0, -0.120135614827586, 0.139273765411964, -0.396519869554477,
                 0.156248639899304, -0.862586104943973, 0.390471041862811, "TRUE",
                 1, -0.283499835571429, 0.163666075582597, -0.608289835972217,
                 0.0412901648293597, -1.73218447721841, 0.0863869418751253, "FALSE")
  )

  for (adjustment in c("none", "Bonferroni", "Sidak")) {
    options$marginalMeansCIAdjustment <- adjustment
    results <- jasptools::run("Anova", "test.csv", options, view=FALSE, quiet=TRUE)
    table <- results[["results"]][["marginalMeans"]][["collection"]][[1]][["data"]]
    expect_equal_tables(table, refTables[[adjustment]], label=paste("Table with CI adjustment", adjustment))
  }
})

test_that("Descriptives table results match", {
  options <- jasptools::analysisOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "contBinom"
  options$modelTerms <- list(list(components="contBinom"))
  options$descriptives <- TRUE
  results <- jasptools::run("Anova", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["descriptivesObj"]][["descriptivesTable"]][["data"]]
  expect_equal_tables(table,
    list(0, 58, -0.120135614827586, 1.10575982846952, "TRUE", 1, 42, -0.283499835571429,
         0.994612407217046, "FALSE")
  )
})

# test_that("Q-Q plot matches", {
#   options <- jasptools::analysisOptions("Anova")
#   options$dependent <- "contNormal"
#   options$fixedFactors <- "contBinom"
#   options$modelTerms <- list(list(components="contBinom"))
#   options$qqPlot <- TRUE
#   results <- jasptools::run("Anova", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "q-q", dir="Anova")
# })

# test_that("Descriptives plots match", {
#   options <- jasptools::analysisOptions("Anova")
#   options$dependent <- "contNormal"
#   options$fixedFactors <- c("facFive", "contBinom")
#   options$wlsWeights <- "facFifty"
#   options$modelTerms <- list(
#     list(components="facFive"),
#     list(components="contBinom"),
#     list(components=c("facFive", "contBinom"))
#   )
#   options$plotHorizontalAxis <- "contBinom"
#   options$plotSeparateLines <- "facFive"
#   options$plotErrorBars <- TRUE
#   options$confidenceIntervalInterval <- 0.90
#
#   options$errorBarType <- "confidenceInterval"
#   results <- jasptools::run("Anova", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "descriptives-ci", dir="Anova")
#
#   options$errorBarType <- "standardError"
#   results <- jasptools::run("Anova", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "descriptives-se", dir="Anova")
# })

test_that("Simple Main Effects table results match", {
  options <- jasptools::analysisOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- c( "facFive", "facExperim")
  options$modelTerms <- list(
    list(components="facExperim"),
    list(components="facFive")
  )
  options$simpleFactor <- "facExperim"
  options$moderatorFactorOne <- "facFive"
  options$moderatorFactorTwo <- ""
  options$homogeneityTests <- TRUE
  options$VovkSellkeMPR <- TRUE
  results <- jasptools::run("Anova", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["simpleEffects"]][["data"]]
  expect_equal_tables(table, list(1, 0.350864897951646, 1, 0.350864897951646, 0.310783783968887,
                                  0.578524772558188, "TRUE", 2, 2.72259751707838, 1, 2.72259751707838,
                                  2.41158110578085, 0.123801175704108, "FALSE", 3, 0.300954391532799,
                                  1, 0.300954391532799, 0.266574813122249, 0.606851206017453,
                                  "FALSE", 4, 3.47907983036715, 1, 3.47907983036715, 3.08164652754846,
                                  0.0824380354608798, "FALSE", 5, 0.313611321775938, 1, 0.313611321775938,
                                  0.27778587668933, 0.599397784945329, "FALSE"))
})

test_that("Analysis handles errors", {
  options <- jasptools::analysisOptions("Anova")
  options$dependent <- "debInf"
  options$fixedFactors <- "contBinom"
  options$modelTerms <- list(list(components="contBinom"))
  results <- jasptools::run("Anova", "test.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["anova"]][["error"]][["errorType"]], "badData",
                   label="Inf dependent check")

  options$dependent <- "contNormal"
  options$fixedFactors <- "contBinom"
  options$wlsWeights <- "debInf"
  options$modelTerms <- list(list(components="contBinom"))
  results <- jasptools::run("Anova", "test.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["anova"]][["error"]][["errorType"]], "badData",
                  label="Inf WLS weights check")

  options$dependent <- "contNormal"
  options$fixedFactors <- "debSame"
  options$modelTerms <- list(list(components="debSame"))
  results <- jasptools::run("Anova", "test.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["anova"]][["error"]][["errorType"]], "badData",
                  label="1-level factor check")

  options$dependent <- "debSame"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(list(components="facFive"))
  results <- jasptools::run("Anova", "test.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["anova"]][["error"]][["errorType"]], "badData",
                  label="No variance check")

  options$dependent <- "contGamma"
  options$fixedFactors <- "facFive"
  options$wlsWeights <- "contNormal"
  options$modelTerms <- list(list(components="facFive"))
  results <- jasptools::run("Anova", "test.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["anova"]][["error"]][["errorType"]], "badData",
                  label="Negative WLS weights check")
})

# Below are the unit tests for Andy Field's book

# Chapter 4
test_that("Fields Book - Chapter 4 results match", {
  options <- jasptools::analysisOptions("Anova")
  options$dependent <- "Happiness"
  options$fixedFactors <- "Dose"
  options$modelTerms <- list(
    list(components="Dose")
  )
  options$descriptives <- TRUE
  options$plotHorizontalAxis <- "Dose"
  options$plotErrorBars <- TRUE
  options$errorBarType <- "confidenceInterval"
  options$postHocTestsVariables <- "Dose"
  options$postHocTestEffectSize <- TRUE
  results <- jasptools::run("Anova", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_04/www/Puppies Dummy.sav"), options, view=FALSE, quiet=TRUE)
  tableOutput2 <- results[["results"]][["descriptivesObj"]][["descriptivesTable"]][["data"]]
  expect_equal_tables(tableOutput2,
                      list(1, 5, 2.2, 1.30384, "TRUE",
                           2, 5, 3.2, 1.30384, "FALSE",
                           3, 5, 5, 1.581139, "FALSE")
  )
  figure3 <- results[["state"]][["figures"]][[1]] # Q-Q-Plot
  #expect_equal_plots(figure3, "?", dir="Anova") # This command needs to be updated
  
  tableOutput3 <- results[["results"]][["anova"]][["data"]]
  expect_equal_tables(tableOutput3,
                      list("Dose", 20.13333, 2, 10.06667, 5.118644, 0.02469429, "TRUE",
                           "Residual", 23.6, 12, 1.966667, "", "", "TRUE")
  )
  # Insert test for Welch and the Brown-Forsythe F-statistics (Output 4)
  
  cohensdPuppiesOutput <- list(results[["results"]][["posthoc"]][["collection"]][[1]][["data"]][[1]][["Cohen's d"]], results[["results"]][["posthoc"]][["collection"]][[1]][["data"]][[2]][["Cohen's d"]], results[["results"]][["posthoc"]][["collection"]][[1]][["data"]][[3]][["Cohen's d"]]) 
  expect_equal_tables(cohensdPuppiesOutput,
                      list(-0.766965, -1.932184, -1.242118)
  )
  
  options <- jasptools::analysisOptions("Anova")
  options$dependent <- "injury"
  options$fixedFactors <- "hero"
  options$modelTerms <- list(
    list(components="hero")
  )
  options$postHocTestsVariables <- "hero"
  options$postHocTestEffectSize <- TRUE
  results <- jasptools::run("Anova", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_04/www/Superhero.sav"), options, view=FALSE, quiet=TRUE)
  cohensdSupermanOutput <- list(results[["results"]][["posthoc"]][["collection"]][[1]][["data"]][[1]][["Cohen's d"]], 
                                results[["results"]][["posthoc"]][["collection"]][[1]][["data"]][[2]][["Cohen's d"]], 
                                results[["results"]][["posthoc"]][["collection"]][[1]][["data"]][[3]][["Cohen's d"]], 
                                results[["results"]][["posthoc"]][["collection"]][[1]][["data"]][[4]][["Cohen's d"]], 
                                results[["results"]][["posthoc"]][["collection"]][[1]][["data"]][[5]][["Cohen's d"]], 
                                results[["results"]][["posthoc"]][["collection"]][[1]][["data"]][[6]][["Cohen's d"]])
  expect_equal_tables(cohensdSupermanOutput,
                      list(1.261983, 1.620304, 2.602089, 0.4878571, 1.480746, 0.8234014)
  )
})

# Chapter 5
test_that("Fields Book - Chapter 5 results match", {
  options <- jasptools::analysisOptions("Anova")
  options$dependent <- "Happiness"
  options$fixedFactors <- "Dose"
  options$modelTerms <- list(
    list(components="Dose")
  )
  options$contrasts <- list(list(contrast = "Helmert", variable = "Dose"))
  options$postHocTestsVariables <- c("Dose")
  results <- jasptools::run("Anova", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_04/www/Puppies Dummy.sav"), options, view=FALSE, quiet=TRUE)
  tableOutput5 <- results[["results"]][["contrasts"]][["collection"]][[1]][["data"]]
  expect_equal_tables(tableOutput5,
                      list("1 - 2, 3", -1.9, 0.7681146, -2.473589, 0.02930022, "TRUE",
                           "2 - 3", -1.8, 0.8869423, -2.029444, 0.06519221, "FALSE")
  )
  tableOutput6 <- results[["results"]][["posthoc"]][["collection"]][[1]][["data"]]
  expect_equal_tables(tableOutput6,
                      list(1, 2, -1, 0.8869423, -1.127469, "", 0.5163208, "", "", "", "", "TRUE",
                           1, 3, -2.8, 0.8869423, -3.156913, "", 0.02097356, "", "", "", "", "FALSE",
                           2, 3, -1.8, 0.8869423, -2.029444, "", 0.1473583, "", "", "", "", "FALSE")
  )

  options <- jasptools::analysisOptions("Anova")
  options$dependent <- "injury"
  options$fixedFactors <- "hero"
  options$modelTerms <- list(
    list(components="hero")
  )
  options$contrasts <- list(list(contrast = "simple", variable = "hero"))
  results <- jasptools::run("Anova", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_05/www/Superhero.sav"), options, view=FALSE, quiet=TRUE)
  tableOutput8Part1 <- results[["results"]][["contrasts"]][["collection"]][[1]][["data"]][[1]]
  options$contrasts <- list(list(contrast = "repeated", variable = "hero"))
  results <- jasptools::run("Anova", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_05/www/Superhero.sav"), options, view=FALSE, quiet=TRUE)
  tableOutput8Part2 <- results[["results"]][["contrasts"]][["collection"]][[1]][["data"]][[3]]
  tableOutput8 <- list(tableOutput8Part1, tableOutput8Part2)
  expect_equal_tables(tableOutput8,
                      list("2 - 1", -18.70833, 6.990846, -2.676119, 0.0127195, "TRUE",
                           "3 - 4", 9.125, 6.472266, 1.409862, 0.1704314, "FALSE")
  )
  
  options <- jasptools::analysisOptions("Anova")
  options$dependent <- "injury"
  options$fixedFactors <- "hero"
  options$modelTerms <- list(
    list(components="hero")
  )
  options$postHocTestsVariables <- "hero"
  options$postHocTestsTukey <- FALSE
  options$postHocTestsBonferroni <- TRUE
  results <- jasptools::run("Anova", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_05/www/Superhero.sav"), options, view=FALSE, quiet=TRUE)
  tableOutput9 <- results[["results"]][["posthoc"]][["collection"]][[1]][["data"]]
  expect_equal_tables(tableOutput9,
                      list(1, 2, 18.70833, 6.990846, 2.676119, "", "", "", 0.07631699, "", "", "TRUE",
                           1, 3, 24.95833, 6.990846, 3.570145, "", "", "", 0.008512499, "", "", "FALSE",
                           1, 4, 34.08333, 6.990846, 4.875423, "", "", "", 0.0002801258, "", "", "FALSE",
                           2, 3, 6.25, 6.472266, 0.9656587, "", "", "", 1, "", "", "FALSE",
                           2, 4, 15.375, 6.472266, 2.37552, "", "", "", 0.1510885, "", "", "FALSE",
                           3, 4, 9.125, 6.472266, 1.409862, "", "", "", 1, "", "", "FALSE")
  )
})


# Chapter 7
test_that("Fields Book - Chapter 7 results match", {
  options <- jasptools::analysisOptions("Anova")
  options$dependent <- "Attractiveness"
  options$fixedFactors <- c("FaceType", "Alcohol")
  options$modelTerms <- list(
    list(components="FaceType"),
    list(components="Alcohol"),
    list(components=c("FaceType", "Alcohol"))
  )
  options$contrasts <- list(list(contrast = "none", variable = "FaceType"), list(contrast = "Helmert", variable = "Alcohol"))
  results <- jasptools::run("Anova", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_07/www/Goggles.sav"), options, view=FALSE, quiet=TRUE)
  output1 <- results[["results"]][["anova"]][["data"]]
  expect_equal_tables(output1,
                      list("FaceType", 21.33333, 1, 21.33333, 15.58261, 0.0002952236, "TRUE",
                           "Alcohol", 16.54167, 2, 8.270833, 6.041304, 0.004943389, "FALSE",
                           "FaceType <unicode> Alcohol", 23.29167, 2, 11.64583, 8.506522, 0.0007912739, "FALSE",
                           "Residual", 57.5, 42, 1.369048, "", "", "TRUE")
  )
  options <- jasptools::analysisOptions("Anova")
  options$dependent <- "Attractiveness"
  options$fixedFactors <- c("FaceType", "Alcohol")
  options$modelTerms <- list(
    list(components="FaceType"),
    list(components="Alcohol"),
    list(components=c("FaceType", "Alcohol"))
  )
  options$contrasts <- list(list(contrast = "none", variable = "FaceType"), list(contrast = "Helmert", variable = "Alcohol"))
  options$plotHorizontalAxis <- "FaceType"
  options$plotErrorBars <- TRUE
  results <- jasptools::run("Anova", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_07/www/Goggles.sav"), options, view=FALSE, quiet=TRUE)
  unnumberedFigure1 <- results[["state"]][["stateDescriptivesPlot"]][[1]]
  #expect_equal_plots(unnumberedFigure1, "?", dir="Ancova") # This command needs to be updated
  
  options <- jasptools::analysisOptions("Anova")
  options$dependent <- "Attractiveness"
  options$fixedFactors <- c("FaceType", "Alcohol")
  options$modelTerms <- list(
    list(components="FaceType"),
    list(components="Alcohol"),
    list(components=c("FaceType", "Alcohol"))
  )
  options$contrasts <- list(list(contrast = "none", variable = "FaceType"), list(contrast = "Helmert", variable = "Alcohol"))
  options$plotHorizontalAxis <- "Alcohol"
  options$plotErrorBars <- TRUE
  results <- jasptools::run("Anova", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_07/www/Goggles.sav"), options, view=FALSE, quiet=TRUE)
  unnumberedFigure2 <- results[["state"]][["stateDescriptivesPlot"]][[1]]
  #expect_equal_plots(unnumberedFigure2, "?", dir="Ancova") # This command needs to be updated
  
  options <- jasptools::analysisOptions("Anova")
  options$dependent <- "Attractiveness"
  options$fixedFactors <- c("FaceType", "Alcohol")
  options$modelTerms <- list(
    list(components="FaceType"),
    list(components="Alcohol"),
    list(components=c("FaceType", "Alcohol"))
  )
  options$contrasts <- list(list(contrast = "none", variable = "FaceType"), list(contrast = "Helmert", variable = "Alcohol"))
  options$postHocTestsVariables <- c("Alcohol")
  options$kruskalVariablesAssigned <- c("Alcohol")
  options$postHocTestsBonferroni <- TRUE
  options$postHocTestsTukey <- FALSE
  options$postHocTestBootstrapping <- TRUE
  options$postHocTestBootstrappingReplicates <- 1000
  set.seed(1)
  results <- jasptools::run("Anova", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_07/www/Goggles.sav"), options, view=FALSE, quiet=TRUE)
  output2 <- results[["results"]][["contrasts"]][["collection"]][[1]][["data"]]
  expect_equal_tables(output2,
                      list("0 - 1, 2", -1.09375, 0.3582572, -3.052974, 0.003921402, "TRUE",
                           "1 - 2", -0.6875, 0.4136798, -1.661914, 0.1039773, "FALSE")
  )
  output3a <- results[["results"]][["posthoc"]][["collection"]][[1]][["data"]]
  expect_equal_tables(output3a,
                      list(0, 1, -0.75, 0.4136798, -1.812997, "", "", "", 0.2309501, "", "", "TRUE",
                           0, 2, -1.4375, 0.4136798, -3.47491, "", "", "", 0.003599568, "", "", "FALSE",
                           1, 2, -0.6875, 0.4136798, -1.661914, "", "", "", 0.3119319, "", "", "FALSE")
  )
  output3b <- results[["results"]][["posthocBoots"]][["collection"]][[1]][["data"]]
  expect_equal_tables(output3b,
                      list(0, 1, -0.75, 0.009656129, 0.4194893, -1.646094, 0.03172597, "TRUE",
                           0, 2, -1.4375, 0.01227821, 0.4321352, -2.284006, -0.648591, "FALSE",
                           1, 2, -0.6875, 0.00262208, 0.3934077, -1.457499, 0.08496317, "FALSE")
  )
  
  options <- jasptools::analysisOptions("Anova")
  options$dependent <- "Attractiveness"
  options$fixedFactors <- c("FaceType", "Alcohol")
  options$modelTerms <- list(
    list(components="FaceType"),
    list(components="Alcohol"),
    list(components=c("FaceType", "Alcohol"))
  )
  options$contrasts <- list(list(contrast = "none", variable = "FaceType"), list(contrast = "none", variable = "Alcohol"))
  options$marginalMeansTerms <- list(list("FaceType", "Alcohol"))
  options$marginalMeansBootstrapping <- TRUE
  options$marginalMeansBootstrappingReplicates <- 1000
  set.seed(1) # For Bootstrapping Unit Tests
  options$plotHorizontalAxis <- "Alcohol"
  options$plotSeparateLines <- "FaceType"
  options$plotErrorBars <- TRUE
  results <- jasptools::run("Anova", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_07/www/Goggles.sav"), options, view=FALSE, quiet=TRUE)
  unnumberedFigure3 <- results[["state"]][["stateDescriptivesPlot"]][[1]]
  #expect_equal_plots(unnumberedFigure3, "?", dir="Ancova") # This command needs to be updated
  output4a <- results[["results"]][["marginalMeans"]][["collection"]][[1]][["data"]]
  expect_equal_tables(output4a,
                      list(0, 0, 3.5, 0.4136798, 2.66516, 4.33484, "TRUE",
                           0, 1, 4.875, 0.4136798, 4.04016, 5.70984, "FALSE",
                           0, 2, 6.625, 0.4136798, 5.79016, 7.45984, "FALSE",
                           1, 0, 6.375, 0.4136798, 5.54016, 7.20984, "TRUE",
                           1, 1, 6.5, 0.4136798, 5.66516, 7.33484, "FALSE",
                           1, 2, 6.125, 0.4136798, 5.29016, 6.95984, "FALSE")
  )
  output4b <- results[["results"]][["marginalMeans"]][["collection"]][[2]][["data"]]
  expect_equal_tables(output4b,
                      list(0, 0, 3.5, 0.006070666, 0.5640803, 2.33385, 4.62333, "TRUE",
                           0, 1, 4.875, -0.01314135, 0.4322475, 4.142857, 5.847589, "FALSE",
                           0, 2, 6.625, -0.01705802, 0.3793216, 6, 7.5, "FALSE",
                           1, 0, 6.375, -0.003094173, 0.3289257, 5.79557, 7.052643, "TRUE",
                           1, 1, 6.5, -0.001616834, 0.3307976, 5.857143, 7.166667, "FALSE",
                           1, 2, 6.125, -0.002099714, 0.3913636, 5.428571, 7, "FALSE")
  )
})