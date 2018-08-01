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
    deviation = list("2 - 1, 2, 3, 4, 5", -0.19513913461, 0.211517937182489, -0.922565420263351, 0.358570880186821,
                      95, -0.615055331660948, 0.224777062440949, "TRUE", "3 - 1, 2, 3, 4, 5", 0.331498558639999, 
                     0.211517937182488, 1.56723615526752, 0.120384543718471, 95, -0.0884176384109483, 0.751414755690946, 
                     "FALSE", "4 - 1, 2, 3, 4, 5", -0.16911442746, 0.211517937182488, -0.799527594267789, 0.425979159402386, 
                     95, -0.589030624510948, 0.250801769590948, "FALSE", "5 - 1, 2, 3, 4, 5", 0.18254372644, 0.211517937182488,
                     0.863017713162119, 0.390301247101766, 95, -0.237372470610948, 0.602459923490948, "FALSE"),
    simple = list("2 - 1", -0.0453504116000002, 0.334439223738541, -0.135601354090735,
                  0.892423350599012, 95,  -0.709296216138537, 0.618595392938537,  "TRUE", "3 - 1", 0.48128728165, 0.334439223738541,
                  1.43908742601993, 0.153412521131058, 95, -0.182658522888538, 1.14523308618854, "FALSE", "4 - 1", -0.0193257044500001,
                  0.334439223738541, -0.0577854003904418, 0.954040939034334, 95, -0.683271508988538, 0.644620100088538, "FALSE",
                  "5 - 1", 0.33233244945, 0.334439223738541, 0.993700576550232,
                  0.322892982956191, 95,  -0.331613355088537, 0.996278253988537, "FALSE"),
    difference = list("2 - 1", -0.0453504116, 0.334439223738541, -0.135601354090734,
                      0.892423350599013, 95,  -0.709296216138538, 0.618595392938538,"TRUE", "3 - 1, 2", 0.50396248745, 0.289632863779524,
                      1.74000450388679, 0.0850966309951808, 95, -0.0710314460164715, 1.07895642091647, "FALSE", "4 - 1, 2, 3",
                      -0.164637994466667, 0.273068482710641, -0.602918333278054, 0.547999665617461,
                      95, -0.706747473793691, 0.377471484860357, "FALSE", "5 - 1, 2, 3, 4", 0.22817965805, 0.26439742147811,
                      0.86301771316212, 0.390301247101765, 95,  -0.296715588263685, 0.753074904363685, "FALSE"),
    Helmert = list("1 - 2, 3, 4, 5", -0.1872359037625, 0.26439742147811, -0.708160853898499,
                   0.480579313597282, 95, -0.712131150076185, 0.337659342551185, "TRUE", "2 - 3, 4, 5", -0.31011508715, 0.273068482710641,
                   -1.13566781516348, 0.25895260195633, 95,  -0.852224566477025, 0.231994392177024,  "FALSE", "3 - 4, 5", 0.32478390915,
                   0.289632863779524, 1.12136414670551, 0.264959410721321, 95,  -0.250210024316471, 0.899777842616471, "FALSE",
                   "4 - 5", -0.3516581539, 0.334439223738541, -1.05148597694067,
                   0.295702769469608, 95,  -1.01560395843854, 0.312287650638537, "FALSE"),
    repeated = list("1 - 2", 0.0453504115999998, 0.334439223738541, 0.135601354090733,
                    0.892423350599014, 95, -0.618595392938537, 0.709296216138537, "TRUE", "2 - 3", -0.52663769325, 0.334439223738541,
                    -1.57468878011066, 0.118652819171739, 95,  -1.19058349778854, 0.137308111288537, "FALSE", "3 - 4", 0.5006129861,
                    0.334439223738541, 1.49687282641037, 0.137741463128169, 95, -0.163332818438537, 1.16455879063854, "FALSE",
                    "4 - 5", -0.3516581539, 0.334439223738541, -1.05148597694067,
                    0.295702769469608, 95, -1.01560395843854, 0.312287650638537, "FALSE"),
    polynomial = list("linear", 0.218415231132241, 0.236484243000287, 0.923593167820386,
                      0.358038105335522, 95, -0.251065349597317, 0.687895811861799,  "TRUE", "quadratic", -0.0623342877876619,
                      0.236484243000287, -0.263587488945664, 0.792668695639493, 95,  -0.53181486851722, 0.407146292941896,  "FALSE",
                      "cubic", 0.0886332780579033, 0.236484243000287, 0.374795702806278,
                      0.70864779281998, 95, -0.380847302671655, 0.558113858787461, "FALSE", "quartic", 0.415791419838834, 0.236484243000287,
                      1.75822039795831, 0.0819306308915546, 95,  -0.0536891608907235, 0.885272000568391, "FALSE")
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
         0.448976320466698, -0.2631059, 0.5898344, "TRUE")
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


test_that("Nonparametric table results match", {
  options <- jasptools::analysisOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- c( "facFive", "facExperim")
  options$kruskalVariablesAssigned <- c( "facFive", "facExperim")
  options$modelTerms <- list(
    list(components="facExperim"),
    list(components="facFive")
  )  
  results <- jasptools::run("Anova", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["kruskal"]][["data"]]
  expect_equal_tables(table,
                      list("facFive", 3.39599999999996, 4, 0.493866894607871, "facExperim",       
                           1.02696237623763, 1, 0.310873187457312)
  )
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
