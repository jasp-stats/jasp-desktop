context("ANOVA")

# does not test
# - if analysis handles too few observations

test_that("Main table results match", {
  options <- JASPTools::analysisOptions("Anova")
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
    results <- JASPTools::run("Anova", "debug.csv", options, view=FALSE, quiet=TRUE)
    table <- results[["results"]][["anova"]][["data"]]
    expect_equal_tables(table, refTables[[type]], label=paste("Table with SS", type))
  }
})

test_that("Homogeneity of Variances table results match", {
  options <- JASPTools::analysisOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facExperim"
  options$modelTerms <- list(list(components="facExperim"))
  options$homogeneityTests <- TRUE
  options$VovkSellkeMPR <- TRUE
  results <- JASPTools::run("Anova", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["assumptionsObj"]][["levene"]][["data"]]
  expect_equal_tables(table, list(3.1459013381035, 1, 98, 0.0792241296904395, 1.83142365040653, 1))
})

test_that("Contrasts table results match", {
  options <- JASPTools::analysisOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(list(components="facFive"))

  refTables <- list(
    deviation = list("2 - 1, 2, 3, 4, 5", 0.19513913461, 0.211517937182489, 0.922565420263351,
                     0.358570880186821, "TRUE", "3 - 1, 2, 3, 4, 5", -0.331498558639999,
                     0.211517937182488, -1.56723615526752, 0.120384543718471, "FALSE",
                     "4 - 1, 2, 3, 4, 5", 0.16911442746, 0.211517937182488, 0.799527594267789,
                     0.425979159402386, "FALSE", "5 - 1, 2, 3, 4, 5", -0.18254372644,
                     0.211517937182488, -0.863017713162119, 0.390301247101766, "FALSE"),
    simple = list("2 - 1", -0.00907008232000005, 0.0668878447477081, -0.135601354090735,
                  0.892423350599012, "TRUE", "3 - 1", 0.0962574563299998, 0.0668878447477081,
                  1.43908742601993, 0.153412521131059, "FALSE", "4 - 1", -0.00386514089000006,
                  0.0668878447477081, -0.0577854003904424, 0.954040939034333,
                  "FALSE", "5 - 1", 0.0664664898899999, 0.0668878447477081, 0.993700576550232,
                  0.322892982956192, "FALSE"),
    difference = list("2 - 1", -0.0226752058000001, 0.167219611869271, -0.135601354090734,
                      0.892423350599012, "TRUE", "3 - 1, 2", 0.167987495816666, 0.0965442879265079,
                      1.74000450388679, 0.085096630995181, "FALSE", "4 - 1, 2, 3",
                      -0.0411594986166666, 0.0682671206776603, -0.602918333278052,
                      0.547999665617461, "FALSE", "5 - 1, 2, 3, 4", 0.04563593161,
                      0.0528794842956221, 0.86301771316212, 0.390301247101766, "FALSE"),
    Helmert = list("1 - 2, 3, 4, 5", -0.1872359037625, 0.26439742147811, -0.708160853898499,
                   0.480579313597282, "TRUE", "2 - 3, 4, 5", -0.31011508715, 0.273068482710641,
                   -1.13566781516348, 0.25895260195633, "FALSE", "3 - 4, 5", 0.32478390915,
                   0.289632863779524, 1.12136414670551, 0.264959410721321, "FALSE",
                   "4 - 5", -0.3516581539, 0.334439223738541, -1.05148597694067,
                   0.295702769469608, "FALSE"),
    repeated = list("1 - 2", -0.14978872301, 0.211517937182488, -0.7081608538985,
                    0.480579313597281, "TRUE", "2 - 3", -0.34492785762, 0.25905550877158,
                    -1.33148242728217, 0.186216477652548, "FALSE", "3 - 4", -0.0134292989800008,
                    0.259055508771581, -0.0518394649999199, 0.958765456828104, "FALSE",
                    "4 - 5", -0.182543726440001, 0.211517937182488, -0.863017713162123,
                    0.390301247101764, "FALSE"),
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
    results <- JASPTools::run("Anova", "debug.csv", options, view=FALSE, quiet=TRUE)
    table <- results[["results"]][["contrasts"]][["collection"]][[1]][["data"]]
    expect_equal_tables(table, refTables[[contrast]], label=paste("Table with contrast", contrast))
  }
})

test_that("Post Hoc table results match", {
  options <- JASPTools::analysisOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "contBinom"
  options$modelTerms <- list(list(components="contBinom"))
  options$postHocTestEffectSize <- TRUE
  options$postHocTestsBonferroni <- TRUE
  options$postHocTestsHolm <- TRUE
  options$postHocTestsScheffe <- TRUE
  options$postHocTestsTukey <- TRUE
  options$postHocTestsVariables <- "contBinom"
  results <- JASPTools::run("Anova", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["posthoc"]][["collection"]][[1]][["data"]]
  expect_equal_tables(table,
    list(0, 1, 0.163364220743842, 0.214904085649005, 0.760172707980336,
         0.0760172707980336, 0.448976320466698, 0.448976320466698, 0.448976320466698,
         0.448976320466698, "TRUE")
    )
})

test_that("Marginal Means table results match", {
  options <- JASPTools::analysisOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "contBinom"
  options$modelTerms <- list(list(components="contBinom"))
  options$marginalMeansCompareMainEffects <- TRUE
  options$marginalMeansTerms <- "contBinom"

  refTables <- list(
    none = list(1, -0.120135614827586, 0.139273765411964, -0.396519869554477,
                0.156248639899305, -0.862586104943972, 0.390471041862811, 1,
                2, -0.283499835571429, 0.163666075582597, -0.608289835972217,
                0.0412901648293599, -1.73218447721841, 0.0863869418751255, 0),
    Bonferroni = list(1, -0.120135614827586, 0.139273765411964, -0.396519869554477,
                      0.156248639899305, -0.862586104943972, 0.390471041862811, 1,
                      2, -0.283499835571429, 0.163666075582597, -0.608289835972217,
                      0.0412901648293599, -1.73218447721841, 0.0863869418751255, 0),
    Sidak = list(1, -0.120135614827586, 0.139273765411964, -0.396519869554477,
                 0.156248639899305, -0.862586104943972, 0.390471041862811, 1,
                 2, -0.283499835571429, 0.163666075582597, -0.608289835972217,
                 0.0412901648293599, -1.73218447721841, 0.0863869418751255, 0)
  )

  for (adjustment in c("none", "Bonferroni", "Sidak")) {
    options$marginalMeansCIAdjustment <- adjustment
    results <- JASPTools::run("Anova", "debug.csv", options, view=FALSE, quiet=TRUE)
    table <- results[["results"]][["marginalMeans"]][["collection"]][[1]][["data"]]
    expect_equal_tables(table, refTables[[adjustment]], label=paste("Table with CI adjustment", adjustment))
  }
})

test_that("Descriptives table results match", {
  options <- JASPTools::analysisOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "contBinom"
  options$modelTerms <- list(list(components="contBinom"))
  options$descriptives <- TRUE
  results <- JASPTools::run("Anova", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["descriptivesObj"]][["descriptivesTable"]][["data"]]
  expect_equal_tables(table,
    list(0, 58, -0.120135614827586, 1.10575982846952, "TRUE", 1, 42, -0.283499835571429,
         0.994612407217046, "FALSE")
  )
})

test_that("Q-Q plot matches", {
  options <- JASPTools::analysisOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- "contBinom"
  options$modelTerms <- list(list(components="contBinom"))
  options$qqPlot <- TRUE
  results <- JASPTools::run("Anova", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "q-q", dir="Anova")
})

test_that("Descriptives plots match", {
  options <- JASPTools::analysisOptions("Anova")
  options$dependent <- "contNormal"
  options$fixedFactors <- c("facFive", "contBinom")
  options$wlsWeights <- "facFifty"
  options$modelTerms <- list(
    list(components="facFive"),
    list(components="contBinom"),
    list(components=c("facFive", "contBinom"))
  )
  options$plotHorizontalAxis <- "contBinom"
  options$plotSeparateLines <- "facFive"
  options$plotErrorBars <- TRUE
  options$confidenceIntervalInterval <- 0.90

  options$errorBarType <- "confidenceInterval"
  results <- JASPTools::run("Anova", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "descriptives-ci", dir="Anova")

  options$errorBarType <- "standardError"
  results <- JASPTools::run("Anova", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "descriptives-se", dir="Anova")
})

test_that("Analysis handles errors", {
  options <- JASPTools::analysisOptions("Anova")
  options$dependent <- "debInf"
  options$fixedFactors <- "contBinom"
  options$modelTerms <- list(list(components="contBinom"))
  results <- JASPTools::run("Anova", "debug.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["anova"]][["error"]][["errorType"]], "badData",
                   label="Inf dependent check")

  options$dependent <- "contNormal"
  options$fixedFactors <- "contBinom"
  options$wlsWeights <- "debInf"
  options$modelTerms <- list(list(components="contBinom"))
  results <- JASPTools::run("Anova", "debug.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["anova"]][["error"]][["errorType"]], "badData",
                  label="Inf WLS weights check")

  options$dependent <- "contNormal"
  options$fixedFactors <- "debSame"
  options$modelTerms <- list(list(components="debSame"))
  results <- JASPTools::run("Anova", "debug.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["anova"]][["error"]][["errorType"]], "badData",
                  label="1-level factor check")

  options$dependent <- "debSame"
  options$fixedFactors <- "facFive"
  options$modelTerms <- list(list(components="facFive"))
  results <- JASPTools::run("Anova", "debug.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["anova"]][["error"]][["errorType"]], "badData",
                  label="No variance check")

  options$dependent <- "contGamma"
  options$fixedFactors <- "facFive"
  options$wlsWeights <- "contNormal"
  options$modelTerms <- list(list(components="facFive"))
  results <- JASPTools::run("Anova", "debug.csv", options, view=FALSE, quiet=TRUE)
  expect_identical(results[["results"]][["anova"]][["error"]][["errorType"]], "badData",
                  label="Negative WLS weights check")
})
