context("MANOVA")

# Also verified with Andy Field example (OCD.sav)

test_that("Main table results match", {
  options <- jasptools::analysisOptions("Manova")
  options$dependent <- c("contNormal", "contGamma")
  options$fixedFactors <- c("contBinom", "facGender")
  options$modelTerms <- list(
    list(components="contBinom"),
    list(components="facGender"))
  options$includeIntercept <- TRUE
  options$VovkSellkeMPR <- TRUE
  options$testWilks <- TRUE

  results <- jasptools::run("Manova", "test.csv", options, view = TRUE)
  table <- results[["results"]][["manovaContainer"]][["collection"]][["manovaContainer_Pillai"]][["data"]]
  expect_equal_tables(table,
                      list(33580470860819210240, 87.6607294486356, "(Intercept)", 1, 96, 2, 2.19674826936402e-22,
                           0.646176161700694, "Pillai", 1, 0.81123253193528, "contBinom", 1, 96, 2, 0.447332275717289,
                           0.0166197919998132, "Pillai", 2.9720945624123, 3.39021145078575, "facGender", 1,
                           96, 2, 0.0377850473862948, 0.0659699844596362, "Pillai", "", "",
                           "Residuals", 97, "", "", "", "", "Pillai"))
    
})

test_that("Andy Field results match", {
  options <- jasptools::analysisOptions("Manova")
  options$dependent <- c("Actions", "Thoughts")
  options$fixedFactors <- c("Group")
  options$modelTerms <- list(
    list(components = "Group")
  )
  options$includeIntercept    <- TRUE
  options$testPillai          <- TRUE
  options$testWilks           <- TRUE
  options$testHotellingLawley <- TRUE
  options$testRoy             <- TRUE
  options$includeAnovaTables  <- TRUE
  results <- jasptools::run("Manova", "manova_ocd.csv", options, view = TRUE)
  
  # Pillai table
  table <- results[['results']][['manovaContainer']][['collection']][['manovaContainer_Pillai']][['data']]
  expect_equal_tables(table,
                      list(6.29125576365301e+20, 745.229671237717, "(Intercept)", 1, 26,
                           2, 1.10625055220703e-23, 0.982854799155012, "Pillai", 2.48808911599818,
                           2.55665815696287, "Group", 2, 54, 4, 0.0490374099251968, 0.318454579025111,
                           "Pillai", "", "", "Residuals", 27, "", "", "", "", "Pillai"))
  
  # Wilks table
  table <- results[['results']][['manovaContainer']][['collection']][['manovaContainer_Wilks']][['data']]
  expect_equal_tables(table,
                      list(6.29125576362038e+20, 745.229671237408, "(Intercept)", 1, 26,
                           2, 1.10625055221288e-23, 0.0171452008449951, "Wilks", 2.46706077418097,
                           2.55454581750828, "Group", 2, 52, 4, 0.0496647855390226, 0.698509047267356,
                           "Wilks", "", "", "Residuals", 27, "", "", "", "", "Wilks"))
  
  # Hotelling table
  table <- results[['results']][['manovaContainer']][['collection']][['manovaContainer_Hotelling-Lawley']][['data']]
  expect_equal_tables(table,
                      list(6.29125576361981e+20, 745.229671237403, "(Intercept)", 1, 26,
                           2, 1.10625055221298e-23, 57.3253593259541, "Hotelling-Lawley",
                           2.4302755126038, 2.54584503551958, "Group", 2, 50, 4, 0.050798343576486,
                           0.407335205683133, "Hotelling-Lawley", "", "", "Residuals",
                           27, "", "", "", "", "Hotelling-Lawley"))
  
  # Roy
  table <- results[['results']][['manovaContainer']][['collection']][['manovaContainer_Roy']][['data']]
  expect_equal_tables(table,
                      list(6.29125576361981e+20, 745.229671237403, "(Intercept)", 1, 26,
                           2, 1.10625055221298e-23, 57.3253593259541, "Roy", 4.65492291160569,
                           4.5197644093943, "Group", 2, 27, 2, 0.0202718277388112, 0.334797363658837,
                           "Roy", "", "", "Residuals", 27, "", "", "", "", "Roy"))
  
  # ANOVA tables
  # for Actions
  table <- results[['results']][['anovaContainer']][['collection']][['anovaContainer_ Response XQWN0aW9ucw']][['data']]
  expect_equal_tables(table,
                      list(1, 326.4, 616.533333333333, 1.31849279790975e-16, 616.533333333333,
                           76306879900674, "(Intercept)", 2, 2.77058823529412, 5.23333333333333,
                           0.0804566489996684, 10.4666666666667, 1.81441530915161, "Group      ",
                           27, "", 1.88888888888889, "", 51, "", "Residuals  "))
  
  # for Thoughts
  table <- results[['results']][['anovaContainer']][['collection']][['anovaContainer_ Response XVGhvdWdodHM']][['data']]
  expect_equal_tables(table,
                      list(1, 1402.34754098361, 6336.53333333333, 8.22509725190572e-25, 6336.53333333333,
                           8.06500573067305e+21, "(Intercept)", 2, 2.15409836065575, 9.73333333333337,
                           0.135527393903097, 19.4666666666667, 1.35817762033172, "Group      ",
                           27, "", 4.51851851851852, "", 122, "", "Residuals  "))
})

