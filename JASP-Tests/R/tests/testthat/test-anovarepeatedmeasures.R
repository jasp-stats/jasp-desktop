context("Repeated Measures ANOVA")

# Does not test:
#    - type I and type II sum of squares
#    - Simple effects
#    - Plots
#    - Contrasts apart from 'repeated'

initOpts <- function(){
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  
  options$repeatedMeasuresFactors <- list(
    list(name = "Drink", levels = c("Beer", "Wine", "Water")),
    list(name = "Imagery", levels = c("Positive", "Neutral", "Negative"))
  )
  
  options$repeatedMeasuresCells <- c("beerpos", "beerneut", "beerneg",
                                     "winepos", "wineneut", "wineneg",
                                     "waterpos", "waterneu", "waterneg")
  options$modelTerms <- list(
    list(components = "Drink"),
    list(components = "Imagery"), 
    list(components = c("Drink", "Imagery"))
  )
  
  options
}

test_that("Within subjects table results match", {
  options <- initOpts()
  options$sphericityCorrections <- TRUE

  results <- jasptools::run(name = "AnovaRepeatedMeasures", dataset = "AnovaRepeatedMeasures.csv",
                            options = options, view = FALSE, quiet = TRUE)
  refTable <- list("Drink", "None", 2092.34444444444, 2, 1046.17222222222, 5.10598105687077,
                0.0108629307294978, "TRUE", "FALSE", 1, 1, 1, 1, 1, "Drink",
                "Greenhouse-Geisser", 2092.34444444444, 1.15422864073086, 1812.76427443316,
                5.10598105687077, 0.0297686804863521, "FALSE", "FALSE", 1, 1,
                1, 1, 1, "Drink", "Huynh-Feldt", 2092.34444444444, 1.18148845405137,
                1770.93939197604, 5.10598105687077, 0.028813909529067, "FALSE",
                "FALSE", 1, 1, 1, 1, 1, "Residual", "None", 7785.87777777778,
                38, 204.891520467836, "", "", "", "", "", "TRUE", "Residual",
                "Greenhouse-Geisser", 7785.87777777778, 21.9303441738863, 355.027614525488,
                "", "", "", "", "", "FALSE", "Residual", "Huynh-Feldt", 7785.87777777778,
                22.4482806269761, 346.836263638895, "", "", "", "", "", "FALSE",
                "Imagery", "None", 21628.6777777778, 2, 10814.3388888889, 122.564824909945,
                2.68019659683571e-17, "TRUE", "FALSE", 1, 1, 1, 1, 1, "Imagery",
                "Greenhouse-Geisser", 21628.6777777778, 1.49488144635967, 14468.4903478118,
                122.564824909945, 1.75728558571484e-13, "FALSE", "FALSE", 1,
                1, 1, 1, 1, "Imagery", "Huynh-Feldt", 21628.6777777778, 1.59368408969683,
                13571.4963320568, 122.564824909945, 3.14280380271786e-14, "FALSE",
                "FALSE", 1, 1, 1, 1, 1, "Residual", "None", 3352.87777777778,
                38, 88.2336257309941, "", "", "", "", "", "TRUE", "Residual",
                "Greenhouse-Geisser", 3352.87777777778, 28.4027474808338, 118.047656482539,
                "", "", "", "", "", "FALSE", "Residual", "Huynh-Feldt", 3352.87777777778,
                30.2799977042398, 110.729129193702, "", "", "", "", "", "FALSE",
                "Drink <unicode> Imagery", "None", 2624.42222222222, 4, 656.105555555556,
                17.1549223629789, 4.58904028152479e-10, "TRUE", "FALSE", "Drink <unicode> Imagery",
                "Greenhouse-Geisser", 2624.42222222222, 3.19359175963514, 821.777615847198,
                17.1549223629789, 1.90024850184092e-08, "FALSE", "FALSE", "Drink <unicode> Imagery",
                "Huynh-Feldt", 2624.42222222222, 3.91435133471376, 670.4615906467,
                17.1549223629789, 6.80963952075043e-10, "FALSE", "FALSE", "Residual",
                "None", 2906.68888888889, 76, 38.2459064327485, "", "", "",
                "", "", "TRUE", "Residual", "Greenhouse-Geisser", 2906.68888888889,
                60.6782434330676, 47.9033130234755, "", "", "", "", "", "FALSE",
                "Residual", "Huynh-Feldt", 2906.68888888889, 74.3726753595615,
                39.0827528367944, "", "", "", "", "", "FALSE")
  
  table <- results[["results"]][["withinSubjectsEffects"]][["data"]]
  expect_equal_tables(table, refTable)
})

test_that("Sphericity Assumptions table match", {
  options <- initOpts()
  
  options$sphericityTests <- TRUE
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures", dataset = "AnovaRepeatedMeasures.csv",
                            options = options, view = FALSE, quiet = TRUE)
  
  refTable <- list("Drink", 0.267241056560857, 6.95230186958065e-06, 0.577114320365429,
                    0.590744227025686, 0.5, "TRUE", "Imagery", 0.662101262364057, 0.0244523015633462,
                    0.747440723179836, 0.796842044848417, 0.5, "FALSE", "Drink <unicode> Imagery",
                    0.595043993796251, 0.435658665786593, 0.798397939908785, 0.97858783367844, 0.25,
                    "FALSE")
  
  table <- results[["results"]][["assumptionsObj"]][["sphericity"]][["data"]]
  expect_equal_tables(table, refTable)
  
})

test_that("Post-hoc tests match", {
  options <- initOpts()
  
  options$postHocTestsVariables <- c("Drink", "Imagery")
  options$postHocTestEffectSize <- TRUE
  options$postHocTestsBonferroni <- TRUE
  options$postHocTestPooledError <- FALSE
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures", dataset = "AnovaRepeatedMeasures.csv",
                            options = options, view = FALSE, quiet = TRUE)
  
  refTable <- list("Beer", "Wine", 3.5, -3.045489, 10.04549, 2.84948954082566, 1.22829017262715, 0.274654032208925,
                   "", "", 0.703009687611414, "", "TRUE", "Beer", "Water", 8.31666666666667, 1.771177, 14.86216,
                   3.3351289023547, 2.49365674016224, 0.557598598355329, "", "",
                   0.0660988675936689, "", "FALSE", "Wine", "Water", 4.81666666666667, -1.728823, 11.36216,
                   1.1164571680934, 4.31424223366509, 0.964693890587566, "", "",
                   0.00112213065327869, "", "FALSE")
  
  table <- results[["results"]][["posthoc"]][["collection"]][[1]][["data"]]
  expect_equal_tables(table, refTable)
})

test_that("Descriptives Match", {
  options <- initOpts()
  
  options$descriptives <- TRUE
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures", dataset = "AnovaRepeatedMeasures.csv",
                            options = options, view = FALSE, quiet = TRUE)
  
  refTable <- list("Beer", "Positive", 20, 21.05, 13.0079934938807, "TRUE", "Beer",
                   "Neutral", 20, 10, 10.295630140987, "FALSE", "Beer", "Negative",
                   20, 4.45, 17.3037111930543, "FALSE", "Wine", "Positive", 20,
                   25.35, 6.73775692801786, "TRUE", "Wine", "Neutral", 20, 11.65,
                   6.24310145596511, "FALSE", "Wine", "Negative", 20, -12, 6.18146635643918,
                   "FALSE", "Water", "Positive", 20, 17.4, 7.07404447704126, "TRUE",
                   "Water", "Neutral", 20, 2.35, 6.83855170878193, "FALSE", "Water",
                   "Negative", 20, -9.2, 6.8024763292882, "FALSE")
  
  table <- results[["results"]][["descriptivesObj"]][["descriptivesTable"]][["data"]]
  expect_equal_tables(table, refTable)
})


# Mixed Effects
initOpts <- function(){
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  
  options$repeatedMeasuresFactors <- list(
    list(name = "Looks", levels = c("Attractive", "Average", "Ugly")),
    list(name = "Charisma", levels = c("High", "Some", "None"))
  )
  
  options$repeatedMeasuresCells <- c("att_high", "att_some", "att_none",
                                     "av_high", "av_some", "av_none",
                                     "ug_high", "ug_some", "ug_none")
  options$modelTerms <- list(
    list(components = "Looks"),
    list(components = "Charisma"), 
    list(components = "gender"),
    list(components = c("Looks", "Charisma"))
  )
  
  options$betweenSubjectFactors <- "gender"
  options
}

test_that("Between Subjects table match", {
  options <- initOpts()
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv", options = options,
                            view = FALSE, quiet = TRUE)
  
  refTable <- list("gender", 0.200000000000001, 1, 0.200000000000001, 0.00473545746857648,
                0.945895847556855, "TRUE", "Residual", 760.222222222222, 18,
                42.2345679012346, "", "", "", "", "", "TRUE")
  
  table <- results[["results"]][["betweenSubjectsEffects"]][["data"]]
  expect_equal_tables(table, refTable)
})

test_that("Homogeneity tests correct", {
  options <- initOpts()

  options$homogeneityTests <- TRUE
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv", options = options,
                            view = FALSE, quiet = TRUE)
  
  refTable <- list("att_high", 1.13105200239091, 1, 18, 0.301611198987337, "TRUE",
                "att_some", 0.598562976996908, 1, 18, 0.449169168742317, "FALSE",
                "att_none", 1.94893878806521, 1, 18, 0.179682774529315, "FALSE",
                "av_high", 0.101977401129945, 1, 18, 0.753145830077659, "FALSE",
                "av_some", 1.76314835904338, 1, 18, 0.200826123727507, "FALSE",
                "av_none", 0.00399511707912524, 1, 18, 0.950298338730636, "FALSE",
                "ug_high", 0.00491266375545877, 1, 18, 0.944894541517532, "FALSE",
                "ug_some", 0.123626373626372, 1, 18, 0.729216564281406, "FALSE",
                "ug_none", 0.0819838056680181, 1, 18, 0.777896246470082, "FALSE")
  
  table <- results[["results"]][["assumptionsObj"]][["levene"]][["data"]]
  expect_equal_tables(table, refTable)
})

test_that("(Repeated) Contrast table match", {
  options <- initOpts()
  
  options$contrasts <- list(list(contrast = "repeated", variable = "Looks"))
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv", options = options,
                            view = FALSE, quiet = TRUE)
  
  refTable <- list("Attractive - Average", 14.31667, 0.9040603, 15.83596,
                   8.431449e-18, "TRUE", "Average - Ugly", 11.96667, 0.9040603,
                   13.23658, 2.1268e-15, "FALSE")
  
  table <- results[["results"]][["contrasts"]][["collection"]][[1]][["data"]]
  expect_equal_tables(table, refTable)
})


test_that("Effect Size Calculation correct", {
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  
  options$repeatedMeasuresFactors <- list(
    list(name = "Animal", levels = c("Stick", "Kangaroo", "Fish", "Grub"))
    )
  options$repeatedMeasuresCells <- c("Stick Insect", "Kangaroo Testicle",
                                     "Fish Eye", "Witchetty Grub")
  
  options$modelTerms <- list(
    list(components = "Animal")
  )
  
  options$effectSizeEstimates <- TRUE
  options$effectSizeEtaSquared <- TRUE
  options$effectSizeOmegaSquared <- TRUE
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaRepeatedMeasuresOneWay.csv",
                            options = options,
                            view = FALSE, quiet = TRUE)
  
  refTable <- list("Animal", 83.1249999999999, 3, 27.7083333333333, 3.79380603096984,
                0.0255702968630395, "TRUE", 1, 1, 1, 1, 1, 0.351479915433403,
                0.351479915433403, 0.238785176929506, "Residual", 153.375, 21,
                7.30357142857143, "", "", "", "", "", "TRUE")

  table <- results[["results"]][["withinSubjectsEffects"]][["data"]]
  expect_equal_tables(table, refTable)
})

test_that("Simple Effects table match", {
  
  options <- initOpts()
  
  options$betweenSubjectFactors <- "gender"
  options$simpleFactor <- "Looks"
  options$moderatorFactorOne <- "gender"
  options$moderatorFactorTwo <- "Charisma"
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv", #mydat,
                            options = options,
                            view = FALSE, quiet = TRUE)
  
  refTable <- list("Female", "High", 42.4666666666668, 2, 21.2333333333334, 0.639629588307488, 
                   0.539062933641058, "TRUE", "Female", "Some", 6444.46666666667, 2, 
                   3222.23333333334, 105.034770010866, 1.18808350406329e-10, "FALSE",               
                   "Female", "None", 187.8, 2, 93.8999999999999, 10.1696750902527, 
                   0.0011082808185639, "FALSE", "Male", "High", 5661.66666666667, 2, 
                   2830.83333333333, 82.5850891410049, 8.54593593608342e-10, "TRUE", 
                   "Male", "Some", 8157.26666666666, 2 ,4078.63333333333, 121.267591674926,
                   3.58637028279497e-11, "FALSE", "Male", "None", 10955, 2, 5477.5,
                   292.566765578635, 1.87815435905324e-14, "FALSE")
  
  table <- results[["results"]][["simpleEffects"]][["data"]]
  expect_equal_tables(table, refTable)
})

# Below are the unit tests for Andy Field's book

# Chapter 8
test_that("Fields Book - Chapter 8 results match", {
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("stick", "testicle", "eye", "witchetty"), "name" = "Animal"))
  options$repeatedMeasuresCells <- list("stick", "testicle", "eye", "witchetty")
  options$modelTerms <- list(list("components" = list("Animal")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "Animal"))
  options$descriptives <- TRUE
  options$sphericityTests <- options$sphericityCorrections <- TRUE
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_08/www/Bushtucker.sav"), options, view=FALSE, quiet=TRUE)
  output1 <- results[["results"]][["descriptivesObj"]][["descriptivesTable"]][["data"]]
  expect_equal_tables(output1,
                      list("stick", 8, 8.125, 2.232071, "TRUE",
                           "testicle", 8, 4.25, 1.832251, "FALSE",
                           "eye", 8, 4.125, 2.748376, "FALSE",
                           "witchetty", 8, 5.75, 2.915476, "FALSE")
  )
  output2 <- results[["results"]][["assumptionsObj"]][["sphericity"]][["data"]]
  expect_equal_tables(output2,
                      list("Animal", 11.23879, 5, 0.136248, 0.04684581, 0.5328456, 0.6657636, 0.3333333, "TRUE")
  )
  output3 <- results[["results"]][["withinSubjectsEffects"]][["data"]]
  expect_equal_tables(output3,
                      list("Animal", "None", 83.125, 3, 27.70833, 3.793806, 0.0255703, "TRUE", "FALSE", 1, 1, 1, 1, 1,
                           "Animal", "Greenhouse-Geisser", 83.125, 1.598537, 52.00068, 3.793806, 0.06258412, "FALSE", "FALSE", 1, 1, 1, 1, 1,
                           "Animal", "Huynh-Feldt", 83.125, 1.997291, 41.61888, 3.793806, 0.04833061, "FALSE", "FALSE", 1, 1, 1, 1, 1,
                           "Residual", "None", 153.375, 21, 7.303571, "", "", "", "", "", "TRUE",
                           "Residual", "Greenhouse-Geisser", 153.375, 11.18976, 13.70673, "", "", "", "", "", "FALSE",
                           "Residual", "Huynh-Feldt", 153.375, 13.98104, 10.97022, "", "", "", "", "", "FALSE")
  )
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("stick", "testicle"), "name" = "Animal"))
  options$repeatedMeasuresCells <- list("stick", "testicle")
  options$modelTerms <- list(list("components" = list("Animal")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "Animal"))
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_08/www/Bushtucker.sav"), options, view=FALSE, quiet=TRUE)
  output4a <- results[["results"]][["withinSubjectsEffects"]][["data"]]
  expect_equal_tables(output4a,
                      list("Animal", 60.0625, 1, 60.0625, 22.80339, 0.002023283, "TRUE",
                           "Residual", 18.4375, 7, 2.633929, "", "", "", "", "", "TRUE")
  )
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("testicle", "eye"), "name" = "Animal"))
  options$repeatedMeasuresCells <- list("testicle", "eye")
  options$modelTerms <- list(list("components" = list("Animal")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "Animal"))
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_08/www/Bushtucker.sav"), options, view=FALSE, quiet=TRUE)
  output4b <- results[["results"]][["withinSubjectsEffects"]][["data"]]
  expect_equal_tables(output4b,
                      list("Animal", 0.0625, 1, 0.0625, 0.01081917, 0.9200747, "TRUE",
                           "Residual", 40.4375, 7, 5.776786, "", "", "", "", "", "TRUE")
  )
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("eye", "witchetty"), "name" = "Animal"))
  options$repeatedMeasuresCells <- list("eye", "witchetty")
  options$modelTerms <- list(list("components" = list("Animal")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "Animal"))
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_08/www/Bushtucker.sav"), options, view=FALSE, quiet=TRUE)
  output4c <- results[["results"]][["withinSubjectsEffects"]][["data"]]
  expect_equal_tables(output4c,
                      list("Animal", 10.5625, 1, 10.5625, 0.7955615, 0.4020421, "TRUE",
                           "Residual", 92.9375, 7, 13.27679, "", "", "", "", "", "TRUE")
  )
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("stick", "testicle", "eye", "witchetty"), "name" = "Animal"))
  options$repeatedMeasuresCells <- list("stick", "testicle", "eye", "witchetty")
  options$modelTerms <- list(list("components" = list("Animal")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "Animal"))
  options$postHocTestsVariables <- list("Animal")
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_08/www/Bushtucker.sav"), options, view=FALSE, quiet=TRUE)
  output5 <- results[["results"]][["posthoc"]][["collection"]][[1]][["data"]]
  expect_equal_tables(output5,
                      list("stick", "testicle", 3.875, -0.05997322, 7.809973, 0.8114691, 4.77529, "", "", "", 0.0121397, "", "TRUE",
                           "stick", "eye", 4, 0.06502678, 7.934973, 0.7319251, 5.46504, "", "", "", 0.00564486, "", "FALSE",
                           "stick", "witchetty", 2.375, -1.559973, 6.309973, 1.79222, 1.325172, "", "", "", 1, "", "FALSE",
                           "testicle", "eye", 0.125, -3.809973, 4.059973, 1.201747, 0.1040152, "", "", "", 1, "", "FALSE",
                           "testicle", "witchetty", -1.5, -5.434973, 2.434973, 1.336306, -1.122497, "", "", "", 1, "", "FALSE",
                           "eye", "witchetty", -1.625, -5.559973, 2.309973, 1.821866, -0.8919426, "", "", "", 1, "", "FALSE")
  )

  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("beer", "wine", "water"), "name" = "drink"), list("levels" = list("positive", "negative", "neutral"), "name" = "imagery"))
  options$repeatedMeasuresCells <- list("beerpos", "beerneg", "beerneut", "winepos", "wineneg", "wineneut", "waterpos", "waterneg", "waterneut")
  options$modelTerms <- list(list("components" = list("drink")), list("components" = list("imagery")), list("components" = list("drink", "imagery")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "drink"), list("contrast" = "none", "variable" = "imagery"))
  options$descriptives <- TRUE
  options$sphericityTests <- options$sphericityCorrections <- TRUE
  options$postHocTestsVariables <- list("drink", "imagery")
  options$marginalMeansTerms <- list("drink", "imagery", list("drink", "imagery"))
  options$plotHorizontalAxis <- "drink"
  options$plotSeparateLines <- "imagery"
  options$plotErrorBars <- TRUE
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_08/www/Attitude.sav"), options, view=FALSE, quiet=TRUE)
  output6 <- results[["results"]][["descriptivesObj"]][["descriptivesTable"]][["data"]]
  expect_equal_tables(output6,
                      list("beer", "positive", 20, 21.05, 13.00799, "TRUE",
                           "beer", "negative", 20, 4.45, 17.30371, "FALSE",
                           "beer", "neutral", 20, 10, 10.29563, "FALSE",
                           "wine", "positive", 20, 25.35, 6.737757, "TRUE",
                           "wine", "negative", 20, -12, 6.181466, "FALSE",
                           "wine", "neutral", 20, 11.65, 6.243101, "FALSE",
                           "water", "positive", 20, 17.4, 7.074044, "TRUE",
                           "water", "negative", 20, -9.2, 6.802476, "FALSE",
                           "water", "neutral", 20, 2.35, 6.838552, "FALSE")
  )
  output7 <- results[["results"]][["assumptionsObj"]][["sphericity"]][["data"]]
  expect_equal_tables(output7,
                      list("drink", 23.75288, 2, 0.2672411, 6.952302e-06, 0.5771143, 0.5907442, 0.5, "TRUE",
                           "imagery", 7.422062, 2, 0.6621013, 0.0244523, 0.7474407, 0.796842, 0.5, "FALSE",
                           "drink <unicode> imagery", 3.786581, 4, 0.595044, 0.4356587, 0.7983979, 0.9785878, 0.25, "FALSE")
  )
  output8 <- results[["results"]][["withinSubjectsEffects"]][["data"]]
  expect_equal_tables(output8,
                      list("drink", "None", 2092.344, 2, 1046.172, 5.105981, 0.01086293, "TRUE", "FALSE", 1, 1, 1, 1, 1,
                           "drink", "Greenhouse-Geisser", 2092.344, 1.154229, 1812.764, 5.105981, 0.02976868, "FALSE", "FALSE", 1, 1, 1, 1, 1,
                           "drink", "Huynh-Feldt", 2092.344, 1.181488, 1770.939, 5.105981, 0.02881391, "FALSE", "FALSE", 1, 1, 1, 1, 1,
                           "Residual", "None", 7785.878, 38, 204.8915, "", "", "", "", "", "TRUE",
                           "Residual", "Greenhouse-Geisser", 7785.878, 21.93034, 355.0276, "", "", "", "", "", "FALSE",
                           "Residual", "Huynh-Feldt", 7785.878, 22.44828, 346.8363, "", "", "", "", "", "FALSE",
                           "imagery", "None", 21628.68, 2, 10814.34, 122.5648, 2.680197e-17, "TRUE", "FALSE", 1, 1, 1, 1, 1,
                           "imagery", "Greenhouse-Geisser", 21628.68, 1.494881, 14468.49, 122.5648, 1.757286e-13, "FALSE", "FALSE", 1, 1, 1, 1, 1,
                           "imagery", "Huynh-Feldt", 21628.68, 1.593684, 13571.5, 122.5648, 3.142804e-14, "FALSE", "FALSE", 1, 1, 1, 1, 1,
                           "Residual", "None", 3352.878, 38, 88.23363, "", "", "", "", "", "TRUE",
                           "Residual", "Greenhouse-Geisser", 3352.878, 28.40275, 118.0477, "", "", "", "", "", "FALSE",
                           "Residual", "Huynh-Feldt", 3352.878, 30.28, 110.7291, "", "", "", "", "", "FALSE",
                           "drink <unicode> imagery", "None", 2624.422, 4, 656.1056, 17.15492, 4.58904e-10, "TRUE", "FALSE",
                           "drink <unicode> imagery", "Greenhouse-Geisser", 2624.422, 3.193592, 821.7776, 17.15492, 1.900249e-08, "FALSE", "FALSE",
                           "drink <unicode> imagery", "Huynh-Feldt", 2624.422, 3.914351, 670.4616, 17.15492, 6.80964e-10, "FALSE", "FALSE",
                           "Residual", "None", 2906.689, 76, 38.24591, "", "", "", "", "", "TRUE",
                           "Residual", "Greenhouse-Geisser", 2906.689, 60.67824, 47.90331, "", "", "", "", "", "FALSE",
                           "Residual", "Huynh-Feldt", 2906.689, 74.37268, 39.08275, "", "", "", "", "", "FALSE")
  )
  output9 <- results[["results"]][["marginalMeans"]][["collection"]][[1]][["data"]]
  expect_equal_tables(output9,
                      list("beer", 11.83333, 1.684638, 8.453851, 15.21282, "TRUE",
                           "wine", 8.333333, 1.684638, 4.953851, 11.71282, "FALSE",
                           "water", 3.516667, 1.684638, 0.1371848, 6.896149, "FALSE")
  )
  output10 <- results[["results"]][["posthoc"]][["collection"]][[1]][["data"]]
  expect_equal_tables(output10,
                      list("beer", "wine", 3.5, -3.045489, 10.04549, 2.84949, 1.22829, "", "", "", 0.7030097, "", "TRUE",
                           "beer", "water", 8.316667, 1.771177, 14.86216, 3.335129, 2.493657, "", "", "", 0.06609887, "", "FALSE",
                           "wine", "water", 4.816667, -1.728823, 11.36216, 1.116457, 4.314242, "", "", "", 0.001122131, "", "FALSE")
  )
  output11 <- results[["results"]][["marginalMeans"]][["collection"]][[2]][["data"]]
  expect_equal_tables(output11,
                      list("positive", 21.26667, 1.241696, 18.77998, 23.75335, "TRUE",
                           "negative", -5.583333, 1.241696, -8.070016, -3.096651, "FALSE",
                           "neutral", 8, 1.241696, 5.513317, 10.48668, "FALSE")
  )
  output12 <- results[["results"]][["posthoc"]][["collection"]][[2]][["data"]]
  expect_equal_tables(output12,
                      list("positive", "negative", 26.85, 22.55466, 31.14534, 1.914621, 14.02366, "", "", "", 5.361804e-11, "", "TRUE",
                           "positive", "neutral", 13.26667, 8.97133, 17.562, 1.112555, 11.92451, "", "", "", 8.646278e-10, "", "FALSE",
                           "negative", "neutral", -13.58333, -17.87867, -9.287997, 1.979851, -6.860786, "", "", "", 4.54656e-06, "", "FALSE")
  )
  output13 <- results[["results"]][["marginalMeans"]][["collection"]][[3]][["data"]]
  expect_equal_tables(output13,
                      list("beer", "positive", 21.05, 2.160622, 16.77064, 25.32936, "TRUE",
                           "beer", "negative", 4.45, 2.160622, 0.1706374, 8.729363, "FALSE",
                           "beer", "neutral", 10, 2.160622, 5.720637, 14.27936, "FALSE",
                           "wine", "positive", 25.35, 2.160622, 21.07064, 29.62936, "TRUE",
                           "wine", "negative", -12, 2.160622, -16.27936, -7.720637, "FALSE",
                           "wine", "neutral", 11.65, 2.160622, 7.370637, 15.92936, "FALSE",
                           "water", "positive", 17.4, 2.160622, 13.12064, 21.67936, "TRUE",
                           "water", "negative", -9.2, 2.160622, -13.47936, -4.920637, "FALSE",
                           "water", "neutral", 2.35, 2.160622, -1.929363, 6.629363, "FALSE")
  )
  unnumberedFigure1 <- results[["state"]][["stateDescriptivesPlot"]][[1]]
  #expect_equal_plots(unnumberedFigure1, "?", dir="Ancova") # This command needs to be updated
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("beer", "water"), "name" = "drink"), list("levels" = list("positive", "negative", "neutral"), "name" = "imagery"))
  options$repeatedMeasuresCells <- list("beerpos", "beerneg", "beerneut", "waterpos", "waterneg", "waterneut")
  options$modelTerms <- list(list("components" = list("drink")), list("components" = list("imagery")), list("components" = list("drink", "imagery")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "drink"), list("contrast" = "none", "variable" = "imagery"))
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_08/www/Attitude.sav"), options, view=FALSE, quiet=TRUE)
  output14a <- list(results[["results"]][["withinSubjectsEffects"]][["data"]][[1]], results[["results"]][["withinSubjectsEffects"]][["data"]][[2]])
  expect_equal_tables(output14a,
                      list("drink", 2075.008, 1, 2075.008, 6.218324, 0.02203296, "TRUE",
                           "Residual", 6340.158, 19, 333.6925, "", "", "", "", "", "TRUE")
  )
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("wine", "water"), "name" = "drink"), list("levels" = list("positive", "negative", "neutral"), "name" = "imagery"))
  options$repeatedMeasuresCells <- list("winepos", "wineneg", "wineneut", "waterpos", "waterneg", "waterneut")
  options$modelTerms <- list(list("components" = list("drink")), list("components" = list("imagery")), list("components" = list("drink", "imagery")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "drink"), list("contrast" = "none", "variable" = "imagery"))
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_08/www/Attitude.sav"), options, view=FALSE, quiet=TRUE)
  output14b <- list(results[["results"]][["withinSubjectsEffects"]][["data"]][[1]], results[["results"]][["withinSubjectsEffects"]][["data"]][[2]])
  expect_equal_tables(output14b,
                      list("drink", 696.0083, 1, 696.0083, 18.61269, 0.0003740436, "TRUE",
                           "Residual", 710.4917, 19, 37.3943, "", "", "", "", "", "TRUE")
  )
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("beer", "wine", "water"), "name" = "drink"), list("levels" = list("positive", "neutral"), "name" = "imagery"))
  options$repeatedMeasuresCells <- list("beerpos", "beerneut", "winepos", "wineneut", "waterpos", "waterneut")
  options$modelTerms <- list(list("components" = list("drink")), list("components" = list("imagery")), list("components" = list("drink", "imagery")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "drink"), list("contrast" = "none", "variable" = "imagery"))
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_08/www/Attitude.sav"), options, view=FALSE, quiet=TRUE)
  output14c <- list(results[["results"]][["withinSubjectsEffects"]][["data"]][[3]], results[["results"]][["withinSubjectsEffects"]][["data"]][[4]])
  expect_equal_tables(output14c,
                      list("imagery", 5280.133, 1, 5280.133, 142.1939, 2.882093e-10, "TRUE",
                           "Residual", 705.5333, 19, 37.13333, "", "", "", "", "", "TRUE")
  )
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("beer", "wine", "water"), "name" = "drink"), list("levels" = list("negative", "neutral"), "name" = "imagery"))
  options$repeatedMeasuresCells <- list("beerneg", "beerneut", "wineneg", "wineneut", "waterneg", "waterneut")
  options$modelTerms <- list(list("components" = list("drink")), list("components" = list("imagery")), list("components" = list("drink", "imagery")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "drink"), list("contrast" = "none", "variable" = "imagery"))
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_08/www/Attitude.sav"), options, view=FALSE, quiet=TRUE)
  output14d <- list(results[["results"]][["withinSubjectsEffects"]][["data"]][[3]], results[["results"]][["withinSubjectsEffects"]][["data"]][[4]])
  expect_equal_tables(output14d,
                      list("imagery", 5535.208, 1, 5535.208, 47.07038, 1.51552e-06, "TRUE",
                           "Residual", 2234.292, 19, 117.5943, "", "", "", "", "", "TRUE")
  )
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("beer", "water"), "name" = "drink"), list("levels" = list("positive", "neutral"), "name" = "imagery"))
  options$repeatedMeasuresCells <- list("beerpos", "beerneut", "waterpos", "waterneut")
  options$modelTerms <- list(list("components" = list("drink")), list("components" = list("imagery")), list("components" = list("drink", "imagery")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "drink"), list("contrast" = "none", "variable" = "imagery"))
  options$plotHorizontalAxis <- "drink"
  options$plotSeparateLines <- "imagery"
  options$plotErrorBars <- TRUE
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_08/www/Attitude.sav"), options, view=FALSE, quiet=TRUE)
  output14e <- list(results[["results"]][["withinSubjectsEffects"]][["data"]][[5]], results[["results"]][["withinSubjectsEffects"]][["data"]][[6]])
  expect_equal_tables(output14e,
                      list("drink <unicode> imagery", 80, 1, 80, 1.575946, 0.2245662, "TRUE",
                           "Residual", 964.5, 19, 50.76316, "", "", "", "", "", "TRUE")
  )
  unnumberedFigure2a <- results[["state"]][["stateDescriptivesPlot"]][[1]]
  #expect_equal_plots(unnumberedFigure2a, "?", dir="Ancova") # This command needs to be updated
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("beer", "water"), "name" = "drink"), list("levels" = list("negative", "neutral"), "name" = "imagery"))
  options$repeatedMeasuresCells <- list("beerneg", "beerneut", "waterneg", "waterneut")
  options$modelTerms <- list(list("components" = list("drink")), list("components" = list("imagery")), list("components" = list("drink", "imagery")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "drink"), list("contrast" = "none", "variable" = "imagery"))
  options$plotHorizontalAxis <- "drink"
  options$plotSeparateLines <- "imagery"
  options$plotErrorBars <- TRUE
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_08/www/Attitude.sav"), options, view=FALSE, quiet=TRUE)
  output14f <- list(results[["results"]][["withinSubjectsEffects"]][["data"]][[5]], results[["results"]][["withinSubjectsEffects"]][["data"]][[6]])
  expect_equal_tables(output14f,
                      list("drink <unicode> imagery", 180, 1, 180, 6.752221, 0.01764146, "TRUE",
                           "Residual", 506.5, 19, 26.65789, "", "", "", "", "", "TRUE")
  )
  unnumberedFigure2b <- results[["state"]][["stateDescriptivesPlot"]][[1]]
  #expect_equal_plots(unnumberedFigure2b, "?", dir="Ancova") # This command needs to be updated
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("wine", "water"), "name" = "drink"), list("levels" = list("positive", "neutral"), "name" = "imagery"))
  options$repeatedMeasuresCells <- list("winepos", "wineneut", "waterpos", "waterneut")
  options$modelTerms <- list(list("components" = list("drink")), list("components" = list("imagery")), list("components" = list("drink", "imagery")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "drink"), list("contrast" = "none", "variable" = "imagery"))
  options$plotHorizontalAxis <- "drink"
  options$plotSeparateLines <- "imagery"
  options$plotErrorBars <- TRUE
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_08/www/Attitude.sav"), options, view=FALSE, quiet=TRUE)
  output14g <- list(results[["results"]][["withinSubjectsEffects"]][["data"]][[5]], results[["results"]][["withinSubjectsEffects"]][["data"]][[6]])
  expect_equal_tables(output14g,
                      list("drink <unicode> imagery", 9.1125, 1, 9.1125, 0.2350376, 0.6333556, "TRUE",
                           "Residual", 736.6375, 19, 38.77039, "", "", "", "", "", "TRUE")
  )
  unnumberedFigure2c <- results[["state"]][["stateDescriptivesPlot"]][[1]]
  #expect_equal_plots(unnumberedFigure2c, "?", dir="Ancova") # This command needs to be updated
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("wine", "water"), "name" = "drink"), list("levels" = list("negative", "neutral"), "name" = "imagery"))
  options$repeatedMeasuresCells <- list("wineneg", "wineneut", "waterneg", "waterneut")
  options$modelTerms <- list(list("components" = list("drink")), list("components" = list("imagery")), list("components" = list("drink", "imagery")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "drink"), list("contrast" = "none", "variable" = "imagery"))
  options$plotHorizontalAxis <- "drink"
  options$plotSeparateLines <- "imagery"
  options$plotErrorBars <- TRUE
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_08/www/Attitude.sav"), options, view=FALSE, quiet=TRUE)
  output14h <- list(results[["results"]][["withinSubjectsEffects"]][["data"]][[5]], results[["results"]][["withinSubjectsEffects"]][["data"]][[6]])
  expect_equal_tables(output14h,
                      list("drink <unicode> imagery", 732.05, 1, 732.05, 26.90579, 5.243827e-05, "TRUE",
                           "Residual", 516.95, 19, 27.20789, "", "", "", "", "", "TRUE")
  )
  unnumberedFigure2d <- results[["state"]][["stateDescriptivesPlot"]][[1]]
  #expect_equal_plots(unnumberedFigure2d, "?", dir="Ancova") # This command needs to be updated
})

# Chapter 9
test_that("Fields Book - Chapter 9 results match", {
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("attractive", "unattractive", "average"), "name" = "Looks"), list("levels" = list("high", "low", "average"), "name" = "Charisma"))
  options$repeatedMeasuresCells <- list("att_high", "att_none", "att_some", "ug_high", "ug_none", "ug_some", "av_high", "av_none", "av_some")
  options$betweenSubjectFactors <- list("Strategy")
  options$modelTerms <- list(list("components" = list("Looks")), list("components" = list("Charisma")), list("components" = list("Strategy")), list("components" = list("Looks", "Charisma")), list("components" = list("Looks", "Strategy")), list("components" = list("Charisma", "Strategy")), list("components" = list("Looks", "Charisma", "Strategy")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "Strategy"), list("contrast" = "none", "variable" = "Looks"), list("contrast" = "none", "variable" = "Charisma"))
  options$sphericityTests <- options$sphericityCorrections <- TRUE
  options$sphericityGreenhouseGeisser <- TRUE
  options$sphericityHuynhFeldt <- options$sphericityNone <- FALSE
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_09/www/LooksOrPersonality.sav"), options, view=FALSE, quiet=TRUE)
  tableOutput1 <- results[["results"]][["assumptionsObj"]][["sphericity"]][["data"]]
  expect_equal_tables(tableOutput1,
                      list("Looks", 0.690337, 2, 0.9602054, 0.708101, 0.9617284, 1, 0.5, "TRUE",
                           "Charisma", 1.245957, 2, 0.9293298, 0.5363446, 0.9339944, 1, 0.5, "FALSE",
                           "Looks <unicode> Charisma", 33.63579, 35, 0.6133545, 0.5339382, 0.7993543, 0.9922411, 0.25, "FALSE")
  )
  tableOutput2 <- results[["results"]][["withinSubjectsEffects"]][["data"]]
  expect_equal_tables(tableOutput2,
                      list("Looks", "Greenhouse-Geisser", 20779.63, 1.923457, 10803.28, 423.7325, 7.624114e-25, "TRUE", "FALSE",
                           "Looks <unicode> Strategy", "Greenhouse-Geisser", 3944.1, 1.923457, 2050.527, 80.42699, 1.487026e-13, "FALSE", "FALSE",
                           "Residual", "Greenhouse-Geisser", 882.7111, 34.62222, 25.49551, "", "", "", "", "", "TRUE",
                           "Charisma", "Greenhouse-Geisser", 23233.6, 1.867989, 12437.76, 328.2498, 2.056621e-22, "TRUE", "FALSE",
                           "Charisma <unicode> Strategy", "Greenhouse-Geisser", 4420.133, 1.867989, 2366.252, 62.44868, 9.442426e-12, "FALSE", "FALSE",
                           "Residual", "Greenhouse-Geisser", 1274.044, 33.6238, 37.89115, "", "", "", "", "", "TRUE",
                           "Looks <unicode> Charisma", "Greenhouse-Geisser", 4055.267, 3.197417, 1268.295, 36.63253, 9.003598e-14, "TRUE", "FALSE",
                           "Looks <unicode> Charisma <unicode> Strategy", "Greenhouse-Geisser", 2669.667, 3.197417, 834.9448, 24.11596, 1.470422e-10, "FALSE", "FALSE",
                           "Residual", "Greenhouse-Geisser", 1992.622, 57.55351, 34.62208, "", "", "", "", "", "TRUE")
  )
  tableOutput3 <- results[["results"]][["betweenSubjectsEffects"]][["data"]]
  expect_equal_tables(tableOutput3,
                      list("Strategy", 0.2, 1, 0.2, 0.004735457, 0.9458958, "TRUE",
                           "Residual", 760.2222, 18, 42.23457, "", "", "", "", "", "TRUE")
  )
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("attractive", "average"), "name" = "Looks"), list("levels" = list("high", "low", "average"), "name" = "Charisma"))
  options$repeatedMeasuresCells <- list("att_high", "att_none", "att_some", "av_high", "av_none", "av_some")
  options$betweenSubjectFactors <- list("Strategy")
  options$modelTerms <- list(list("components" = list("Looks")), list("components" = list("Charisma")), list("components" = list("Strategy")), list("components" = list("Looks", "Charisma")), list("components" = list("Looks", "Strategy")), list("components" = list("Charisma", "Strategy")), list("components" = list("Looks", "Charisma", "Strategy")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "Strategy"), list("contrast" = "none", "variable" = "Looks"), list("contrast" = "none", "variable" = "Charisma"))
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_09/www/LooksOrPersonality.sav"), options, view=FALSE, quiet=TRUE)
  tableOutput4a <- list(results[["results"]][["withinSubjectsEffects"]][["data"]][[1]],
                        results[["results"]][["withinSubjectsEffects"]][["data"]][[2]],
                        results[["results"]][["withinSubjectsEffects"]][["data"]][[3]])
  expect_equal_tables(tableOutput4a,
                      list("Looks", 6149.008, 1, 6149.008, 226.986, 1.197624e-11, "TRUE",
                           "Looks <unicode> Strategy", 1171.875, 1, 1171.875, 43.25888, 3.532683e-06, "FALSE",
                           "Residual", 487.6167, 18, 27.08981, "", "", "", "", "", "TRUE")
  )
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("unattractive", "average"), "name" = "Looks"), list("levels" = list("high", "low", "average"), "name" = "Charisma"))
  options$repeatedMeasuresCells <- list("ug_high", "ug_none", "ug_some", "av_high", "av_none", "av_some")
  options$betweenSubjectFactors <- list("Strategy")
  options$modelTerms <- list(list("components" = list("Looks")), list("components" = list("Charisma")), list("components" = list("Strategy")), list("components" = list("Looks", "Charisma")), list("components" = list("Looks", "Strategy")), list("components" = list("Charisma", "Strategy")), list("components" = list("Looks", "Charisma", "Strategy")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "Strategy"), list("contrast" = "none", "variable" = "Looks"), list("contrast" = "none", "variable" = "Charisma"))
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_09/www/LooksOrPersonality.sav"), options, view=FALSE, quiet=TRUE)
  tableOutput4b <- list(results[["results"]][["withinSubjectsEffects"]][["data"]][[1]],
                        results[["results"]][["withinSubjectsEffects"]][["data"]][[2]],
                        results[["results"]][["withinSubjectsEffects"]][["data"]][[3]])
  expect_equal_tables(tableOutput4b,
                      list("Looks", 4296.033, 1, 4296.033, 160.0675, 2.143852e-10, "TRUE",
                           "Looks <unicode> Strategy", 811.2, 1, 811.2, 30.2248, 3.203201e-05, "FALSE",
                           "Residual", 483.1, 18, 26.83889, "", "", "", "", "", "TRUE")
  )
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("attractive", "unattractive", "average"), "name" = "Looks"), list("levels" = list("high", "average"), "name" = "Charisma"))
  options$repeatedMeasuresCells <- list("att_high", "att_some", "ug_high", "ug_some", "av_high", "av_some")
  options$betweenSubjectFactors <- list("Strategy")
  options$modelTerms <- list(list("components" = list("Looks")), list("components" = list("Charisma")), list("components" = list("Strategy")), list("components" = list("Looks", "Charisma")), list("components" = list("Looks", "Strategy")), list("components" = list("Charisma", "Strategy")), list("components" = list("Looks", "Charisma", "Strategy")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "Strategy"), list("contrast" = "none", "variable" = "Looks"), list("contrast" = "none", "variable" = "Charisma"))
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_09/www/LooksOrPersonality.sav"), options, view=FALSE, quiet=TRUE)
  tableOutput4c <- list(results[["results"]][["withinSubjectsEffects"]][["data"]][[4]],
                        results[["results"]][["withinSubjectsEffects"]][["data"]][[5]],
                        results[["results"]][["withinSubjectsEffects"]][["data"]][[6]])
  expect_equal_tables(tableOutput4c,
                      list("Charisma", 4915.2, 1, 4915.2, 109.937, 4.287429e-09, "TRUE",
                           "Charisma <unicode> Strategy", 1216.033, 1, 1216.033, 27.19869, 5.840526e-05, "FALSE",
                           "Residual", 804.7667, 18, 44.70926, "", "", "", "", "", "TRUE")
  )
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("attractive", "unattractive", "average"), "name" = "Looks"), list("levels" = list("low", "average"), "name" = "Charisma"))
  options$repeatedMeasuresCells <- list("att_none", "att_some", "ug_none", "ug_some", "av_none", "av_some")
  options$betweenSubjectFactors <- list("Strategy")
  options$modelTerms <- list(list("components" = list("Looks")), list("components" = list("Charisma")), list("components" = list("Strategy")), list("components" = list("Looks", "Charisma")), list("components" = list("Looks", "Strategy")), list("components" = list("Charisma", "Strategy")), list("components" = list("Looks", "Charisma", "Strategy")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "Strategy"), list("contrast" = "none", "variable" = "Looks"), list("contrast" = "none", "variable" = "Charisma"))
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_09/www/LooksOrPersonality.sav"), options, view=FALSE, quiet=TRUE)
  tableOutput4d <- list(results[["results"]][["withinSubjectsEffects"]][["data"]][[4]],
                        results[["results"]][["withinSubjectsEffects"]][["data"]][[5]],
                        results[["results"]][["withinSubjectsEffects"]][["data"]][[6]])
  expect_equal_tables(tableOutput4d,
                      list("Charisma", 6750, 1, 6750, 227.9407, 1.156267e-11, "TRUE",
                           "Charisma <unicode> Strategy", 997.6333, 1, 997.6333, 33.68908, 1.687006e-05, "FALSE",
                           "Residual", 533.0333, 18, 29.61296, "", "", "", "", "", "TRUE")
  )
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("attractive", "average"), "name" = "Looks"), list("levels" = list("high", "average"), "name" = "Charisma"))
  options$repeatedMeasuresCells <- list("att_high", "att_some", "av_high", "av_some")
  options$betweenSubjectFactors <- list("Strategy")
  options$modelTerms <- list(list("components" = list("Looks")), list("components" = list("Charisma")), list("components" = list("Strategy")), list("components" = list("Looks", "Charisma")), list("components" = list("Looks", "Strategy")), list("components" = list("Charisma", "Strategy")), list("components" = list("Looks", "Charisma", "Strategy")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "Strategy"), list("contrast" = "none", "variable" = "Looks"), list("contrast" = "none", "variable" = "Charisma"))
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_09/www/LooksOrPersonality.sav"), options, view=FALSE, quiet=TRUE)
  tableOutput4e <- list(results[["results"]][["withinSubjectsEffects"]][["data"]][[7]],
                        results[["results"]][["withinSubjectsEffects"]][["data"]][[8]],
                        results[["results"]][["withinSubjectsEffects"]][["data"]][[9]])
  expect_equal_tables(tableOutput4e,
                      list("Looks <unicode> Charisma", 994.05, 1, 994.05, 21.94371, 0.0001846524, "TRUE",
                           "Looks <unicode> Charisma <unicode> Strategy", 42.05, 1, 42.05, 0.9282561, 0.3480825, "FALSE",
                           "Residual", 815.4, 18, 45.3, "", "", "", "", "", "TRUE")
  )
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("attractive", "average"), "name" = "Looks"), list("levels" = list("low", "average"), "name" = "Charisma"))
  options$repeatedMeasuresCells <- list("att_none", "att_some", "av_none", "av_some")
  options$betweenSubjectFactors <- list("Strategy")
  options$modelTerms <- list(list("components" = list("Looks")), list("components" = list("Charisma")), list("components" = list("Strategy")), list("components" = list("Looks", "Charisma")), list("components" = list("Looks", "Strategy")), list("components" = list("Charisma", "Strategy")), list("components" = list("Looks", "Charisma", "Strategy")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "Strategy"), list("contrast" = "none", "variable" = "Looks"), list("contrast" = "none", "variable" = "Charisma"))
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_09/www/LooksOrPersonality.sav"), options, view=FALSE, quiet=TRUE)
  tableOutput4f <- list(results[["results"]][["withinSubjectsEffects"]][["data"]][[7]],
                        results[["results"]][["withinSubjectsEffects"]][["data"]][[8]],
                        results[["results"]][["withinSubjectsEffects"]][["data"]][[9]])
  expect_equal_tables(tableOutput4f,
                      list("Looks <unicode> Charisma", 110.45, 1, 110.45, 4.090741, 0.05823426, "TRUE",
                           "Looks <unicode> Charisma <unicode> Strategy", 1638.05, 1, 1638.05, 60.66852, 3.578459e-07, "FALSE",
                           "Residual", 486, 18, 27, "", "", "", "", "", "TRUE")
  )
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("unattractive", "average"), "name" = "Looks"), list("levels" = list("high", "average"), "name" = "Charisma"))
  options$repeatedMeasuresCells <- list("ug_high", "ug_some", "av_high", "av_some")
  options$betweenSubjectFactors <- list("Strategy")
  options$modelTerms <- list(list("components" = list("Looks")), list("components" = list("Charisma")), list("components" = list("Strategy")), list("components" = list("Looks", "Charisma")), list("components" = list("Looks", "Strategy")), list("components" = list("Charisma", "Strategy")), list("components" = list("Looks", "Charisma", "Strategy")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "Strategy"), list("contrast" = "none", "variable" = "Looks"), list("contrast" = "none", "variable" = "Charisma"))
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_09/www/LooksOrPersonality.sav"), options, view=FALSE, quiet=TRUE)
  tableOutput4g <- list(results[["results"]][["withinSubjectsEffects"]][["data"]][[7]],
                        results[["results"]][["withinSubjectsEffects"]][["data"]][[8]],
                        results[["results"]][["withinSubjectsEffects"]][["data"]][[9]])
  expect_equal_tables(tableOutput4g,
                      list("Looks <unicode> Charisma", 227.8125, 1, 227.8125, 6.230769, 0.02248718, "TRUE",
                           "Looks <unicode> Charisma <unicode> Strategy", 427.8125, 1, 427.8125, 11.70085, 0.003048789, "FALSE",
                           "Residual", 658.125, 18, 36.5625, "", "", "", "", "", "TRUE")
  )
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("unattractive", "average"), "name" = "Looks"), list("levels" = list("low", "average"), "name" = "Charisma"))
  options$repeatedMeasuresCells <- list("ug_none", "ug_some", "av_none", "av_some")
  options$betweenSubjectFactors <- list("Strategy")
  options$modelTerms <- list(list("components" = list("Looks")), list("components" = list("Charisma")), list("components" = list("Strategy")), list("components" = list("Looks", "Charisma")), list("components" = list("Looks", "Strategy")), list("components" = list("Charisma", "Strategy")), list("components" = list("Looks", "Charisma", "Strategy")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "Strategy"), list("contrast" = "none", "variable" = "Looks"), list("contrast" = "none", "variable" = "Charisma"))
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_09/www/LooksOrPersonality.sav"), options, view=FALSE, quiet=TRUE)
  tableOutput4h <- list(results[["results"]][["withinSubjectsEffects"]][["data"]][[7]],
                        results[["results"]][["withinSubjectsEffects"]][["data"]][[8]],
                        results[["results"]][["withinSubjectsEffects"]][["data"]][[9]])
  expect_equal_tables(tableOutput4h,
                      list("Looks <unicode> Charisma", 1833.612, 1, 1833.612, 88.59815, 2.248161e-08, "TRUE",
                           "Looks <unicode> Charisma <unicode> Strategy", 27.6125, 1, 27.6125, 1.334206, 0.2631624, "FALSE",
                           "Residual", 372.525, 18, 20.69583, "", "", "", "", "", "TRUE")
  )
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("attractive", "unattractive", "average"), "name" = "Looks"), list("levels" = list("high", "low", "average"), "name" = "Charisma"))
  options$repeatedMeasuresCells <- list("att_high", "att_none", "att_some", "ug_high", "ug_none", "ug_some", "av_high", "av_none", "av_some")
  options$betweenSubjectFactors <- list("Strategy")
  options$modelTerms <- list(list("components" = list("Looks")), list("components" = list("Charisma")), list("components" = list("Strategy")), list("components" = list("Looks", "Charisma")), list("components" = list("Looks", "Strategy")), list("components" = list("Charisma", "Strategy")), list("components" = list("Looks", "Charisma", "Strategy")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "Strategy"), list("contrast" = "none", "variable" = "Looks"), list("contrast" = "none", "variable" = "Charisma"))
  options$marginalMeansTerms <- list("Charisma", list("Strategy", "Looks"), list("Strategy", "Charisma"), list("Looks", "Charisma"), list("Strategy", "Looks", "Charisma"))
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_09/www/LooksOrPersonality.sav"), options, view=FALSE, quiet=TRUE)
  output6 <- results[["results"]][["marginalMeans"]][["collection"]][[1]][["data"]]
  expect_equal_tables(output6,
                      list("high", 82.1, 0.7923762, 80.51111, 83.68889, "TRUE",
                           "low", 54.3, 0.7923762, 52.71111, 55.88889, "FALSE",
                           "average", 69.3, 0.7923762, 67.71111, 70.88889, "FALSE")
  )
  output7 <- results[["results"]][["marginalMeans"]][["collection"]][[2]][["data"]]
  expect_equal_tables(output7,
                      list("0", "attractive", 76.16667, 1.007053, 74.14416, 78.18918, "TRUE",
                           "0", "unattractive", 61.33333, 1.007053, 59.31082, 63.35584, "FALSE",
                           "0", "average", 68.1, 1.007053, 66.07749, 70.12251, "FALSE",
                           "1", "attractive", 88.03333, 1.007053, 86.01082, 90.05584, "TRUE",
                           "1", "unattractive", 50.3, 1.007053, 48.27749, 52.32251, "FALSE",
                           "1", "average", 67.46667, 1.007053, 65.44416, 69.48918, "FALSE")
  )
  output8 <- results[["results"]][["marginalMeans"]][["collection"]][[3]][["data"]]
  expect_equal_tables(output8,
                      list("0", "high", 88.23333, 1.120589, 85.98631, 90.48036, "TRUE",
                           "0", "low", 48.3, 1.120589, 46.05298, 50.54702, "FALSE",
                           "0", "average", 69.06667, 1.120589, 66.81964, 71.31369, "FALSE",
                           "1", "high", 75.96667, 1.120589, 73.71964, 78.21369, "TRUE",
                           "1", "low", 60.3, 1.120589, 58.05298, 62.54702, "FALSE",
                           "1", "average", 69.53333, 1.120589, 67.28631, 71.78036, "FALSE")
  )
  output9 <- results[["results"]][["marginalMeans"]][["collection"]][[4]][["data"]]
  expect_equal_tables(output9,
                      list("attractive", "high", 88.95, 1.230979, 86.51853, 91.38147, "TRUE",
                           "attractive", "low", 69.55, 1.230979, 67.11853, 71.98147, "FALSE",
                           "attractive", "average", 87.8, 1.230979, 85.36853, 90.23147, "FALSE",
                           "unattractive", "high", 71.75, 1.230979, 69.31853, 74.18147, "TRUE",
                           "unattractive", "low", 45.95, 1.230979, 43.51853, 48.38147, "FALSE",
                           "unattractive", "average", 49.75, 1.230979, 47.31853, 52.18147, "FALSE",
                           "average", "high", 85.6, 1.230979, 83.16853, 88.03147, "TRUE",
                           "average", "low", 47.4, 1.230979, 44.96853, 49.83147, "FALSE",
                           "average", "average", 70.35, 1.230979, 67.91853, 72.78147, "FALSE")
  )
  output10 <- results[["results"]][["marginalMeans"]][["collection"]][[5]][["data"]]
  expect_equal_tables(output10,
                      list("0", "attractive", "high", 89.6, 1.740867, 86.16138, 93.03862, "TRUE",
                           "0", "attractive", "low", 51.8, 1.740867, 48.36138, 55.23862, "FALSE",
                           "0", "attractive", "average", 87.1, 1.740867, 83.66138, 90.53862, "FALSE",
                           "0", "unattractive", "high", 86.7, 1.740867, 83.26138, 90.13862, "TRUE",
                           "0", "unattractive", "low", 46.1, 1.740867, 42.66138, 49.53862, "FALSE",
                           "0", "unattractive", "average", 51.2, 1.740867, 47.76138, 54.63862, "FALSE",
                           "0", "average", "high", 88.4, 1.740867, 84.96138, 91.83862, "TRUE",
                           "0", "average", "low", 47, 1.740867, 43.56138, 50.43862, "FALSE",
                           "0", "average", "average", 68.9, 1.740867, 65.46138, 72.33862, "FALSE",
                           "1", "attractive", "high", 88.3, 1.740867, 84.86138, 91.73862, "TRUE",
                           "1", "attractive", "low", 87.3, 1.740867, 83.86138, 90.73862, "FALSE",
                           "1", "attractive", "average", 88.5, 1.740867, 85.06138, 91.93862, "FALSE",
                           "1", "unattractive", "high", 56.8, 1.740867, 53.36138, 60.23862, "TRUE",
                           "1", "unattractive", "low", 45.8, 1.740867, 42.36138, 49.23862, "FALSE",
                           "1", "unattractive", "average", 48.3, 1.740867, 44.86138, 51.73862, "FALSE",
                           "1", "average", "high", 82.8, 1.740867, 79.36138, 86.23862, "TRUE",
                           "1", "average", "low", 47.8, 1.740867, 44.36138, 51.23862, "FALSE",
                           "1", "average", "average", 71.8, 1.740867, 68.36138, 75.23862, "FALSE")
  )
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("attractive", "unattractive", "average"), "name" = "Looks"), list("levels" = list("high", "low", "average"), "name" = "Charisma"))
  options$repeatedMeasuresCells <- list("att_high", "att_none", "att_some", "ug_high", "ug_none", "ug_some", "av_high", "av_none", "av_some")
  options$betweenSubjectFactors <- list("Strategy")
  options$modelTerms <- list(list("components" = list("Looks")), list("components" = list("Charisma")), list("components" = list("Strategy")), list("components" = list("Looks", "Charisma")), list("components" = list("Looks", "Strategy")), list("components" = list("Charisma", "Strategy")), list("components" = list("Looks", "Charisma", "Strategy")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "Strategy"), list("contrast" = "none", "variable" = "Looks"), list("contrast" = "none", "variable" = "Charisma"))
  options$plotHorizontalAxis <- "Looks"
  options$plotSeparateLines <- "Strategy"
  options$plotErrorBars <- TRUE
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_09/www/LooksOrPersonality.sav"), options, view=FALSE, quiet=TRUE)
  unnumberedFigure1 <- results[["state"]][["stateDescriptivesPlot"]][[1]]
  #expect_equal_plots(unnumberedFigure1, "?", dir="Ancova") # This command needs to be updated
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("attractive", "unattractive", "average"), "name" = "Looks"), list("levels" = list("high", "low", "average"), "name" = "Charisma"))
  options$repeatedMeasuresCells <- list("att_high", "att_none", "att_some", "ug_high", "ug_none", "ug_some", "av_high", "av_none", "av_some")
  options$betweenSubjectFactors <- list("Strategy")
  options$modelTerms <- list(list("components" = list("Looks")), list("components" = list("Charisma")), list("components" = list("Strategy")), list("components" = list("Looks", "Charisma")), list("components" = list("Looks", "Strategy")), list("components" = list("Charisma", "Strategy")), list("components" = list("Looks", "Charisma", "Strategy")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "Strategy"), list("contrast" = "none", "variable" = "Looks"), list("contrast" = "none", "variable" = "Charisma"))
  options$plotHorizontalAxis <- "Charisma"
  options$plotSeparateLines <- "Strategy"
  options$plotErrorBars <- TRUE
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_09/www/LooksOrPersonality.sav"), options, view=FALSE, quiet=TRUE)
  unnumberedFigure2 <- results[["state"]][["stateDescriptivesPlot"]][[1]]
  #expect_equal_plots(unnumberedFigure2, "?", dir="Ancova") # This command needs to be updated
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("attractive", "unattractive", "average"), "name" = "Looks"), list("levels" = list("high", "low", "average"), "name" = "Charisma"))
  options$repeatedMeasuresCells <- list("att_high", "att_none", "att_some", "ug_high", "ug_none", "ug_some", "av_high", "av_none", "av_some")
  options$betweenSubjectFactors <- list("Strategy")
  options$modelTerms <- list(list("components" = list("Looks")), list("components" = list("Charisma")), list("components" = list("Strategy")), list("components" = list("Looks", "Charisma")), list("components" = list("Looks", "Strategy")), list("components" = list("Charisma", "Strategy")), list("components" = list("Looks", "Charisma", "Strategy")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "Strategy"), list("contrast" = "none", "variable" = "Looks"), list("contrast" = "none", "variable" = "Charisma"))
  options$plotHorizontalAxis <- "Looks"
  options$plotSeparateLines <- "Charisma"
  options$plotErrorBars <- TRUE
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_09/www/LooksOrPersonality.sav"), options, view=FALSE, quiet=TRUE)
  unnumberedFigure3 <- results[["state"]][["stateDescriptivesPlot"]][[1]]
  #expect_equal_plots(unnumberedFigure3, "?", dir="Ancova") # This command needs to be updated
  
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  options$repeatedMeasuresFactors <- list(list("levels" = list("attractive", "unattractive", "average"), "name" = "Looks"), list("levels" = list("high", "low", "average"), "name" = "Charisma"))
  options$repeatedMeasuresCells <- list("att_high", "att_none", "att_some", "ug_high", "ug_none", "ug_some", "av_high", "av_none", "av_some")
  options$betweenSubjectFactors <- list("Strategy")
  options$modelTerms <- list(list("components" = list("Looks")), list("components" = list("Charisma")), list("components" = list("Strategy")), list("components" = list("Looks", "Charisma")), list("components" = list("Looks", "Strategy")), list("components" = list("Charisma", "Strategy")), list("components" = list("Looks", "Charisma", "Strategy")))
  options$contrasts <- list(list("contrast" = "none", "variable" = "Strategy"), list("contrast" = "none", "variable" = "Looks"), list("contrast" = "none", "variable" = "Charisma"))
  options$plotHorizontalAxis <- "Looks"
  options$plotSeparateLines <- "Charisma"
  options$plotSeparatePlots <- "Strategy"
  options$plotErrorBars <- TRUE
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = rio::import("~/Dropbox/ej_andy_shared/spss_tutorials/spss_glm_09/www/LooksOrPersonality.sav"), options, view=FALSE, quiet=TRUE)
  unnumberedFigure4a <- results[["state"]][["stateDescriptivesPlot"]][[1]]
  #expect_equal_plots(unnumberedFigure4a, "?", dir="Ancova") # This command needs to be updated
  unnumberedFigure4b <- results[["state"]][["stateDescriptivesPlot"]][[2]]
  #expect_equal_plots(unnumberedFigure4b, "?", dir="Ancova") # This command needs to be updated
})
