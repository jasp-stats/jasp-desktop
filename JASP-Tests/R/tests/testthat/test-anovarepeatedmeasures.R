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
  options$withinModelTerms <- list(
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
                    0.590744227025686, "TRUE", "Imagery", 0.662101262364057, 0.0244523015633462,
                    0.747440723179836, 0.796842044848417, "FALSE", "Drink <unicode> Imagery",
                    0.595043993796251, 0.435658665786593, 0.798397939908785, 0.97858783367844,
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
  
  refTable <- list("Beer", "Water", 8.31666666666667,
                   3.3351289023547, 2.49365674016224, 0.557598598355329, 0.0440659117291126,
                   0.0660988675936689, "", "", "TRUE", "Beer", "Wine", 3.5, 2.84948954082566, 1.22829017262715, 0.274654032208925,
                   0.234336562537138, 0.703009687611414, "", "", "FALSE", "Water", "Wine", -4.81666666666667,
                   1.1164571680934, -4.31424223366509, -0.964693890587566, 0.00112213065327869,
                   0.00112213065327869, "", "", "FALSE")
  
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
  options$withinModelTerms <- list(
    list(components = "Looks"),
    list(components = "Charisma"), 
    list(components = c("Looks", "Charisma"))
  )
  
  options$betweenSubjectFactors <- "gender"
  options$betweenModelTerms <- list(
    list(components = "gender")
  )
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
  
  options$withinModelTerms <- list(
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
  options$betweenModelTerms <- list(
    list(components = "gender")
  )
  
  options$simpleFactor <- "Looks"
  options$moderatorFactorOne <- "gender"
  options$moderatorFactorTwo <- "Charisma"
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv",
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

test_that("Nonparametric table match", {
  
  options <- initOpts()
  
  options$betweenSubjectFactors <- "gender"
  options$betweenModelTerms <- list(
    list(components = "gender")
  )
  
  options$friedmanWithinFactor <- "Charisma"
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv",                             options = options,
                            view = FALSE, quiet = TRUE)
  
  refTable <- list( "Charisma", 40.074508162411, 2, 1.98577994376659e-09, -170.212868480726,
                    26.3987755757377, 8, 158, 1.2968573602055e-25)
  
  table <- results[["results"]][["friedman"]][["data"]]
  expect_equal_tables(table, refTable)
})



test_that("Conover table match", {
  
  options <- initOpts()
  
  options$betweenSubjectFactors <- "gender"
  options$betweenModelTerms <- list(
    list(components = "gender")
  )
  
  options$conoverTest <- TRUE
  
  options$friedmanWithinFactor <- "Charisma"
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv",
                            options = options,
                            view = FALSE, quiet = TRUE)
  
  refTable <- list( "High", "Some", 1.31809226918034, 406, 306.5, 0.189380510753162, 0.568141532259486, 
                    0.233862092301299, 158, "High", "None", 2.89450412880305, 406, 187.5,
                    0.00433496034546402, 0.0130048810363921, 0.0130048810363921, 158, "Some",
                    "None", 1.57641185962271, 306.5, 187.5, 0.116931046150649, 0.350793138451948,
                    0.233862092301299, 158)
  
  table <- results[["results"]][["conover"]][[1]][[1]][["data"]]
  expect_equal_tables(table, refTable)
})
