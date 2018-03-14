context("Repeated Measures ANOVA")

# Does not test:
#    - type I and type II sum of squares
#    - Simple effects
#    - Plots
#    - Contrasts apart from 'repeated'

# Create data:
AnovaRepeatedMeasures <- data.frame(
  beerpos = c(1, 43, 15, 40, 8, 17, 30, 34, 34, 26, 1, 7, 22, 30, 40, 15, 20, 9, 14, 15),
  beerneg = c(6, 30, 15, 30, 12, 17, 21, 23, 20, 27, -19, -18, -8, -6, -6, -9, -17, -12, -11, -6),
  beerneut = c(5, 8, 12, 19, 8, 15, 21, 28, 26, 27, -10, 6, 4, 3, 0, 4, 9, -5, 7, 13),
  winepos = c(38, 20, 20, 28, 11, 17, 15, 27, 24, 23, 28, 26, 34, 32, 24, 29, 30, 24, 34, 23),
  wineneg = c(-5, -12, -15, -4, -2, -6, -2, -7, -10, -15, -13, -16, -23, -22, -9, -18, -17, -15, -14, -15),
  wineneut = c(4, 4, 6, 0, 6, 6, 16, 7, 12, 14, 13, 19, 14, 21, 19, 7, 12, 18, 20, 15),
  waterpos = c(10, 9, 6, 20, 27, 9, 19, 12, 12, 21, 33, 23, 21, 17, 15, 13, 16, 17, 19, 29),
  waterneg = c(-14, -10, -16, -10, 5, -6, -20, -12, -9, -6, -2, -17, -19, -11, -10, -17, -4, -4, -1, -1),
  waterneu = c(-2, -13, 1, 2, -5, -13, 3, 2, 4, 0, 9, 5, 0, 4, 2, 8, 10, 8, 12, 10),
  participant = c(1, 12, 14, 15, 16, 17, 18, 19, 20, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13))


AnovaMixedEffects <- data.frame(
  participant = 1:20,
  gender = c("Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male"),
  att_high = c(86, 91, 89, 89, 80, 80, 89, 100, 90, 89, 89, 84, 99, 86, 89, 80, 82, 97, 95, 95),
  av_high = c(84, 83, 88, 69, 81, 84, 85, 94, 74, 86, 91, 90, 100, 89, 87, 81, 92, 69, 92, 93),
  ug_high = c(67, 53, 48, 58, 57, 51, 61, 56, 54, 63, 93, 85, 89, 83, 80, 79, 85, 87, 90, 96),
  att_some = c(88, 83, 99, 86, 88, 96, 87, 86, 92, 80, 88, 95, 80, 86, 83, 86, 81, 95, 98, 79),
  av_some = c(69, 74, 70, 77, 71, 63, 79, 71, 71, 73, 65, 70, 79, 74, 74, 59, 66, 72, 64, 66),
  ug_some = c(50, 48, 48, 40, 50, 42, 44, 54, 58, 49, 54, 60, 53, 58, 43, 47, 47, 51, 53, 46),
  att_none = c(97, 86, 90, 87, 82, 92, 86, 84, 78, 91, 55, 50, 51, 52, 58, 51, 50, 45, 54, 52),
  av_none = c(48, 50, 45, 47, 50, 48, 50, 54, 38, 48, 48, 44, 48, 48, 50, 47, 45, 48, 53, 39),
  ug_none = c(47, 46, 48, 53, 45, 43, 45, 47, 45, 39, 52, 45, 44, 47, 48, 40, 47, 46, 45, 47))


AnovaRepeatedMeasuresOneWay <- data.frame(
  Celebrity = 1:8, Stick.Insect = c(8, 9, 6, 5, 8, 7, 10, 12),
  Kangaroo.Testicle = c(7, 5, 2, 3, 4, 5, 2, 6),
  Fish.Eye = c(1, 2, 3, 1, 5, 6, 7, 8),
  Witchetty.Grub = c(6, 5, 8, 9, 8, 7, 2, 1)
)

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

  results <- jasptools::run(name = "AnovaRepeatedMeasures", dataset = AnovaRepeatedMeasures,
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
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures", dataset = AnovaRepeatedMeasures,
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
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures", dataset = AnovaRepeatedMeasures,
                            options = options, view = FALSE, quiet = TRUE)
  
  refTable <- list("Beer", "Wine", 3.5, 2.84948954082566, 1.22829017262715, 0.274654032208925,
                   "", "", 0.703009687611414, "", "TRUE", "Beer", "Water", 8.31666666666667,
                   3.3351289023547, 2.49365674016224, 0.557598598355329, "", "",
                   0.0660988675936689, "", "FALSE", "Wine", "Water", 4.81666666666667,
                   1.1164571680934, 4.31424223366509, 0.964693890587566, "", "",
                   0.00112213065327869, "", "FALSE")
  
  table <- results[["results"]][["posthoc"]][["collection"]][[1]][["data"]]
  expect_equal_tables(table, refTable)
})

test_that("Descriptives Match", {
  options <- initOpts()
  
  options$descriptives <- TRUE
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures", dataset = AnovaRepeatedMeasures,
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
                            dataset = AnovaMixedEffects, options = options,
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
                            dataset = AnovaMixedEffects, options = options,
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
                            dataset = AnovaMixedEffects, options = options,
                            view = FALSE, quiet = TRUE)
  
  refTable <- list("Attractive - Average", 13.5333333333333, 0.521959482105862, 25.9279384651327,
                   6.94207506292919e-25, "TRUE", "Average - Ugly", 12.75, 0.521959482105862,
                   24.4271834061903, 5.30699268885104e-24, "FALSE")
  
  table <- results[["results"]][["contrasts"]][["collection"]][[1]][["data"]]
  expect_equal_tables(table, refTable)
})


test_that("Effect Size Calculation correct", {
  options <- jasptools::analysisOptions("AnovaRepeatedMeasures")
  
  options$repeatedMeasuresFactors <- list(
    list(name = "Animal", levels = c("Stick", "Kangaroo", "Fish", "Grub"))
    )
  options$repeatedMeasuresCells <- c("Stick.Insect", "Kangaroo.Testicle",
                                     "Fish.Eye", "Witchetty.Grub")
  
  options$withinModelTerms <- list(
    list(components = "Animal")
  )
  
  options$effectSizeEstimates <- TRUE
  options$effectSizeEtaSquared <- TRUE
  options$effectSizeOmegaSquared <- TRUE
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures",
                            dataset = AnovaRepeatedMeasuresOneWay,
                            options = options,
                            view = FALSE, quiet = TRUE)
  
  refTable <- list("Animal", 83.1249999999999, 3, 27.7083333333333, 3.79380603096984,
                0.0255702968630395, "TRUE", 1, 1, 1, 1, 1, 0.351479915433403,
                0.351479915433403, 0.238785176929506, "Residual", 153.375, 21,
                7.30357142857143, "", "", "", "", "", "TRUE")

  table <- results[["results"]][["withinSubjectsEffects"]][["data"]]
  expect_equal_tables(table, refTable)
})


