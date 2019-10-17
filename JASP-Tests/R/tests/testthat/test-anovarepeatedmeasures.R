context("Repeated Measures ANOVA")

# Does not test:
#    - type I and type II sum of squares
#    - Simple effects
#    - Plots
#    - Contrasts apart from 'repeated'

opts <- options()
on.exit(options(opts))
options(list(
  afex.type = 3,
  afex.set_data_arg = FALSE,
  afex.check_contrasts = TRUE,
  afex.method_mixed = "KR",
  afex.return_aov = "afex_aov",
  afex.es_aov = "ges",
  afex.correction_aov = "GG",
  afex.factorize = TRUE,
  afex.lmer_function = "lmerTest",
  afex.sig_symbols = c(" +", " *", " **", " ***"),
  afex.emmeans_model = c("univariate"),
  afex.include_aov = TRUE
))

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
                            options = options)
  refTable <- list("Drink", "None", 2092.34444444444, 2, 1046.17222222222, 5.10598105687077,
                0.0108629307294978, "TRUE", "FALSE", 1, 1, 1, 1, 1, "Drink",
                "Greenhouse-Geisser", 2092.34444444444, 1.15422864073086, 1812.76427443316,
                5.10598105687077, 0.0297686804863521, "FALSE", "FALSE", 1, 1,
                1, 1, 1, "Drink", "Huynh-Feldt", 2092.34444444444, 1.18148845405137,
                1770.93939197604, 5.10598105687077, 0.028813909529067, "FALSE",
                "FALSE", 1, 1, 1, 1, 1, "Residual", "None", 7785.87777777778,
                38, 204.891520467836, "", "", "", "", "", "", "TRUE", "Residual",
                "Greenhouse-Geisser", 7785.87777777778, 21.9303441738863, 355.027614525488,
                "", "", "", "", "", "", "FALSE", "Residual", "Huynh-Feldt", 7785.87777777778,
                22.4482806269761, 346.836263638895, "", "", "", "", "", "", "FALSE",
                "Imagery", "None", 21628.6777777778, 2, 10814.3388888889, 122.564824909945,
                2.68019659683571e-17, "TRUE", "FALSE", 1, 1, 1, 1, 1, "Imagery",
                "Greenhouse-Geisser", 21628.6777777778, 1.49488144635967, 14468.4903478118,
                122.564824909945, 1.75728558571484e-13, "FALSE", "FALSE", 1,
                1, 1, 1, 1, "Imagery", "Huynh-Feldt", 21628.6777777778, 1.59368408969683,
                13571.4963320568, 122.564824909945, 3.14280380271786e-14, "FALSE",
                "FALSE", 1, 1, 1, 1, 1, "Residual", "None", 3352.87777777778,
                38, 88.2336257309941, "", "", "", "", "", "", "TRUE", "Residual",
                "Greenhouse-Geisser", 3352.87777777778, 28.4027474808338, 118.047656482539,
                "", "", "", "", "", "", "FALSE", "Residual", "Huynh-Feldt", 3352.87777777778,
                30.2799977042398, 110.729129193702, "", "", "", "", "", "", "FALSE",
                "Drink <unicode> Imagery", "None", 2624.42222222222, 4, 656.105555555556,
                17.1549223629789, 4.58904028152479e-10, "TRUE", "FALSE", "Drink <unicode> Imagery",
                "Greenhouse-Geisser", 2624.42222222222, 3.19359175963514, 821.777615847198,
                17.1549223629789, 1.90024850184092e-08, "FALSE", "FALSE", "Drink <unicode> Imagery",
                "Huynh-Feldt", 2624.42222222222, 3.91435133471376, 670.4615906467,
                17.1549223629789, 6.80963952075043e-10, "FALSE", "FALSE", "Residual",
                "None", 2906.68888888889, 76, 38.2459064327485, "", "", "",
                "", "", "", "TRUE", "Residual", "Greenhouse-Geisser", 2906.68888888889,
                60.6782434330676, 47.9033130234755, "", "", "", "", "", "", "FALSE",
                "Residual", "Huynh-Feldt", 2906.68888888889, 74.3726753595615,
                39.0827528367944, "", "", "", "", "", "", "FALSE")
  
  table <- results[["results"]][["withinSubjectsEffects"]][["data"]]
  expect_equal_tables(table, refTable)
})

test_that("Sphericity Assumptions table match (Field Chapter 8)", {
  options <- initOpts()
  
  options$sphericityTests <- TRUE
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures", dataset = "AnovaRepeatedMeasures.csv",
                            options = options)
  
  refTable <- list("Drink", 0.267241056560857, 23.7528754979348, 2, 6.95230186958065e-06,
                   0.577114320365429, 0.590744227025686, 0.5, "TRUE", "Imagery",
                   0.662101262364057, 7.42206186804268, 2, 0.0244523015633462,
                   0.747440723179836, 0.796842044848417, 0.5, "FALSE", "Drink <unicode> Imagery",
                   0.595043993796251, 9.04133890303752, 9, 0.435658665786593, 0.798397939908785,
                   0.97858783367844, 0.25, "FALSE")
  
  table <- results[["results"]][["assumptionsObj"]][["sphericity"]][["data"]]
  expect_equal_tables(table, refTable)
  
})

test_that("Post-hoc tests match (Field Chapter 8)", {
  options <- initOpts()
  
  options$postHocTestsVariables <- list(list(components = "Drink"), 
                                        list(components = "Imagery"), 
                                        list(components = c("Drink", "Imagery")))
  options$postHocTestEffectSize <- TRUE
  options$postHocTestsBonferroni <- TRUE
  options$postHocTestPooledError <- FALSE
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures", dataset = "AnovaRepeatedMeasures.csv",
                            options = options)
  
  refTable <- list("Beer", "Water", 8.31666666666667, -0.438399936264868, 17.0717332695982,
                   3.3351289023547, 2.49365674016224, 0.557598598355329, 0.0440659117291126,
                   0.0660988675936689, "", "", "TRUE", "Beer", "Wine", 3.5, -3.98021184328794,
                   10.9802118432879, 2.84948954082566, 1.22829017262715, 0.274654032208925,
                   0.234336562537138, 0.703009687611414, "", "", "FALSE", "Water",
                   "Wine", -4.81666666666667, -7.74748498048862, -1.88584835284472,
                   1.1164571680934, -4.31424223366509, -0.964693890587566, 0.00112213065327869,
                   0.00112213065327869, "", "", "FALSE")
  
  table <- results[["results"]][["posthoc"]][["collection"]][[1]][["data"]]
  expect_equal_tables(table, refTable)
  
  refTable <- list("Negative", "Neutral", -13.5833333333333, -18.780651873764, -8.38601479290266,
                   1.97985098972637, -6.86078568731614, -1.53411831758965, 1.51551995402052e-06,
                   4.54655986206157e-06, "", "", "TRUE", "Negative", "Positive",
                   -26.85, -31.8760837862839, -21.8239162137161, 1.91462133431161,
                   -14.0236607201777, -3.13578586637111, 5.36180351283324e-11,
                   5.36180351283324e-11, "", "", "FALSE", "Neutral", "Positive",
                   -13.2666666666667, -16.1872403728605, -10.3460929604728, 1.11255461788524,
                   -11.9245082024684, -2.66640109389732, 5.76418507457459e-10,
                   8.64627761186188e-10, "", "", "FALSE")
  
  table <- results[["results"]][["posthoc"]][["collection"]][[2]][["data"]]
  expect_equal_tables(table, refTable)
  
  refTable <- list("Beer,Negative", "Beer,Neutral", -5.54999999999999, -13.2654734038749,
                   2.16547340387494, 2.34325584459239, -2.36849937355663, -0.52961256039383,
                   0.178723872049576, 0.714895488198306, "", "", "TRUE", "Beer,Negative",
                   "Beer,Positive", -16.6, -24.3154734038749, -8.88452659612507,
                   2.34325584459239, -7.08416028847572, -1.58406639685362, 6.68424111051914e-09,
                   8.29767861995479e-09, "", "", "FALSE", "Beer,Negative", "Water,Negative",
                   13.65, 3.43302479226212, 23.8669752077379, 3.06258786722021,
                   4.45701497942312, 0.996618847072493, 0.000518594859931535, 0.00116683843484595,
                   "", "", "FALSE", "Beer,Negative", "Water,Neutral", 2.09999999999999,
                   -8.89721324755597, 13.097213247556, 3.32350744762006, 0.631862582857695,
                   0.14128876877084, 1, 1, "", "", "FALSE", "Beer,Negative", "Water,Positive",
                   -12.95, -23.947213247556, -1.95278675244405, 3.32350744762006,
                   -3.89648592762247, -0.871280740753516, 0.00276428747376255,
                   0.00710816778967513, "", "", "FALSE", "Beer,Negative", "Wine,Negative",
                   16.45, 6.23302479226211, 26.6669752077379, 3.06258786722021,
                   5.37127446238171, 1.20105348236941, 2.11606666553922e-05, 3.8089199979706e-05,
                   "", "", "FALSE", "Beer,Negative", "Wine,Neutral", -7.20000000000001,
                   -18.197213247556, 3.79721324755596, 3.32350744762006, -2.16638599836925,
                   -0.484418635785739, 0.232136379173532, 1, "", "", "FALSE", "Beer,Negative",
                   "Wine,Positive", -20.9, -31.897213247556, -9.90278675244405,
                   3.32350744762006, -6.28853713415517, -1.40615965110027, 3.61026403934783e-07,
                   5.19878021666088e-07, "", "", "FALSE", "Beer,Neutral", "Beer,Positive",
                   -11.05, -18.7654734038749, -3.33452659612508, 2.34325584459239,
                   -4.71566091491908, -1.05445383645979, 0.000146504830277281,
                   0.000293009660554563, "", "", "FALSE", "Beer,Neutral", "Water,Negative",
                   19.2, 8.20278675244404, 30.197213247556, 3.32350744762006, 5.77702932898466,
                   1.29178302876197, 2.85707165791565e-06, 4.67520816749833e-06,
                   "", "", "FALSE", "Beer,Neutral", "Water,Neutral", 7.64999999999998,
                   -2.56697520773791, 17.8669752077379, 3.06258786722021, 2.49788751594042,
                   0.558544628579088, 0.149550399983076, 0.538381439939075, "",
                   "", "FALSE", "Beer,Neutral", "Water,Positive", -7.40000000000002,
                   -18.397213247556, 3.59721324755594, 3.32350744762006, -2.22656338721284,
                   -0.49787470900201, 0.229581612565896, 1, "", "", "FALSE", "Beer,Neutral",
                   "Wine,Negative", 22, 11.002786752444, 32.997213247556, 3.32350744762006,
                   6.61951277279492, 1.48016805378976, 9.12810921255052e-08, 1.21708122834007e-07,
                   "", "", "FALSE", "Beer,Neutral", "Wine,Neutral", -1.65000000000002,
                   -11.8669752077379, 8.56697520773787, 3.06258786722021, -0.538760052457745,
                   -0.120470410085687, 1, 1, "", "", "FALSE", "Beer,Neutral", "Wine,Positive",
                   -15.35, -26.347213247556, -4.35278675244406, 3.32350744762006,
                   -4.61861459374555, -1.03275361934876, 0.000238685519515609,
                   0.000505451688385996, "", "", "FALSE", "Beer,Positive", "Water,Negative",
                   30.25, 19.2527867524441, 41.247213247556, 3.32350744762006,
                   9.10183006259302, 2.03523107396092, 1.31342073601263e-12, 1.52526279020822e-12,
                   "", "", "FALSE", "Beer,Positive", "Water,Neutral", 18.7, 7.70278675244403,
                   29.697213247556, 3.32350744762006, 5.62658585687568, 1.25814284572129,
                   5.13665053226808e-06, 8.80568662674528e-06, "", "", "FALSE",
                   "Beer,Positive", "Water,Positive", 3.64999999999999, -6.5669752077379,
                   13.8669752077379, 3.06258786722021, 1.1918025402853, 0.266495149583486,
                   0.950172562866207, 1, "", "", "FALSE", "Beer,Positive", "Wine,Negative",
                   33.05, 22.052786752444, 44.047213247556, 3.32350744762006, 9.94431350640328,
                   2.2236160989887, 2.84418933680612e-14, 3.19971300390688e-14,
                   "", "", "FALSE", "Beer,Positive", "Wine,Neutral", 9.4, -1.59721324755597,
                   20.397213247556, 3.32350744762006, 2.82833727564874, 0.632435441164714,
                   0.0704362311055042, 0.211308693316513, "", "", "FALSE", "Beer,Positive",
                   "Wine,Positive", -4.30000000000001, -14.5169752077379, 5.91697520773788,
                   3.06258786722021, -1.40404134882926, -0.313953189920273, 0.824625255450165,
                   1, "", "", "FALSE", "Water,Negative", "Water,Neutral", -11.55,
                   -19.2654734038749, -3.83452659612509, 2.34325584459239, -4.92903923686112,
                   -1.10216667973852, 6.53584869825456e-05, 0.000123837133230086,
                   "", "", "FALSE", "Water,Negative", "Water,Positive", -26.6,
                   -34.315473403875, -18.8845265961251, 2.34325584459239, -11.3517267273165,
                   -2.53832326242809, 6.48463692189467e-18, 6.66991226252023e-18,
                   "", "", "FALSE", "Water,Negative", "Wine,Negative", 2.79999999999999,
                   -7.41697520773789, 13.0169752077379, 3.06258786722021, 0.914259482958587,
                   0.204434635296921, 1, 1, "", "", "FALSE", "Water,Negative",
                   "Wine,Neutral", -20.85, -31.847213247556, -9.85278675244406,
                   3.32350744762006, -6.27349278694428, -1.4027956327962, 3.70036622224379e-07,
                   5.55054933336568e-07, "", "", "FALSE", "Water,Negative", "Wine,Positive",
                   -34.55, -45.547213247556, -23.5527867524441, 3.32350744762005,
                   -10.3956439227302, -2.32453664811073, 3.74858090018894e-15,
                   4.0893609820243e-15, "", "", "FALSE", "Water,Neutral", "Water,Positive",
                   -15.05, -22.7654734038749, -7.33452659612507, 2.34325584459239,
                   -6.42268749045539, -1.43615658268958, 1.32038138503692e-07,
                   1.82822037928189e-07, "", "", "FALSE", "Water,Neutral", "Wine,Negative",
                   14.35, 3.35278675244405, 25.347213247556, 3.32350744762006,
                   4.3177276495276, 0.96547325326741, 0.000652922798550143, 0.00156701471652034,
                   "", "", "FALSE", "Water,Neutral", "Wine,Neutral", -9.3, -19.5169752077379,
                   0.916975207737885, 3.06258786722021, -3.03664756839817, -0.679015038664776,
                   0.0442824831484033, 0.122628414872502, "", "", "FALSE", "Water,Neutral",
                   "Wine,Positive", -23, -33.997213247556, -12.002786752444, 3.32350744762006,
                   -6.92039971701287, -1.54744841987111, 2.48612142458194e-08,
                   3.19644183160535e-08, "", "", "FALSE", "Water,Positive", "Wine,Negative",
                   29.4, 18.4027867524441, 40.397213247556, 3.32350744762005, 8.84607616000776,
                   1.97804276279177, 4.12026475014241e-12, 4.94431770017089e-12,
                   "", "", "FALSE", "Water,Positive", "Wine,Neutral", 5.75000000000001,
                   -5.24721324755595, 16.747213247556, 3.32350744762006, 1.73009992925322,
                   0.386862104967778, 0.524074945350722, 1, "", "", "FALSE", "Water,Positive",
                   "Wine,Positive", -7.95, -18.1669752077379, 2.26697520773789,
                   3.06258786722021, -2.59584388911457, -0.58044833950376, 0.127432462916826,
                   0.417051696818704, "", "", "FALSE", "Wine,Negative", "Wine,Neutral",
                   -23.65, -31.3654734038749, -15.9345265961251, 2.34325584459239,
                   -10.0927946278585, -2.25681748708362, 3.10543488939126e-15,
                   3.28810752994369e-15, "", "", "FALSE", "Wine,Negative", "Wine,Positive",
                   -37.35, -45.0654734038749, -29.6345265961251, 2.34325584459239,
                   -15.9393606490704, -3.56414939292065, 3.3906654401086e-27, 3.3906654401086e-27,
                   "", "", "FALSE", "Wine,Neutral", "Wine,Positive", -13.7, -21.4154734038749,
                   -5.98452659612508, 2.34325584459239, -5.84656602121189, -1.30733190583703,
                   1.57765790036778e-06, 2.46937758318435e-06, "", "", "FALSE")
  
  table <- results[["results"]][["posthoc"]][["collection"]][[3]][["data"]]
  expect_equal_tables(table, refTable)
  
})

test_that("Descriptives Match", {
  options <- initOpts()
  
  options$descriptives <- TRUE
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures", dataset = "AnovaRepeatedMeasures.csv",
                            options = options)
  
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

test_that("Field - Chapter 8 marginal means match", {
  # compared to SPSS, we pool the standard errors in marginal means
  options <- initOpts()
  
  options$marginalMeansTerms <- options$withinModelTerms[3]
  options$marginalMeansBootstrapping <- TRUE
  options$marginalMeansBootstrappingReplicates <- 500
  set.seed(1)
  results <- jasptools::run("AnovaRepeatedMeasures", dataset = "AnovaRepeatedMeasures.csv", options = options)
  
  table <- results$results$marginalMeans$collection[[1]]$data
  refTable <- list("Beer", "Positive", 21.05, 2.1606224736599, 16.7706374246617,
                   25.3293625753383, "TRUE", "Beer", "Neutral", 10, 2.1606224736599,
                   5.72063742466172, 14.2793625753383, "FALSE", "Beer", "Negative",
                   4.45000000000001, 2.1606224736599, 0.170637424661735, 8.72936257533829,
                   "FALSE", "Wine", "Positive", 25.35, 2.1606224736599, 21.0706374246617,
                   29.6293625753383, "TRUE", "Wine", "Neutral", 11.65, 2.1606224736599,
                   7.37063742466174, 15.9293625753383, "FALSE", "Wine", "Negative",
                   -12, 2.1606224736599, -16.2793625753383, -7.72063742466172,
                   "FALSE", "Water", "Positive", 17.4, 2.1606224736599, 13.1206374246617,
                   21.6793625753383, "TRUE", "Water", "Neutral", 2.35000000000002,
                   2.1606224736599, -1.92936257533826, 6.62936257533829, "FALSE",
                   "Water", "Negative", -9.2, 2.1606224736599, -13.4793625753383,
                   -4.92063742466173, "FALSE")
  
  expect_equal_tables(table, refTable)
  
  table <- results$results$marginalMeansBoots$collection[[1]]$data
  refTable <- list("Beer", "Positive", 21.175, -0.0460999999999991, 2.86790158635509,
                   15.2090610346416, 26.25, "TRUE", "Beer", "Neutral", 10.15, 0.111500000000007,
                   2.34101761568637, 4.70000000000001, 14.65, "FALSE", "Beer",
                   "Negative", 4.70000000000001, 0.156300000000011, 3.86263448961985,
                   -3.11760706046903, 11.5528285326562, "FALSE", "Wine", "Positive",
                   25.175, -0.0318999999999932, 1.41905772950904, 22.75, 28.4027564717744,
                   "TRUE", "Wine", "Neutral", 11.625, -0.0412999999999979, 1.39196783688339,
                   8.80000000000002, 14.2593188494378, "FALSE", "Wine", "Negative",
                   -12, 0.0551000000000048, 1.35802704867387, -14.6, -9.21860910945718,
                   "FALSE", "Water", "Positive", 17.25, -0.0897000000000112, 1.58643387977874,
                   14.75, 21.6001985182309, "TRUE", "Water", "Neutral", 2.35000000000001,
                   -0.0161000000000051, 1.52816109840729, -0.999999999999996, 5.19101856310838,
                   "FALSE", "Water", "Negative", -9.14999999999998, 0.0620000000000118,
                   1.4412391684524, -12.1435538334573, -6.44999999999998, "FALSE")
  
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
                            dataset = "AnovaMixedEffects.csv", options = options)
  
  refTable <- list("gender", 0.200000000000001, 1, 0.200000000000001, 0.00473545746857648,
                0.945895847556855, "TRUE", "Residual", 760.222222222222, 18,
                42.2345679012346, "", "", "", "", "", "", "TRUE")
  
  table <- results[["results"]][["betweenSubjectsEffects"]][["data"]]
  expect_equal_tables(table, refTable)
})

test_that("Homogeneity tests correct", {
  options <- initOpts()

  options$homogeneityTests <- TRUE
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv", options = options)
  
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
                            dataset = "AnovaMixedEffects.csv", options = options)
  
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
                            options = options)
  
  refTable <- list("Animal", 83.1249999999999, 3, 27.7083333333333, 3.79380603096984,
                0.0255702968630395, "TRUE", 1, 1, 1, 1, 1, 0.327424913835549,
                0.351479915433403, 0.327424913835549, 0.238785176929506, "Residual", 153.375, 21,
                7.30357142857143, "", "", "", "", "", "", "TRUE")

  table <- results[["results"]][["withinSubjectsEffects"]][["data"]]
  expect_equal_tables(table, refTable)
})

test_that("Simple Effects table match", {
  skip("This test fails after updating our R packages, but this is fixed in Johnny's rewrite")
  
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
                            options = options)
  
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
                            dataset = "AnovaMixedEffects.csv",
                            options = options)
  
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
                            options = options)
  
  refTable <- list( "High", "Some", 1.31809226918034, 406, 306.5, 0.189380510753162, 0.568141532259486, 
                    0.233862092301299, 158, "High", "None", 2.89450412880305, 406, 187.5,
                    0.00433496034546402, 0.0130048810363921, 0.0130048810363921, 158, "Some",
                    "None", 1.57641185962271, 306.5, 187.5, 0.116931046150649, 0.350793138451948,
                    0.233862092301299, 158)
  
  table <- results[["results"]][["conover"]][[1]][[1]][["data"]]
  expect_equal_tables(table, refTable)
})


### Andy Field tests ---

test_that("Field - Chapter 8 results match", {
  options <- jasptools::analysisOptions("anovarepeatedmeasures")
  
  options$repeatedMeasuresFactors <- list(
    list(name = "Animal", levels = c("Stick", "Kangaroo", "Fish", "Grub"))
  )
  options$repeatedMeasuresCells <- c("Stick Insect", "Kangaroo Testicle",
                                     "Fish Eye", "Witchetty Grub")
  
  options$withinModelTerms <- list(
    list(components = "Animal")
  )
  
  options$sphericityTests <- TRUE
  options$sphericityCorrections <- TRUE
  
  options$postHocTestsVariables <- "Animal"
  options$postHocTestPooledError <- FALSE
  options$postHocTestsBonferroni <- TRUE
  options$confidenceIntervalsPostHoc <- TRUE
  results <- jasptools::run(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaRepeatedMeasuresOneWay.csv",
                            options = options)
  
  # output 2 (chi square and df for sphericity)
  table <- results$results$assumptionsObj$sphericity$data
  refTable <- list("Animal", 0.136248029372535, 11.4059814340564, 5, 0.0468458123067897,
                   0.532845552798473, 0.66576361409737, 0.333333333333333, "TRUE")
  
  expect_equal_tables(table, refTable)
  
  # sphericity corrections
  table <- results$results$withinSubjectsEffects$data
  refTable <- list("Animal", "None", 83.1249999999999, 3, 27.7083333333333, 3.79380603096984,
                   0.0255702968630395, "TRUE", "FALSE", 1, 1, 1, 1, 1, "Animal",
                   "Greenhouse-Geisser", 83.1249999999999, 1.59853665839542, 52.0006842279357,
                   3.79380603096984, 0.0625841206869634, "FALSE", "FALSE", 1, 1,
                   1, 1, 1, "Animal", "Huynh-Feldt", 83.1249999999999, 1.99729084229211,
                   41.6188760494215, 3.79380603096984, 0.0483306135519432, "FALSE",
                   "FALSE", 1, 1, 1, 1, 1, "Residual", "None", 153.375, 21, 7.30357142857143,
                   "", "", "", "", "", "", "TRUE", "Residual", "Greenhouse-Geisser",
                   153.375, 11.1897566087679, 13.7067324484806, "", "", "", "", "",
                   "", "FALSE", "Residual", "Huynh-Feldt", 153.375, 13.9810358960448,
                   10.9702171670548, "", "", "", "", "", "", "FALSE")
  
  expect_equal_tables(table, refTable)
  
  # post hoc tests (bonferroni, confidence intervals, se not pooled)
  table <- results$results$posthoc$collection[[1]]$data
  refTable <-  list("Fish", "Grub", -1.625, -8.24895462968414, 4.99895462968414, 1.82186619392628,
                    -0.891942561653216, -0.315349316886945, 0.906918466006362, 1,
                    "", "", "TRUE", "Fish", "Kangaroo", -0.125, -4.49432153408686,
                    4.24432153408686, 1.20174723988509, -0.104015217053423, 0.0367749326625332,
                    0.920074728368496, 1, "", "", "FALSE", "Fish", "Stick", -4,
                    -6.66113854623375, -1.33886145376625, 0.7319250547114, -5.46504040851179,
                    1.93218356615859, 0.00564485983507568, 0.00564485983507568,
                    "", "", "FALSE", "Grub", "Kangaroo", 1.5, -3.3585520347291,
                    6.3585520347291, 1.33630620956212, 1.12249721603218, -0.396862696659689,
                    0.906918466006362, 1, "", "", "FALSE", "Grub", "Stick", -2.375,
                    -8.89116783575007, 4.14116783575007, 1.79222029098785, -1.32517191772833,
                    0.468519024631843, 0.906918466006362, 1, "", "", "FALSE", "Kangaroo",
                    "Stick", -3.875, -6.82534547190668, -0.924654528093315, 0.811469126250126,
                    -4.77528950227193, 1.68831979459271, 0.0101164143794359, 0.0121396972553231,
                    "", "", "FALSE")
  
  expect_equal_tables(table, refTable)
})

test_that("Field - Chapter 9 match", {
  options <- initOpts()
  
  options$sphericityTests <- TRUE
  
  options$marginalMeansTerms <- list(
    list(components = "Charisma"),
    list(components = "Looks"),
    list(components = c("Looks", "Charisma")),
    list(components = "gender")
  )
  
  results <- jasptools::run(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv",
                            options = options)
  
  # sphericity
  table <- results$results$assumptionsObj$sphericity$data
  refTable <- list("Looks", 0.960205401636332, 0.690336975266099, 2, 0.708101037148711,
                   0.961728404411513, 1, 0.5, "TRUE", "Charisma", 0.929329811889938,
                   1.24595694485668, 2, 0.536344568684782, 0.933994437414187, 1,
                   0.5, "FALSE", "Looks <unicode> Charisma", 0.613354486153378,
                   8.02466743180245, 9, 0.533938195513754, 0.799354278085918, 0.992241110561882,
                   0.25, "FALSE")
  
  expect_equal_tables(table, refTable)
  
  # marginal charisma
  table <- results$results$marginalMeans$collection[[1]]$data
  refTable <- list("High", 82.1000000000002, 0.792376225226709, 80.5111143833479,
                   83.6888856166524, "TRUE", "Some", 69.3000000000002, 0.792376225226709,
                   67.7111143833479, 70.8888856166524, "FALSE", "None", 54.3000000000002,
                   0.792376225226709, 52.7111143833479, 55.8888856166524, "FALSE")
  
  expect_equal_tables(table, refTable)
  
  # gender (strategy) * looks interaction
  table <- results$results$marginalMeans$collection[[6]]$data
  refTable <- list("Female", "Attractive", 76.1666666666668, 1.00705331467645, 74.1441569164011,
                   78.1891764169326, "TRUE", "Female", "Average", 68.1000000000002,
                   1.00705331467645, 66.0774902497344, 70.1225097502659, "FALSE",
                   "Female", "Ugly", 61.3333333333335, 1.00705331467645, 59.3108235830677,
                   63.3558430835992, "FALSE", "Male", "Attractive", 88.0333333333335,
                   1.00705331467645, 86.0108235830677, 90.0558430835993, "TRUE",
                   "Male", "Average", 67.4666666666668, 1.00705331467645, 65.4441569164011,
                   69.4891764169326, "FALSE", "Male", "Ugly", 50.3000000000002,
                   1.00705331467645, 48.2774902497344, 52.3225097502659, "FALSE")
  
  expect_equal_tables(table, refTable)
  
  # gender (strategy) * charisma interaction
  table <- results$results$marginalMeans$collection[[5]]$data
  refTable <- list("Female", "High", 88.2333333333335, 1.12058920421761, 85.9863097452043,
                   90.4803569214627, "TRUE", "Female", "Some", 69.0666666666668,
                   1.12058920421761, 66.8196430785377, 71.313690254796, "FALSE",
                   "Female", "None", 48.3000000000002, 1.12058920421761, 46.052976411871,
                   50.5470235881293, "FALSE", "Male", "High", 75.9666666666669,
                   1.12058920421761, 73.7196430785377, 78.213690254796, "TRUE",
                   "Male", "Some", 69.5333333333335, 1.12058920421761, 67.2863097452043,
                   71.7803569214627, "FALSE", "Male", "None", 60.3000000000002,
                   1.12058920421761, 58.052976411871, 62.5470235881293, "FALSE")
  
  expect_equal_tables(table, refTable)
  
  # looks * charisma interaction
  table <- results$results$marginalMeans$collection[[3]]$data
  refTable <- list("Attractive", "High", 88.9500000000002, 1.23097873335623, 86.5185279595704,
                   91.38147204043, "TRUE", "Attractive", "Some", 87.8000000000002,
                   1.23097873335623, 85.3685279595704, 90.2314720404299, "FALSE",
                   "Attractive", "None", 69.5500000000002, 1.23097873335623, 67.1185279595704,
                   71.9814720404299, "FALSE", "Average", "High", 85.6000000000002,
                   1.23097873335623, 83.1685279595704, 88.0314720404299, "TRUE",
                   "Average", "Some", 70.3500000000002, 1.23097873335623, 67.9185279595704,
                   72.7814720404299, "FALSE", "Average", "None", 47.4000000000001,
                   1.23097873335623, 44.9685279595704, 49.8314720404299, "FALSE",
                   "Ugly", "High", 71.7500000000002, 1.23097873335623, 69.3185279595704,
                   74.1814720404299, "TRUE", "Ugly", "Some", 49.7500000000002,
                   1.23097873335623, 47.3185279595704, 52.1814720404299, "FALSE",
                   "Ugly", "None", 45.9500000000002, 1.23097873335623, 43.5185279595704,
                   48.3814720404299, "FALSE")
  expect_equal_tables(table, refTable)
  
  # gender (strategy) * looks * charisma interaction
  table <- results$results$marginalMeans$collection[[7]]$data
  refTable <- list("Female", "Attractive", "High", 89.6000000000002, 1.74086681970523,
                  86.1613792638934, 93.0386207361069, "TRUE", "Female", "Attractive",
                  "Some", 87.1000000000002, 1.74086681970523, 83.6613792638934,
                  90.5386207361069, "FALSE", "Female", "Attractive", "None", 51.8000000000002,
                  1.74086681970523, 48.3613792638934, 55.2386207361069, "FALSE",
                  "Female", "Average", "High", 88.4000000000002, 1.74086681970523,
                  84.9613792638934, 91.8386207361069, "TRUE", "Female", "Average",
                  "Some", 68.9000000000002, 1.74086681970523, 65.4613792638934,
                  72.3386207361069, "FALSE", "Female", "Average", "None", 47.0000000000001,
                  1.74086681970523, 43.5613792638934, 50.4386207361069, "FALSE",
                  "Female", "Ugly", "High", 86.7000000000001, 1.74086681970523,
                  83.2613792638934, 90.1386207361069, "TRUE", "Female", "Ugly",
                  "Some", 51.2000000000001, 1.74086681970523, 47.7613792638934,
                  54.6386207361069, "FALSE", "Female", "Ugly", "None", 46.1000000000002,
                  1.74086681970523, 42.6613792638934, 49.5386207361069, "FALSE",
                  "Male", "Attractive", "High", 88.3000000000002, 1.74086681970523,
                  84.8613792638934, 91.738620736107, "TRUE", "Male", "Attractive",
                  "Some", 88.5000000000002, 1.74086681970524, 85.0613792638934,
                  91.9386207361069, "FALSE", "Male", "Attractive", "None", 87.3000000000002,
                  1.74086681970524, 83.8613792638934, 90.738620736107, "FALSE",
                  "Male", "Average", "High", 82.8000000000002, 1.74086681970523,
                  79.3613792638934, 86.238620736107, "TRUE", "Male", "Average",
                  "Some", 71.8000000000002, 1.74086681970523, 68.3613792638934,
                  75.2386207361069, "FALSE", "Male", "Average", "None", 47.8000000000001,
                  1.74086681970523, 44.3613792638934, 51.2386207361069, "FALSE",
                  "Male", "Ugly", "High", 56.8000000000002, 1.74086681970523,
                  53.3613792638934, 60.2386207361069, "TRUE", "Male", "Ugly",
                  "Some", 48.3000000000002, 1.74086681970523, 44.8613792638934,
                  51.7386207361069, "FALSE", "Male", "Ugly", "None", 45.8000000000002,
                  1.74086681970523, 42.3613792638934, 49.2386207361069, "FALSE")
  
  expect_equal_tables(table, refTable)
})