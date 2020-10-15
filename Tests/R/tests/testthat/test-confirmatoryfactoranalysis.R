context("Confirmatory Factor Analysis")

# 3-factor run
options <- jaspTools::analysisOptions("ConfirmatoryFactorAnalysis")
options$groupvar <- ""
options$invariance <- "configural"
options$mimic <- "lavaan"
options$se <- "standard"
options$estimator <- "default"
options$std <- "none"
options$factors <- list(
  list(indicators = list("x1", "x2", "x3"), name = "Factor1", title = "Factor 1"), 
  list(indicators = list("x4", "x5", "x6"), name = "Factor2", title = "Factor 2"), 
  list(indicators = list("x7", "x8", "x9"), name = "Factor3", title = "Factor 3")
)
options$identify <- "factor"
options$missing <- "FIML"
set.seed(1)
results <- jaspTools::run("ConfirmatoryFactorAnalysis", "holzingerswineford.csv", options)


test_that("[CFA 3-Factor] Factor Covariances table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fc"]][["data"]]
  expect_equal_tables(table,
                      list(0.333504675491998, 0.583514923484581, 0.45850979948829, "Factor 1",
                           "<unicode><unicode><unicode>", 6.52589093874667e-13, "Factor 2",
                           0.063779296447443, 7.18900685689003, 0.327797106505315, 0.613272259288624,
                           0.47053468289697, "Factor 1", "<unicode><unicode><unicode>",
                           1.03996145028873e-10, "Factor 3", 0.0728266322838328, 6.46102487704112,
                           0.148278758433621, 0.417692356258714, 0.282985557346168, "Factor 2",
                           "<unicode><unicode><unicode>", 3.83174073319559e-05, "Factor 3",
                           0.0687292215444246, 4.11739797115633))
})

test_that("[CFA 3-Factor] Factor loadings table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fl1"]][["data"]]
  expect_equal_tables(table,
                      list(0.741164020131046, 1.05807660760393, 0.899620313867488, "<unicode><unicode>11",
                           "Factor 1", 0, "x1", 0.0808465333987386, 11.1275063512064, 0.346131928251774,
                           0.649749093967046, 0.49794051110941, "<unicode><unicode>12",
                           "Factor 1", 1.28623778294923e-10, "x2", 0.0774547818506273,
                           6.42878979466621, 0.510293170576109, 0.802019014680793, 0.656156092628451,
                           "<unicode><unicode>13", "Factor 1", 0, "x3", 0.0744212256974568,
                           8.81678696472846, 0.878687521771487, 1.1006993764173, 0.989693449094392,
                           "<unicode><unicode>21", "Factor 2", 0, "x4", 0.0566367179185465,
                           17.4744138690689, 0.978762425338572, 1.22444687472433, 1.10160465003145,
                           "<unicode><unicode>22", "Factor 2", 0, "x5", 0.0626757561168699,
                           17.5762482701815, 0.811432261987188, 1.02176969353156, 0.916600977759373,
                           "<unicode><unicode>23", "Factor 2", 0, "x6", 0.0536584940344529,
                           17.0821226769958, 0.483096088879332, 0.75585477823652, 0.619475433557926,
                           "<unicode><unicode>31", "Factor 3", 0, "x7", 0.0695825769015842,
                           8.90273774186456, 0.601768916407813, 0.860128689422337, 0.730948802915075,
                           "<unicode><unicode>32", "Factor 3", 0, "x8", 0.0659093164600047,
                           11.0902197469857, 0.54254918241612, 0.797411035146398, 0.669980108781259,
                           "<unicode><unicode>33", "Factor 3", 0, "x9", 0.0650169734598685,
                           10.3046954222623))
})

test_that("[CFA 3-Factor] Factor variances table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fv"]][["data"]]
  expect_equal_tables(table,
                      list(1, 1, 1, "Factor 1", "", 0, "", 1, 1, 1, "Factor 2", "", 0, "",
                           1, 1, 1, "Factor 3", "", 0, ""))
})

test_that("[CFA 3-Factor] Residual variances table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_rv"]][["data"]]
  expect_equal_tables(table,
                      list(0.326399894319287, 0.771706936936134, 0.549053415627711, "x1",
                           1.34368055770828e-06, 0.113600822803218, 4.83318168019605, 0.934462587447057,
                           1.33321007058646, 1.13383632901676, "x2", 0, 0.10172316590628,
                           11.1462941495686, 0.666706476085054, 1.02194282083783, 0.844324648461441,
                           "x3", 0, 0.0906231817407956, 9.31687270566615, 0.277647747273574,
                           0.464697982595281, 0.371172864934427, "x4", 7.32747196252603e-15,
                           0.047717773591029, 7.77850341710428, 0.331807290171594, 0.560702710555418,
                           0.446255000363506, "x5", 2.1316282072803e-14, 0.0583927618541264,
                           7.64229993913142, 0.271855645519897, 0.44054959934542, 0.356202622432658,
                           "x6", 2.22044604925031e-16, 0.0430349626718039, 8.27705196700539,
                           0.639885213665674, 0.958894859526553, 0.799390036596114, "x7",
                           0, 0.0813815071034943, 9.82274800563124, 0.342279857209668,
                           0.63311496350216, 0.487697410355914, "x8", 4.92208496183366e-11,
                           0.0741939924882706, 6.57327357646934, 0.427489578257306, 0.704773115754749,
                           0.566131347006028, "x9", 1.11022302462516e-15, 0.0707368961074339,
                           8.00333882541577))
})

test_that("[CFA 3-Factor] Chi-square test table results match", {
  table <- results[["results"]][["maincontainer"]][["collection"]][["maincontainer_cfatab"]][["data"]]
  expect_equal_tables(table,
                      list(918.851589292384, 36, "Baseline model", "", 85.305521772505, 24,
                           "Factor model", 8.50255310602677e-09))
})


# Second-order factor 
options <- jaspTools::analysisOptions("ConfirmatoryFactorAnalysis")
options$secondOrder <- list("Factor 1", "Factor 2", "Factor 3")
options$groupvar <- ""
options$invariance <- "configural"
options$mimic <- "lavaan"
options$se <- "standard"
options$estimator <- "default"
options$std <- "none"
options$factors <- list(
  list(indicators = list("x1", "x2", "x3"), name = "Factor1", title = "Factor 1"), 
  list(indicators = list("x4", "x5", "x6"), name = "Factor2", title = "Factor 2"), 
  list(indicators = list("x7", "x8", "x9"), name = "Factor3", title = "Factor 3")
)
options$identify <- "factor"
options$missing <- "FIML"
set.seed(1)
results <- jaspTools::run("ConfirmatoryFactorAnalysis", "holzingerswineford.csv", options)


test_that("[CFA Second order] Factor loadings table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fl1"]][["data"]]
  expect_equal_tables(table,
                      list(0.0577039904414908, 0.819337251278242, 0.438520620859867, "<unicode><unicode>11",
                           "Factor 1", 0.0240111123688183, "x1", 0.194297769460158, 2.25695139001474,
                           0.0315641080012427, 0.453878987089273, 0.242721547545258, "<unicode><unicode>12",
                           "Factor 1", 0.0242627980684818, "x2", 0.107735367185111, 2.25294212928438,
                           0.0503061064249717, 0.589382650102084, 0.319844378263528, "<unicode><unicode>13",
                           "Factor 1", 0.0200309356495567, "x3", 0.137522053448247, 2.32576790590095,
                           0.71768085442071, 0.966830831063184, 0.842255842741947, "<unicode><unicode>21",
                           "Factor 2", 0, "x4", 0.0635598354377267, 13.2513848870354, 0.799268757944165,
                           1.07572151104007, 0.937495134492118, "<unicode><unicode>22",
                           "Factor 2", 0, "x5", 0.0705249574167, 13.2930974910468, 0.663204523777401,
                           0.896899759208491, 0.780052141492946, "<unicode><unicode>23",
                           "Factor 2", 0, "x6", 0.0596172269680586, 13.0843412410121, 0.392491072303089,
                           0.651164920657023, 0.521827996480056, "<unicode><unicode>31",
                           "Factor 3", 2.66453525910038e-15, "x7", 0.0659894391923322,
                           7.90775013194371, 0.483529756039699, 0.747933394180798, 0.615731575110249,
                           "<unicode><unicode>32", "Factor 3", 0, "x8", 0.0674511471197128,
                           9.12855602021777, 0.438789772679437, 0.689958088778821, 0.564373930729129,
                           "<unicode><unicode>33", "Factor 3", 0, "x9", 0.0640747274135055,
                           8.80805823935775))
})

test_that("[CFA Second order] Second-order factor loadings table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fl2"]][["data"]]
  expect_equal_tables(table,
                      list(-0.149104054075191, 3.73161619230762, 1.79125606911621, "<unicode><unicode>11",
                           "SecondOrder", 0.0703961021446211, "Factor 1", 0.989997846132234,
                           1.80935350123676, 0.364985992598308, 0.869100374746346, 0.617043183672327,
                           "<unicode><unicode>12", "SecondOrder", 1.6021965671964e-06,
                           "Factor 2", 0.128602970800593, 4.79804766430388, 0.360410276598517,
                           0.919053360468363, 0.63973181853344, "<unicode><unicode>13",
                           "SecondOrder", 7.15860527811252e-06, "Factor 3", 0.142513609504142,
                           4.48891737960541))
})

test_that("[CFA Second order] Factor variances table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_fv"]][["data"]]
  expect_equal_tables(table,
                      list(1, 1, 1, "Factor 1", "", 0, "", 1, 1, 1, "Factor 2", "", 0, "",
                           1, 1, 1, "Factor 3", "", 0, "", 1, 1, 1, "Second-Order", "",
                           0, ""))
})

test_that("[CFA Second order] Residual variances table results match", {
  table <- results[["results"]][["estimates"]][["collection"]][["estimates_rv"]][["data"]]
  expect_equal_tables(table,
                      list(0.326401450955574, 0.771708269797423, 0.549054860376498, "x1",
                           1.34357828596165e-06, 0.11360076571671, 4.83319682673349, 0.934464040890642,
                           1.33321206655867, 1.13383805372466, "x2", 0, 0.101723304308982,
                           11.1462959390372, 0.666705522978852, 1.02194196604298, 0.844323744510915,
                           "x3", 0, 0.0906232068206831, 9.3168601524065, 0.2776477911548,
                           0.464698082724097, 0.371172936939448, "x4", 7.32747196252603e-15,
                           0.0477177879401676, 7.77850258701964, 0.331807404885689, 0.560702857278213,
                           0.446255131081951, "x5", 2.1316282072803e-14, 0.0583927700197611,
                           7.64230110904024, 0.271855604188449, 0.44054957527582, 0.356202589732134,
                           "x6", 2.22044604925031e-16, 0.0430349670754176, 8.27705036018498,
                           0.63988809304374, 0.958898264282657, 0.799393178663198, "x7",
                           0, 0.0813816411309667, 9.82277043758238, 0.342280111000572,
                           0.633115637435664, 0.487697874218118, "x8", 4.92219598413612e-11,
                           0.0741940996694749, 6.57327033269154, 0.427488355941731, 0.704772286642682,
                           0.566130321292206, "x9", 1.11022302462516e-15, 0.0707369964162943,
                           8.00331297586447))
})

test_that("[CFA Second order] Chi-square test table results match", {
  table <- results[["results"]][["maincontainer"]][["collection"]][["maincontainer_cfatab"]][["data"]]
  expect_equal_tables(table,
                      list(918.851589292384, 36, "Baseline model", "", 85.3055217707089,
                           24, "Factor model", 8.50255321704907e-09))
})
