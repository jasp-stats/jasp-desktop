context("Robust Bayesian Meta-Analysis")

skip("rjags needs to work for this to be testable")

# path to the pre-fitted RoBMA model
fitted_path <- file.path(jasptools:::.pkgOptions$tests.dir, "RoBMA_testfit.RDS")

### prior distibutions plots 
{
  options <- jasptools::analysisOptions("RobustBayesianMetaAnalysis")
  options$.meta <- list(input_CI = list(containsColumn = TRUE), input_ES = list(
    containsColumn = TRUE), input_N = list(containsColumn = TRUE), 
    input_N1 = list(containsColumn = TRUE), input_N2 = list(containsColumn = TRUE), 
    input_SE = list(containsColumn = TRUE), input_labels = list(
      containsColumn = TRUE), input_t = list(containsColumn = TRUE), 
    priors_mu = list(list(name = list(containsColumn = TRUE)), 
                     list(name = list(containsColumn = TRUE)), list(name = list(
                       containsColumn = TRUE)), list(name = list(containsColumn = TRUE)), 
                     list(name = list(containsColumn = TRUE)), list(name = list(
                       containsColumn = TRUE)), list(name = list(containsColumn = TRUE)), 
                     list(name = list(containsColumn = TRUE))), priors_omega = list(
                       list(name = list(containsColumn = TRUE)), list(name = list(
                         containsColumn = TRUE))), priors_tau = list(list(
                           name = list(containsColumn = TRUE))))
  options$advanced_control <- "clever"
  options$fitted_path <- ""
  options$input_CI <- list()
  options$plots_theta_order <- "labels"
  options$plots_theta_show <- "observed"
  options$plots_type_individual_by <- "model"
  options$plots_type_individual_order <- "ascending"
  options$priors_mu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "2", truncationUpper = "7", type = "normal"), 
                            list(name = "2", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "4", truncationUpper = "7", type = "t"), 
                            list(name = "3", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-3", truncationUpper = "Inf", type = "cauchy"), 
                            list(name = "4", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "gamma_k0"), 
                            list(name = "5", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "invgamma"), 
                            list(name = "6", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"), 
                            list(name = "7", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "uniform"), 
                            list(name = "8", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "0", truncationUpper = "Inf", type = "gamma_ab"))
  options$priors_mu_null <- list()
  options$priors_null <- TRUE
  options$priors_omega <- list(list(name = "", parAlpha = "(1,1)", parAlpha1 = "(1,1,1)", 
                                    parAlpha2 = "(1,1)", parCuts = "(.05)", priorOdds = "1/2", 
                                    type = "Two-sided"), list(name = "2", parAlpha = "(1,1,1)", 
                                                              parAlpha1 = "(1,1,1)", parAlpha2 = "(1,1)", parCuts = "(.05, .10)", 
                                                              priorOdds = "1/2", type = "Two-sided"))
  options$priors_omega_null <- list()
  options$priors_plot <- TRUE
  options$priors_tau <- list(list(`	` = "1", name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale2 = "1", parShape = "1", priorOdds = "1", truncationLower = "0", 
                                  truncationUpper = "Inf", type = "invgamma"))
  options$priors_tau_null <- list()
  options$results_models_BF <- "inclusion"
  options$results_models_order <- "default"
  options$save_path <- ""
  set.seed(1)
  dataset <- NULL
  results <- jasptools::run("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Models Overview table results match", {
    table <- results[["results"]][["model_preview"]][["collection"]][["model_preview_models_summary"]][["data"]]
    expect_equal_tables(table,
                        list(1, 0.0625, "Normal(0, 1)[2, 7]", "Two-sided((0.05), (1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]", 2, 0.0625, "Normal(0, 1)[2, 7]",
                             "Two-sided((0.1, 0.05), (1, 1, 1))", "InvGamma(1, 0.15)[0, Inf]",
                             3, 0.0625, "gen. Student-t(0, 1, 2)[4, 7]", "Two-sided((0.05), (1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]", 4, 0.0625, "gen. Student-t(0, 1, 2)[4, 7]",
                             "Two-sided((0.1, 0.05), (1, 1, 1))", "InvGamma(1, 0.15)[0, Inf]",
                             5, 0.0625, "Cauchy(0, 1)[-3, Inf]", "Two-sided((0.05), (1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]", 6, 0.0625, "Cauchy(0, 1)[-3, Inf]",
                             "Two-sided((0.1, 0.05), (1, 1, 1))", "InvGamma(1, 0.15)[0, Inf]",
                             7, 0.0625, "Gamma(1, 1)[0, Inf]", "Two-sided((0.05), (1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]", 8, 0.0625, "Gamma(1, 1)[0, Inf]",
                             "Two-sided((0.1, 0.05), (1, 1, 1))", "InvGamma(1, 0.15)[0, Inf]",
                             9, 0.0625, "InvGamma(1, 0.15)[0, Inf]", "Two-sided((0.05), (1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]", 10, 0.0625, "InvGamma(1, 0.15)[0, Inf]",
                             "Two-sided((0.1, 0.05), (1, 1, 1))", "InvGamma(1, 0.15)[0, Inf]",
                             11, 0.0625, "Spike(0)", "Two-sided((0.05), (1, 1))", "InvGamma(1, 0.15)[0, Inf]",
                             12, 0.0625, "Spike(0)", "Two-sided((0.1, 0.05), (1, 1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]", 13, 0.0625, "Uniform(0, 1)", "Two-sided((0.05), (1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]", 14, 0.0625, "Uniform(0, 1)", "Two-sided((0.1, 0.05), (1, 1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]", 15, 0.0625, "Gamma(1, 0.15)[0, Inf]",
                             "Two-sided((0.05), (1, 1))", "InvGamma(1, 0.15)[0, Inf]", 16,
                             0.0625, "Gamma(1, 0.15)[0, Inf]", "Two-sided((0.1, 0.05), (1, 1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]"))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["model_preview"]][["collection"]][["model_preview_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list("16/16", 1, "Effect", "16/16", 1, "Heterogeneity", "16/16", 1,
                             "Publication bias"))
  })
  
  test_that("Priors plot - mu - truncated normal", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_mu_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "priors-plot-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot - mu - truncated t", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_mu_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "priors-plot-2", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot - mu - truncated Cauchy", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_mu_3"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "priors-plot-3", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot - mu - Gamma", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_mu_4"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "priors-plot-4", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot - mu - InvGamma", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_mu_5"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "priors-plot-5", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot - mu - spike", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_mu_6"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "priors-plot-6", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot - mu - uniform", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_mu_7"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "priors-plot-7", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot - mu - Gamma 2", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_mu_8"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "priors-plot-8", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot - weight function - 2 step", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_omega"]][["collection"]][["prior_plots_omega_omega_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "priors-plot-9", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot - weight function - 3 step", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_omega"]][["collection"]][["prior_plots_omega_omega_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "priors-plot-10", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot - heterogeneity - InvGamma", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_tau"]][["collection"]][["prior_plots_tau_tau_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "priors-plot-11", dir="RobustBayesianMetaAnalysis")
  })
}
{
  options <- jasptools::analysisOptions("RobustBayesianMetaAnalysis")
  options$.meta <- list(input_CI = list(containsColumn = TRUE), input_ES = list(
    containsColumn = TRUE), input_N = list(containsColumn = TRUE), 
    input_N1 = list(containsColumn = TRUE), input_N2 = list(containsColumn = TRUE), 
    input_SE = list(containsColumn = TRUE), input_labels = list(
      containsColumn = TRUE), input_t = list(containsColumn = TRUE), 
    priors_mu = list(list(name = list(containsColumn = TRUE)), 
                     list(name = list(containsColumn = TRUE))), priors_mu_null = list(
                       list(name = list(containsColumn = TRUE))), priors_omega = list(
                         list(name = list(containsColumn = TRUE)), list(name = list(
                           containsColumn = TRUE)), list(name = list(containsColumn = TRUE))), 
    priors_omega_null = list(list(name = list(containsColumn = TRUE))), 
    priors_tau_null = list(list(name = list(containsColumn = TRUE))))
  options$advanced_control <- "clever"
  options$fitted_path <- ""
  options$input_CI <- list()
  options$measures <- "correlation"
  options$plots_theta_order <- "labels"
  options$plots_theta_show <- "observed"
  options$plots_type_individual_by <- "model"
  options$plots_type_individual_order <- "ascending"
  options$priors_mu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"), 
                            list(name = "2", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "1", 
                                 parScale = "2", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$priors_mu_null <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priors_omega <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1)", 
                                    parAlpha2 = "(1,1)", parCuts = "(.05, .10)", priorOdds = "1/2", 
                                    type = "Two-sided"), list(name = "2", parAlpha = "(1,1,1)", 
                                                              parAlpha1 = "(1,1)", parAlpha2 = "(1,1)", parCuts = "(.05, .95)", 
                                                              priorOdds = "1/2", type = "One-sided (mon.)"), list(name = "3", 
                                                                                                                  parAlpha = "(1,1,1)", parAlpha1 = "(1,1)", parAlpha2 = "(1,1)", 
                                                                                                                  parCuts = "(.05, .95)", priorOdds = "1", type = "One-sided"))
  options$priors_omega_null <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .10)", priorOdds = "1", 
                                         type = "spike"))
  options$priors_plot <- TRUE
  options$priors_tau <- list()
  options$priors_tau_null <- list(list(`	` = "1", name = "", parA = "0", parAlpha = "1", parB = "1", 
                                       parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                       parScale2 = "1", parShape = "1", priorOdds = "1", truncationLower = "0", 
                                       truncationUpper = "Inf", type = "spike"))
  options$results_models_BF <- "inclusion"
  options$results_models_order <- "default"
  options$save_path <- ""
  set.seed(1)
  dataset <- NULL
  results <- jasptools::run("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Models Overview table results match", {
    table <- results[["results"]][["model_preview"]][["collection"]][["model_preview_models_summary"]][["data"]]
    expect_equal_tables(table,
                        list(1, 0.111111111111111, "Spike(0)", "Spike(1)", "Spike(0)", 2, 0.0555555555555556,
                             "Spike(0)", "Two-sided((0.1, 0.05), (1, 1, 1))", "Spike(0)",
                             3, 0.0555555555555556, "Spike(0)", "One-sided((0.95, 0.05), (1, 1, 1))",
                             "Spike(0)", 4, 0.111111111111111, "Spike(0)", "One-sided((0.95, 0.05), (1, 1), (1, 1))",
                             "Spike(0)", 5, 0.111111111111111, "Normal(0, 1)[-Inf, Inf]",
                             "Spike(1)", "Spike(0)", 6, 0.0555555555555556, "Normal(0, 1)[-Inf, Inf]",
                             "Two-sided((0.1, 0.05), (1, 1, 1))", "Spike(0)", 7, 0.0555555555555556,
                             "Normal(0, 1)[-Inf, Inf]", "One-sided((0.95, 0.05), (1, 1, 1))",
                             "Spike(0)", 8, 0.111111111111111, "Normal(0, 1)[-Inf, Inf]",
                             "One-sided((0.95, 0.05), (1, 1), (1, 1))", "Spike(0)", 9, 0.111111111111111,
                             "Normal(1, 2)[-Inf, Inf]", "Spike(1)", "Spike(0)", 10, 0.0555555555555556,
                             "Normal(1, 2)[-Inf, Inf]", "Two-sided((0.1, 0.05), (1, 1, 1))",
                             "Spike(0)", 11, 0.0555555555555556, "Normal(1, 2)[-Inf, Inf]",
                             "One-sided((0.95, 0.05), (1, 1, 1))", "Spike(0)", 12, 0.111111111111111,
                             "Normal(1, 2)[-Inf, Inf]", "One-sided((0.95, 0.05), (1, 1), (1, 1))",
                             "Spike(0)"))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["model_preview"]][["collection"]][["model_preview_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list("8/12", 0.666666666666667, "Effect", "0/12", 0, "Heterogeneity",
                             "9/12", 0.666666666666667, "Publication bias"))
  })
  
  test_that("Priors plot - mu - transformed normal", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_mu_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "transformed-priors-plot-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot - mu - transformed normal (1,2)", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_mu_3"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "transformed-priors-plot-2", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot - weight function - two sided", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_omega"]][["collection"]][["prior_plots_omega_omega_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weights-priors-plot-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot - weight function - one sided (monotonic)", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_omega"]][["collection"]][["prior_plots_omega_omega_3"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weights-priors-plot-2", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot - weight function - one sided", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_omega"]][["collection"]][["prior_plots_omega_omega_4"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weights-priors-plot-3", dir="RobustBayesianMetaAnalysis")
  })
  
}

### fit a default model using d + se, (wihout the more complex weight function) and main output
{
  options <- jasptools::analysisOptions("RobustBayesianMetaAnalysis")
  options$.meta <- list(input_CI = list(containsColumn = TRUE), input_ES = list(
    containsColumn = TRUE), input_N = list(containsColumn = TRUE), 
    input_N1 = list(containsColumn = TRUE), input_N2 = list(containsColumn = TRUE), 
    input_SE = list(containsColumn = TRUE), input_labels = list(
      containsColumn = TRUE), input_t = list(containsColumn = TRUE), 
    priors_mu = list(list(name = list(containsColumn = TRUE))), 
    priors_mu_null = list(list(name = list(containsColumn = TRUE))), 
    priors_omega = list(list(name = list(containsColumn = TRUE))), 
    priors_omega_null = list(list(name = list(containsColumn = TRUE))), 
    priors_tau = list(list(name = list(containsColumn = TRUE))), 
    priors_tau_null = list(list(name = list(containsColumn = TRUE))))
  options$advanced_adapt <- 500
  options$advanced_burnin <- 1000
  options$advanced_chains <- 2
  options$advanced_control <- "clever"
  options$advanced_iteration <- 4000
  options$fitted_path <- ""
  options$input_CI <- list()
  options$input_ES <- "d"
  options$input_SE <- "se"
  options$plots_individual_mu <- TRUE
  options$plots_individual_omega <- TRUE
  options$plots_individual_tau <- TRUE
  options$plots_mu <- TRUE
  options$plots_omega <- TRUE
  options$plots_tau <- TRUE
  options$plots_theta <- TRUE
  options$plots_theta_order <- "labels"
  options$plots_theta_show <- "observed"
  options$plots_type_individual_by <- "model"
  options$plots_type_individual_order <- "ascending"
  options$priors_mu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$priors_mu_null <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priors_omega <- list(list(name = "", parAlpha = "(1,1)", parAlpha1 = "(1,1,1)", 
                                    parAlpha2 = "(1,1)", parCuts = "(.05)", priorOdds = "1/2", 
                                    type = "Two-sided"))
  options$priors_omega_null <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .10)", priorOdds = "1", 
                                         type = "spike"))
  options$priors_tau <- list(list(`	` = "1", name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale2 = "1", parShape = "1", priorOdds = "1", truncationLower = "0", 
                                  truncationUpper = "Inf", type = "invgamma"))
  options$priors_tau_null <- list(list(`	` = "1", name = "", parA = "0", parAlpha = "1", parB = "1", 
                                       parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                       parScale2 = "1", parShape = "1", priorOdds = "1", truncationLower = "0", 
                                       truncationUpper = "Inf", type = "spike"))
  options$results_conditional <- TRUE
  options$results_models <- TRUE
  options$results_models_BF <- "inclusion"
  options$results_models_order <- "default"
  options$save_path <- ""
  set.seed(1)
  dataset <- structure(list(study = structure(c(1L, 3L, 2L), .Label = c("study one", 
                                                                        "study three", "study two"), class = "factor"), t = c(2.51, 2.39, 
                                                                                                                              2.55), N = c(100L, 150L, 97L), d = c(0.25, 0.2, 0.26), se = c(0.1, 
                                                                                                                                                                                            0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L, 75L, 48L), lCI = c(0.05, 
                                                                                                                                                                                                                                                              0.04, 0.06), uCI = c(0.45, 0.41, 0.38)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                           -3L))
  results <- jasptools::run("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0, 0.190081931593515, 0.190081931593515, "Effect size (<unicode><unicode>)",
                             0.333866564932042, 0, 0, 0, "Heterogeneity (<unicode><unicode>)",
                             0.314967238642941))
  })
  
  test_that("Model Averaged Weights (Ď‰) table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_weights"]][["data"]]
    expect_equal_tables(table,
                        list(1, 0, 1, 1, 1, 0.05, 0.00599704774931339, 0.05, 0.573062069482425,
                             0.573062069482425, 1, 1))
  })
  
  test_that("Conditional Estimates table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_conditional_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0.0601405747266103, 0.207177943751351, 0.210330333086386, "Effect size (<unicode><unicode>)",
                             0.342005856042393, 0.0326111357269888, 0.153342846880509, 0.120242501856159,
                             "Heterogeneity (<unicode><unicode>)", 0.449773965162367))
  })
  
  test_that("Conditional Weights (Ď‰) table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_conditional_weights"]][["data"]]
    expect_equal_tables(table,
                        list(1, 0, 1, 1, 1, 0.05, 0.00325701104562544, 0.05, 0.29169072246424,
                             0.194146475231978, 0.921947822250772, 1))
  })
  
  test_that("Models Overview table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_models_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0.00217700774875682, -12.3417718561692, 1, 0.00043521205774695,
                             0.166666666666667, "Spike(0)", "Spike(1)", "Spike(0)", 0.531157751119659,
                             -6.98669647910159, 2, 0.0460628292998666, 0.0833333333333333,
                             "Spike(0)", "Two-sided((0.05), (1, 1))", "Spike(0)", 0.178550966011138,
                             -7.96950117507418, 3, 0.0344789434695223, 0.166666666666667,
                             "Spike(0)", "Spike(1)", "InvGamma(1, 0.15)[0, Inf]", 1.30622293098484,
                             -6.15191286898532, 4, 0.106143285255788, 0.0833333333333333,
                             "Spike(0)", "Two-sided((0.05), (1, 1))", "InvGamma(1, 0.15)[0, Inf]",
                             1.9645792909097, -5.86765378059505, 5, 0.282081545610932, 0.166666666666667,
                             "Normal(0, 1)[-Inf, Inf]", "Spike(1)", "Spike(0)", 6.10327696329363,
                             -4.93939176541967, 6, 0.356848396736616, 0.0833333333333333,
                             "Normal(0, 1)[-Inf, Inf]", "Two-sided((0.05), (1, 1))", "Spike(0)",
                             0.403082528335052, -7.19767828191783, 7, 0.07460232676832, 0.166666666666667,
                             "Normal(0, 1)[-Inf, Inf]", "Spike(1)", "InvGamma(1, 0.15)[0, Inf]",
                             1.2133669992042, -6.21807938744952, 8, 0.0993474608012077, 0.0833333333333333,
                             "Normal(0, 1)[-Inf, Inf]", "Two-sided((0.05), (1, 1))", "InvGamma(1, 0.15)[0, Inf]"
                        ))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list(4.34415645914171, "4/8", 0.812879729917076, 0.5, "Effect", 0.458942476486562,
                             "4/8", 0.314572016294838, 0.5, "Heterogeneity", 3.10727801846188,
                             "4/8", 0.608401972093478, 0.333333333333333, "Publication bias"
                        ))
  })
  
  test_that("Effect size (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-model-averaged-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weight function (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_omega"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-function-model-averaged-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-model-averaged-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Forest plot (Model Averaged) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-model-averaged-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Effect size (Conditional Models) plot matches", {
    plotName <- results[["results"]][["plots_individual"]][["collection"]][["plots_individual_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-conditional-models-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weights (Conditional Models) plot matches", {
    plotName <- results[["results"]][["plots_individual"]][["collection"]][["plots_individual_omega"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weights-conditional-models-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Conditional Models) plot matches", {
    plotName <- results[["results"]][["plots_individual"]][["collection"]][["plots_individual_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-conditional-models-1", dir="RobustBayesianMetaAnalysis")
  })
  
}

### fit models with a truncated priors and t + se
{
  options <- jasptools::analysisOptions("RobustBayesianMetaAnalysis")
  options$.meta <- list(input_CI = list(containsColumn = TRUE), input_ES = list(
    containsColumn = TRUE), input_N = list(containsColumn = TRUE), 
    input_N1 = list(containsColumn = TRUE), input_N2 = list(containsColumn = TRUE), 
    input_SE = list(containsColumn = TRUE), input_labels = list(
      containsColumn = TRUE), input_t = list(containsColumn = TRUE), 
    priors_mu = list(list(name = list(containsColumn = TRUE))), 
    priors_mu_null = list(list(name = list(containsColumn = TRUE))), 
    priors_omega_null = list(list(name = list(containsColumn = TRUE))), 
    priors_tau = list(list(name = list(containsColumn = TRUE))), 
    priors_tau_null = list(list(name = list(containsColumn = TRUE))))
  options$advanced_adapt <- 500
  options$advanced_burnin <- 1000
  options$advanced_chains <- 2
  options$advanced_control <- "clever"
  options$advanced_iteration <- 4000
  options$fitted_path <- ""
  options$input_CI <- list()
  options$input_N <- "N"
  options$input_t <- "t"
  options$plots_mu <- TRUE
  options$plots_tau <- TRUE
  options$plots_theta <- TRUE
  options$plots_theta_order <- "labels"
  options$plots_theta_show <- "both"
  options$plots_type <- "conditional"
  options$plots_type_individual_by <- "model"
  options$plots_type_individual_order <- "ascending"
  options$priors_mu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = ".5", truncationUpper = "Inf", type = "normal"))
  options$priors_mu_null <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priors_omega <- list()
  options$priors_omega_null <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .95)", priorOdds = "1", 
                                         type = "spike"))
  options$priors_tau <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale = ".3", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                  truncationLower = ".25", truncationUpper = ".50", type = "normal"))
  options$priors_tau_null <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                       parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                       parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                       truncationLower = "0", truncationUpper = "Inf", type = "spike"))
  options$results_models_BF <- "inclusion"
  options$results_models_order <- "default"
  options$save_path <- ""
  set.seed(1)
  dataset <- structure(list(study = structure(c(1L, 3L, 2L), .Label = c("study one", 
                                                                        "study three", "study two"), class = "factor"), t = c(2.51, 2.39, 
                                                                                                                              2.55), N = c(100L, 150L, 97L), d = c(0.25, 0.2, 0.26), se = c(0.1, 
                                                                                                                                                                                            0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L, 75L, 48L), lCI = c(0.05, 
                                                                                                                                                                                                                                                              0.04, 0.06), uCI = c(0.45, 0.41, 0.38)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                           -3L))
  results <- jasptools::run("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0, 0.558554923066283, 0.558554923066283, "Effect size (<unicode><unicode>)",
                             0.824625889978887, 0, 0, 0, "Heterogeneity (<unicode><unicode>)",
                             0.455426559873941))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list(9.50763462152257, "2/4", 0.90483110271538, 0.5, "Effect", 0.455993686087569,
                             "2/4", 0.313183834823405, 0.5, "Heterogeneity", "", "0/4", 0,
                             0, "Publication bias"))
  })
  
  test_that("Effect size (Conditional) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-conditional-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Conditional) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-conditional-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Forest plot (Conditional) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-conditional-1", dir="RobustBayesianMetaAnalysis")
  }) 
}

### fit models with only an effect size, d + (N1 + N2) and names
{
  options <- jasptools::analysisOptions("RobustBayesianMetaAnalysis")
  options$.meta <- list(input_CI = list(containsColumn = TRUE), input_ES = list(
    containsColumn = TRUE), input_N = list(containsColumn = TRUE), 
    input_N1 = list(containsColumn = TRUE), input_N2 = list(containsColumn = TRUE), 
    input_SE = list(containsColumn = TRUE), input_labels = list(
      containsColumn = TRUE), input_t = list(containsColumn = TRUE), 
    priors_mu = list(list(name = list(containsColumn = TRUE))), 
    priors_mu_null = list(list(name = list(containsColumn = TRUE))), 
    priors_omega_null = list(list(name = list(containsColumn = TRUE))), 
    priors_tau_null = list(list(name = list(containsColumn = TRUE))))
  options$advanced_adapt <- 500
  options$advanced_burnin <- 1000
  options$advanced_chains <- 2
  options$advanced_control <- "clever"
  options$advanced_iteration <- 4000
  options$fitted_path <- ""
  options$input_CI <- list()
  options$input_ES <- "d"
  options$input_N1 <- "N1"
  options$input_N2 <- "N2"
  options$input_labels <- "study"
  options$plots_mu <- TRUE
  options$plots_tau <- TRUE
  options$plots_theta <- TRUE
  options$plots_theta_order <- "labels"
  options$plots_theta_show <- "observed"
  options$plots_type_individual_by <- "model"
  options$plots_type_individual_order <- "ascending"
  options$priors_mu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = ".15", type = "invgamma"))
  options$priors_mu_null <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priors_null <- TRUE
  options$priors_omega <- list()
  options$priors_omega_null <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .95)", priorOdds = "1", 
                                         type = "spike"))
  options$priors_tau <- list()
  options$priors_tau_null <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                       parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                       parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                       truncationLower = "0", truncationUpper = "Inf", type = "spike"))
  options$results_models_BF <- "inclusion"
  options$results_models_order <- "default"
  options$save_path <- ""
  set.seed(1)
  dataset <- structure(list(study = structure(c(1L, 3L, 2L), .Label = c("study one", 
                                                                        "study three", "study two"), class = "factor"), t = c(2.51, 2.39, 
                                                                                                                              2.55), N = c(100L, 150L, 97L), d = c(0.25, 0.2, 0.26), se = c(0.1, 
                                                                                                                                                                                            0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L, 75L, 48L), lCI = c(0.05, 
                                                                                                                                                                                                                                                              0.04, 0.06), uCI = c(0.45, 0.41, 0.38)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                           -3L))
  results <- jasptools::run("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0, 0.0939706937772783, 0.0939706937772783, "Effect size (<unicode><unicode>)",
                             0.147125401141212, 0, 0, 0, "Heterogeneity (<unicode><unicode>)",
                             0))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list(4.37039740753456, "1/2", 0.813794040903375, 0.5, "Effect", "",
                             "0/2", 0, 0, "Heterogeneity", "", "0/2", 0, 0, "Publication bias"
                        ))
  })
  
  test_that("Effect size (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-model-averaged-2", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-model-averaged-2", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Forest plot (Model Averaged) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-model-averaged-2", dir="RobustBayesianMetaAnalysis")
  })
}

### fit models with only one publication bias function, y + (lCI & uCI)
{
  options <- jasptools::analysisOptions("RobustBayesianMetaAnalysis")
  options$.meta <- list(input_CI = list(containsColumn = TRUE), input_ES = list(
    containsColumn = TRUE), input_N = list(containsColumn = TRUE), 
    input_N1 = list(containsColumn = TRUE), input_N2 = list(containsColumn = TRUE), 
    input_SE = list(containsColumn = TRUE), input_labels = list(
      containsColumn = TRUE), input_t = list(containsColumn = TRUE), 
    priors_mu_null = list(list(name = list(containsColumn = TRUE))), 
    priors_omega = list(list(name = list(containsColumn = TRUE))), 
    priors_omega_null = list(list(name = list(containsColumn = TRUE))), 
    priors_tau_null = list(list(name = list(containsColumn = TRUE))))
  options$advanced_adapt <- 500
  options$advanced_burnin <- 1000
  options$advanced_chains <- 2
  options$advanced_control <- "clever"
  options$advanced_iteration <- 4000
  options$fitted_path <- ""
  options$input_CI <- list(c("uCI", "lCI"))
  options$input_ES <- "d"
  options$measures <- "general"
  options$plots_mu <- TRUE
  options$plots_omega <- TRUE
  options$plots_tau <- TRUE
  options$plots_theta <- TRUE
  options$plots_theta_order <- "labels"
  options$plots_theta_show <- "both"
  options$plots_type_individual_by <- "model"
  options$plots_type_individual_order <- "ascending"
  options$priors_mu <- list()
  options$priors_mu_null <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priors_null <- TRUE
  options$priors_omega <- list(list(name = "", parAlpha = "(1,1)", parAlpha1 = "(1,1)", 
                                    parAlpha2 = "(1,1)", parCuts = "(.05)", priorOdds = "1/2", 
                                    type = "Two-sided"))
  options$priors_omega_null <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .95)", priorOdds = "1", 
                                         type = "spike"))
  options$priors_tau <- list()
  options$priors_tau_null <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                       parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                       parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                       truncationLower = "0", truncationUpper = "Inf", type = "spike"))
  options$results_models_BF <- "inclusion"
  options$results_models_order <- "default"
  options$save_path <- ""
  set.seed(1)
  dataset <- structure(list(study = structure(c(1L, 3L, 2L), .Label = c("study one", 
                                                                        "study three", "study two"), class = "factor"), t = c(2.51, 2.39, 
                                                                                                                              2.55), N = c(100L, 150L, 97L), d = c(0.25, 0.2, 0.26), se = c(0.1, 
                                                                                                                                                                                            0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L, 75L, 48L), lCI = c(0.05, 
                                                                                                                                                                                                                                                              0.04, 0.06), uCI = c(0.45, 0.41, 0.38)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                           -3L))
  results <- jasptools::run("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0, 0, 0, "Effect size (<unicode><unicode>)", 0, 0, 0, 0, "Heterogeneity (<unicode><unicode>)",
                             0))
  })
  
  test_that("Model Averaged Weights (Ď‰) table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_weights"]][["data"]]
    expect_equal_tables(table,
                        list(1, 0, 1, 1, 1, 0.05, 0.000767371189249364, 0.05, 0.0219980890559249,
                             0.0219980890559249, 0.314633344471127, 1))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list("", "0/2", 0, 0, "Effect", "", "0/2", 0, 0, "Heterogeneity", 211.679931564072,
                             "1/2", 0.990640206661615, 0.333333333333333, "Publication bias"
                        ))
  })
  
  test_that("Effect size (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-model-averaged-3", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weight function (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_omega"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-function-model-averaged-3", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-model-averaged-3", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Forest plot (Model Averaged) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-model-averaged-3", dir="RobustBayesianMetaAnalysis")
  })
}

### fit models with only heterogeneity, rho + (N)
{
  options <- jasptools::analysisOptions("RobustBayesianMetaAnalysis")
  options$.meta <- list(input_CI = list(containsColumn = TRUE), input_ES = list(
    containsColumn = TRUE), input_N = list(containsColumn = TRUE), 
    input_N1 = list(containsColumn = TRUE), input_N2 = list(containsColumn = TRUE), 
    input_SE = list(containsColumn = TRUE), input_labels = list(
      containsColumn = TRUE), input_t = list(containsColumn = TRUE), 
    priors_mu_null = list(list(name = list(containsColumn = TRUE))), 
    priors_omega_null = list(list(name = list(containsColumn = TRUE))), 
    priors_tau = list(list(name = list(containsColumn = TRUE))), 
    priors_tau_null = list(list(name = list(containsColumn = TRUE))))
  options$advanced_adapt <- 500
  options$advanced_burnin <- 1000
  options$advanced_chains <- 2
  options$advanced_control <- "clever"
  options$advanced_iteration <- 4000
  options$fitted_path <- ""
  options$input_CI <- list(c("uCI", "lCI"))
  options$input_ES <- "d"
  options$input_N <- "N"
  options$measures <- "correlation"
  options$plots_mu <- TRUE
  options$plots_tau <- TRUE
  options$plots_theta <- TRUE
  options$plots_theta_order <- "labels"
  options$plots_theta_show <- "both"
  options$plots_type_individual_by <- "model"
  options$plots_type_individual_order <- "ascending"
  options$priors_mu <- list()
  options$priors_mu_null <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priors_null <- TRUE
  options$priors_omega <- list()
  options$priors_omega_null <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .95)", priorOdds = "1", 
                                         type = "spike"))
  options$priors_tau <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                  truncationLower = "0", truncationUpper = ".2", type = "normal"))
  options$priors_tau_null <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                       parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                       parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                       truncationLower = "0", truncationUpper = "Inf", type = "spike"))
  options$results_models_BF <- "inclusion"
  options$results_models_order <- "default"
  options$save_path <- ""
  set.seed(1)
  dataset <- structure(list(study = structure(c(1L, 3L, 2L), .Label = c("study one", 
                                                                        "study three", "study two"), class = "factor"), t = c(2.51, 2.39, 
                                                                                                                              2.55), N = c(100L, 150L, 97L), d = c(0.25, 0.2, 0.26), se = c(0.1, 
                                                                                                                                                                                            0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L, 75L, 48L), lCI = c(0.05, 
                                                                                                                                                                                                                                                              0.04, 0.06), uCI = c(0.45, 0.41, 0.38)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                           -3L))
  results <- jasptools::run("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0, 0, 0, "Effect size (<unicode><unicode>)", 0, 0, 0.164511938302287,
                             0.164511938302287, "Heterogeneity (<unicode><unicode>)", 0.1986654086427
                        ))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list("", "0/2", 0, 0, "Effect", 11.3024893141746, "1/2", 0.918715637586628,
                             0.5, "Heterogeneity", "", "0/2", 0, 0, "Publication bias"))
  })
  
  test_that("Effect size (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-model-averaged-4", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-model-averaged-4", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Forest plot (Model Averaged) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-model-averaged-4", dir="RobustBayesianMetaAnalysis")
  })
}

### more options tested using a preloaded model
{
  options <- jasptools::analysisOptions("RobustBayesianMetaAnalysis")
  options$.meta <- list(input_CI = list(containsColumn = TRUE), input_ES = list(
    containsColumn = TRUE), input_N = list(containsColumn = TRUE), 
    input_N1 = list(containsColumn = TRUE), input_N2 = list(containsColumn = TRUE), 
    input_SE = list(containsColumn = TRUE), input_labels = list(
      containsColumn = TRUE), input_t = list(containsColumn = TRUE), 
    priors_mu = list(list(name = list(containsColumn = TRUE))), 
    priors_mu_null = list(list(name = list(containsColumn = TRUE))), 
    priors_omega = list(list(name = list(containsColumn = TRUE)), 
                        list(name = list(containsColumn = TRUE))), priors_omega_null = list(
                          list(name = list(containsColumn = TRUE))), priors_tau = list(
                            list(name = list(containsColumn = TRUE))), priors_tau_null = list(
                              list(name = list(containsColumn = TRUE))))
  options$advanced_control <- "clever"
  options$diagnostics_autocorrelation <- TRUE
  options$diagnostics_mu <- TRUE
  options$diagnostics_omega <- TRUE
  options$diagnostics_overview <- TRUE
  options$diagnostics_samples <- TRUE
  options$diagnostics_single <- TRUE
  options$diagnostics_single_model <- 12
  options$diagnostics_tau <- TRUE
  options$diagnostics_trace <- TRUE
  options$fitted_path <- fitted_path
  options$input_CI <- list()
  options$measures <- "fitted"
  options$plots_mu <- TRUE
  options$plots_omega <- TRUE
  options$plots_tau <- TRUE
  options$plots_theta <- TRUE
  options$plots_theta_order <- "labels"
  options$plots_theta_show <- "observed"
  options$plots_type <- "conditional"
  options$plots_type_individual_by <- "model"
  options$plots_type_individual_order <- "ascending"
  options$priors_mu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$priors_mu_null <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priors_omega <- list(list(name = "", parAlpha = "(1,1)", parAlpha1 = "(1,1,1)", 
                                    parAlpha2 = "(1,1)", parCuts = "(.05)", priorOdds = "1/2", 
                                    type = "Two-sided"), list(name = "2", parAlpha = "(1,1,1)", 
                                                              parAlpha1 = "(1,1,1)", parAlpha2 = "(1,1)", parCuts = "(.05, .10)", 
                                                              priorOdds = "1/2", type = "Two-sided"))
  options$priors_omega_null <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .10)", priorOdds = "1", 
                                         type = "spike"))
  options$priors_tau <- list(list(`	` = "1", name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale2 = "1", parShape = "1", priorOdds = "1", truncationLower = "0", 
                                  truncationUpper = "Inf", type = "invgamma"))
  options$priors_tau_null <- list(list(`	` = "1", name = "", parA = "0", parAlpha = "1", parB = "1", 
                                       parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                       parScale2 = "1", parShape = "1", priorOdds = "1", truncationLower = "0", 
                                       truncationUpper = "Inf", type = "spike"))
  options$results_individual <- TRUE
  options$results_individual_single <- TRUE
  options$results_individual_single_number <- 12
  options$results_models_BF <- "inclusion"
  options$results_models_order <- "default"
  options$results_theta <- TRUE
  options$save_path <- ""
  set.seed(1)
  dataset <- NULL
  results <- jasptools::run("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Diagnostic Plots - Effect size autocorrelation matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_mu"]][["collection"]][["diagnostics_model_12_mu_autocor"]][["collection"]][["diagnostics_model_12_mu_autocor_autocor_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-plot-0", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostic Plots - Effect size densities matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_mu"]][["collection"]][["diagnostics_model_12_mu_samples"]][["collection"]][["diagnostics_model_12_mu_samples_samples_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-plot-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostic Plots - Effect size chains matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_mu"]][["collection"]][["diagnostics_model_12_mu_trace"]][["collection"]][["diagnostics_model_12_mu_trace_trace_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-plot-2", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostic Plots - Weights [2] autocorrelation matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_autocor"]][["collection"]][["diagnostics_model_12_omega_autocor_autocor_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-plot-3", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostic Plots - Weights [3] autocorrelation matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_autocor"]][["collection"]][["diagnostics_model_12_omega_autocor_autocor_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-plot-4", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostic Plots - Weights [2] densities matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_samples"]][["collection"]][["diagnostics_model_12_omega_samples_samples_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-plot-5", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostic Plots - Weights [3] densities matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_samples"]][["collection"]][["diagnostics_model_12_omega_samples_samples_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-plot-6", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostic Plots - Weights [2] chains matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_trace"]][["collection"]][["diagnostics_model_12_omega_trace_trace_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-plot-7", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostic Plots - Weights [3] chains matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_trace"]][["collection"]][["diagnostics_model_12_omega_trace_trace_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-plot-8", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostic Plots - Heterogeneity autocorrelation matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_tau"]][["collection"]][["diagnostics_model_12_tau_autocor"]][["collection"]][["diagnostics_model_12_tau_autocor_autocor_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-plot-9", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostic Plots - Heterogeneity densities matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_tau"]][["collection"]][["diagnostics_model_12_tau_samples"]][["collection"]][["diagnostics_model_12_tau_samples_samples_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-plot-10", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostic Plots - Heterogeneity chains matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_tau"]][["collection"]][["diagnostics_model_12_tau_trace"]][["collection"]][["diagnostics_model_12_tau_trace_trace_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-plot-11", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Models Diagnostics Overview table results match", {
    table <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_models_diagnostics"]][["data"]]
    expect_equal_tables(table,
                        list("", "", "", 1, "Spike(0)", "Spike(1)", "Spike(0)", 10466, 1.00010049364457,
                             0.000115094597818856, 2, "Spike(0)", "Two-sided((0.05), (1, 1))",
                             "Spike(0)", 9331, 1.000085163241, 0.00159202246136869, 3, "Spike(0)",
                             "Two-sided((0.1, 0.05), (1, 1, 1))", "Spike(0)", 7507, 1.00030361193045,
                             0.00108772876719897, 4, "Spike(0)", "Spike(1)", "InvGamma(1, 0.15)[0, Inf]",
                             1093, 1.00117114947046, 0.002655287299943, 5, "Spike(0)", "Two-sided((0.05), (1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]", 1103, 1.00370110498082, 0.00269610373657982,
                             6, "Spike(0)", "Two-sided((0.1, 0.05), (1, 1, 1))", "InvGamma(1, 0.15)[0, Inf]",
                             18231, 1.00028627530142, 0.000309706897676008, 7, "Normal(0, 1)[-Inf, Inf]",
                             "Spike(1)", "Spike(0)", 7303, 1.00070481635947, 0.00146835764065996,
                             8, "Normal(0, 1)[-Inf, Inf]", "Two-sided((0.05), (1, 1))", "Spike(0)",
                             4109, 1.00083686191282, 0.00232405766583934, 9, "Normal(0, 1)[-Inf, Inf]",
                             "Two-sided((0.1, 0.05), (1, 1, 1))", "Spike(0)", 997, 1.00162860705,
                             0.00140948483318384, 10, "Normal(0, 1)[-Inf, Inf]", "Spike(1)",
                             "InvGamma(1, 0.15)[0, Inf]", 574, 1.01040819322647, 0.00374251928756153,
                             11, "Normal(0, 1)[-Inf, Inf]", "Two-sided((0.05), (1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]", 618, 1.00760178942564, 0.0028040331933867,
                             12, "Normal(0, 1)[-Inf, Inf]", "Two-sided((0.1, 0.05), (1, 1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]"))
  })
  
  test_that("Model Estimates table results match", {
    table <- results[["results"]][["individual_models"]][["collection"]][["individual_models_model_12"]][["collection"]][["individual_models_model_12_temp_coef"]][["data"]]
    expect_equal_tables(table,
                        list(0.00126147153001014, 618, 0.0932223132127736, 0.153187957087545,
                             0.153464069255454, 1.00760178942564, "Effect size (<unicode><unicode>)",
                             0.211319027553267, 0.000988904353088519, 1066, 0.0208588078550267,
                             0.0710380084816596, 0.0650065040133452, 1.00053721072253, "Heterogeneity (<unicode><unicode>)",
                             0.134527829353758))
  })
  
  test_that("Information table results match", {
    table <- results[["results"]][["individual_models"]][["collection"]][["individual_models_model_12"]][["collection"]][["individual_models_model_12_temp_info"]][["data"]]
    expect_equal_tables(table,
                        list(2.0792294240899, -27.2219234757986, 0.121740236193396, 0.0625
                        ))
  })
  
  test_that("Priors table results match", {
    table <- results[["results"]][["individual_models"]][["collection"]][["individual_models_model_12"]][["collection"]][["individual_models_model_12_temp_priors"]][["data"]]
    expect_equal_tables(table,
                        list("Normal(0, 1)[-Inf, Inf]", "Two-sided((0.1, 0.05), (1, 1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]"))
  })
  
  test_that("Estimated Studies' Effects (Î¸) table results match", {
    table <- results[["results"]][["individual_models"]][["collection"]][["individual_models_model_12"]][["collection"]][["individual_models_model_12_temp_studies"]][["data"]]
    expect_equal_tables(table,
                        list(0.00118688637129725, 1560, 0.0627664742209605, 0.154523245344715,
                             0.153690730421082, 1.00339505724863, "Anderson (2004; Exp. 2)",
                             0.241173702107704, 0.00112955962618415, 1492, 0.0698010502811825,
                             0.152779753070973, 0.152599336635102, 1.00239291265114, "Anderson (2004; Exp. 3)",
                             0.234984055915514, 0.00124618492311774, 1401, 0.0689755103580689,
                             0.155983072032323, 0.154920007131437, 1.00309465749598, "Anderson (in press)",
                             0.244630628537246, 0.00110341500002332, 1558, 0.0597272695519908,
                             0.146426303481604, 0.14679349266614, 1.00339425145671, "Anderson (2000)",
                             0.226105703128576, 0.0011262922481393, 1579, 0.0590402625754587,
                             0.147390765973535, 0.147676023623913, 1.00242021695147, "Arriaga (2008)",
                             0.22924387931336, 0.00133451311147185, 1260, 0.0682307143148739,
                             0.157587689345041, 0.15629519193073, 1.00283145674438, "Anderson (2003)",
                             0.246525009881734, 0.0014310699301483, 1234, 0.0763658363310853,
                             0.167013058316743, 0.163897292004967, 1.00246737646966, "Barlett (2009)",
                             0.263346181018786, 0.00117802180893373, 1516, 0.0637828064097584,
                             0.152818011242218, 0.152078730818824, 1.00258976126556, "Ballard (1999)",
                             0.237933975315872, 0.00123778539537918, 1449, 0.0671842226666939,
                             0.155130284083594, 0.154072598247243, 1.00223327392586, "Bartholow (2005)",
                             0.246231631213309, 0.0011916982403025, 1469, 0.0629968248031648,
                             0.151181866074427, 0.150664756828034, 1.00159032380496, "Carnagey (2005)",
                             0.236789483376031, 0.00115609244571203, 1634, 0.0527857559828623,
                             0.147913511968537, 0.148238636316897, 1.00291565932601, "Cicchiriool (2006)",
                             0.23106019374081, 0.00120746258684542, 1362, 0.0656512521782747,
                             0.15320400762662, 0.152612190904567, 1.00260703362534, "Gentile (in press)",
                             0.234437797531761, 0.00133097928472719, 1323, 0.0679649066942493,
                             0.157783932387765, 0.155899051978766, 1.00246881411072, "Irwin (1995)",
                             0.250858897786932, 0.00124844903687008, 1477, 0.0542260453978141,
                             0.149391520910075, 0.148864473825508, 1.00311163300669, "Katori (2001)",
                             0.237019664550896, 0.00126560351334505, 1350, 0.0645409819622838,
                             0.154588428560622, 0.153005458716874, 1.00247303615928, "Konijn (2007)",
                             0.240980470188945, 0.00128578810155593, 1368, 0.0649122512501964,
                             0.156329546629931, 0.154663786913019, 1.00240780156172, "Sheese (2005)",
                             0.244656013279677, 0.00128262553474706, 1372, 0.0691279295403818,
                             0.155798047993054, 0.154545821242817, 1.00315622278313, "Sakamoto (2001; Exp. 1)",
                             0.248588032613739, 0.0012505173149723, 1485, 0.0617874777226259,
                             0.154130522966276, 0.152619752735264, 1.00183948917286, "Schutte (1988)",
                             0.245958814131438, 0.00131402187142727, 1356, 0.0621610500572623,
                             0.154740207443713, 0.153345506781616, 1.00332370639166, "Sakamoto (2001; Exp. 1)",
                             0.24358820073225, 0.00122787617930207, 1487, 0.0597698182037126,
                             0.152396116448412, 0.151751149382207, 1.00293613864426, "Sakamoto (2001; Exp. 2)",
                             0.239509192199654, 0.00118789973197807, 1581, 0.0522774414078521,
                             0.144724136391252, 0.145041598393974, 1.00297128742555, "Yukawa (2000)",
                             0.233525733566131, 0.000921418775797227, 1529, 0.0693022676787508,
                             0.139518596072963, 0.140408644795474, 1.0021484714064, "Anderson (2007)",
                             0.204910125134164, 0.0013363933248547, 1403, 0.0683512340527687,
                             0.163246891077896, 0.160893100662774, 1.00298017625409, "Bartholow (2002)",
                             0.257561804022334))
  })
  
  test_that("Estimated Weights (Ď‰) table results match", {
    table <- results[["results"]][["individual_models"]][["collection"]][["individual_models_model_12"]][["collection"]][["individual_models_model_12_temp_weights"]][["data"]]
    expect_equal_tables(table,
                        list("<unicode><unicode><unicode>", 0, 1, 0, 1, 1, 1.00062993417959,
                             1, 0.05, 0.0028040331933867, 6554, 0.199294973945084, 0.05,
                             0.550618160777274, 0.540182899062307, 1.00098003434939, 0.998234527198636,
                             0.1, 0.00191804640479239, 1942, 0.00815564945392979, 0.1, 0.112534613890062,
                             0.0899850976559516, 1.00282589692782, 0.282599573067571, 1
                        ))
  })
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0.0946053997042164, 0.152208200580981, 0.152208200580981, "Effect size (<unicode><unicode>)",
                             0.207990864321621, 0, 0, 0, "Heterogeneity (<unicode><unicode>)",
                             0.0876252147626617))
  })
  
  test_that("Model Averaged Weights (Ď‰) table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_weights"]][["data"]]
    expect_equal_tables(table,
                        list(1, 0, 1, 1, 1, 0.05, 0.0945052266099806, 0.05, 0.505983624767411,
                             0.505983624767411, 0.961965550168008, 0.1, 0.0224360274260941,
                             0.1, 0.0986787722822264, 0.0986787722822264, 0.36834063889696,
                             1))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list(11547021.5313713, "6/12", 0.999999913397588, 0.5, "Effect", 0.158577012507722,
                             "6/12", 0.13687222411265, 0.5, "Heterogeneity", 529.088343904832,
                             "8/12", 0.998113521997798, 0.5, "Publication bias"))
  })
  
  test_that("Model Averaged Estimated Studies' Effects (Î¸) table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_studies_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0.0903712028165508, 0.148447007028325, 0.148447007028325, "Anderson (2004; Exp. 2)",
                             0.20667950354462, 0.0911433972007817, 0.151341205650675, 0.151341205650675,
                             "Anderson (2004; Exp. 3)", 0.209084514495961, 0.091006089540715,
                             0.150979666400309, 0.150979666400309, "Anderson (in press)",
                             0.203521157657346, 0.0882173761871017, 0.148584006166866, 0.148584006166866,
                             "Anderson (2000)", 0.201616650754228, 0.0912963264855289, 0.148500644483084,
                             0.148500644483084, "Arriaga (2008)", 0.204359025089417, 0.0882173761871017,
                             0.150979666400309, 0.150979666400309, "Anderson (2003)", 0.204367405965642,
                             0.0914274969068644, 0.151101661326525, 0.151101661326525, "Barlett (2009)",
                             0.217917108882882, 0.0869587243318078, 0.148754313363224, 0.148754313363224,
                             "Ballard (1999)", 0.207037237467093, 0.0882173761871017, 0.149581633766109,
                             0.149581633766109, "Bartholow (2005)", 0.204902147172008, 0.0898819694269352,
                             0.150104830528991, 0.150104830528991, "Carnagey (2005)", 0.204100012067219,
                             0.0903712028165508, 0.149548043660973, 0.149548043660973, "Cicchiriool (2006)",
                             0.204359025089417, 0.0928473613250039, 0.149257190027554, 0.149257190027554,
                             "Gentile (in press)", 0.203219859969572, 0.0891684098355765,
                             0.149581633766109, 0.149581633766109, "Irwin (1995)", 0.205877110241537,
                             0.0914274969068644, 0.149257190027554, 0.149257190027554, "Katori (2001)",
                             0.203521157657346, 0.0903766383851239, 0.152382609220662, 0.152382609220662,
                             "Konijn (2007)", 0.205063319528532, 0.0880135758536917, 0.149747686878157,
                             0.149747686878157, "Sheese (2005)", 0.211590112079562, 0.0918803221743606,
                             0.151055090724261, 0.151055090724261, "Sakamoto (2001; Exp. 1)",
                             0.20463765554737, 0.0891684098355765, 0.149100225017513, 0.149100225017513,
                             "Schutte (1988)", 0.204100012067219, 0.0903712028165508, 0.149486725146314,
                             0.149486725146314, "Sakamoto (2001; Exp. 1)", 0.204359025089417,
                             0.0882173761871017, 0.148754313363224, 0.148754313363224, "Sakamoto (2001; Exp. 2)",
                             0.203767163890428, 0.0865173233841308, 0.148375414365553, 0.148375414365553,
                             "Yukawa (2000)", 0.204359025089417, 0.089251239382399, 0.147987763601752,
                             0.147987763601752, "Anderson (2007)", 0.196737278855289, 0.0891684098355765,
                             0.150979666400309, 0.150979666400309, "Bartholow (2002)", 0.212299505210962
                        ))
  })
  
  test_that("Effect size (Conditional) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-conditional-2", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weight function (Conditional) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_omega"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-function-conditional-2", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Conditional) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-conditional-2", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Forest plot (Conditional) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-conditional-2", dir="RobustBayesianMetaAnalysis")
  })
}
{
  options <- jasptools::analysisOptions("RobustBayesianMetaAnalysis")
  options$.meta <- list(input_CI = list(containsColumn = TRUE), input_ES = list(
    containsColumn = TRUE), input_N = list(containsColumn = TRUE), 
    input_N1 = list(containsColumn = TRUE), input_N2 = list(containsColumn = TRUE), 
    input_SE = list(containsColumn = TRUE), input_labels = list(
      containsColumn = TRUE), input_t = list(containsColumn = TRUE), 
    priors_mu = list(list(name = list(containsColumn = TRUE))), 
    priors_mu_null = list(list(name = list(containsColumn = TRUE))), 
    priors_omega = list(list(name = list(containsColumn = TRUE)), 
                        list(name = list(containsColumn = TRUE))), priors_omega_null = list(
                          list(name = list(containsColumn = TRUE))), priors_tau = list(
                            list(name = list(containsColumn = TRUE))), priors_tau_null = list(
                              list(name = list(containsColumn = TRUE))))
  options$advanced_control <- "clever"
  options$bayesFactorType <- "BF01"
  options$diagnostics_single_model <- 12
  options$diagnostics_transformed <- FALSE
  options$fitted_path <- fitted_path
  options$input_CI <- list()
  options$measures <- "fitted"
  options$plots_mu <- TRUE
  options$plots_omega <- TRUE
  options$plots_priors <- FALSE
  options$plots_tau <- TRUE
  options$plots_theta <- TRUE
  options$plots_theta_order <- "ascending"
  options$plots_theta_show <- "both"
  options$plots_type_individual_by <- "model"
  options$plots_type_individual_order <- "ascending"
  options$priors_mu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$priors_mu_null <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priors_omega <- list(list(name = "", parAlpha = "(1,1)", parAlpha1 = "(1,1,1)", 
                                    parAlpha2 = "(1,1)", parCuts = "(.05)", priorOdds = "1/2", 
                                    type = "Two-sided"), list(name = "2", parAlpha = "(1,1,1)", 
                                                              parAlpha1 = "(1,1,1)", parAlpha2 = "(1,1)", parCuts = "(.05, .10)", 
                                                              priorOdds = "1/2", type = "Two-sided"))
  options$priors_omega_null <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .10)", priorOdds = "1", 
                                         type = "spike"))
  options$priors_tau <- list(list(`	` = "1", name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale2 = "1", parShape = "1", priorOdds = "1", truncationLower = "0", 
                                  truncationUpper = "Inf", type = "invgamma"))
  options$priors_tau_null <- list(list(`	` = "1", name = "", parA = "0", parAlpha = "1", parB = "1", 
                                       parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                       parScale2 = "1", parShape = "1", priorOdds = "1", truncationLower = "0", 
                                       truncationUpper = "Inf", type = "spike"))
  options$results_CI <- 0.8
  options$results_individual_single <- TRUE
  options$results_individual_single_number <- 12
  options$results_models_BF <- "inclusion"
  options$results_models_order <- "default"
  options$save_path <- ""
  set.seed(1)
  dataset <- NULL
  results <- jasptools::run("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match (different CI + BF01)", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0.113189082589328, 0.152208200580981, 0.152208200580981, "Effect size (<unicode><unicode>)",
                             0.189074473607484, 0, 0, 0, "Heterogeneity (<unicode><unicode>)",
                             0.048039183915011))
  })
  
  test_that("Model Averaged Weights (Ď‰) table results match (different CI + BF01)", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_weights"]][["data"]]
    expect_equal_tables(table,
                        list(1, 0, 1, 1, 1, 0.05, 0.181151141450075, 0.05, 0.505983624767411,
                             0.505983624767411, 0.867889982664734, 0.1, 0.0390524447569886,
                             0.1, 0.0986787722822264, 0.0986787722822264, 0.227823462705588,
                             1))
  })
  
  test_that("Model Summary table results match (different CI + BF01)", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list(8.66024192717725e-08, "6/12", 0.999999913397588, 0.5, "Effect",
                             6.30608424377591, "6/12", 0.13687222411265, 0.5, "Heterogeneity",
                             0.00189004352774, "8/12", 0.998113521997798, 0.5, "Publication bias"
                        ))
  })
  
  test_that("Effect size (Model Averaged) plot matches (no prior)", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-model-averaged-5", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Effect size vs Heterogeneity (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mutau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-vs-heterogeneity-model-averaged-5", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weight function (Model Averaged) plot matches  (no prior)", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_omega"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-function-model-averaged-5", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Model Averaged) plot matches  (no prior)", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-model-averaged-5", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Forest plot (Model Averaged) matches  (observed + predicted)", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-model-averaged-5", dir="RobustBayesianMetaAnalysis")
  })
}
{
  options <- jasptools::analysisOptions("RobustBayesianMetaAnalysis")
  options$.meta <- list(input_CI = list(containsColumn = TRUE), input_ES = list(
    containsColumn = TRUE), input_N = list(containsColumn = TRUE), 
    input_N1 = list(containsColumn = TRUE), input_N2 = list(containsColumn = TRUE), 
    input_SE = list(containsColumn = TRUE), input_labels = list(
      containsColumn = TRUE), input_t = list(containsColumn = TRUE), 
    priors_mu = list(list(name = list(containsColumn = TRUE))), 
    priors_mu_null = list(list(name = list(containsColumn = TRUE))), 
    priors_omega = list(list(name = list(containsColumn = TRUE)), 
                        list(name = list(containsColumn = TRUE))), priors_omega_null = list(
                          list(name = list(containsColumn = TRUE))), priors_tau = list(
                            list(name = list(containsColumn = TRUE))), priors_tau_null = list(
                              list(name = list(containsColumn = TRUE))))
  options$advanced_control <- "clever"
  options$bayesFactorType <- "LogBF10"
  options$diagnostics_single_model <- 12
  options$diagnostics_transformed <- FALSE
  options$fitted_path <- fitted_path
  options$input_CI <- list()
  options$measures <- "fitted"
  options$plots_individual_mu <- TRUE
  options$plots_individual_omega <- TRUE
  options$plots_individual_tau <- TRUE
  options$plots_priors <- FALSE
  options$plots_theta <- TRUE
  options$plots_theta_order <- "descending"
  options$plots_theta_show <- "observed"
  options$plots_type_individual_by <- "prob"
  options$plots_type_individual_conditional <- FALSE
  options$plots_type_individual_order <- "descending"
  options$priors_mu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$priors_mu_null <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priors_omega <- list(list(name = "", parAlpha = "(1,1)", parAlpha1 = "(1,1,1)", 
                                    parAlpha2 = "(1,1)", parCuts = "(.05)", priorOdds = "1/2", 
                                    type = "Two-sided"), list(name = "2", parAlpha = "(1,1,1)", 
                                                              parAlpha1 = "(1,1,1)", parAlpha2 = "(1,1)", parCuts = "(.05, .10)", 
                                                              priorOdds = "1/2", type = "Two-sided"))
  options$priors_omega_null <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .10)", priorOdds = "1", 
                                         type = "spike"))
  options$priors_tau <- list(list(`	` = "1", name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale2 = "1", parShape = "1", priorOdds = "1", truncationLower = "0", 
                                  truncationUpper = "Inf", type = "invgamma"))
  options$priors_tau_null <- list(list(`	` = "1", name = "", parA = "0", parAlpha = "1", parB = "1", 
                                       parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                       parScale2 = "1", parShape = "1", priorOdds = "1", truncationLower = "0", 
                                       truncationUpper = "Inf", type = "spike"))
  options$results_CI <- 0.8
  options$results_individual_single <- TRUE
  options$results_individual_single_number <- 12
  options$results_models_BF <- "inclusion"
  options$results_models_order <- "default"
  options$save_path <- ""
  set.seed(1)
  dataset <- NULL
  results <- jasptools::run("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match (log BF & different CI)", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0.113189082589328, 0.152208200580981, 0.152208200580981, "Effect size (<unicode><unicode>)",
                             0.189074473607484, 0, 0, 0, "Heterogeneity (<unicode><unicode>)",
                             0.048039183915011))
  })
  
  test_that("Model Averaged Weights (Ď‰) table results match (log BF & different CI)", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_weights"]][["data"]]
    expect_equal_tables(table,
                        list(1, 0, 1, 1, 1, 0.05, 0.181151141450075, 0.05, 0.505983624767411,
                             0.505983624767411, 0.867889982664734, 0.1, 0.0390524447569886,
                             0.1, 0.0986787722822264, 0.0986787722822264, 0.227823462705588,
                             1))
  })
  
  test_that("Model Summary table results match (log BF & different CI)", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list(16.2619380856046, "6/12", 0.999999913397588, 0.5, "Effect", -1.84151492033841,
                             "6/12", 0.13687222411265, 0.5, "Heterogeneity", 6.27115541962552,
                             "8/12", 0.998113521997798, 0.5, "Publication bias"))
  })
  
  test_that("Forest plot (Model Averaged) matches (observed, descending)", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-model-averaged-6", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Effect size (Models) plot matches (all models, descending by post. prob)", {
    plotName <- results[["results"]][["plots_individual"]][["collection"]][["plots_individual_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-models-6", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weights [2] (Models) plot matches (all models, descending by post. prob)", {
    plotName <- results[["results"]][["plots_individual"]][["collection"]][["plots_individual_omega"]][["collection"]][["plots_individual_omega_plot_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "model-weights-plot-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weights [3] (Models) plot matches (all models, descending by post. prob)", {
    plotName <- results[["results"]][["plots_individual"]][["collection"]][["plots_individual_omega"]][["collection"]][["plots_individual_omega_plot_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "model-weights-plot-2", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Models) plot matches (all models, descending by post. prob)", {
    plotName <- results[["results"]][["plots_individual"]][["collection"]][["plots_individual_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-models-6", dir="RobustBayesianMetaAnalysis")
  })
}