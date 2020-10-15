context("Robust Bayesian Meta-Analysis")

skip("Travis limit reached")

### create a pre-fitted model (in case that the package needs to be updated)
if (FALSE){
  # fit a default model
  fit <- RoBMA::RoBMA(d = c(.3, .2, .1), n = c(30, 35, 40), iter = 4000, burnin = 4000, chains = 2, control = list(silent = TRUE), seed = 666)
  # remove majority of the samples to save space
  for(i in 2:length(fit$models)){
    for(j in seq_along(fit$models[[i]]$fit$mcmc)){
      fit$models[[i]]$fit$mcmc[[j]] <- fit$models[[i]]$fit$mcmc[[j]][1:100,]
    }
  }
  set.seed(666)
  for(i in 1:2){
    for(p in c("mu", "tau")){
      fit$RoBMA$samples[[i]][[p]] <- sample(fit$RoBMA$samples[[i]][[p]], 100)
    }
    for(p in c("omega", "theta")){
      fit$RoBMA$samples[[i]][[p]] <- fit$RoBMA$samples[[i]][[p]][sample(nrow(fit$RoBMA$samples[[i]][[p]]), 100),]
    }
  }
  saveRDS(fit, file = "../jasp-desktop/Tests/R/tests/testthat/robmaFit.RDS")
}

# path to the pre-fitted RoBMA model
fitted_path <- file.path(jasptools:::.pkgOptions$tests.dir, "robmaFit.RDS")

### prior distibutions plots 
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
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
  results <- jaspTools::run("RobustBayesianMetaAnalysis", dataset, options)
  
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
  
  test_that("Priors plot mu (1) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_alternative"]][["collection"]][["prior_plots_mu_alternative_mu_alternative_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-2-default", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (2) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_alternative"]][["collection"]][["prior_plots_mu_alternative_mu_alternative_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-3-default", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (3) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_alternative"]][["collection"]][["prior_plots_mu_alternative_mu_alternative_3"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-4-default", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (4) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_alternative"]][["collection"]][["prior_plots_mu_alternative_mu_alternative_4"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-5-default", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (5) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_alternative"]][["collection"]][["prior_plots_mu_alternative_mu_alternative_5"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-6-default", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (6) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_alternative"]][["collection"]][["prior_plots_mu_alternative_mu_alternative_6"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-7-default", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (7) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_alternative"]][["collection"]][["prior_plots_mu_alternative_mu_alternative_7"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-8-default", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (8) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_alternative"]][["collection"]][["prior_plots_mu_alternative_mu_alternative_8"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-9-default", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot omega (1) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_omega"]][["collection"]][["prior_plots_omega_alternative"]][["collection"]][["prior_plots_omega_alternative_omega_alternative_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-10-default", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot omega (2) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_omega"]][["collection"]][["prior_plots_omega_alternative"]][["collection"]][["prior_plots_omega_alternative_omega_alternative_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-11-default", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot tau (1) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_tau"]][["collection"]][["prior_plots_tau_alternative"]][["collection"]][["prior_plots_tau_alternative_tau_alternative_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-12-default", dir="RobustBayesianMetaAnalysis")
  })
  
}
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
  options$advanced_control <- "clever"
  options$advanced_mu_transform <- "cohens_d"
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
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "3", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
  options$priors_mu_null <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priors_omega <- list()
  options$priors_omega_null <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .95)", priorOdds = "1", 
                                         type = "spike"))
  options$priors_plot <- TRUE
  options$priors_tau <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                  truncationLower = "0", truncationUpper = "Inf", type = "invgamma"))
  options$priors_tau_null <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                       parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                       parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                       truncationLower = "0", truncationUpper = "Inf", type = "spike"))
  options$results_models_BF <- "inclusion"
  options$results_models_order <- "default"
  options$save_path <- ""
  set.seed(1)
  dataset <- NULL
  results <- jaspTools::run("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Models Overview table results match", {
    table <- results[["results"]][["model_preview"]][["collection"]][["model_preview_models_summary"]][["data"]]
    expect_equal_tables(table,
                        list(1, 0.166666666666667, "Spike(0)", "Spike(1)", "Spike(0)", 2, 0.166666666666667,
                             "Spike(0)", "Spike(1)", "InvGamma(1, 0.15)[0, Inf]", 3, 0.166666666666667,
                             "Normal(0, 1)[-Inf, Inf]", "Spike(1)", "Spike(0)", 4, 0.166666666666667,
                             "Normal(0, 1)[-Inf, Inf]", "Spike(1)", "InvGamma(1, 0.15)[0, Inf]",
                             5, 0.166666666666667, "Normal(3, 1)[-Inf, Inf]", "Spike(1)",
                             "Spike(0)", 6, 0.166666666666667, "Normal(3, 1)[-Inf, Inf]",
                             "Spike(1)", "InvGamma(1, 0.15)[0, Inf]"))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["model_preview"]][["collection"]][["model_preview_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list("4/6", 0.666666666666667, "Effect", "3/6", 0.5, "Heterogeneity",
                             "0/6", 0, "Publication bias"))
  })
  
  test_that("Priors plot mu (1) (correlation) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_alternative"]][["collection"]][["prior_plots_mu_alternative_mu_alternative_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-2-correlations", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (2) (correlation) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_alternative"]][["collection"]][["prior_plots_mu_alternative_mu_alternative_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-3-correlations", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (3) (correlation) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_null"]][["collection"]][["prior_plots_mu_null_mu_null_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-4-correlations", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot omega (1) (correlation) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_omega"]][["collection"]][["prior_plots_omega_null"]][["collection"]][["prior_plots_omega_null_omega_null_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-5-correlations", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot tau (1) (correlation) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_tau"]][["collection"]][["prior_plots_tau_alternative"]][["collection"]][["prior_plots_tau_alternative_tau_alternative_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-6-correlations", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot tau (2) (correlation) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_tau"]][["collection"]][["prior_plots_tau_null"]][["collection"]][["prior_plots_tau_null_tau_null_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-7-correlations", dir="RobustBayesianMetaAnalysis")
  })
}
{
  options <- jasptools::analysisOptions("RobustBayesianMetaAnalysis")
  options$advanced_control <- "clever"
  options$advanced_mu_transform <- "log_OR"
  options$fitted_path <- ""
  options$input_CI <- list()
  options$measures <- "OR"
  options$plots_theta_order <- "labels"
  options$plots_theta_show <- "observed"
  options$plots_type_individual_by <- "model"
  options$plots_type_individual_order <- "ascending"
  options$priors_mu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = ".3", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"), 
                            list(name = "2", parA = "-.10", parAlpha = "1", parB = ".10", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "3", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "uniform"))
  options$priors_mu_null <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                      parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                      parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                      truncationLower = "-Inf", truncationUpper = "Inf", type = "spike"))
  options$priors_null <- TRUE
  options$priors_omega <- list()
  options$priors_omega_null <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .95)", priorOdds = "1", 
                                         type = "spike"))
  options$priors_plot <- TRUE
  options$priors_tau <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                  truncationLower = "0", truncationUpper = "Inf", type = "invgamma"))
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
                        list(1, 0.333333333333333, "Spike(0)", "Spike(1)", "InvGamma(1, 0.15)[0, Inf]",
                             2, 0.333333333333333, "Normal(0, 0.3)[-Inf, Inf]", "Spike(1)",
                             "InvGamma(1, 0.15)[0, Inf]", 3, 0.333333333333333, "Uniform(-0.1, 0.1)",
                             "Spike(1)", "InvGamma(1, 0.15)[0, Inf]"))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["model_preview"]][["collection"]][["model_preview_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list("2/3", 0.666666666666667, "Effect", "3/3", 1, "Heterogeneity",
                             "0/3", 0, "Publication bias"))
  })
  
  test_that("Priors plot mu (1) (OR) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_alternative"]][["collection"]][["prior_plots_mu_alternative_mu_alternative_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-2-OR", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (2) (OR) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_alternative"]][["collection"]][["prior_plots_mu_alternative_mu_alternative_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-3-OR", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot mu (3) (OR) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_mu"]][["collection"]][["prior_plots_mu_null"]][["collection"]][["prior_plots_mu_null_mu_null_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-4-OR", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot omega (1) (OR) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_omega"]][["collection"]][["prior_plots_omega_null"]][["collection"]][["prior_plots_omega_null_omega_null_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-5-OR", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Priors plot tau (1) (OR) matches", {
    plotName <- results[["results"]][["prior_plots"]][["collection"]][["prior_plots_tau"]][["collection"]][["prior_plots_tau_alternative"]][["collection"]][["prior_plots_tau_alternative_tau_alternative_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "prior-plot-6-OR", dir="RobustBayesianMetaAnalysis")
  })
}

### fit a default model using d + se, (wihout the more complex weight function) and main output
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
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
  options$setSeed   <- TRUE
  set.seed(1)
  dataset <- structure(list(study = structure(c(1L, 3L, 2L), .Label = c("study one", 
                                                                        "study three", "study two"), class = "factor"), t = c(2.51, 2.39, 
                                                                                                                              2.55), N = c(100L, 150L, 97L), d = c(0.25, 0.2, 0.26), se = c(0.1, 
                                                                                                                                                                                            0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L, 75L, 48L), lCI = c(0.05, 
                                                                                                                                                                                                                                                              0.04, 0.06), uCI = c(0.45, 0.41, 0.38)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                           -3L))
  results <- jaspTools::run("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0, 0.168712291757783, 0.189827028815157, "Effect size (<unicode><unicode>)",
                             0.337208378977972, 0, 0.0480806513171957, 0, "Heterogeneity (<unicode><unicode>)",
                             0.312443196435157))
  })
  
  test_that("Model Averaged Weights (omega) table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_weights"]][["data"]]
    expect_equal_tables(table,
                        list(1, 0, 1, 1, 1, 0.05, 0.00605631178628108, 0.05, 0.562366380513431,
                             0.56391916627436, 1, 1))
  })
  
  test_that("Conditional Estimates table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_conditional_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0.0572361924282677, 0.206723023237374, 0.210186930065265, "Effect size (<unicode><unicode>)",
                             0.34044908725073, 0.0347697656712412, 0.154449387376258, 0.122302414943436,
                             "Heterogeneity (<unicode><unicode>)", 0.461332883262839))
  })
  
  test_that("Conditional Weights (omega) table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_conditional_weights"]][["data"]]
    expect_equal_tables(table,
                        list(1, 0, 1, 1, 1, 0.05, 0.00303360000468365, 0.05, 0.281089511980057,
                             0.183217040722455, 0.917228230024058, 1))
  })
  
  test_that("Models Overview table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_models_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0.00218093631921237, -12.3417718561692, 1, 0.000435997087465849,
                             0.166666666666667, "Spike(0)", "Spike(1)", "Spike(0)", 0.523350520352504,
                             -7.00262896601826, 2, 0.0454165235560754, 0.0833333333333333,
                             "Spike(0)", "Two-sided((0.05), (1, 1))", "Spike(0)", 0.179575873688028,
                             -7.96577750238348, 3, 0.0346699957809797, 0.166666666666667,
                             "Spike(0)", "Spike(1)", "InvGamma(1, 0.15)[0, Inf]", 1.30334729581458,
                             -6.1556852459012, 4, 0.105934366028825, 0.0833333333333333,
                             "Spike(0)", "Two-sided((0.05), (1, 1))", "InvGamma(1, 0.15)[0, Inf]",
                             1.96675398250927, -5.86866180586203, 5, 0.282305645849847, 0.166666666666667,
                             "Normal(0, 1)[-Inf, Inf]", "Spike(1)", "Spike(0)", 6.04357955617748,
                             -4.94752676988312, 6, 0.354595672596663, 0.0833333333333333,
                             "Normal(0, 1)[-Inf, Inf]", "Two-sided((0.05), (1, 1))", "Spike(0)",
                             0.402517316496111, -7.20077903677421, 7, 0.0745055115819915,
                             0.166666666666667, "Normal(0, 1)[-Inf, Inf]", "Spike(1)", "InvGamma(1, 0.15)[0, Inf]",
                             1.25130256082422, -6.19219688626355, 8, 0.102136287518153, 0.0833333333333333,
                             "Normal(0, 1)[-Inf, Inf]", "Two-sided((0.05), (1, 1))", "InvGamma(1, 0.15)[0, Inf]"
                        ))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list(4.36317022381951, "4/8", 0.813543117546654, 0.5, "Effect", 0.464656722154449,
                             "4/8", 0.317246160909949, 0.5, "Heterogeneity", 3.10311936711015,
                             "4/8", 0.608082849699716, 0.333333333333333, "Publication bias"
                        ))
  })
  
  test_that("Effect size (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-model-averaged-default-model", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weight function (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_omega"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-function-model-averaged-default-model", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-model-averaged-default-model", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Forest plot (Model Averaged) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-model-averaged-default-model", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Effect size (Conditional Models) plot matches", {
    plotName <- results[["results"]][["plots_individual"]][["collection"]][["plots_individual_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-conditional-models-default-model", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weights (Conditional Models) plot matches", {
    plotName <- results[["results"]][["plots_individual"]][["collection"]][["plots_individual_omega"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weights-conditional-models-default-model", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Conditional Models) plot matches", {
    plotName <- results[["results"]][["plots_individual"]][["collection"]][["plots_individual_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-conditional-models-default-model", dir="RobustBayesianMetaAnalysis")
  })
  
}

### fit models with a truncated priors and t + se
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
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
  options$setSeed <- TRUE
  set.seed(1)
  dataset <- structure(list(study = structure(c(1L, 3L, 2L), .Label = c("study one", 
                                                                        "study three", "study two"), class = "factor"), t = c(2.51, 2.39, 
                                                                                                                              2.55), N = c(100L, 150L, 97L), d = c(0.25, 0.2, 0.26), se = c(0.1, 
                                                                                                                                                                                            0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L, 75L, 48L), lCI = c(0.05, 
                                                                                                                                                                                                                                                              0.04, 0.06), uCI = c(0.45, 0.41, 0.38)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                           -3L))
  results <- jaspTools::run("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0, 0.535604763915645, 0.559463132312447, "Effect size (<unicode><unicode>)",
                             0.828987540642641, 0, 0.107300145478871, 0, "Heterogeneity (<unicode><unicode>)",
                             0.458067108966629))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list(9.47365692566878, "2/4", 0.904522364337789, 0.5, "Effect", 0.455716903634993,
                             "2/4", 0.313053247164367, 0.5, "Heterogeneity", "", "0/4", 0,
                             0, "Publication bias"))
  })
  
  test_that("Effect size (Conditional) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-conditional-truncated-priors", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Conditional) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-conditional-truncated-priors", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Forest plot (Conditional) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-conditional-truncated-priors", dir="RobustBayesianMetaAnalysis")
  })
}

### fit models with only an effect size, d + (N1 + N2) and names
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
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
  options$setSeed <- TRUE
  set.seed(1)
  dataset <- structure(list(study = structure(c(1L, 3L, 2L), .Label = c("study one", 
                                                                        "study three", "study two"), class = "factor"), t = c(2.51, 2.39, 
                                                                                                                              2.55), N = c(100L, 150L, 97L), d = c(0.25, 0.2, 0.26), se = c(0.1, 
                                                                                                                                                                                            0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L, 75L, 48L), lCI = c(0.05, 
                                                                                                                                                                                                                                                              0.04, 0.06), uCI = c(0.45, 0.41, 0.38)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                           -3L))
  results <- jaspTools::run("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0, 0.0831886793185899, 0.0944310887618068, "Effect size (<unicode><unicode>)",
                             0.147425398292625, 0, 0, 0, "Heterogeneity (<unicode><unicode>)",
                             0))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list(4.33614576895865, "1/2", 0.812598822577677, 0.5, "Effect", "",
                             "0/2", 0, 0, "Heterogeneity", "", "0/2", 0, 0, "Publication bias"
                        ))
  })
  
  test_that("Effect size (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-model-averaged-d-and-n", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-model-averaged-d-and-n", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Forest plot (Model Averaged) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-model-averaged-d-and-n", dir="RobustBayesianMetaAnalysis")
  })
}

### fit models with only one publication bias function, y + (lCI & uCI)
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
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
  options$input_CI <- list(c("lCI", "uCI"))
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
  options$setSeed <- TRUE
  set.seed(1)
  dataset <- structure(list(study = structure(c(1L, 3L, 2L), .Label = c("study one", 
                                                                        "study three", "study two"), class = "factor"), t = c(2.51, 2.39, 
                                                                                                                              2.55), N = c(100L, 150L, 97L), d = c(0.25, 0.2, 0.26), se = c(0.1, 
                                                                                                                                                                                            0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L, 75L, 48L), lCI = c(0.05, 
                                                                                                                                                                                                                                                              0.04, 0.06), uCI = c(0.45, 0.41, 0.38)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                           -3L))
  results <- jaspTools::run("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0, 0, 0, "Effect size (<unicode><unicode>)", 0, 0, 0, 0, "Heterogeneity (<unicode><unicode>)",
                             0))
  })
  
  test_that("Model Averaged Weights (omega) table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_weights"]][["data"]]
    expect_equal_tables(table,
                        list(1, 0, 1, 1, 1, 0.05, 0.000674752411048917, 0.05, 0.0614219245352222,
                             0.0223643133148829, 0.389613651090792, 1))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list("", "0/2", 0, 0, "Effect", "", "0/2", 0, 0, "Heterogeneity", 208.334068560185,
                             "1/2", 0.990491316914607, 0.333333333333333, "Publication bias"
                        ))
  })
  
  test_that("Effect size (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-model-averaged-y-CI", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weight function (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_omega"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-function-model-averaged-y-CI", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-model-averaged-y-CI", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Forest plot (Model Averaged) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-model-averaged-y-CI", dir="RobustBayesianMetaAnalysis")
  })
}

### fit models with OR
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
  options$advanced_adapt <- 100
  options$advanced_burnin <- 1000
  options$advanced_chains <- 2
  options$advanced_control <- "clever"
  options$advanced_iteration <- 4000
  options$advanced_mu_transform <- "cohens_d"
  options$fitted_path <- ""
  options$input_CI <- list(c("ORlCI", "ORuCI"))
  options$input_ES <- "OR"
  options$measures <- "OR"
  options$plots_individual_mu <- TRUE
  options$plots_mu <- TRUE
  options$plots_priors <- FALSE
  options$plots_tau <- TRUE
  options$plots_theta_order <- "labels"
  options$plots_theta_show <- "observed"
  options$plots_type_individual_by <- "model"
  options$plots_type_individual_conditional <- FALSE
  options$plots_type_individual_order <- "ascending"
  options$priors_mu <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                 parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                 parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                 truncationLower = "-Inf", truncationUpper = "Inf", type = "normal"))
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
  options$priors_tau <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                  truncationLower = "0", truncationUpper = "Inf", type = "invgamma"))
  options$priors_tau_null <- list()
  options$results_models_BF <- "inclusion"
  options$results_models_order <- "default"
  options$save_path <- ""
  options$setSeed <- TRUE
  set.seed(1)
  dataset <- structure(list(study = c("study one", "study two", "study three"
  ), t = c(2.51, 2.39, 2.55), N = c(100L, 150L, 97L), d = c(0.25, 
                                                            0.2, 0.26), se = c(0.1, 0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L, 
                                                                                                                              75L, 48L), lCI = c(0.05, 0.04, 0.06), uCI = c(0.45, 0.41, 0.38
                                                                                                                              ), OR = c(1.1, 1.05, 1.2), ORlCI = c(1, 0.98, 1.1), ORuCI = c(1.2, 
                                                                                                                                                                                            1.1, 1.3)), class = "data.frame", row.names = c(NA, -3L))
  results <- jaspTools::run("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_summary"]][["data"]]
    expect_equal_tables(table,
                        list(1, 1.01527374333754, 1, "Effect size (OR)", 1.18518278022787,
                             0.0358341967386345, 0.0984568324014358, 0.082166145661641, "Heterogeneity (<unicode><unicode>)",
                             0.252094438158819))
  })
  
  test_that("Model Averaged Weights (Ď‰) table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_weights"]][["data"]]
    expect_equal_tables(table,
                        list(1, 0, 1, 1, 1, 0.05, 0.122013680867879, 0.05, 0.842151031560929,
                             1, 1, 1))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0.132496130369426, "2/4", 0.116994775360693, 0.5, "Effect", "",
                             "4/4", 1, 1, "Heterogeneity", 1.01423543293882, "2/4", 0.336481822838224,
                             0.333333333333333, "Publication bias"))
  })
  
  test_that("Effect size (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-model-averaged-OR", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Effect size vs Heterogeneity (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mutau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-vs-heterogeneity-model-averaged-OR", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-model-averaged-OR", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Effect size (Models) plot matches", {
    plotName <- results[["results"]][["plots_individual"]][["collection"]][["plots_individual_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-models-OR", dir="RobustBayesianMetaAnalysis")
  })
}

### fit models with expected negative effect sizes
{
  options <- jasptools::analysisOptions("RobustBayesianMetaAnalysis")
  options$advanced_adapt <- 100
  options$advanced_burnin <- 1000
  options$advanced_chains <- 2
  options$advanced_control <- "clever"
  options$advanced_iteration <- 4000
  options$advanced_mu_transform <- "cohens_d"
  options$effect_direction <- "negative"
  options$fitted_path <- ""
  options$input_CI <- list()
  options$input_ES <- "d"
  options$input_SE <- "se"
  options$measures <- "general"
  options$plots_mu <- TRUE
  options$plots_priors <- FALSE
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
  options$priors_null <- TRUE
  options$priors_omega <- list(list(name = "", parAlpha = "(1,1)", parAlpha1 = "(1,1)", 
                                    parAlpha2 = "(1,1)", parCuts = "(.05)", priorOdds = "1/2", 
                                    type = "Two-sided"))
  options$priors_omega_null <- list(list(name = "", parAlpha = "(1,1,1)", parAlpha1 = "(1,1)", 
                                         parAlpha2 = "(1,1)", parCuts = "(.05, .95)", priorOdds = "1", 
                                         type = "spike"))
  options$priors_tau <- list(list(name = "", parA = "0", parAlpha = "1", parB = "1", 
                                  parBeta = "0.15", parDF = "2", parLocation = "0", parMean = "0", 
                                  parScale = "1", parScale2 = "1", parShape = "1", priorOdds = "1", 
                                  truncationLower = "0", truncationUpper = "Inf", type = "invgamma"))
  options$priors_tau_null <- list()
  options$results_models_BF <- "inclusion"
  options$results_models_order <- "default"
  options$save_path <- ""
  options$setSeed <- TRUE
  set.seed(1)
  dataset <- structure(list(study = c("study one", "study two", "study three"
  ), t = c(2.51, 2.39, 2.55), N = c(100L, 150L, 97L), d = c(0.25, 
                                                            0.2, 0.26), se = c(0.1, 0.08, 0.1), N1 = c(50L, 75L, 49L), N2 = c(50L, 
                                                                                                                              75L, 48L), lCI = c(0.05, 0.04, 0.06), uCI = c(0.45, 0.41, 0.38
                                                                                                                              ), OR = c(1.1, 1.05, 1.2), ORlCI = c(1, 0.98, 1.1), ORuCI = c(1.2, 
                                                                                                                                                                                            1.1, 1.3)), class = "data.frame", row.names = c(NA, -3L))
  results <- jasptools::run("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0, 0.118084257741258, 0.0974468746788708, "Effect size (<unicode><unicode>)",
                             0.371445404295019, 0.0360039463828572, 0.152105773972153, 0.11831544827935,
                             "Heterogeneity (<unicode><unicode>)", 0.453723802791823))
  })
  
  test_that("Model Averaged Weights (omega) table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_weights"]][["data"]]
    expect_equal_tables(table,
                        list(1, 0, 1, 1, 1, 0.05, 0.00532256931870136, 0.05, 0.511524704078504,
                             0.411251834300105, 1, 1))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list(1.2900164973612, "2/4", 0.563321923159808, 0.5, "Effect", "",
                             "4/4", 1, 1, "Heterogeneity", 3.73217626824764, "2/4", 0.651092376366959,
                             0.333333333333333, "Publication bias"))
  })
  
  test_that("Effect size (Model Averaged) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-model-averaged-negative-ES", dir="RobustBayesianMetaAnalysis")
  })
}

### more options tested using a preloaded model
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
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
  results <- jaspTools::run("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Diagnostics autocorrelations (mu) plot matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_mu"]][["collection"]][["diagnostics_model_12_mu_autocor"]][["collection"]][["diagnostics_model_12_mu_autocor_autocor_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-0", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics samples (mu) plot matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_mu"]][["collection"]][["diagnostics_model_12_mu_samples"]][["collection"]][["diagnostics_model_12_mu_samples_samples_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics traceplot (mu) plot  matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_mu"]][["collection"]][["diagnostics_model_12_mu_trace"]][["collection"]][["diagnostics_model_12_mu_trace_trace_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-2", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics autocorrelations (omega 1) plot  matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_autocor"]][["collection"]][["diagnostics_model_12_omega_autocor_autocor_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-3", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics autocorrelations (omega 2) plot ", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_autocor"]][["collection"]][["diagnostics_model_12_omega_autocor_autocor_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-4", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics samples (omega 1) plot  matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_samples"]][["collection"]][["diagnostics_model_12_omega_samples_samples_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-5", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics samples (omega 2) plotmatches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_samples"]][["collection"]][["diagnostics_model_12_omega_samples_samples_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-6", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics traceplot (omega 1) plot matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_trace"]][["collection"]][["diagnostics_model_12_omega_trace_trace_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-7", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics traceplot (omega 2) plot matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_omega"]][["collection"]][["diagnostics_model_12_omega_trace"]][["collection"]][["diagnostics_model_12_omega_trace_trace_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-8", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics autocorrelation (tau) plot matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_tau"]][["collection"]][["diagnostics_model_12_tau_autocor"]][["collection"]][["diagnostics_model_12_tau_autocor_autocor_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-9", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics samples (tau) matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_tau"]][["collection"]][["diagnostics_model_12_tau_samples"]][["collection"]][["diagnostics_model_12_tau_samples_samples_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-10", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Diagnostics traceplot (tau) matches", {
    plotName <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_model_12"]][["collection"]][["diagnostics_model_12_tau"]][["collection"]][["diagnostics_model_12_tau_trace"]][["collection"]][["diagnostics_model_12_tau_trace_trace_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "diagnostics-prefitted-1-titleless-plot-11", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Models Diagnostics Overview table results match", {
    table <- results[["results"]][["diagnostics"]][["collection"]][["diagnostics_models_diagnostics"]][["data"]]
    expect_equal_tables(table,
                        list("", "", "", 1, "Spike(0)", "Spike(1)", "Spike(0)", 2640, 1.00003411674087,
                             0.00499106325851357, 2, "Spike(0)", "Two-sided((0.05), (1, 1))",
                             "Spike(0)", 2065, 1.00018724141735, 0.00457281380618246, 3,
                             "Spike(0)", "Two-sided((0.1, 0.05), (1, 1, 1))", "Spike(0)",
                             1314, 1.00140746545812, 0.00403857529523985, 4, "Spike(0)",
                             "Spike(1)", "InvGamma(1, 0.15)[0, Inf]", 2256, 1.00093573490301,
                             0.00476716834572168, 5, "Spike(0)", "Two-sided((0.05), (1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]", 1599, 1.00543812368937, 0.00453407225202122,
                             6, "Spike(0)", "Two-sided((0.1, 0.05), (1, 1, 1))", "InvGamma(1, 0.15)[0, Inf]",
                             4856, 0.99996990865339, 0.00271797365574783, 7, "Normal(0, 1)[-Inf, Inf]",
                             "Spike(1)", "Spike(0)", 2166, 1.00020484967232, 0.00526137911926847,
                             8, "Normal(0, 1)[-Inf, Inf]", "Two-sided((0.05), (1, 1))", "Spike(0)",
                             2173, 1.00057613895391, 0.0043135085739478, 9, "Normal(0, 1)[-Inf, Inf]",
                             "Two-sided((0.1, 0.05), (1, 1, 1))", "Spike(0)", 658, 1.00131079141925,
                             0.00887624435526559, 10, "Normal(0, 1)[-Inf, Inf]", "Spike(1)",
                             "InvGamma(1, 0.15)[0, Inf]", 686, 1.00103786173391, 0.00832076930050759,
                             11, "Normal(0, 1)[-Inf, Inf]", "Two-sided((0.05), (1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]", 650, 1.0006477761088, 0.0089700481583478,
                             12, "Normal(0, 1)[-Inf, Inf]", "Two-sided((0.1, 0.05), (1, 1, 1))",
                             "InvGamma(1, 0.15)[0, Inf]"))
  })
  
  test_that("Model Estimates table results match", {
    table <- results[["results"]][["individual_models"]][["collection"]][["individual_models_model_12"]][["collection"]][["individual_models_model_12_temp_coef"]][["data"]]
    expect_equal_tables(table,
                        list(0.0089700481583478, 650, -0.27577848560309, 0.156953845294782,
                             0.15201967236391, 1.00062065346842, "Effect size (<unicode><unicode>)",
                             0.62325922718054, 0.00460957412865408, 1495, 0.0367527372164884,
                             0.184712184393409, 0.131520521597414, 1.00015376926195, "Heterogeneity (<unicode><unicode>)",
                             0.64396175120006))
  })
  
  test_that("Information table results match", {
    table <- results[["results"]][["individual_models"]][["collection"]][["individual_models_model_12"]][["collection"]][["individual_models_model_12_temp_info"]][["data"]]
    expect_equal_tables(table,
                        list(0.176759472366701, -5.90832379591304, 0.0116467202823197, 0.0625
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
                        list(0.00852024439881299, 735, -0.255923008947252, 0.176748964322513,
                             0.167154704813216, 1.00091629944768, "Study 1", 0.663245585745701,
                             0.00809948454702872, 749, -0.274041610154144, 0.160779535771706,
                             0.157944785429571, 1.00068785496871, "Study 2", 0.612301087856998,
                             0.00807508788266646, 709, -0.280653418186545, 0.136876373175994,
                             0.134293585298117, 1.00066087953453, "Study 3", 0.561929530379696
                        ))
  })
  
  test_that("Estimated Weights (Ď‰) table results match", {
    table <- results[["results"]][["individual_models"]][["collection"]][["individual_models_model_12"]][["collection"]][["individual_models_model_12_temp_weights"]][["data"]]
    expect_equal_tables(table,
                        list("<unicode><unicode><unicode>", 0, 1, 0, 1, 1, 0.999981156237916,
                             1, 0.05, 0.00447268814768601, 1929, 0.289090563535792, 0.05,
                             0.736649016128691, 0.774405065576981, 1.00039941621822, 0.990080716733073,
                             0.1, 0.00415766999088018, 2644, 0.119733401287344, 0.1, 0.490236116608678,
                             0.485039880388114, 1.0006477761088, 0.89768722132576, 1))
  })
  
  test_that("Model Averaged Estimates table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_summary"]][["data"]]
    expect_equal_tables(table,
                        list(-0.104773490907481, 0.038323039556351, 0, "Effect size (<unicode><unicode>)",
                             0.360885167995635, 0, 0.073578226708627, 0, "Heterogeneity (<unicode><unicode>)",
                             0.339482111457866))
  })
  
  test_that("Model Averaged Weights (Ď‰) table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_weights"]][["data"]]
    expect_equal_tables(table,
                        list(1, 0, 1, 1, 1, 0.05, 0.245436596470257, 0.05, 0.835686040987476,
                             1, 1, 0.1, 0.130458675198804, 0.1, 0.795555189151253, 1, 1,
                             1))
  })
  
  test_that("Model Summary table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0.287745674806251, "6/12", 0.223449148722277, 0.5, "Effect", 0.570584756327829,
                             "6/12", 0.363294469801113, 0.5, "Heterogeneity", 0.615135637490406,
                             "8/12", 0.380856952946814, 0.5, "Publication bias"))
  })
  
  test_that("Model Averaged Estimated Studies' Effects (Î¸) table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_studies_summary"]][["data"]]
    expect_equal_tables(table,
                        list(-0.107450163928454, 0.0555020197211975, 0, "Study 1", 0.482061559243418,
                             -0.0740703424511303, 0.0558393845146318, 0, "Study 2", 0.428338020099332,
                             -0.152940987187001, 0.0539633761646308, 0, "Study 3", 0.435371637741933
                        ))
  })
  
  test_that("Effect size (Conditional) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-conditional-prefitted-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weight function (Conditional) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_omega"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-function-conditional-prefitted-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Conditional) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-conditional-prefitted-1", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Forest plot (Conditional) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-conditional-prefitted-1", dir="RobustBayesianMetaAnalysis")
  })
}
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
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
  results <- jaspTools::run("RobustBayesianMetaAnalysis", dataset, options)
  
  
  test_that("Model Averaged Estimates (different CI + BF01) table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_summary"]][["data"]]
    expect_equal_tables(table,
                        list(0, 0.038323039556351, 0, "Effect size (<unicode><unicode>)", 0.207512189502431,
                             0, 0.073578226708627, 0, "Heterogeneity (<unicode><unicode>)",
                             0.222322562122289))
  })
  
  test_that("Model Averaged Weights (omega) (different CI + BF01) table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_weights"]][["data"]]
    expect_equal_tables(table,
                        list(1, 0, 1, 1, 1, 0.05, 0.376517122973235, 0.05, 0.835686040987476,
                             1, 1, 0.1, 0.310983178144945, 0.1, 0.795555189151253, 1, 1,
                             1))
  })
  
  test_that("Model Summary (different CI + BF01) table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list(3.47529116006117, "6/12", 0.223449148722277, 0.5, "Effect", 1.75258800539258,
                             "6/12", 0.363294469801113, 0.5, "Heterogeneity", 1.62565772335959,
                             "8/12", 0.380856952946814, 0.5, "Publication bias"))
  })
  
  test_that("Effect size (Model Averaged) (no prior) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-model-averaged-prefitted-2", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Effect size vs Heterogeneity (Model Averaged) (no prior) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_mutau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-vs-heterogeneity-model-averaged-prefitted-2", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weight function (Model Averaged) (no prior) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_omega"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-function-model-averaged-prefitted-2", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Model Averaged) (no prior) plot matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-model-averaged-prefitted-2", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Forest plot (Model Averaged) (observed + predicted) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-model-averaged-prefitted-2", dir="RobustBayesianMetaAnalysis")
  })
}
{
  options <- jaspTools::analysisOptions("RobustBayesianMetaAnalysis")
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
  results <- jaspTools::run("RobustBayesianMetaAnalysis", dataset, options)
  
    expect_equal_tables(table,
                        list(0, 0.038323039556351, 0, "Effect size (<unicode><unicode>)", 0.207512189502431,
                             0, 0.073578226708627, 0, "Heterogeneity (<unicode><unicode>)",
                             0.222322562122289))
  })
  
  test_that("Model Averaged Weights (omega) (log BF & different CI) table results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_averaged_weights"]][["data"]]
    expect_equal_tables(table,
                        list(1, 0, 1, 1, 1, 0.05, 0.376517122973235, 0.05, 0.835686040987476,
                             1, 1, 0.1, 0.310983178144945, 0.1, 0.795555189151253, 1, 1,
                             1))
  })
  
  test_that("Model Summary table (log BF & different CI) results match", {
    table <- results[["results"]][["main_summary"]][["collection"]][["main_summary_overall_summary"]][["data"]]
    expect_equal_tables(table,
                        list(-1.24567826257478, "6/12", 0.223449148722277, 0.5, "Effect", -0.56109355572288,
                             "6/12", 0.363294469801113, 0.5, "Heterogeneity", -0.485912486728035,
                             "8/12", 0.380856952946814, 0.5, "Publication bias"))
  })
  
  test_that("Forest plot (Model Averaged) (observed, descending) matches", {
    plotName <- results[["results"]][["plots"]][["collection"]][["plots_theta"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "forest-plot-model-averaged-prefitted-3", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Effect size (Models) (all models, descending by post. prob) plot matches", {
    plotName <- results[["results"]][["plots_individual"]][["collection"]][["plots_individual_mu"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "effect-size-models-prefitted-3", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weights (Models) (all models, descending by post. prob) plot matches", {
    plotName <- results[["results"]][["plots_individual"]][["collection"]][["plots_individual_omega"]][["collection"]][["plots_individual_omega_plot_1"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-1-models-prefitted-3", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Weights (Models) (all models, descending by post. prob) plot matches", {
    plotName <- results[["results"]][["plots_individual"]][["collection"]][["plots_individual_omega"]][["collection"]][["plots_individual_omega_plot_2"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "weight-2-models-prefitted-3", dir="RobustBayesianMetaAnalysis")
  })
  
  test_that("Heterogeneity (Models) (all models, descending by post. prob) plot matches", {
    plotName <- results[["results"]][["plots_individual"]][["collection"]][["plots_individual_tau"]][["data"]]
    testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
    expect_equal_plots(testPlot, "heterogeneity-models-prefitted-3", dir="RobustBayesianMetaAnalysis")
  })
  
}