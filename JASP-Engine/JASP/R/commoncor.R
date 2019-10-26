.getCorTests <- function(pearson=c(TRUE, FALSE), kendall=c(TRUE, FALSE), spearman=c(TRUE, FALSE)) {
  
  tests <- c()
  if (pearson) 
    tests <- c(tests, "pearson")
  if (kendall)
    tests <- c(tests, "kendall")
  if (spearman)
    tests <- c(tests, "spearman")
  
  return(tests)
}

.bCorCitationsList <- list("pearson"=c("Ly, A., Verhagen, A. J. & Wagenmakers, E.-J. (2016). Harold Jeffreys's Default Bayes Factor Hypothesis Tests: Explanation, Extension, and Application in Psychology. Journal of Mathematical Psychology, 72, 19-32.",
                                       "Ly, A., Marsman, M., Wagenmakers, E.-J. (2018). Analytic Posteriors for Pearson’s Correlation Coefficient. Statistica Neerlandica, 72(1), 4-13."),
                           "kendall"=c("van Doorn, J.B., Ly, A., Marsman, M. & Wagenmakers, E.-J. (2018). Bayesian Inference for Kendall’s Rank Correlation Coefficient. The American Statistician, 72(4), 303-308.", 
                                       "van Doorn, J.B., Ly, A., Marsman, M., & Wagenmakers, E.-J. (2019). Bayesian Estimation of Kendall’s tau Using a Latent Normal Approach. Statistics and Probability Letters, 145, 268-272."),
                           "spearman"=c("van Doorn, J.B., Ly, A., Marsman, M. & Wagenmakers, E.-J. (in press). Bayesian Rank-Based Hypothesis Testing for the Rank Sum Test, the Signed Rank Test, and Spearman's rho. Manuscript submitted for publication")
)

.corTestNamesList <- list(pearson="Pearson's r", spearman="Spearman's rho", kendall="Kendall's tau B")
.corOverTitlesList <- list(pearson="Pearson", spearman="Spearman", kendall="Kendall")