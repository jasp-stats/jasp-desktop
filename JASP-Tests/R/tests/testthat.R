library(jasptools)
library(testthat)
develop(path = "../../../")
result <- test_dir("testthat")
result <- as.data.frame(result)

if (sum(result$failed) > 0 || sum(result$error) > 0)
  quit(save = "no", status = 1)