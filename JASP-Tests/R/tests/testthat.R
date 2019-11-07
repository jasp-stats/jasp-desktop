.libPaths("~/pkgs/Frameworks/R.framework/Versions/3.6/Resources/library")

library(jaspResults)
library(jasptools)
library(testthat)
develop(path = file.path("..", "..", ".."))
setPkgOption("pkgs.dir", "~/pkgs/Frameworks/R.framework/Versions/3.6/Resources/library")

options("testthat.progress.max_fails" = 1E3L)

result <- test_dir("testthat")
result <- as.data.frame(result)

if (sum(result$failed) > 0 || sum(result$error) > 0)
  quit(save = "no", status = 1)