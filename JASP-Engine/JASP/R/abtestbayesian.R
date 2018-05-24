#
# Copyright (C) 2018 University of Amsterdam
#

ABTestBayesian <- function(
   dataset = NULL,
   options,
   perform = "run",
   callback = function(...) list(status = "ok"),
   state = NULL,
   ...
) {

   table <- list()
   table[["title"]] <- "Bayesian A/B Test"
   table[["schema"]] <- list(fields = list())
   table[["data"]] <- list()

   results <- list()
   results[[".meta"]] <- list(list(name = "table", type = "table"))
   results[["table"]] <- table

   return (list(results = results, status = "complete"))
}
