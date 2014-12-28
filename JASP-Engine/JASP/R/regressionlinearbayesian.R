RegressionLinearBayesian <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

    options$fixedFactors <- NULL
    options$randomFactors <- NULL

    results <- AncovaBayesian(dataset, options, perform, callback)
    
    return(results)
}