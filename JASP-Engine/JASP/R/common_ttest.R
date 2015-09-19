#' The plan of this file is to successively abstract functionality
#' from the individual t-tests into a common interface to reduce clutter
#'
#' returns the result object and the dataset
.initialize_ttest <- function(dataset, options, perform, type = 'not-paired') {
    
    groups <- options$groupingVariable
    depvars <- unlist(options$variables)
    
    ## a paired t-test works with pairs ...
    if (type == 'paired') {
        depvars <- unlist(options$pairs)
        depvars <- depvars[depvars != ""]
    }
    
    ## !is.null(groups) is there to avoid errors for paired and one-sample t-test
    if (!is.null(groups) && groups == "") groups <- NULL

    if (is.null(dataset)) {
        ## if we are ready to run, read in the dataset
        if (perform == "run") {
            
            if (options$missingValues == "excludeListwise") {
                exclude <- depvars
            } else {
                exclude <- NULL
            }
                
            dataset <- .readDataSetToEnd(columns.as.numeric = depvars,
                                         columns.as.factor = groups,
                                         exclude.na.listwise = exclude)
            
        ## else just read in the headers (and create an empty table)
        } else {
            dataset <- .readDataSetHeader(columns.as.numeric = depvars,
                                          columns.as.factor = groups)
        }
    } 
    
    ## this is the main object; we add stuff to it and return it
    results <- list("title" = "T-Test")

    #### META
    meta <- list()
    meta[[1]] <- list(name = "title", type = "title")
    meta[[2]] <- list(name = "ttest", type = "table")
    meta[[3]] <- list(name = "descriptives", type = "table")
    meta[[4]] <- list(name = "normalityTests", type = "table")
    meta[[5]] <- list(name = "headerDescriptivesPlots", type = "h1")
    meta[[6]] <- list(name = "descriptivesPlots", type = "images")
    
    if (options$descriptivesPlots) {
        results[["headerDescriptivePlots"]] <- ifelse(length(depvars) > 1,
                                               "Descriptive Plots", "Descriptives Plot")
    }
    
    results[[".meta"]] <- meta
    list("results" = results, "dataset" = dataset)
}