.analysisOptionsFromJSONString <- function(x) {
  json <- try(jsonlite::fromJSON(x, simplifyVector=FALSE), silent = TRUE)
  
  if (inherits(json, "try-error")) {
    stop("Your json is invalid, please copy the entire message
         including the outer braces { } that was send to R in the Qt terminal.
         Remember to use single quotes around the message.")
  }
  
  if ("options" %in% names(json)) {
    return(json[["options"]])
  } else {
    stop("The JSON file appears to be invalid")
  }
  
}


.analysisOptionsFromJSONFile <- function(file) {
  
  analysisOptsRaw <- try(jsonlite::read_json(file), silent = TRUE)
  
  if (inherits(analysisOptsRaw, "try-error")) {
    stop("The file for the analysis you supplied could not be found.
         Please ensure that (1) its name matches the main R function.")
  }
  
  if (!"options" %in% names(analysisOptsRaw)) {
    stop("The JSON file was found, but it appears to be invalid")
  }

  analysisOpts <- .fillOptions(analysisOptsRaw[["options"]])
  return(analysisOpts)
}


.fillOptions <- function(options) {
  output <- list()
  for (i in 1:length(options)) {
    option <- options[[i]]
    if ("default" %in% names(option)) {
      output[[option[["name"]]]] <- option[["default"]]
    } else {
      output[[option[["name"]]]] <- .optionTypeToValue(option)
    }
  }
  return(output)
}


.optionTypeToValue <- function(option) {
  switch(option[["type"]],
    Boolean =
     FALSE,
    
    Integer =
     "",
    
    IntegerArray =
     list(),
    
    List =
     option[["options"]][[1]],
    
    Number =
     option[["value"]],
    
    Table =
     list(),
    
    String =
     "",
    
    Term =
     "",
    
    Terms =
     list(),
    
    Variable =
     "",
    
    Variables =
     list(),
    
    VariablesGroups =
     list(),
    
    NULL
  )
}