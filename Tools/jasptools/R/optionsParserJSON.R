.analysisOptionsFromJSONString <- function(x) {
  json <- try(rjson::fromJSON(x)) # jsonlite can't deal with \n in strings.. rjson can.
  
  if (inherits(json, "try-error")) {
    stop("There was a problem parsing the JSON string, cannot create the options list")
  }
  
  if ("options" %in% names(json)) {
    return(json[["options"]])
  } else {
    stop("There is no \"options\" field in your JSON string, cannot create options list")
  }
  
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