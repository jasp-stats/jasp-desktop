replaceNA <- function(column, replaceWith) { return(ifelse(is.na(column), replaceWith, column)) }
