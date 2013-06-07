
fields <- options$main$fields
stats.options <- options[["statistics"]]
central.tendency <- stats.options[["centralTendency"]]

results <- list()

s <- function(value) {

  if (is.finite(value))
	return(value)

  if (is.na(value))
	return("NaN")

  if (value == Inf)
	return("Inf")

  if (value == -Inf)
	return("-Inf")

  NULL
}

for (field in fields) {

  field.results = list()
  column <- dataset[[field]]

  rows <- nrow(dataset)
  na.omitted = na.omit(column)

  field.results[[length(field.results)+1]] = length(na.omitted)
  field.results[[length(field.results)+1]] = rows - length(na.omitted)

  if (central.tendency["mean"]) {
	if (class(na.omitted) != "factor")
	  field.results[[length(field.results)+1]] <- s(mean(na.omitted))
	else
	  field.results[[length(field.results)+1]] <- ""
  }

  if (central.tendency["median"]) {
	if (class(na.omitted) != "factor")
	  field.results[[length(field.results)+1]] <- s(median(na.omitted))
	else
	  field.results[[length(field.results)+1]] <- ""
  }

  if (central.tendency["mode"]) {
	if (class(na.omitted) != "factor")
	  field.results[[length(field.results)+1]] <- s(mean(as.numeric(names(table(na.omitted)[table(na.omitted)==max(table(na.omitted))]))))
	else
	  field.results[[length(field.results)+1]] <- ""
  }

  if (central.tendency["sum"]) {
	if (class(na.omitted) != "factor")
	  field.results[[length(field.results)+1]] <- s(sum(na.omitted))
	else
	  field.results[[length(field.results)+1]] <- ""
  }

  results[[length(results) + 1]] <- field.results
}

results









