

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

variables <- options$main$fields
stats.options <- options[["statistics"]]
central.tendency <- stats.options[["centralTendency"]]

run <- perform == "run"

results <- list()

#### STATS TABLE

stats.results <- list()

fields <- list()

fields[[length(fields) + 1]] <- list(id="Valid", type="integer")
fields[[length(fields) + 1]] <- list(id="Missing", type="integer")

if (central.tendency[["mean"]])
  fields[[length(fields) + 1]] <- list(id="Mean", type="number", format="sf:4")
if (central.tendency[["median"]])
  fields[[length(fields) + 1]] <- list(id="Median", type="number", format="sf:4")
if (central.tendency[["mode"]])
  fields[[length(fields) + 1]] <- list(id="Mode", type="number", format="sf:4")
if (central.tendency[["sum"]])
  fields[[length(fields) + 1]] <- list(id="Sum", type="number", format="sf:4")

stats.results[["title"]] <- "Descriptive Statistics"
stats.results[["schema"]] <- list(fields=fields)
stats.results[["cases"]] <- as.list(variables)

if (perform == "run")
{
  stats.values <- list()

  for (field in variables) {

	field.results <- list()
	column <- dataset[[field]]

	rows <- nrow(dataset)
	na.omitted = na.omit(column)

	field.results[["Valid"]] = length(na.omitted)
	field.results[["Missing"]] = rows - length(na.omitted)

	if (central.tendency["mean"]) {
	  if (class(na.omitted) != "factor")
		field.results[["Mean"]] <- s(mean(na.omitted))
	  else
		field.results[["Mean"]] <- ""
	}

	if (central.tendency["median"]) {
	  if (class(na.omitted) != "factor")
		field.results[["Median"]] <- s(median(na.omitted))
	  else
		field.results[["Median"]] <- ""
	}

	if (central.tendency["mode"]) {
	  if (class(na.omitted) != "factor")
		field.results[["Mode"]] <- s(mean(as.numeric(names(table(na.omitted)[table(na.omitted)==max(table(na.omitted))]))))
	  else
		field.results[["Mode"]] <- ""
	}

	if (central.tendency["sum"]) {
	  if (class(na.omitted) != "factor")
		field.results[["Sum"]] <- s(sum(na.omitted))
	  else
		field.results[["Sum"]] <- ""
	}

	stats.values[[length(stats.values) + 1]] <- field.results
  }

  stats.results[["data"]] <- stats.values
}

results[["stats"]] <- stats.results

#### FREQUENCIES TABLES

if (options$main$displayFrequencyTables)
{
  frequency.tables <- list()

  for (variable in variables)
  {
	column <- dataset[[variable]]
	if (class(column) == "numeric")
	  next

	frequency.table <- list()

	fields <- list(
	  list(id="Frequency", type="integer"),
	  list(id="Percent", type="number", format="dp:1"),
	  list(id="Valid Percent", type="number", format="dp:1"),
	  list(id="Cumulative Percent", type="number", format="dp:1"))

	frequency.table[["title"]] <- paste("Frequencies for", variable)
	frequency.table[["schema"]] <- list(fields=fields)

	if (class(column) == "factor")
	  frequency.table[["cases"]] <- levels(dataset[[variable]])
	else
	  frequency.table[["cases"]] <- list()

	if (perform == "run")
	{
	  lvls <- c()

	  if (class(column) == "factor")
		lvls <- levels(dataset[[variable]])
	  else if (class(column) == "integer")
		lvls <- sort(unique(dataset[[variable]]))

	  frequency.table[["cases"]] <- c(lvls, "Total")

	  t <- table(column)
	  total <- sum(t)

	  freqs <- list()
	  percent <- list()
	  validPercent <- list()
	  cumPercent <- list()

	  cumFreq <- 0

	  for (n in names(t))
	  {
		freq <- as.vector(t[n])
		cumFreq <- cumFreq + freq

		freqs[[length(freqs)+1]] <- freq
		percent[[length(percent)+1]] <- freq / total * 100
		validPercent[[length(validPercent)+1]] <- freq / total * 100
		cumPercent[[length(cumPercent)+1]] <- cumFreq / total * 100
	  }

	  freqs[[length(freqs)+1]] <- total
	  percent[[length(percent)+1]] <- 100
	  validPercent[[length(validPercent)+1]] <- 100
	  cumPercent[[length(cumPercent)+1]] <- ""

	  data <- list()

	  for (i in 1:length(freqs))
		data[[length(data)+1]] <- list("Frequency"=freqs[[i]], "Percent"=percent[[i]], "Valid Percent"=validPercent[[i]], "Cumulative Percent"=cumPercent[[i]])

	  frequency.table[["data"]] <- data

	}
	else
	{
	  if (class(column) == "factor")
		frequency.table[["cases"]] <- levels(dataset[[variable]])
	  else
		frequency.table[["cases"]] <- list()
	}

	frequency.tables[[length(frequency.tables)+1]] <- frequency.table
  }

  results[["tables"]] <- frequency.tables
}

#### FREQUENCY PLOTS

results
