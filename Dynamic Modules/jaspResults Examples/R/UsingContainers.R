UsingContainers <- function(jaspResults, dataset, options)
{
  ready <- options$weAreReady
  jaspResults$addCitation("Our analysis citation")
  
  if (!options$useContainers) {
    # this is the more R-like way, but less optimal;
    # you have to copy dependencies, manually propogate errors and can't always show default empty tables
    model <- .calculateModel(jaspResults, options, ready)
    .defaultTable(jaspResults, model, options, ready)
    .optionalTable(jaspResults, model, options, ready)
  } else {
    # this is not very R-like, but more optimal;
    # dependencies are set only once, errors are propogated automatically and a default empty table can be shown
    # two things to note: we make a model container first and no longer call the results computation here
    modelContainer <- .getModelContainer(jaspResults)
    .defaultTableWithContainer(modelContainer, options, ready)
    .optionalTableWithContainer(modelContainer, options, ready)
  }
  
}

.calculateModel <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["stateModel"]]))
    return(jaspResults[["stateModel"]]$object)
	
  # we must propogate both possible errors and results
  # -- this is not necessary in the container version
	model <- list(result=NULL, error=NULL)
  
  # create a state item; we must do this at the start because we use 
  # dependOn(optionsFromObject=jaspResults[["stateModel"]]) in other functions
  # -- this is not necessary in the container version
  stateModel <- jaspResults[["stateModel"]] <- createJaspState(model)
  stateModel$dependOn(c("randomDependency", "weAreReady", "useContainers", "setError"))
  
  # if we're not ready we don't want to compute anything
    # -- this is not necessary in the container version
  if (!ready)
    return(model)
  
  # imitate a 5 second sampling procedure
  startProgressbar(5)
  for (i in 1:5) {
    Sys.sleep(1)
    progressbarTick()
  }
  
  # usually the computation of a model can fail and we must account for this
  result <- try({
    if (options$setError)
      stop("Error! We cannot recover from this.. ")
    data.frame(x=1:5, y=6:10, z=11:15)
  })
  
  if (isTryError(result)) {
    model$error <- as.character(result)
  } else {
    model$result <- result
  }
  
  # we won't actually use this stateModel anymore,
  # except to see if it was already computed between analysis runs
  jaspResults[["stateModel"]]$object <- model
  
  return(model)
}

.defaultTable <- function(jaspResults, model, options, ready) {
  if (!is.null(jaspResults[["defaultTable"]]))
    return()
  
  table <- createJaspTable(title="This is our default table without using containers")
  
  # this table has the exact same dependencies as stateModel, so we copy them
  # -- this is not necessary in the container version
  table$dependOn(optionsFromObject=jaspResults[["stateModel"]])
  
  table$addColumnInfo(name="x")
  table$addColumnInfo(name="y")
  
  # our data has x, y, z and we only want x and y, so we must tell jaspResults this
  table$showSpecifiedColumnsOnly <- TRUE
  
  jaspResults[["defaultTable"]] <- table
  
  # if an error occurred we place it on the first table
  # -- this is not necessary in the container version
  if (!is.null(model$error))
    table$setError(model$error)
  
  if (!ready || !is.null(model$error))
    return()
  
  table$setData(model$result)
  
  return()
}

.optionalTable <- function(jaspResults, model, options, ready) {
  if (!options$optionalTable || !is.null(jaspResults[["optionalTable"]]))
    return()
  
  table <- createJaspTable(title="This is our optional table without using containers")
  
  # this table has one additional dependency, but otherwise the same ones as stateModel
  # -- this is not necessary in the container version
  table$dependOn("optionalTable", optionsFromObject=jaspResults[["stateModel"]])
  
  table$addColumnInfo(name="z")
  
  # our data has x, y, z and we only want z, so we must tell jaspResults this
  table$showSpecifiedColumnsOnly <- TRUE
  
  jaspResults[["optionalTable"]] <- table
  
  # even though we don't want to show an error message on this element (don't need the message multiple times),
  # we should still gray out the table to make it clear there was an error
  # -- this is not necessary in the container version
  if (!is.null(model$error))
    table$setError("")
  
  if (!ready || !is.null(model$error))
    return()
  
  # you can also use addRows to repeatedly add cells, rather than using setData once
  table$addRows(model$result[1, ])
  
  # let's also add an imaginary footnote to the first column of the table
  table$addFootnote("this is a great footnote", colNames="z")
  
  return()
}

.getModelContainer <- function(jaspResults) {
  if (!is.null(jaspResults[["modelContainer"]])) {
    modelContainer <- jaspResults[["modelContainer"]]
  } else {
    modelContainer <- createJaspContainer()
    # we set the dependencies on the container, this means that all items inside the container automatically have these dependencies
    modelContainer$dependOn(c("randomDependency", "weAreReady", "useContainers", "setError"))
    jaspResults[["modelContainer"]] <- modelContainer
  }
  return(modelContainer)
}

.calculateModelWithContainer <- function(modelContainer, options) {
  if (!is.null(modelContainer[["model"]]))
    return(modelContainer[["model"]]$object)
  
  # imitate a 5 second sampling procedure
  startProgressbar(5)
  for (i in 1:5) {
    Sys.sleep(1)
    progressbarTick()
  }
  
  # usually the computation of a model can fail and we must account for this
  result <- try({
    if (options$setError)
      stop("Error! We cannot recover from this.. ")
    data.frame(x=1:5, y=6:10, z=11:15)
  })
  
  # if an error occurs we put it on the container that holds all the tables and the model,
  # the container then places this message on the first output element (the default table)
  if (isTryError(result))
    modelContainer$setError(as.character(result))

  # we also need to create a state for an error, because otherwise is.null(modelContainer[["model"]] is false and we recompute
  # because we set no dependencies, the model will live while the container is not destroyed
  modelContainer[["model"]] <- createJaspState(result)
  
  # at this point we can use modelContainer[["model"]]$object anywhere where we have access to the modelContainer,
  # but it's more clear to make .calculateModelWithContainer() return its result directly.
  return(result)
}

.defaultTableWithContainer <- function(modelContainer, options, ready) {
  if (!is.null(modelContainer[["defaultTable"]]))
    return()
  
  # we do not need any dependencies, as they are all the same as what the container already has
  table <- createJaspTable(title="This is our default table using containers")
  
  table$addColumnInfo(name="x")
  table$addColumnInfo(name="y")
  
  # our data has x, y, z and we only want x and y, so we must tell jaspResults this
  table$showSpecifiedColumnsOnly <- TRUE
  
  modelContainer[["defaultTable"]] <- table
  
  if (!ready)
    return()
  
  # at this point you normally know the dimensions of your table
  # the 5 corresponds to the number of rows in the model data.frame
  table$setExpectedSize(rows=5)
  
  # here we finally ask for the model to be computed
  model <- .calculateModelWithContainer(modelContainer, options)
  # the model could also be retrieved by asking the $object of the state item
  # modelContainer[["model"]]$object
  
  # we check to see if the calculation did not cause any errors;
  # if it did, the container will automatically put the error on this table
  if (!modelContainer$getError())
    table$setData(model)
  
  return()
}

.optionalTableWithContainer <- function(modelContainer, options, ready) {
  if (!options$optionalTable || !is.null(modelContainer[["optionalTable"]]))
    return()
  
  table <- createJaspTable(title="This is our optional table using containers")
  
  table$dependOn("optionalTable")
  
  table$addColumnInfo(name="z")
  
  # our data has x, y, z and we only want z, so we must tell jaspResults this
  table$showSpecifiedColumnsOnly <- TRUE
  
  modelContainer[["optionalTable"]] <- table
  
  # here we can return if we are not ready
  if (!ready)
    return()
  
  # the model was previously computed in .defaultTableWithContainer,
  # so it will simply be returned and not recomputed
  model <- .calculateModelWithContainer(modelContainer, options)
  
  # we check to see if the calculation did not cause any errors;
  # the container takes care of "graying out" this table
  if (!modelContainer$getError())
    table$addRows(model[1, ])
  
  # let's also add an imaginary footnote to the first column of the table
  table$addFootnote("this is a great footnote", colNames="z")
  
  return()
}