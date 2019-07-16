UsingContainers <- function(jaspResults, dataset, options)
{
  ready <- options$weAreReady
  
  if (!options$useContainers) {
    # this is the more R-like way, but less optimal;
    # you have to copy dependencies, manually propogate errors and can't show default tables
    model <- .calculateModel(jaspResults, options, ready)
    .defaultTable(jaspResults, model, options, ready)
    .optionalTable(jaspResults, model, options, ready)
  } else {
    # this is not very R-like, but more optimal;
    # dependencies are set only once, errors are propogated automatically and a default table can be shown
    modelContainer <- .getModelContainer(jaspResults)
    .defaultTableWithContainer(modelContainer, options, ready)
    .optionalTableWithContainer(modelContainer, options, ready)
  }
  
}

.calculateModel <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["stateModel"]]))
    return(jaspResults[["stateModel"]]$object)
	
	model <- list(result=NULL, error=NULL)
  
  # create a state item; we must do this at the start because we use 
  # dependOn(optionsFromObject=jaspResults[["stateModel"]]) in other functions
  stateModel <- jaspResults[["stateModel"]] <- createJaspState(model)
  stateModel$dependOn(c("randomDependency", "weAreReady", "useContainers", "setError"))
  
  # if we're not ready we don't want to compute anything
  if (!ready)
    return(model)
  
  # imitate a 5 second sampling procedure
  startProgressbar(5)
  for (i in 1:5) {
    Sys.sleep(1)
    progressbarTick()
  }
  
  # usually the computation of a model can fail and we must account for this
  tryModel <- try({
    if (options$setError)
      stop("Error! We cannot recover from this.. ")
    result <- data.frame(x=1:5, y=6:10, z=11:15)
  })
  
  if (isTryError(tryModel)) {
    model$error <- as.character(tryModel)
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
  table$dependOn(optionsFromObject=jaspResults[["stateModel"]])
  
  table$addColumnInfo(name="x")
  table$addColumnInfo(name="y")
  
  # our data has x, y, z and we only want x and y, so we must tell jaspResults this
  table$showSpecifiedColumnsOnly <- TRUE
  
  jaspResults[["defaultTable"]] <- table
  
  # if an error occurred we place it on the first table
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
  table$dependOn("optionalTable", optionsFromObject=jaspResults[["stateModel"]])
  
  table$addColumnInfo(name="z")
  
  # our data has x, y, z and we only want z, so we must tell jaspResults this
  table$showSpecifiedColumnsOnly <- TRUE
  
  jaspResults[["optionalTable"]] <- table
  
  # even though we don't want to show an error message on this element (don't need the message multiple times),
  # we should still gray out the table to make it clear there was an error
  if (!is.null(model$error))
    table$setError("")
  
  if (!ready || !is.null(model$error))
    return()
  
  table$setData(model$result)
  
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
  
  # imitate a 5 second sampling procedure
  startProgressbar(5)
  for (i in 1:5) {
    Sys.sleep(1)
    progressbarTick()
  }
  
  # usually the computation of a model can fail and we must account for this
  tryModel <- try({
    if (options$setError)
      stop("Error! We cannot recover from this.. ")
    result <- data.frame(x=1:5, y=6:10, z=11:15)
  })
  
  # if an error occurs we put it on the container that holds all the tables and the model,
  # the container then places this message on the first output element (the default table)
  if (isTryError(tryModel)) {
    modelContainer$setError(as.character(tryModel))
    return()
  }
  
  # at this point we can use modelContainer[["model"]]$object anywhere where we have access to the modelContainer
  modelContainer[["model"]] <- createJaspState(result)
}

.defaultTableWithContainer <- function(modelContainer, options, ready) {
  if (!is.null(modelContainer[["defaultTable"]]))
    return()
  
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
  .calculateModelWithContainer(modelContainer, options)
  
  # we check to see if the calculation did not cause any errors;
  # if it did, the container will automatically put the error on the table
  if (modelContainer$getError())
    return()
  
  # the model can be retrieved by asking the $object of a state item
  model <- modelContainer[["model"]]$object
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
  
  # here we can return if there was an error
  # the container takes care of "graying out" this table
  if (!ready || modelContainer$getError())
    return()
  
  model <- modelContainer[["model"]]$object
  table$setData(model)
  
  return()
}