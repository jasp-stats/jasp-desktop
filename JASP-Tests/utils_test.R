#
# Copyright (C) 2017 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

.readFromResourcesLibrary <- function(analysis) {
  # Converts the json file in the Resources folder to a R - list for the
  # analysis passed as argument
  #
  # Args:
  #   analysis: Analysis name passed as string
  #
  # Returns:
  #   'NA' if there is an error converting json into R - list
  #   list containing the json data as keys and values

  file.relative.path = paste0("../../Resources/Library/", analysis, ".json")

  analysis.resources <- tryCatch({
      rjson::fromJSON(file = file.relative.path)
    },
    warning = function(warn) {
      message("Warning: ")
      message(warn)
    },
    error = function(err) {
      message("Error: ")
      message(err)
      return(NA)
    },
    finally = {}
  )

  return(analysis.resources)
}

.createOptions <- function(analysis, type = 'default') {
  # Fetches the json data for the analysis and returns an 'options'
  # list with default selections
  #
  # Args:
  #   analysis: Analysis name passed as string
  #   type: can be either 'default' or 'random'
  #
  # Returns:
  #   empty list if file read is unsuccessful
  #   'options' list containing the different options available to the user

  options <- list()

  # fetch options from json file for the analysis
  analysis.resources <- .readFromResourcesLibrary(analysis)

  if (analysis.resources[[1]] == 'NA') {
    return(options)
  }

  analysis.name <- analysis.resources[[1]]
  number.of.options <- length(analysis.resources$options)

  # create options list with NULL values
  options.names <- NULL
  for (i in 1:number.of.options) {
    options.names <- c(options.names,
                       analysis.resources[["options"]][[i]][["name"]])
  }

  options[options.names] <- list(NULL)

  if (type == 'default') {
    options <- .createDefaultOptions(options, analysis.resources)
  } else if (type == 'random') {
    options <- .createRandomOptions(options, analysis.resources)
  } else {
    message("argument 'type' unknown")
    return(list(NULL))
  }

  return(options)
}

.createDefaultOptions <- function(options, analysis.resources) {
  # Fetches the json data for the analysis and returns an 'options'
  # list with default selections
  #
  # Args:
  #   options: contains list of names of options initialized to NULL
  #   analysis.resources: json file converted list list
  #
  # Returns:
  #   'options' list containing the different options available to the user

  number.of.options <- length(analysis.resources$options)

  # give default values to options
  for (i in 1:number.of.options) {
    options.attribute <- analysis.resources[["options"]][[i]]
    if ("default" %in% names(options.attribute)) {
      options[[options.attribute[["name"]]]] <- options.attribute[["default"]]
    } else if ("value" %in% names(options.attribute)) {
      options[[options.attribute[["name"]]]] <- options.attribute[["value"]]
    } else if (options.attribute[["type"]] != "Variables") {
      options[[options.attribute[["name"]]]] <- 0
    }
  }

  return(options)
}

.createRandomOptions <- function(options, analysis.resources) {
  # Fetches the json data for the analysis and returns an 'options'
  # list with default selections
  #
  # Args:
  #   options: contains list of names of options initialized to NULL
  #   analysis.resources: json file converted list list
  #
  # Returns:
  #   'options' list containing the different options available to the user
  #       and gives random values to them.

  # FIXME: Currently random values are given only to types: List, Number
  #        Integer, Boolean. Other types to be handled: Variable, Variables,
  #        Item, Items, Table

  number.of.options <- length(analysis.resources$options)

  # give default values to options
  for (i in 1:number.of.options) {
    options.attribute <- analysis.resources[["options"]][[i]]

    if (options.attribute[["name"]] == "plotWidth" || 
        options.attribute[["name"]] == "plotHeight") {

      options[[options.attribute[["name"]]]] <- options.attribute[["default"]]
      next
    }

    options.type <- options.attribute[["type"]]

    min.value <- -999999
    if ("min" %in% names(options.attribute)) {
      min.value <- options.attribute[["min"]]
    }

    max.value <- 999999
    if ("max" %in% names(options.attribute)) {
      max.value <- options.attribute[["max"]]
    }

    if (options.type == "List") {
      options[[options.attribute[["name"]]]] <- sample(
              options.attribute[["options"]],
              1
            )
    } else if (options.type == "Number") {
      options[[options.attribute[["name"]]]] <- runif(
              1,
              min = min.value,
              max = max.value
            )
    } else if (options.type == "Integer") {
      options[[options.attribute[["name"]]]] <- floor(runif(
              1,
              min = min.value,
              max = max.value
            )
          )
    } else if (options.type == "Boolean") {
      options[[options.attribute[["name"]]]] <- sample(c(TRUE, FALSE),
                                                       1, replace = T)
    }
  }

  return(options)
}

.createBehaviourEmpty <- function() {

  # Create a series of GUI selections/flow input by user
  # FIXME: yet to be implemented
}

.createBehaviourEJ <- function() {

  # Create a series of GUI selections/flow input by user
  # FIXME: yet to be implemented
}
