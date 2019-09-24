#
# Copyright (C) 2013-2018 University of Amsterdam
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

# When making changes to this file always mention @koenderks as a reviewer in the Pull Request

.randomSampling <- function(dataset, options, jaspResults, position = 1, selectionContainer){

    if(!is.null(selectionContainer[["sampleTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    recordVariable                  <- unlist(options[["recordNumberVariable"]])
    if(recordVariable == "")        recordVariable <- NULL
    monetaryVariable                <- unlist(options[["monetaryVariable"]])
    if(monetaryVariable == "")      monetaryVariable <- NULL
    variables                       <- unlist(options[["additionalVariables"]])

    sampleSize                      <- jaspResults[["sampleSize"]]$object
    set.seed(options[["seed"]])

    if(is.null(jaspResults[["sample"]])){
      
      recordColumn <- dataset[, .v(recordVariable)]

      if(options[["selectionType"]] == "recordSampling"){
          samplingRegion        <- recordColumn
          sampleVector          <- base::sample(x = samplingRegion, size = sampleSize, replace = FALSE)
          sample                <- as.data.frame(dataset[recordColumn %in% sampleVector, ])
          colnames(sample)[1]   <- .v(options[["recordNumberVariable"]])
      } else {
          monetaryColumn        <- dataset[, .v(options[["monetaryVariable"]])]
          monetaryColumn        <- ceiling(monetaryColumn)
          sampleVector          <- base::sample(recordColumn, size = sampleSize, replace = TRUE, prob = abs(monetaryColumn))
          sample                <- as.data.frame(dataset[recordColumn %in% sampleVector, ])
          colnames(sample)[1]   <- .v(options[["recordNumberVariable"]])
      }
      jaspResults[["sample"]] <- createJaspState(sample)
      jaspResults[["sample"]]$dependOn(options = c("additionalVariables", "seed", "recordNumberVariable", "monetaryVariable", "selectionMethod", "selectionType", "materiality",
                                                  "expectedErrors", "expectedNumber", "expectedPercentage", "planningModel", "IR", "CR"))
      jaspResults[["sampleVector"]] <- createJaspState(sampleVector)
      jaspResults[["sampleVector"]]$dependOn(optionsFromObject = jaspResults[["sample"]])
      if(any(duplicated(jaspResults[["sampleVector"]]$object))){
        jaspResults[["containsDoubleObservations"]] <- createJaspState(TRUE)
      } else {
        jaspResults[["containsDoubleObservations"]] <- createJaspState(FALSE)
      }
      jaspResults[["containsDoubleObservations"]]$dependOn(optionsFromObject = jaspResults[["sample"]])
    }
    
    sample <- jaspResults[["sample"]]$object
    
    if(options[["displaySample"]]){
      sampleTable <- createJaspTable("Selected Observations")
      sampleTable$position <- position
      sampleTable$dependOn(options = c("additionalVariables", "seed", "recordNumberVariable", "monetaryVariable", "selectionType", "materiality",
                                    "displaySample", "selectionMethod", "expectedErrors", "expectedNumber", "expectedPercentage"))

      sampleTable$addColumnInfo(name="number", title ="", type = "string")
      sampleTable$addColumnInfo(name="recordNumber", title ="Record number", type = "string")
      if(options[["monetaryVariable"]] != "")
        sampleTable$addColumnInfo(name="bookValue", title ="Book value", type = "string")
      for(i in variables){
          sampleTable$addColumnInfo(name=i,     type="string")
      }

      selectionContainer[["sampleTable"]]          <- sampleTable
    
      recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
      for(i in 1:nrow(sample)){
          row <- list()
          row[["number"]] <- i
          row[["recordNumber"]] <- sample[i, recordColumnIndex]
          if(options[["monetaryVariable"]] != ""){
            row[["bookValue"]] <- sample[i, .v(options[["monetaryVariable"]])]
          }
          for(j in c(variables)){
              row[[j]] <- as.character(sample[i, .v(j)])
          }
          sampleTable$addRows(row)
      }
    }
}

.cellSampling <- function(dataset, options, jaspResults, position = 2, selectionContainer){

    if(!is.null(selectionContainer[["sampleTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    recordVariable                  <- unlist(options[["recordNumberVariable"]])
    if(recordVariable == "")        recordVariable <- NULL
    monetaryVariable                <- unlist(options[["monetaryVariable"]])
    if(monetaryVariable == "")      monetaryVariable <- NULL
    rankingVariable                 <- unlist(options[["rankingVariable"]])
    if(rankingVariable == "")        rankingVariable <- NULL
    variables                       <- unlist(options[["additionalVariables"]])

    sampleSize                          <- jaspResults[["sampleSize"]]$object
    set.seed(options[["seed"]])

    if(is.null(jaspResults[["sample"]])){
      
      if(!is.null(rankingVariable)){
          rankingColumn       <- dataset[, .v(rankingVariable)]
          dataset             <- dataset[order(rankingColumn), ]
      }

      if(options[["selectionType"]] == "recordSampling"){
        
          interval <- ceiling(nrow(dataset) / jaspResults[["sampleSize"]]$object)
        
          recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
          recordColumn        <- dataset[, .v(recordVariable)]
          interval.mat        <- matrix(dataset[, .v(recordVariable)], ncol = interval, byrow = TRUE, nrow = sampleSize)
          
          sample.rows <- numeric(jaspResults[["sampleSize"]]$object)
          for(i in 1:nrow(interval.mat)){
              sample.rows[i] <- interval.mat[i, base::sample(1:ncol(interval.mat), size = 1)]
          }
          sample                  <- as.data.frame(dataset[recordColumn %in% sample.rows, ])
          colnames(sample)[1] <- .v(options[["recordNumberVariable"]])
          
      } else {
        
        if(is.null(rankingVariable) && options[["monetaryVariable"]] != "")
          dataset             <- dataset[order(dataset[, .v(monetaryVariable)]), ]
        
          interval <- ceiling(sum(dataset[, .v(monetaryVariable)]) / jaspResults[["sampleSize"]]$object)
        
          recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
          recordColumn        <- dataset[, .v(recordVariable)]
          monetaryColumn      <- dataset[, .v(monetaryVariable)]
          musList             <- rep(recordColumn, times = abs(monetaryColumn))
          interval.mat        <- matrix(musList, nrow = sampleSize, byrow = TRUE, ncol = interval)

          sample.rows <- numeric(jaspResults[["sampleSize"]]$object)
          for(i in 1:nrow(interval.mat)){
              sample.rows[i] <- interval.mat[i, base::sample(1:ncol(interval.mat), size = 1)]
          }
          sample                  <- as.data.frame(dataset[recordColumn %in% sample.rows, ])
          colnames(sample)[1] <- .v(options[["recordNumberVariable"]])
      }

      jaspResults[["sample"]] <- createJaspState(sample)
      jaspResults[["sample"]]$dependOn(options = c("additionalVariables", "seed", "recordNumberVariable", "monetaryVariable", "selectionMethod", "selectionType", "materiality", "intervalStartingPoint",
                                                  "expectedErrors", "expectedNumber", "expectedPercentage", "planningModel", "IR", "CR"))
      jaspResults[["sampleVector"]] <- createJaspState(sample.rows)
      jaspResults[["sampleVector"]]$dependOn(optionsFromObject = jaspResults[["sample"]])
      if(any(duplicated(jaspResults[["sampleVector"]]$object))){
        jaspResults[["containsDoubleObservations"]] <- createJaspState(TRUE)
      } else {
        jaspResults[["containsDoubleObservations"]] <- createJaspState(FALSE)
      }
      jaspResults[["containsDoubleObservations"]]$dependOn(optionsFromObject = jaspResults[["sample"]])
    }
    
    sample <- jaspResults[["sample"]]$object
    
    if(options[["displaySample"]]){
      sampleTable                           <- createJaspTable("Selected Observations")
      sampleTable$position                  <- position
      sampleTable$dependOn(options = c("additionalVariables", "seed", "recordNumberVariable", "monetaryVariable", "selectionType", "materiality",
                                    "displaySample", "selectionMethod", "expectedErrors", "expectedNumber", "expectedPercentage"))

      sampleTable$addColumnInfo(name="number", title ="", type = "string")
      sampleTable$addColumnInfo(name="recordNumber", title ="Record number", type = "string")
      if(options[["monetaryVariable"]] != "")
        sampleTable$addColumnInfo(name="bookValue", title ="Book value", type = "string")
      for(i in variables){
          sampleTable$addColumnInfo(name=i,     type="string")
      }

      selectionContainer[["sampleTable"]]          <- sampleTable
    
      recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
      for(i in 1:nrow(sample)){
          row <- list()
          row[["number"]] <- i
          row[["recordNumber"]] <- sample[i, recordColumnIndex]
          if(options[["monetaryVariable"]] != ""){
            row[["bookValue"]] <- sample[i, .v(options[["monetaryVariable"]])]
          }
          for(j in c(variables)){
              row[[j]] <- as.character(sample[i, .v(j)])
          }
          sampleTable$addRows(row)
      }
    }
}

.systematicSampling <- function(dataset, options, jaspResults, position = 2, selectionContainer) {

    if(!is.null(selectionContainer[["sampleTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

    recordVariable                  <- unlist(options[["recordNumberVariable"]])
    if(recordVariable == "")        recordVariable <- NULL
    monetaryVariable                <- unlist(options[["monetaryVariable"]])
    if(monetaryVariable == "")      monetaryVariable <- NULL
    rankingVariable                 <- unlist(options[["rankingVariable"]])
    if(rankingVariable == "")        rankingVariable <- NULL
    variables                       <- unlist(options[["additionalVariables"]])

    sampleSize                          <- jaspResults[["sampleSize"]]$object
    set.seed(options[["seed"]])

    if(is.null(jaspResults[["sample"]])){
      
      if(!is.null(rankingVariable)){
          rankingColumn       <- dataset[, .v(rankingVariable)]
          dataset             <- dataset[order(rankingColumn), ]
      }

      if(options[["selectionType"]] == "recordSampling"){
        
        interval <- ceiling(nrow(dataset) / jaspResults[["sampleSize"]]$object)

        startingPoint <- base::sample(1: interval, size = 1)
        
        # Error message for when starting point is outside of interval
        if(startingPoint > interval){
          jaspResults[["sample"]] <- createJaspState("startingPointOutsideInterval")
          jaspResults[["containsDoubleObservations"]] <- createJaspState(FALSE)
          jaspResults[["sample"]]$dependOn(options = c("additionalVariables", "seed", "recordNumberVariable", "monetaryVariable", "selectionMethod", "selectionType", "materiality", "intervalStartingPoint",
                                            "expectedErrors", "expectedNumber", "expectedPercentage", "planningModel", "IR", "CR"))
          jaspResults[["containsDoubleObservations"]]$dependOn(optionsFromObject = jaspResults[["sample"]])
          return()
        }
        
        recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
        recordColumn        <- dataset[, .v(recordVariable)]
        interval.mat        <- matrix(dataset[, .v(recordVariable)], ncol = interval, byrow = TRUE, nrow = sampleSize)  
        sample.rows         <- interval.mat[1:nrow(interval.mat), startingPoint]
        sample              <- as.data.frame(dataset[recordColumn %in% sample.rows, ])
        colnames(sample)[1] <- .v(options[["recordNumberVariable"]])
          
      } else {
        
        if(is.null(rankingVariable) && options[["monetaryVariable"]] != "")
          dataset             <- dataset[order(dataset[, .v(monetaryVariable)]), ]
        
        interval <- ceiling(sum(dataset[, .v(monetaryVariable)]) / jaspResults[["sampleSize"]]$object)

        startingPoint <- base::sample(1: interval, size = 1)
        
        # Error message for when starting point is outside of interval
        if(startingPoint > interval){
          jaspResults[["sample"]] <- createJaspState("startingPointOutsideInterval")
          jaspResults[["containsDoubleObservations"]] <- createJaspState(FALSE)
          jaspResults[["sample"]]$dependOn(options = c("additionalVariables", "seed", "recordNumberVariable", "monetaryVariable", "selectionMethod", "selectionType", "materiality", "intervalStartingPoint",
                                            "expectedErrors", "expectedNumber", "expectedPercentage", "planningModel", "IR", "CR"))
          jaspResults[["containsDoubleObservations"]]$dependOn(optionsFromObject = jaspResults[["sample"]])
          return()
        }
        
        recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
        recordColumn        <- dataset[, .v(recordVariable)]
        monetaryColumn      <- dataset[, .v(monetaryVariable)]
        musList             <- rep(recordColumn, times = abs(monetaryColumn))
        interval.mat        <- matrix(musList, nrow = sampleSize, byrow = TRUE, ncol = interval)

        sample.rows         <- interval.mat[1:nrow(interval.mat), startingPoint]
        sample              <- as.data.frame(dataset[recordColumn %in% sample.rows, ])
        colnames(sample)[1] <- .v(options[["recordNumberVariable"]])
      }

      jaspResults[["sample"]] <- createJaspState(sample)
      jaspResults[["sample"]]$dependOn(options = c("additionalVariables", "seed", "recordNumberVariable", "monetaryVariable", "selectionMethod", "selectionType", "materiality", "intervalStartingPoint",
                                                  "expectedErrors", "expectedNumber", "expectedPercentage", "planningModel", "IR", "CR"))
      jaspResults[["sampleVector"]] <- createJaspState(sample.rows)
      jaspResults[["sampleVector"]]$dependOn(optionsFromObject = jaspResults[["sample"]])
      if(any(duplicated(jaspResults[["sampleVector"]]$object))){
        jaspResults[["containsDoubleObservations"]] <- createJaspState(TRUE)
      } else {
        jaspResults[["containsDoubleObservations"]] <- createJaspState(FALSE)
      }
      jaspResults[["containsDoubleObservations"]]$dependOn(optionsFromObject = jaspResults[["sample"]])
    }
    
    sample <- jaspResults[["sample"]]$object
    
    if(options[["displaySample"]]){
      sampleTable                           <- createJaspTable("Selected Observations")
      sampleTable$position                  <- position
      sampleTable$dependOn(options = c("additionalVariables", "seed", "recordNumberVariable", "monetaryVariable", "selectionType", "materiality", "displaySample", "selectionMethod", 
                                    "expectedErrors", "expectedNumber", "expectedPercentage", "intervalStartingPoint"))

      sampleTable$addColumnInfo(name="number", title ="", type = "string")
      sampleTable$addColumnInfo(name="recordNumber", title ="Record number", type = "string")
      if(options[["monetaryVariable"]] != "")
        sampleTable$addColumnInfo(name="bookValue", title ="Book value", type = "string")
      for(i in variables){
          sampleTable$addColumnInfo(name=i,     type="string")
      }

      selectionContainer[["sampleTable"]]          <- sampleTable
    
      recordColumnIndex   <- which(colnames(dataset) == .v(recordVariable))
      for(i in 1:nrow(sample)){
          row <- list()
          row[["number"]] <- i
          row[["recordNumber"]] <- sample[i, recordColumnIndex]
          if(options[["monetaryVariable"]] != ""){
            row[["bookValue"]] <- sample[i, .v(options[["monetaryVariable"]])]
          }
          for(j in c(variables)){
              row[[j]] <- as.character(sample[i, .v(j)])
          }
          sampleTable$addRows(row)
      }
    }
}

.sampleDescriptivesTable <- function(dataset, options, jaspResults, position = 3, selectionContainer){

    if(!is.null(selectionContainer[["sampleDescriptivesTable"]])) return()

    sample <- jaspResults[["sample"]]$object

    recordVariable                  <- unlist(options[["recordNumberVariable"]])
    if(recordVariable == "")        recordVariable <- NULL
    rankingVariable                 <- unlist(options[["rankingVariable"]])
    if(rankingVariable == "")       rankingVariable <- NULL
    monetaryVariable                <- unlist(options[["monetaryVariable"]])
    if(monetaryVariable == "")      monetaryVariable <- NULL
    variables                       <- unlist(options[["additionalVariables"]])

    all.variables                   <- c(rankingVariable, monetaryVariable, variables)
    sampleDescriptivesTable                    <- createJaspTable("Selection Descriptives")
    sampleDescriptivesTable$transpose          <- TRUE
    sampleDescriptivesTable$position           <- position

    sampleDescriptivesTable$addFootnote(message="Not all statistics are available for <i>Nominal Text</i> variables", symbol="<i>Note.</i>")

    sampleDescriptivesTable$dependOn(options = c("additionalVariables", "seed", "sampleDescriptives", "mean", "sd", "var", "range", "min", "max", "median", "recordNumberVariable", "rankingVariable", "additionalVariables", "monetaryVariable"))

                                    sampleDescriptivesTable$addColumnInfo(name="name",                        type="string", format="sf:4", title = "")
                                    sampleDescriptivesTable$addColumnInfo(name="Valid cases",                 type="integer")
    if (options$mean)               sampleDescriptivesTable$addColumnInfo(name="Mean",                        type="number", format="sf:4")
    if (options$median)             sampleDescriptivesTable$addColumnInfo(name="Median",                      type="number", format="sf:4")
    if (options$sd)                 sampleDescriptivesTable$addColumnInfo(name="Std. Deviation",              type="number", format="sf:4")
    if (options$var)                sampleDescriptivesTable$addColumnInfo(name="Variance",                    type="number", format="sf:4")
    if (options$range)              sampleDescriptivesTable$addColumnInfo(name="Range",                       type="number", format="sf:4")
    if (options$min)                sampleDescriptivesTable$addColumnInfo(name="Minimum",                     type="number", format="sf:4")
    if (options$max)                sampleDescriptivesTable$addColumnInfo(name="Maximum",                     type="number", format="sf:4")

    selectionContainer[["sampleDescriptivesTable"]]   <- sampleDescriptivesTable

    if(is.null(sample))
        return()

    for (variable in all.variables) {
      column    <- sample[[ .v(variable) ]]
      row <- list()

      row[["name"]]                   <- variable
      row[["Valid cases"]]            <- base::length(column)
      if(!is.factor(column))
      {
      if(options[["mean"]])              row[["Mean"]]                   <- base::mean(column, na.rm = TRUE)
      if(options[["sd"]])                row[["Std. Deviation"]]         <- stats::sd(column, na.rm = TRUE)
      if(options[["var"]])               row[["Variance"]]               <- stats::var(column, na.rm = TRUE)
      if(options[["median"]])            row[["Median"]]                 <- stats::median(column, na.rm = TRUE)
      if(options[["range"]])             row[["Range"]]                  <- base::abs(base::range(column, na.rm = TRUE)[1] - base::range(column, na.rm = TRUE)[2])
      if(options[["min"]])               row[["Minimum"]]                <- base::min(column, na.rm = TRUE)
      if(options[["max"]])               row[["Maximum"]]                <- base::max(column, na.rm = TRUE)
      }
      sampleDescriptivesTable$addRows(row)
    }
}

.selectionInformationTable <- function(dataset, options, jaspResults, position = 1, selectionContainer){

  if(!is.null(selectionContainer[["selectionInformationTable"]])) return()

  selectionInformationTable                           <- createJaspTable("Selection Summary")
  selectionInformationTable$position                  <- position
  selectionInformationTable$dependOn(options = c("additionalVariables", "intervalStartingPoint", "rankingVariable", "selectionType", "selectionMethod", "monetaryVariable", "recordNumberVariable", "seed", "valuta"))
  
  selectionInformationTable$addColumnInfo(name="n", title ="Sample size", type = "string")
  if(options[["materiality"]] == "materialityAbsolute"){
    selectionInformationTable$addColumnInfo(name="V", title ="Sample value", type = "string")
    selectionInformationTable$addColumnInfo(name="P", title ="% of total value", type = "string")  
  } else {
    selectionInformationTable$addColumnInfo(name="P", title ="% of total observations", type = "string")  
  }
  if(options[["selectionMethod"]] != "randomSampling")
    selectionInformationTable$addColumnInfo(name="I", title ="Interval", type = "string")

  message <- paste0("The sample is drawn with <i>seed ", options[["seed"]], "</i>.")
  selectionInformationTable$addFootnote(message = message, symbol="<i>Note.</i>")

  selectionContainer[["selectionInformationTable"]]          <- selectionInformationTable
  
  sample <- jaspResults[["sample"]]$object

  total_data_value <- jaspResults[["total_data_value"]]$object 
  
  if(options[["selectionType"]] == "recordSampling"){
    interval                                <- ceiling(nrow(dataset) / jaspResults[["sampleSize"]]$object)
  } else {
    interval                                <- ceiling(sum(dataset[, .v(options[["monetaryVariable"]])]) / jaspResults[["sampleSize"]]$object)
  }
  sampleSize                                <- length(unique(sample[, .v(options[["recordNumberVariable"]])]))
  if(options[["materiality"]] == "materialityAbsolute"){
    sampleValue                             <- ceiling(sum(abs(sample[, .v(options[["monetaryVariable"]])])))
    percOfTotal                             <- paste0(round(sampleValue / total_data_value * 100, 2), "%")
    row                                     <- data.frame("n" = sampleSize, "V" = paste(jaspResults[["valutaTitle"]]$object, sampleValue), "P" = percOfTotal)
  } else {
    percOfTotal                             <- paste0(round(sampleSize / jaspResults[["N"]]$object * 100, 2), "%")
    row                                     <- data.frame("n" = sampleSize, "P" = percOfTotal)
  }

  if(options[["selectionMethod"]] != "randomSampling"){
    if(options[["selectionType"]] == "musSampling"){
      row                                   <- cbind(row, I = paste(jaspResults[["valutaTitle"]]$object, interval))
    } else {
      row                                   <- cbind(row, I = interval)
    }
  }
  selectionInformationTable$addRows(row)
}
