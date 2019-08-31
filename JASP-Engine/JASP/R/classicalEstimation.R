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

classicalEstimation <- function(jaspResults, dataset, options, ...){

  # Read data
  dataset <- .readDataEstimation(dataset, options)

  # Ready for analysis
  ready <- .classicalEstimationReady(options)

  # Create explanatory text
  .calculationsClassicalRegression(dataset, options, jaspResults, ready, position = 1)

  # Create results table
  .classicalRegressionTable(dataset, options, jaspResults, ready, position = 2)

  # Required sample size table

  # Correlation plot

}

.readDataEstimation <- function(dataset, options){
  bookValues <- options[["bookValues"]]
  if(bookValues == "")  bookValues <- NULL
  auditValues <- options[["auditValues"]]
  if(auditValues == "")  auditValues <- NULL
  variables <- c(bookValues, auditValues)

  dataset <- .readDataSetToEnd(columns.as.numeric = variables, exclude.na.listwise = variables)
  return(dataset)
}

.classicalEstimationReady <- function(options){
  if(options[["estimator"]] == "mpu"){
    ready <- options[["auditValues"]] != "" && options[["populationSize"]] != 0
  } else {
    ready <- options[["bookValues"]] != "" && options[["auditValues"]] != "" && options[["populationSize"]] != 0 && options[["populationValue"]] != 0 # adjust for mpu
  }
}

.classicalRegressionTable <- function(dataset, options, jaspResults, ready, position){

  if(!is.null(jaspResults[["regressionTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  title <- base::switch(options[["estimator"]],
                        "mpu"         = "Mean-per-unit estimator",
                        "difference"  = "Difference estimator",
                        "ratio"       = "Ratio estimator",
                        "regression"  = "Regression estimator")

  regressionTable                       <- createJaspTable(title)
  regressionTable$position <- position
  regressionTable$dependOn(options = c("bookValues", "auditValues", "populationValue", "populationSize", "confidence", "estimator"))

  regressionTable$addColumnInfo(name = 'estimate', title = 'Point estimate', type = 'string')
  regressionTable$addColumnInfo(name = 'uncertainty', title = 'Uncertainty', type = 'string')
  regressionTable$addColumnInfo(name = 'lower', title = 'Lower', type = 'string', overtitle = paste0(round(options[["confidence"]] * 100, 2), "% Confidence interval"))
  regressionTable$addColumnInfo(name = 'upper', title = 'Upper', type = 'string', overtitle = paste0(round(options[["confidence"]] * 100, 2), "% Confidence interval"))

  jaspResults[["regressionTable"]]      <- regressionTable

  if(!ready)
    regressionTable$addFootnote(message = "Please specify the population size, population value, and your sample variables.", symbol="<i>Note.</i>")

  if(!ready) return()

  regressionTable$addFootnote(message = "Displayed numbers are estimates of the true value of the population.", symbol="<i>Note.</i>")

  dataset           <- na.omit(dataset)

  N                 <- options[["populationSize"]]
  n                 <- nrow(dataset)

  if(options[["estimator"]] == "mpu"){

    meanW           <- round(mean(dataset[, .v(options[["auditValues"]])]), 2)
    sW              <- round(sd(dataset[, .v(options[["auditValues"]])]), 2)

    pointEstimate   <- round(N * meanW, 2)
    lowerCI         <- round(pointEstimate - qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * sW * (N / sqrt(n)) * sqrt((N-n)/(N-1)), 2)
    upperCI         <- round(pointEstimate + qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * sW * (N / sqrt(n)) * sqrt((N-n)/(N-1)), 2)
    uncertainty     <- round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * sW * (N / sqrt(n)) * sqrt((N-n)/(N-1)), 2)

  } else if(options[["estimator"]] == "difference"){

    B               <- round(options[["populationValue"]], 2)
    meanE           <- round(mean(dataset[, .v(options[["bookValues"]])] - dataset[, .v(options[["auditValues"]])]), 2)
    sE              <- round(sd(dataset[, .v(options[["bookValues"]])] - dataset[, .v(options[["auditValues"]])]), 2)
    
    pointEstimate   <- round(B - N * meanE, 2)
    lowerCI         <- round(pointEstimate - qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * sE * (N / sqrt(n)) * sqrt((N-n)/(N-1)), 2)
    upperCI         <- round(pointEstimate + qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * sE * (N / sqrt(n)) * sqrt((N-n)/(N-1)), 2)
    uncertainty     <- round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * sE * (N / sqrt(n)) * sqrt((N-n)/(N-1)), 2)

  } else if(options[["estimator"]] == "ratio"){

    B             <- round(options[["populationValue"]], 2)
    meanB         <- round(mean(dataset[, .v(options[["bookValues"]])]), 2)
    sB            <- round(sd(dataset[, .v(options[["bookValues"]])]), 2)
    meanW         <- round(mean(dataset[, .v(options[["auditValues"]])]), 2)
    sW            <- round(sd(dataset[, .v(options[["auditValues"]])]), 2)
    r             <- round(cor(dataset[, .v(options[["bookValues"]])], dataset[, .v(options[["auditValues"]])]), 2)

    q             <- round(meanW / meanB, 2)
    s             <- sqrt( sW^2 - 2*q*r*sB*sW + q^2 * sB^2 )

    pointEstimate <- round(q * B, 2)
    lowerCI       <- round(pointEstimate - qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * s * (N / sqrt(n)) * sqrt((N-n)/(N-1)), 2)
    upperCI       <- round(pointEstimate + qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * s * (N / sqrt(n)) * sqrt((N-n)/(N-1)), 2)
    uncertainty   <- round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * s * (N / sqrt(n)) * sqrt((N-n)/(N-1)), 2)
 
  } else if(options[["estimator"]] == "regression"){

    B             <- round(options[["populationValue"]], 2)
    meanB         <- round(mean(dataset[, .v(options[["bookValues"]])]), 2)
    meanW         <- round(mean(dataset[, .v(options[["auditValues"]])]), 2)
    sW            <- round(sd(dataset[, .v(options[["auditValues"]])]), 2)
    r             <- round(cor(dataset[, .v(options[["bookValues"]])], dataset[, .v(options[["auditValues"]])]), 2)

    b1            <- round((sum(dataset[, .v(options[["bookValues"]])] * dataset[, .v(options[["auditValues"]])]) - n*meanB*meanW) / (sum(dataset[, .v(options[["bookValues"]])]^2) - (sum(dataset[, .v(options[["bookValues"]])])^2) / n), 2)
    s             <- sW * sqrt(1 - r^2)

    pointEstimate <- round(N * meanW + b1 * (B - N * meanB), 2)
    lowerCI       <- round(pointEstimate - qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * s * (N / sqrt(n)) * sqrt((N-n)/(N-1)), 2)
    upperCI       <- round(pointEstimate + qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * s * (N / sqrt(n)) * sqrt((N-n)/(N-1)), 2)
    uncertainty   <- round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * s * (N / sqrt(n)) * sqrt((N-n)/(N-1)), 2)

  }

  row             <- data.frame(estimate = pointEstimate, uncertainty = uncertainty, lower = lowerCI, upper = upperCI)
  regressionTable$addRows(row)
}

.calculationsClassicalRegression <- function(dataset, options, jaspResults, ready, position){

  if(options[["explanatoryText"]] || !is.null(jaspResults[["calculationsContainer"]])){

    calculationsContainer <- createJaspContainer(title= "<u>Calculations</u>")
    calculationsContainer$position <- 1
    calculationsContainer$dependOn(options = c("explanatoryText", "bookValues", "auditValues", "populationValue", "populationSize", "confidence", "estimator"))

    if(options[["estimator"]] == "mpu"){

      calculationsContainer[["intro"]] <- createJaspHtml(paste0("The required information for the <b>mean-per-unit</b> estimator consists of:"), "p")

      calc1 <- "The population size <i>N</i>"
      if(options[["populationSize"]] != 0){
        N <- options[["populationSize"]]
        calc1 <- paste0(calc1, " = ", N)
      }

      calc2 <- "The sample size <i>n</i>"
      if(options[["auditValues"]] != ""){
        n <- nrow(dataset)
        calc2 <- paste0(calc2, " = ", n)
      }

      calc3 <- "The mean of the sample true values <i>w&#772</i>"
      if(options[["auditValues"]] != ""){
        meanW <- round(mean(dataset[, .v(options[["auditValues"]])]), 2)
        calc3 <- paste0(calc3, " = ", meanW)
      } 

      calc4 <- "The standard deviation of the sample true values <i>s<sub>w</sub></i>"
      if(options[["auditValues"]] != ""){
        sW <- round(sd(dataset[, .v(options[["auditValues"]])]), 2)
        calc4 <- paste0(calc4, " = ", sW)
      } 

      calc5 <- paste0("The t-value <i>(df = n - 1)</i> corresponding to ", round(options[["confidence"]] * 100), "% confidence <i>t<sub>\u03B1/2</sub></i>")
      if(options[["auditValues"]] != ""){
        calc5 <- paste0("The t-value <i>(df = ", n ," - 1)</i> corresponding to ", round(options[["confidence"]] * 100), "% confidence <i>t<sub>\u03B1/2</sub></i> = ", round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1), 3))
      } 

      calculations <- paste0(calc1, "
                        ", calc2, "
                        ", calc3, "
                        ", calc4, "
                        ", calc5)
      calculationsContainer[["all"]] <- createJaspHtml(calculations, "p")

      calculationsContainer[["intro2"]] <- createJaspHtml(paste0("This information allows for calculation of:"), "p")

      pointEstimate <- "The point estimate of the true population value <i>W&#770 = N x w&#772</i>"
      if(ready){
        W <- round(N * meanW, 2)
        pointEstimate <- paste0(pointEstimate, " = ", W)
      }
      
      uncertainty <- "The uncertainty of the estimator <i>U = t<sub>\u03B1/2</sub> x s<sub>w</sub> x (N / \u221A n) x \u221A (N-n) / (N - 1)</i>"
      if(ready){
        U <- round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * sW * (N / sqrt(n)) * sqrt((N-n)/(N-1)), 2)
        uncertainty <- paste0(uncertainty, " = ", U)
      }
      
      confidenceInterval <- "The confidence interval around the point estimate <i>W&#770 \u00B1 U"
      if(ready)
        confidenceInterval <- paste0(confidenceInterval, " = ", W, " \u00B1 ", U)
      
      results <- paste0(pointEstimate, "
                  ", uncertainty, "
                  ", confidenceInterval)
      calculationsContainer[["results"]] <- createJaspHtml(results, "p")

    } else if(options[["estimator"]] == "difference"){

      calculationsContainer[["intro"]] <- createJaspHtml(paste0("The required information for the <b>difference</b> estimator consists of:"), "p")

      calc1 <- "The population size <i>N</i>"
      if(options[["populationSize"]] != 0){
        N <- options[["populationSize"]]
        calc1 <- paste0(calc1, " = ", N)
      }

      calc2 <- "The sample size <i>n</i>"
      if(options[["auditValues"]] != "" || options[["bookValues"]] != ""){
        n <- nrow(dataset)
        calc2 <- paste0(calc2, " = ", n)
      }

      calc3 <- "The sum of the population book values <i>B</i>"
      if(options[["populationValue"]] != 0){
        B <- round(options[["populationValue"]], 2)
        calc3 <- paste0(calc3, " = ", B)
      } 

      calc4 <- "The mean of the sample errors <i>e&#772</i>"
      if(ready){
        meanE <- round(mean(dataset[, .v(options[["bookValues"]])] - dataset[, .v(options[["auditValues"]])]), 2)
        calc4 <- paste0(calc4, " = ", meanE)
      } 

      calc5 <- "The standard deviation of the sample errors <i>s<sub>e</sub></i>"
      if(ready){
        sE <- round(sd(dataset[, .v(options[["bookValues"]])] - dataset[, .v(options[["auditValues"]])]), 2)
        calc5 <- paste0(calc5, " = ", sE)
      } 

      calc6 <- paste0("The t-value <i>(df = n - 1)</i> corresponding to ", round(options[["confidence"]] * 100), "% confidence <i>t<sub>\u03B1/2</sub></i>")
      if(options[["auditValues"]] != ""){
        calc6 <- paste0("The t-value <i>(df = ", n ," - 1)</i> corresponding to ", round(options[["confidence"]] * 100), "% confidence <i>t<sub>\u03B1/2</sub></i> = ", round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1), 3))
      } 

      calculations <- paste0(calc1, "
                        ", calc2, "
                        ", calc3, "
                        ", calc4, "
                        ", calc5, "
                        ", calc6)
      calculationsContainer[["all"]] <- createJaspHtml(calculations, "p")

      calculationsContainer[["intro2"]] <- createJaspHtml(paste0("This information allows for calculation of:"), "p")

      pointEstimate <- "The point estimate of the true population value <i>W&#770 = B - N x e&#772</i>"
      if(ready){
        W <- round(B - N * meanE, 2)
        pointEstimate <- paste0(pointEstimate, " = ", W)
      }

      uncertainty <- "The uncertainty of the estimator <i>U = t<sub>\u03B1/2</sub> x s<sub>e</sub> x (N / \u221A n) x \u221A (N-n) / (N - 1)</i>"
      if(ready){
        U <- round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * sE * (N / sqrt(n)) * sqrt((N-n)/(N-1)), 2)
        uncertainty <- paste0(uncertainty, " = ", U)
      }

      confidenceInterval <- "The confidence interval around the point estimate <i>W&#770 \u00B1 U"
      if(ready)
        confidenceInterval <- paste0(confidenceInterval, " = ", W, " \u00B1 ", U)
      
      results <- paste0(pointEstimate, "
                  ", uncertainty, "
                  ", confidenceInterval)
      calculationsContainer[["results"]] <- createJaspHtml(results, "p")

      
    } else if(options[["estimator"]] == "ratio"){

      calculationsContainer[["intro"]] <- createJaspHtml(paste0("The required information for the <b>ratio</b> estimator consists of:"), "p")

      calc1 <- "The population size <i>N</i>"
      if(options[["populationSize"]] != 0){
        N <- options[["populationSize"]]
        calc1 <- paste0(calc1, " = ", N)
      }

      calc2 <- "The sample size <i>n</i>"
      if(options[["auditValues"]] != "" || options[["bookValues"]] != ""){
        n <- nrow(dataset)
        calc2 <- paste0(calc2, " = ", n)
      }

      calc3 <- "The sum of the population book values <i>B</i>"
      if(options[["populationValue"]] != 0){
        B <- round(options[["populationValue"]], 2)
        calc3 <- paste0(calc3, " = ", B)
      } 

      calc4 <- "The mean of the sample book values <i>b&#772</i>"
      if(options[["bookValues"]] != ""){
        meanB <- round(mean(dataset[, .v(options[["bookValues"]])]), 2)
        calc4 <- paste0(calc4, " = ", meanB)
      } 

      calc5 <- "The standard deviation of the sample book values <i>s<sub>b</sub></i>"
      if(options[["bookValues"]] != ""){
        sB <- round(sd(dataset[, .v(options[["bookValues"]])]), 2)
        calc5 <- paste0(calc5, " = ", sB)
      } 

      calc6 <- "The mean of the sample true values <i>w&#772</i>"
      if(options[["auditValues"]] != ""){
        meanW <- round(mean(dataset[, .v(options[["auditValues"]])]), 2)
        calc6 <- paste0(calc6, " = ", meanW)
      } 

      calc7 <- "The standard deviation of the sample true values <i>s<sub>w</sub></i>"
      if(options[["auditValues"]] != ""){
        sW <- round(sd(dataset[, .v(options[["auditValues"]])]), 2)
        calc7 <- paste0(calc7, " = ", sW)
      } 

      calc8 <- "The correlation coefficient of the sample book values and true values <i>r<sub>bw</sub></i>"
      if(ready){
        r <- round(cor(dataset[, .v(options[["bookValues"]])], dataset[, .v(options[["auditValues"]])]), 2)
        calc8 <- paste0(calc8, " = ", r)
      } 

      calc9 <- "The correctness ratio <i>q<sub>bw</sub></i>"
      if(ready){
        q <- round(meanW / meanB, 2)
        calc9 <- paste0(calc9, " = ", q)
      } 

      calc10 <- paste0("The t-value <i>(df = n - 1)</i> corresponding to ", round(options[["confidence"]] * 100), "% confidence <i>t<sub>\u03B1/2</sub></i>")
      if(options[["auditValues"]] != ""){
        calc10 <- paste0("The t-value <i>(df = ", n ," - 1)</i> corresponding to ", round(options[["confidence"]] * 100), "% confidence <i>t<sub>\u03B1/2</sub></i> = ", round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1), 3))
      } 

      calculations <- paste0(calc1, "
                  ", calc2, "
                  ", calc3, "
                  ", calc4, "
                  ", calc5, "
                  ", calc6, "
                  ", calc7, "
                  ", calc8, "
                  ", calc9, "
                  ", calc10)
      calculationsContainer[["all"]] <- createJaspHtml(calculations, "p")

      calculationsContainer[["intro2"]] <- createJaspHtml(paste0("This information allows for calculation of:"), "p")

      pointEstimate <- "The point estimate of the true population value <i>W&#770 = q x B</i>"
      if(ready){
        W <- round(q * B, 2)
        pointEstimate <- paste0(pointEstimate, " = ", W)
      }

      uncertainty <- "The uncertainty of the estimator <i>U = t<sub>\u03B1/2</sub> x \u221A(s<sub>w</sub><sup>2</sup> - 2 x q x r x s<sub>b</sub> x s<sub>w</sub> + q<sup>2</sup> x s<sub>b</sub><sup>2</sup>) x (N / \u221A n) x \u221A (N-n) / (N - 1)</i>"
      if(ready){
        U <- round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * sqrt( sW^2 - 2*q*r*sB*sW + q^2 * sB^2 ) * (N / sqrt(n)) * sqrt((N-n)/(N-1)), 2)
        uncertainty <- paste0(uncertainty, " = ", U)
      }

      confidenceInterval <- "The confidence interval around the point estimate <i>W&#770 \u00B1 U"
      if(ready)
        confidenceInterval <- paste0(confidenceInterval, " = ", W, " \u00B1 ", U)
      
      results <- paste0(pointEstimate, "
                  ", uncertainty, "
                  ", confidenceInterval)
      calculationsContainer[["results"]] <- createJaspHtml(results, "p")
      
    } else if(options[["estimator"]] == "regression"){

            calculationsContainer[["intro"]] <- createJaspHtml(paste0("The required information for the <b>regression</b> estimator consists of:"), "p")

      calc1 <- "The population size <i>N</i>"
      if(options[["populationSize"]] != 0){
        N <- options[["populationSize"]]
        calc1 <- paste0(calc1, " = ", N)
      }

      calc2 <- "The sample size <i>n</i>"
      if(options[["auditValues"]] != "" || options[["bookValues"]] != ""){
        n <- nrow(dataset)
        calc2 <- paste0(calc2, " = ", n)
      }

      calc3 <- "The sum of the population book values <i>B</i>"
      if(options[["populationValue"]] != 0){
        B <- round(options[["populationValue"]], 2)
        calc3 <- paste0(calc3, " = ", B)
      } 

      calc4 <- "The mean of the sample book values <i>b&#772</i>"
      if(options[["bookValues"]] != ""){
        meanB <- round(mean(dataset[, .v(options[["bookValues"]])]), 2)
        calc4 <- paste0(calc4, " = ", meanB)
      } 

      calc5 <- "The mean of the sample true values <i>w&#772</i>"
      if(options[["auditValues"]] != ""){
        meanW <- round(mean(dataset[, .v(options[["auditValues"]])]), 2)
        calc5 <- paste0(calc5, " = ", meanW)
      } 

      calc6 <- "The standard deviation of the sample true values <i>s<sub>w</sub></i>"
      if(options[["auditValues"]] != ""){
        sW <- round(sd(dataset[, .v(options[["auditValues"]])]), 2)
        calc6 <- paste0(calc6, " = ", sW)
      } 

      calc7 <- "The correlation coefficient of the sample book values and true values <i>r<sub>bw</sub></i>"
      if(ready){
        r <- round(cor(dataset[, .v(options[["bookValues"]])], dataset[, .v(options[["auditValues"]])]), 2)
        calc7 <- paste0(calc7, " = ", r)
      } 

      calc8 <- "The regression coefficient of the sample book values and true values <i>b<sub>1</sub></i>"
      if(ready){
        b1 <- round((sum(dataset[, .v(options[["bookValues"]])] * dataset[, .v(options[["auditValues"]])]) - n * meanB * meanW) / (sum(dataset[, .v(options[["bookValues"]])]^2) - (sum(dataset[, .v(options[["bookValues"]])])^2) / n), 2)
        calc8 <- paste0(calc8, " = ", b1)
      } 

      calculations <- paste0(calc1, "
            ", calc2, "
            ", calc3, "
            ", calc4, "
            ", calc5, "
            ", calc6, "
            ", calc7, "
            ", calc8)
      calculationsContainer[["all"]] <- createJaspHtml(calculations, "p")

      calculationsContainer[["intro2"]] <- createJaspHtml(paste0("This information allows for calculation of:"), "p")
      pointEstimate <- "The point estimate of the true population value <i>W&#770 = N x w&#772 + b<sub>1</sub> x (B - N x b&#772)</i>"
      if(ready){
        W <- round(N * meanW + b1 * (B - N * meanB), 2)
        pointEstimate <- paste0(pointEstimate, " = ", W)
      }
      
      uncertainty <- "The uncertainty of the estimator <i>U = t<sub>\u03B1/2</sub> x s<sub>w</sub> x \u221A(1 - r<sup>2</sup>) x (N / \u221A n) x \u221A (N-n) / (N - 1)</i>"
      if(ready){
        U <- round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * sW * sqrt(1 - r^2) * (N / sqrt(n)) * sqrt((N-n)/(N-1)), 2)
        uncertainty <- paste0(uncertainty, " = ", U)
      }
      
      confidenceInterval <- "The confidence interval around the point estimate <i>W&#770 \u00B1 U"
      if(ready)
        confidenceInterval <- paste0(confidenceInterval, " = ", W, " \u00B1 ", U)
      
      results <- paste0(pointEstimate, "
                  ", uncertainty, "
                  ", confidenceInterval)
      calculationsContainer[["results"]] <- createJaspHtml(results, "p")
      
    }

    jaspResults[["calculationsContainer"]] <- calculationsContainer

  } else {

    return()

  }
}