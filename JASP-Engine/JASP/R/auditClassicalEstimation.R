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

# When making changes to this file always mention @koenderks as a 
# reviewer in the Pull Request

auditClassicalEstimation <- function(jaspResults, dataset, options, ...){

  # Read in the data 
  dataset <- .auditReadDataEstimation(dataset, 
                                      options)

  # Perform early error checks
  .auditClassicalEstimatinonErrorCheck(dataset, 
                                       options)

  # Ready for analysis
  ready <- .auditClassicalEstimationReady(options)

  # Create explanatory text
  .auditClassicalEstimationParagraph(dataset, 
                                     options, 
                                     jaspResults, 
                                     ready, 
                                     position = 1)

  # --- TABLES
  
  # Create results table
  .auditClassicalEstimationSummaryTable(dataset, 
                                        options, 
                                        jaspResults, 
                                        ready, 
                                        position = 2)

  # Required sample size table
  .auditClassicalEstimationSampleSizeTable(dataset, 
                                           options, 
                                           jaspResults, 
                                           ready, 
                                           position = 3)

  # ---
  
  # --- PLOTS
  
  # Correlation plot
  .auditEstimationCorrelationPlot(dataset, 
                                  options, 
                                  jaspResults, 
                                  ready, 
                                  position = 4)

  # ---
  
  # --- BADGES
  
  # Provide the analysis badges
  .auditBadgeSection(options,
                     type = "estimation",
                     stateContainer = NULL,
                     jaspResults, 
                     ready, 
                     position = 6)

  # ---
}

.auditReadDataEstimation <- function(dataset, 
                                     options){

  bookValues <- options[["bookValues"]]
  if(bookValues == "")  
    bookValues <- NULL

  auditValues <- options[["auditValues"]]
  if(auditValues == "")  
    auditValues <- NULL

  variables <- c(bookValues, auditValues)

  dataset <- .readDataSetToEnd(columns.as.numeric = variables, 
                               exclude.na.listwise = variables)
  return(dataset)
}

.auditClassicalEstimatinonErrorCheck <- function(dataset, 
                                                 options){
  
  variables <- NULL

  if(options[["bookValues"]] != "")
    variables <- c(variables, options[["bookValues"]])
    
  if(options[["auditValues"]] != "")
    variables <- c(variables, options[["auditValues"]])

  .hasErrors(dataset, 
              type=c("infinity", "variance", "observations"),
              all.target = variables, 
              message="short", 
              observations.amount= "< 3",
              exitAnalysisIfErrors = TRUE)
}

.auditClassicalEstimationReady <- function(options){

  if(options[["estimator"]] == "mpu"){

    ready <- options[["auditValues"]] != "" && 
              options[["populationSize"]] != 0
  } else {

    ready <- options[["bookValues"]] != "" && 
              options[["auditValues"]] != "" && 
              options[["populationSize"]] != 0 && 
              options[["populationValue"]] != 0 # adjust for mpu

  }
}

.auditClassicalEstimationSummaryTable <- function(dataset, 
                                                 options, 
                                                 jaspResults, 
                                                 ready, 
                                                 position){

  if(!is.null(jaspResults[["regressionTable"]])) 
    return() #The options for this table didn't change so we don't need to rebuild it

  title <- base::switch(options[["estimator"]],
                        "mpu"         = gettext("Direct estimator"),
                        "difference"  = gettext("Difference estimator"),
                        "ratio"       = gettext("Ratio estimator"),
                        "regression"  = gettext("Regression estimator"))

  regressionTable <- createJaspTable(title)
  regressionTable$position <- position

  regressionTable$dependOn(options = c("bookValues", 
                                       "auditValues", 
                                       "populationValue", 
                                       "populationSize", 
                                       "confidence", 
                                       "estimator"))

  regressionTable$addColumnInfo(name = 'estimate', 
                                title = gettext("Estimate W\u302"), 
                                type = 'string')
  regressionTable$addColumnInfo(name = 'uncertainty', 
                                title = gettext('Uncertainty'), 
                                type = 'string')
  regressionTable$addColumnInfo(name = 'lower',  
                                title = gettext('Lower'), 
                                type = 'string', 
                                overtitle = gettextf("%1$s%% Confidence interval", round(options[["confidence"]] * 100, 2)))
  regressionTable$addColumnInfo(name = 'upper', 
                                title = gettext('Upper'), 
                                type = 'string', 
                                overtitle = gettextf("%1$s%% Confidence interval", round(options[["confidence"]] * 100, 2)))

  jaspResults[["regressionTable"]] <- regressionTable

  if(!ready){

    if(options[["estimator"]] == "mpu"){

      regressionTable$addFootnote(gettext("Please specify the population size and your audit values."))

    } else {

      regressionTable$addFootnote(gettext("Please specify the population size, population value and your sample variables."))
    
    }

    return()
  }

  regressionTable$addFootnote(gettext("Displayed numbers may differ from exact outcomes due to rounding in the calculations."))

  dataset           <- na.omit(dataset)

  N                 <- options[["populationSize"]]
  n                 <- nrow(dataset)

  if(options[["estimator"]] == "mpu"){

    meanW           <- round(mean(dataset[, .v(options[["auditValues"]])]), 2)
    sW              <- round(sd(dataset[, .v(options[["auditValues"]])]), 2)

    pointEstimate   <- round(N * meanW, 2)
    lowerCI         <- round(pointEstimate - 
                             qt(p = (1 - (1 - options[["confidence"]])/2), 
                                df = n - 1) * 
                             sW * 
                             (N / sqrt(n)) * 
                             sqrt((N - n) / (N - 1)), 2)

    upperCI         <- round(pointEstimate + 
                             qt(p = (1 - (1 - options[["confidence"]])/2), 
                                df = n - 1) * 
                             sW * 
                             (N / sqrt(n)) * 
                             sqrt((N - n) / (N - 1)), 2)

    uncertainty     <- round(qt(p = (1 - (1 - options[["confidence"]])/2), 
                                df = n - 1) * 
                             sW * 
                             (N / sqrt(n)) * 
                             sqrt((N - n)/(N - 1)), 2)

    result <- list(sW = sW)

  } else if(options[["estimator"]] == "difference"){

    B               <- round(options[["populationValue"]], 2)

    meanE           <- round(mean(dataset[, .v(options[["bookValues"]])] - 
                                  dataset[, .v(options[["auditValues"]])]), 2)

    sE              <- round(sd(dataset[, .v(options[["bookValues"]])] - 
                                dataset[, .v(options[["auditValues"]])]), 2)
    
    pointEstimate   <- round(B - N * meanE, 2)

    lowerCI         <- round(pointEstimate - 
                              qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * 
                              sE * (N / sqrt(n)) * sqrt((N - n) / (N - 1)), 2)

    upperCI         <- round(pointEstimate + 
                              qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * 
                              sE * (N / sqrt(n)) * sqrt((N - n) / (N - 1)), 2)

    uncertainty     <- round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * 
                              sE * (N / sqrt(n)) * sqrt((N - n)/(N - 1)), 2)

    result <- list(sE = sE)

  } else if(options[["estimator"]] == "ratio"){

    B             <- round(options[["populationValue"]], 2)
    meanB         <- round(mean(dataset[, .v(options[["bookValues"]])]), 2)
    sB            <- round(sd(dataset[, .v(options[["bookValues"]])]), 2)
    meanW         <- round(mean(dataset[, .v(options[["auditValues"]])]), 2)
    sW            <- round(sd(dataset[, .v(options[["auditValues"]])]), 2)
    r             <- round(cor(dataset[, .v(options[["bookValues"]])], 
                               dataset[, .v(options[["auditValues"]])]), 2)

    q             <- round(meanW / meanB, 4)
    s             <- sqrt( sW^2 - 2 * q * r * sB * sW + q^2 * sB^2 )
    pointEstimate <- round(q * B, 2)

    lowerCI       <- round(pointEstimate - 
                            qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * 
                            s * (N / sqrt(n)) * sqrt((N - n) / (N - 1)), 2)

    upperCI       <- round(pointEstimate + 
                            qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * 
                            s * (N / sqrt(n)) * sqrt((N - n)/(N - 1)), 2)

    uncertainty   <- round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * 
                           s * (N / sqrt(n)) * sqrt((N - n)/(N - 1)), 2)

    result <- list(s = s)
 
  } else if(options[["estimator"]] == "regression"){

    B             <- round(options[["populationValue"]], 2)
    meanB         <- round(mean(dataset[, .v(options[["bookValues"]])]), 2)
    meanW         <- round(mean(dataset[, .v(options[["auditValues"]])]), 2)
    sW            <- round(sd(dataset[, .v(options[["auditValues"]])]), 2)
    r             <- round(cor(dataset[, .v(options[["bookValues"]])], 
                               dataset[, .v(options[["auditValues"]])]), 2)

    b1            <- round((sum(dataset[, .v(options[["bookValues"]])] * 
                                dataset[, .v(options[["auditValues"]])]) - 
                            n*meanB*meanW) / 
                           (sum(dataset[, .v(options[["bookValues"]])]^2) - 
                            (sum(dataset[, .v(options[["bookValues"]])])^2) / 
                            n), 2)
    s             <- sW * sqrt(1 - r^2)

    pointEstimate <- round(N * meanW + b1 * (B - N * meanB), 2)
    lowerCI       <- round(pointEstimate - 
                            qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * 
                            s * (N / sqrt(n)) * sqrt((N - n) / (N - 1)), 2)

    upperCI       <- round(pointEstimate + 
                            qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * 
                            s * (N / sqrt(n)) * sqrt((N - n) / (N - 1)), 2)

    uncertainty   <- round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * 
                           s * (N / sqrt(n)) * sqrt((N - n) / (N - 1)), 2)

    result <- list(sW = sW, r = r)

  }

  row <- data.frame(estimate = pointEstimate, 
                    uncertainty = uncertainty, 
                    lower = lowerCI, 
                    upper = upperCI)

  regressionTable$addRows(row)

  jaspResults[["result"]] <- createJaspState(result)
}

.auditClassicalEstimationParagraph <- function(dataset, 
                                               options, 
                                               jaspResults, 
                                               ready, 
                                               position){

  if(options[["explanatoryText"]] || !is.null(jaspResults[["calculationsContainer"]])){

    calculationsContainer <- createJaspContainer(title= gettext("<u>Calculations</u>"))
    calculationsContainer$position <- 1
    calculationsContainer$dependOn(options = c("explanatoryText", "bookValues", "auditValues", "populationValue", "populationSize", "confidence", "estimator"))

    if(options[["estimator"]] == "mpu"){

      calculationsContainer[["intro"]] <- createJaspHtml(gettext("<i>The required information for the <b>direct</b> estimator consists of:</i>"), "p")

      calc1 <- gettext("The population size <i>N</i>")
      if(options[["populationSize"]] != 0){
        N <- options[["populationSize"]]
        calc1 <- gettextf("%1$s = %2$i", calc1, N)
      }

      calc2 <- gettext("The sample size <i>n</i>")
      if(options[["auditValues"]] != ""){
        n <- nrow(dataset)
        calc2 <- gettextf("%1$s = %2$i", calc2, n)
      }

      calc3 <- gettext("The mean of the sample audit values <i>w\u304</i>")
      if(options[["auditValues"]] != ""){
        meanW <- round(mean(dataset[, .v(options[["auditValues"]])]), 2)
        calc3 <- gettextf("%1$s = %2$s", calc3, meanW)
      } 

      calc4 <- gettext("The standard deviation of the sample audit values <i>s<sub>w</sub></i>")
      if(options[["auditValues"]] != ""){
        sW <- round(sd(dataset[, .v(options[["auditValues"]])]), 2)
        calc4 <- gettextf("%1$s = %2$s", calc4, sW)
      } 

      calc5 <- gettextf("The t-value <i>(df = n - 1)</i> corresponding to %1$s%% confidence <i>t<sub>%2$s/2</sub></i>", round(options[["confidence"]] * 100), round(1 - options[["confidence"]], 2))
      if(options[["auditValues"]] != ""){
        calc5 <- gettextf("The t-value <i>(df = %1$s - 1)</i> corresponding to %2$s%% confidence <i>t<sub>%3$s/2</sub></i> = %4$s",
                          n, round(options[["confidence"]] * 100), round(1 - options[["confidence"]], 2),
                          round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1), 3))
      } 

      calculations <- gettextf("%1$s
                                %2$s
                                %3$s
                                %4$s
                                %5$s", calc1, calc2, calc3, calc4, calc5)
      calculationsContainer[["all"]] <- createJaspHtml(calculations, "p")

      calculationsContainer[["intro2"]] <- createJaspHtml(gettext("<i>This information allows for calculation of:</i>"), "p")

      pointEstimate <- gettext("The point estimate of the true population value <i>W\u302 = N \u00D7 w\u304</i>")
      if(ready){
        W <- round(N * meanW, 2)
        pointEstimate <- gettextf("%1$s = %2$s", pointEstimate, W)
      }

      uncertainty <- gettextf("The uncertainty of the estimator <i>U = t<sub>%1$s/2</sub> \u00D7 s<sub>w</sub> \u00D7 <sup>N</sup>&frasl;<sub>\u221A n</sub> \u00D7 \u221A (<sup>N - n</sup>&frasl;<sub>N - 1</sub>)</i>", round(1 - options[["confidence"]], 2))     
      if(ready){
        U <- round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * sW * (N / sqrt(n)) * sqrt((N-n)/(N-1)), 2)
        uncertainty <- gettextf("%1$s = %2$s", uncertainty, U)
      }
      
      confidenceInterval <- gettext("The confidence interval around the point estimate <i>W\u302 \u00B1 U</i>")
      if(ready)
        confidenceInterval <- gettextf("%1$s = %2$s \u00B1 %3$s", confidenceInterval, W, U)
      
      results <- gettextf("%1$s
                           %2$s
                           %3$s", 
                           pointEstimate, 
                           uncertainty, 
                           confidenceInterval)
      calculationsContainer[["results"]] <- createJaspHtml(results, "p")

    } else if(options[["estimator"]] == "difference"){

      calculationsContainer[["intro"]] <- createJaspHtml(gettext("<i>The required information for the <b>difference</b> estimator consists of:</i>"), "p")

      calc1 <- gettext("The population size <i>N</i>")
      if(options[["populationSize"]] != 0){
        N <- options[["populationSize"]]
        calc1 <- gettextf("%1$s = %2$s", calc1, N)
      }

      calc2 <- gettext("The sample size <i>n</i>")
      if(options[["auditValues"]] != "" || options[["bookValues"]] != ""){
        n <- nrow(dataset)
        calc2 <- gettextf("%1$s = %2$s", calc2, n)
      }

      calc3 <- gettext("The sum of the population book values <i>B</i>")
      if(options[["populationValue"]] != 0){
        B <- round(options[["populationValue"]], 2)
        calc3 <- gettextf("%1$s = %2$s", calc3, B)
      } 

      calc4 <- gettext("The mean of the sample errors <i>e\u304</i>")
      if(ready){
        meanE <- round(mean(dataset[, .v(options[["bookValues"]])] - dataset[, .v(options[["auditValues"]])]), 2)
        calc4 <- gettextf("%1$s = %2$s", calc4, meanE)
      } 

      calc5 <- gettext("The standard deviation of the sample errors <i>s<sub>e</sub></i>")
      if(ready){
        sE <- round(sd(dataset[, .v(options[["bookValues"]])] - dataset[, .v(options[["auditValues"]])]), 2)
        calc5 <- gettextf("%1$s = %2$s", calc5, sE)
      } 

      calc6 <- gettextf("The t-value <i>(df = n - 1)</i> corresponding to %1$s%% confidence <i>t<sub>%2$s/2</sub></i>",
                        round(options[["confidence"]] * 100),
                        round(1 - options[["confidence"]], 2))
      if(options[["auditValues"]] != ""){
        calc6 <- gettextf("The t-value <i>(df = %1$s - 1)</i> corresponding to %2$s%% confidence <i>t<sub>%3$s/2</sub></i> = %4$s",
                          n,
                          round(options[["confidence"]] * 100),
                          round(1 - options[["confidence"]], 2),
                          round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1), 3))
      } 

      calculations <- gettextf("%1$s
                                %2$s
                                %3$s
                                %4$s
                                %5$s
                                %6$s",
                                calc1, calc2, calc3, calc4, calc5, calc6)
      calculationsContainer[["all"]] <- createJaspHtml(calculations, "p")

      calculationsContainer[["intro2"]] <- createJaspHtml(gettext("<i>This information allows for calculation of:</i>"), "p")

      pointEstimate <- gettext("The point estimate of the true population value <i>W\u302 = B - N \u00D7 e\u304</i>")
      if(ready){
        W <- round(B - N * meanE, 2)
        pointEstimate <- gettextf("%1$s = %2$s", pointEstimate, W)
      }

      uncertainty <- gettextf("The uncertainty of the estimator <i>U = t<sub>%1$s/2</sub> \u00D7 s<sub>e</sub> \u00D7 <sup>N</sup>&frasl;<sub>\u221A n</sub> \u00D7 \u221A (<sup>N - n</sup>&frasl;<sub>N - 1</sub>)</i>", round(1 - options[["confidence"]], 2))
      if(ready){
        U <- round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * sE * (N / sqrt(n)) * sqrt((N-n)/(N-1)), 2)
        uncertainty <- gettextf("%1$s = %2$s", uncertainty, U)
      }

      confidenceInterval <- gettext("The confidence interval around the point estimate <i>W\u302 \u00B1 U</i>")
      if(ready)
        confidenceInterval <- gettextf("%1$s = %2$s \u00B1 %3$s", confidenceInterval, W, U)
      
      results <- gettextf("%1$s
                            %2$s
                            %3$s",
                            pointEstimate,  
                            uncertainty, 
                            confidenceInterval)
      calculationsContainer[["results"]] <- createJaspHtml(results, "p")
      
    } else if(options[["estimator"]] == "ratio"){
 
      calculationsContainer[["intro"]] <- createJaspHtml(gettext("<i>The required information for the <b>ratio</b> estimator consists of:</i>"), "p")

      calc1 <- gettext("The population size <i>N</i>")
      if(options[["populationSize"]] != 0){
        N <- options[["populationSize"]]
        calc1 <- gettextf("%1$s = %2$s", calc1, N)
      }

      calc2 <- gettext("The sample size <i>n</i>")
      if(options[["auditValues"]] != "" || options[["bookValues"]] != ""){
        n <- nrow(dataset)
        calc2 <- gettextf("%1$s = %2$s", calc2, n)
      }

      calc3 <- gettext("The sum of the population book values <i>B</i>")
      if(options[["populationValue"]] != 0){
        B <- round(options[["populationValue"]], 2)
        calc3 <- gettextf("%1$s = %2$s", calc3, B)
      } 

      calc4 <- gettext("The mean of the sample book values <i>b\u304</i>")
      if(options[["bookValues"]] != ""){
        meanB <- round(mean(dataset[, .v(options[["bookValues"]])]), 2)
        calc4 <- gettextf("%1$s = %2$s", calc4, meanB)
      } 

      calc5 <- gettextf("The standard deviation of the sample book values %1$s", "<i>s<sub>b</sub></i>")
      if(options[["bookValues"]] != ""){
        sB <- round(sd(dataset[, .v(options[["bookValues"]])]), 2)
        calc5 <- gettextf("%1$s = %2$s", calc5, sB)
      } 

      calc6 <- gettext("The mean of the sample audit values <i>w\u304</i>")
      if(options[["auditValues"]] != ""){
        meanW <- round(mean(dataset[, .v(options[["auditValues"]])]), 2)
        calc6 <- gettextf("%1$s = %2$s", calc6, meanW)
      } 

      calc7 <- gettext("The standard deviation of the sample audit values <i>s<sub>w</sub></i>")
      if(options[["auditValues"]] != ""){
        sW <- round(sd(dataset[, .v(options[["auditValues"]])]), 2)
        calc7 <- gettextf("%1$s = %2$s", calc7, sW)
      } 

      calc8 <- gettextf("The correlation coefficient of the sample book values and audit values %1$s", "<i>r<sub>bw</sub></i>")
      if(ready){
        r <- round(cor(dataset[, .v(options[["bookValues"]])], dataset[, .v(options[["auditValues"]])]), 2)
        calc8 <- gettextf("%1$s = %2$s", calc8, r)
      } 

      calc9 <- gettextf("The correctness ratio %1$s", "<i>q<sub>bw</sub></i>")
      if(ready){
        q <- round(meanW / meanB, 4)
        calc9 <- gettextf("%1$s = %2$s", calc9, q)
      } 

      calc10 <- gettextf("The t-value <i>(df = n - 1)</i> corresponding to %1$s%% confidence <i>t<sub>%2$s/2</sub></i>",
                         round(options[["confidence"]] * 100),
                         round(1 - options[["confidence"]], 2))
      if(options[["auditValues"]] != ""){
        calc10 <- gettextf("The t-value <i>(df = %1$s - 1)</i> corresponding to %2$s%% confidence <i>t<sub>%3$s/2</sub></i> = %4$s",
                           n, 
                           round(options[["confidence"]] * 100),
                           round(1 - options[["confidence"]], 2),
                           round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1), 3))
      } 

      calculations <- gettextf("%1$s
                                %2$s
                                %3$s
                                %4$s
                                %5$s
                                %6$s
                                %7$s
                                %8$s
                                %9$s
                                %10$s",
                              calc1, calc2, calc3, calc4, calc5,  
                              calc6, calc7, calc8, calc9, calc10)
      calculationsContainer[["all"]] <- createJaspHtml(calculations, "p")

      calculationsContainer[["intro2"]] <- createJaspHtml(gettext("<i>This information allows for calculation of:</i>"), "p")

      pointEstimate <- gettext("The point estimate of the true population value <i>W\u302 = q<sub>bw</sub> \u00D7 B</i>")
      if(ready){
        W <- round(q * B, 2)
        pointEstimate <- gettextf("%1$s = %2$s", pointEstimate, W)
      }

      uncertainty <- gettextf("The uncertainty of the estimator <i>U = t<sub>%1$s/2</sub> \u00D7 \u221A(s<sub>w</sub><sup>2</sup> - 2 \u00D7 q<sub>bw</sub> \u00D7 r<sub>bw</sub> \u00D7 s<sub>b</sub> \u00D7 s<sub>w</sub> + q<sub>bw</sub><sup>2</sup> \u00D7 s<sub>b</sub><sup>2</sup>) \u00D7 <sup>N</sup>&frasl;<sub>\u221A n</sub> \u00D7 \u221A (<sup>N - n</sup>&frasl;<sub>N - 1</sub>)</i>",round(1 - options[["confidence"]], 2))
      if(ready){
        U <- round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * sqrt( sW^2 - 2*q*r*sB*sW + q^2 * sB^2 ) * (N / sqrt(n)) * sqrt((N-n)/(N-1)), 2)
        uncertainty <- gettextf("%1$s = %2$s", uncertainty, U)
      }

      confidenceInterval <- gettext("The confidence interval around the point estimate <i>W\u302 \u00B1 U</i>")
      if(ready)
        confidenceInterval <- gettextf("%1$s = %2$s \u00B1 %3$s", confidenceInterval, W, U)
      
      results <- gettextf("%1$s
                           %2$s
                           %3$s",
                          pointEstimate, uncertainty, confidenceInterval)
      calculationsContainer[["results"]] <- createJaspHtml(results, "p")
      
    } else if(options[["estimator"]] == "regression"){

      calculationsContainer[["intro"]] <- createJaspHtml(gettext("<i>The required information for the <b>regression</b> estimator consists of:</i>"), "p")

      calc1 <- gettext("The population size <i>N</i>")
      if(options[["populationSize"]] != 0){
        N <- options[["populationSize"]]
        calc1 <- gettextf("%1$s = %2$s", calc1, N)
      }

      calc2 <- gettext("The sample size <i>n</i>")
      if(options[["auditValues"]] != "" || options[["bookValues"]] != ""){
        n <- nrow(dataset)
        calc2 <- gettextf("%1$s = %2$s", calc2, n)
      }

      calc3 <- gettext("The sum of the population book values <i>B</i>")
      if(options[["populationValue"]] != 0){
        B <- round(options[["populationValue"]], 2)
        calc3 <- gettextf("%1$s = %2$s", calc3, B)
      } 

      calc4 <- gettext("The mean of the sample book values <i>b\u304</i>")
      if(options[["bookValues"]] != ""){
        meanB <- round(mean(dataset[, .v(options[["bookValues"]])]), 2)
        calc4 <- gettextf("%1$s = %2$s", calc4, meanB)
      } 

      calc5 <- gettext("The mean of the sample audit values <i>w\u304</i>")
      if(options[["auditValues"]] != ""){
        meanW <- round(mean(dataset[, .v(options[["auditValues"]])]), 2)
        calc5 <- gettextf("%1$s = %2$s", calc5, meanW)
      } 

      calc6 <- gettext("The standard deviation of the sample audit values <i>s<sub>w</sub></i>")
      if(options[["auditValues"]] != ""){
        sW <- round(sd(dataset[, .v(options[["auditValues"]])]), 2)
        calc6 <- gettextf("%1$s = %2$s", calc6, sW)
      } 

      calc7 <- gettext("The correlation coefficient of the sample book values and audit values <i>r<sub>bw</sub></i>")
      if(ready){
        r <- round(cor(dataset[, .v(options[["bookValues"]])], dataset[, .v(options[["auditValues"]])]), 2)
        calc7 <- gettextf("%1$s = %2$s", calc7, r)
      } 

      calc8 <- gettext("The regression coefficient of the sample book values and audit values <i>b<sub>1</sub></i>")
      if(ready){
        b1 <- round((sum(dataset[, .v(options[["bookValues"]])] * dataset[, .v(options[["auditValues"]])]) - n * meanB * meanW) / (sum(dataset[, .v(options[["bookValues"]])]^2) - (sum(dataset[, .v(options[["bookValues"]])])^2) / n), 2)
        calc8 <- gettextf("%1$s = %2$s", calc8, b1)
      } 

      calc9 <- gettextf("The t-value <i>(df = n - 1)</i> corresponding to %1$s%% confidence <i>t<sub>%2$s/2</sub></i>",
                       round(options[["confidence"]] * 100, 2),
                       round(1 - options[["confidence"]], 2))
      if(options[["auditValues"]] != ""){
        calc9 <- gettextf("The t-value <i>(df = %1$s - 1)</i> corresponding to %2$s%% confidence <i>t<sub>%3$s/2</sub></i> = %4$s",
                         n,
                         round(options[["confidence"]] * 100, 2),
                         round(1 - options[["confidence"]], 2),
                         round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1), 3))
      } 

      calculations <- gettextf("%1$s
                                %2$s
                                %3$s
                                %4$s
                                %5$s
                                %6$s
                                %7$s
                                %8$s
                                %9$s",
                                calc1, calc2, calc3, calc4, calc5, 
                                calc6, calc7, calc8, calc9)
      calculationsContainer[["all"]] <- createJaspHtml(calculations, "p")

      calculationsContainer[["intro2"]] <- createJaspHtml(gettext("<i>This information allows for calculation of:</i>"), "p")
      
      pointEstimate <- gettext("The point estimate of the true population value <i>W\u302 = N \u00D7 w\u304 + b<sub>1</sub> x (B - N \u00D7 b\u304)</i>")
      if(ready){
        W <- round(N * meanW + b1 * (B - N * meanB), 2)
        pointEstimate <- gettextf("%1$s = %2$s", pointEstimate, W)
      }
     
      uncertainty <- gettextf("The uncertainty of the estimator <i>U = t<sub>%1$s/2</sub> \u00D7 s<sub>w</sub> \u00D7 \u221A(1 - r<sub>bw</sub><sup>2</sup>) \u00D7 <sup>N</sup>&frasl;<sub>\u221A n</sub> \u00D7 \u221A (<sup>N - n</sup>&frasl;<sub>N - 1</sub>)</i>",round(1 - options[["confidence"]], 2))
      if(ready){
        U <- round(qt(p = (1 - (1 - options[["confidence"]])/2), df = n - 1) * sW * sqrt(1 - r^2) * (N / sqrt(n)) * sqrt((N-n)/(N-1)), 2)
        uncertainty <- gettextf("%1$s = %2$s", uncertainty, U)
      }
      
      confidenceInterval <- gettext("The confidence interval around the point estimate <i>W\u302 \u00B1 U</i>")
      if(ready)
        confidenceInterval <- gettextf("%1$s = %2$s \u00B1 %3$s", confidenceInterval, W, U)
      
      results <- gettextf("%1$s
                           %2$s
                           %3$s",
                           pointEstimate,  
                           uncertainty, 
                           confidenceInterval)
      calculationsContainer[["results"]] <- createJaspHtml(results, "p")
      
    }

    jaspResults[["calculationsContainer"]] <- calculationsContainer

  } else {

    return()

  }
}

.auditClassicalEstimationSampleSizeTable <- function(dataset, 
                                                     options, 
                                                     jaspResults, 
                                                     ready, 
                                                     position){

  if(!is.null(jaspResults[["requiredSampleSizeTable"]]) || 
      !options[["requiredSampleSizeTable"]]) return()

  title <- base::switch(options[["estimator"]],
                        "mpu"         = "Direct",
                        "difference"  = "Difference",
                        "ratio"       = "Ratio",
                        "regression"  = "Regression")

  requiredSampleSizeTable <- createJaspTable(gettext("Required Sample Size"))
  requiredSampleSizeTable$position <- position
  requiredSampleSizeTable$dependOn(options = c("requiredSampleSizeTable", 
                                                "bookValues", 
                                                "auditValues", 
                                                "populationValue", 
                                                "populationSize", 
                                                "confidence", 
                                                "estimator", 
                                                "requiredUncertainty"))

  requiredSampleSizeTable$addColumnInfo(name = 'estimator', 
                                        title = gettext('Estimator'), 
                                        type = 'string')
  requiredSampleSizeTable$addColumnInfo(name = 'uncertainty', 
                                        title = gettext('Uncertainty'), 
                                        type = 'string')
  requiredSampleSizeTable$addColumnInfo(name = 'n', 
                                        title = gettext('Required <i>n</i>'), 
                                        type = 'string')
  requiredSampleSizeTable$addColumnInfo(name = 'nextra', 
                                        title = gettext('Additional <i>n</i>'), 
                                        type = 'string')

  requiredSampleSizeTable[["estimator"]]    <- title
  E                                         <- options[["requiredUncertainty"]]
  requiredSampleSizeTable[["uncertainty"]]  <- E

  jaspResults[["requiredSampleSizeTable"]]  <- requiredSampleSizeTable

  if(!ready) return()

  result <- jaspResults[["result"]]$object

  if(options[["estimator"]] == "mpu"){
    gamma <- E^2 / (qt(p = (1 - (1 - options[["confidence"]])/2), df = nrow(dataset) - 1)^2 * options[["populationSize"]] * result[["sW"]]^2)
  } else if(options[["estimator"]] == "difference"){
    gamma <- E^2 / (qt(p = (1 - (1 - options[["confidence"]])/2), df = nrow(dataset) - 1)^2 * options[["populationSize"]] * result[["sE"]]^2)
  } else if(options[["estimator"]] == "ratio"){
    gamma <- E^2 / (qt(p = (1 - (1 - options[["confidence"]])/2), df = nrow(dataset) - 1)^2 * options[["populationSize"]] * result[["s"]]^2)
  } else if(options[["estimator"]] == "regression"){
    gamma <- E^2 / (qt(p = (1 - (1 - options[["confidence"]])/2), df = nrow(dataset) - 1)^2 * options[["populationSize"]] * result[["sW"]]^2 * (1 - result[["r"]]^2))
  }
  n2 <- ceiling(options[["populationSize"]] / (1 + gamma))

  requiredSampleSizeTable[["n"]] <- n2
  nExtra <- n2 - nrow(dataset)
  if(nExtra < 1)
    nExtra <- 0
  requiredSampleSizeTable[["nextra"]] <- nExtra

}

.auditEstimationCorrelationPlot <- function(dataset, 
                                            options, 
                                            jaspResults, 
                                            ready, 
                                            position){

  if(!is.null(jaspResults[["correlationPlot"]]) || 
      !options[["correlationPlot"]] || 
      options[["estimator"]] == "mpu") return()

  correlationPlot <- createJaspPlot(plot = NULL, title = gettext("Correlation Plot"), width = 500, height = 400)
  correlationPlot$position <- position
  correlationPlot$dependOn(options = c("correlationPlot", 
                                       "bookValues", 
                                       "auditValues", 
                                       "explanatoryText", 
                                       "estimator",
                                       "populationValue",
                                       "populationSize"))

  jaspResults[["correlationPlot"]] <- correlationPlot

  if(!ready) return()

  d <- data.frame(xx= dataset[,.v(options[["bookValues"]])], yy= dataset[,.v(options[["auditValues"]])])
  co <- cor(d$xx, d$yy, method = "pearson")
  d <- na.omit(d)
  d <- ceiling(d)
  xVar <- d$xx
  yVar <- d$yy

  fit <- vector("list", 1)# vector("list", 4)
  fit[[1]] <- lm(yy ~ poly(xx, 1, raw= TRUE), data = d)

  bestModel <- 1 # which.min(Bic)

  # format x labels
  xlow <- min(pretty(xVar))
  xhigh <- max(pretty(xVar))
  xticks <- pretty(c(xlow, xhigh))
  xLabs <- vector("character", length(xticks))
  xLabs <- format(xticks, digits= 3, scientific = FALSE)

  # Format y labels
  yticks <- xticks
  yLabs <- vector("character", length(yticks))
  yLabs <- format(yticks, digits= 3, scientific = FALSE)

  co <- round(co, 2)

  cols <- rep("gray", nrow(d))
  cols[which(d$xx != d$yy)] <- "red"

  p <- ggplot2::ggplot(data = d, mapping = ggplot2::aes(x = xx, y = yy)) +
        ggplot2::scale_x_continuous(name = gettext("Book values"),
                            breaks = xticks,
                            labels = xLabs) +
        ggplot2::scale_y_continuous(name = gettext("Audit values"),
                            breaks = yticks,
                            labels = yLabs) + 
        JASPgraphs::geom_point(size = 3, fill = cols)
        
  p <- .auditCorrelationPlotAddLine(fit = fit[[bestModel]], 
                                  plot = p, 
                                  line = TRUE, 
                                  xMin= xticks[1], 
                                  xMax = xticks[length(xticks)], 
                                  lwd = 1)
  p <- p + ggplot2::annotate("text", x = xticks[1], y = (yticks[length(yticks)] - ((yticks[length(yticks)] - yticks[length(yticks) - 1]) / 2)),
                              label = paste0("italic(r) == ", co), size = 8, parse = TRUE, hjust = -0.5, vjust = 0.5)
  p <- p + ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color="#cbcbcb"), panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"))
  
  p <- JASPgraphs::themeJasp(p)

  correlationPlot$plotObject <- p

  if(options[["explanatoryText"]]){
      figure1 <- createJaspHtml(gettext("<b>Figure 1.</b> Scatter plot of the book values in the selection and their audit values. Red dots indicate observations that did not match their original book value. If these red dots lie in the bottom part of the graph, the book values are overstated. If these red dots lie in the upper part of the graph, they are understated. The value <i>r</i> is the Pearson correlation coefficient of the book values and the audit values, an indicator of the strength of the linear relationship between the two variables."), "p")
      figure1$position <- position + 1
      figure1$dependOn(optionsFromObject = correlationPlot)
      figure1$dependOn(options = "explanatoryText")
      jaspResults[["figure1"]] <- figure1
  }
}
