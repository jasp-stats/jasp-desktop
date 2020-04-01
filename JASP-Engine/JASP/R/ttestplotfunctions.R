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


.plotSequentialBF.ttest <- function(x= NULL, y= NULL, paired= FALSE, BF10post, callback=function(...) 0, formula= NULL, data= NULL, rscale= 1, oneSided= FALSE, lwd= 2, cexPoints= 1.4,
                                    cexAxis= 1.2, cexYlab= 1.5, cexXlab= 1.6, cexTextBF= 1.4, cexText=1.2, cexLegend= 1.2, cexEvidence= 1.6,	lwdAxis= 1.2, plotDifferentPriors= FALSE,
                                    BFH1H0= TRUE, dontPlotData= FALSE, level1=NULL, level2= NULL, subDataSet=NULL, options) {

  #### settings ####

  if (!plotDifferentPriors) {

    evidenceText <-  TRUE
  } else {

    evidenceText <-  FALSE
  }


  if (rscale == "medium") {

    r <- sqrt(2) / 2
  }

  if (rscale == "wide") {

    r <- 1
  }

  if (rscale == "ultrawide") {

    r <- sqrt(2)
  }

  if (mode(rscale) == "numeric") {

    r <- rscale
  }


  if (oneSided == FALSE) {

    nullInterval <- NULL
  }

  if (oneSided == "right") {

    nullInterval <- c(0, Inf)
  }

  if (oneSided == "left") {

    nullInterval <- c(-Inf, 0)
  }


  par(mar= c(5.6, 6, 7, 7) + 0.1, las=1)


  if (dontPlotData) {

    plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")

    axis(1, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, xlab="")
    axis(2, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, ylab="")

    mtext("n", side = 1, cex = cexXlab, line= 2.5)

    if (oneSided == FALSE) {

      if (BFH1H0) {

        mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
      } else {

        mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYlab, line= 3.1)
      }
    }

    if (oneSided == "right") {

      if (BFH1H0) {

        mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
      } else {

        mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYlab, line= 3.1)
      }
    }

    if (oneSided == "left") {

      if (BFH1H0) {

        mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
      } else {

        mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYlab, line= 3.1)
      }
    }

    return()
  }


  if (is.null(y) || paired) {

    BF10 <- vector("numeric", max(length(x), length(y)))
    BF10w <- vector("numeric", max(length(x), length(y)))
    BF10u <- vector("numeric", max(length(x), length(y)))

    idData <- 1

    if (is.null(y)) {

      ind <- which(x == x[1])
      idData <- sum((ind+1)-(1:(length(ind))) == 1)

    } else {

      idData <- 1


      for (i in 2:(min(c(length(x), length(y))))) {

        previous  <- c(x[i-1], y[i-1])

        if (all(c(x[i], y[i]) == previous)) {

          idData <- idData + 1

        } else if (x[i] == y[i]) {

          idData <- idData + 1

        } else {

          break
        }
      }
    }

    if ( ! .shouldContinue(callback()))
      return()

    BF10[1:idData] <- 1
    BF10w[1:idData] <- 1
    BF10u[1:idData] <- 1


    if (idData < length(x)) {

      i <- idData + 1

    } else {

      i <- idData

    }

    if (idData < length(y)) {

      j <- idData + 1

    } else {

      j <- idData

    }

    k <- idData + 1


    while ((i <= length(x) | j <= length(y)) & k <= length(BF10)) {

      bfObject <- .generalTtestBF(x = x[1:i], y = y[1:j], paired = paired, oneSided = oneSided, options = options)
      BF10[k] <- bfObject[["bf"]]
      # if (oneSided == FALSE) {
      #
      # 	BF <- BayesFactor::ttestBF(x = x[1:i], y= y[1:j], paired = paired, rscale=r, nullInterval = nullInterval)
      # 	BF10[k] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]
      #
      # } else {
      #
      # 	BF10[k] <- .oneSidedTtestBFRichard(x = x[1:i], y= y[1:j], paired = paired, r=r, oneSided=oneSided)
      # }

      k <- k + 1

      if (i < length(x)) {

        i <- i + 1
      }
      if (j < length(y)) {

        j <- j + 1
      }

      if ( ! .shouldContinue(callback()))
        return()
    }


    BF10 <- BF10[is.finite(BF10)]

    if ( ! .shouldContinue(callback()))
      return()

    if (plotDifferentPriors) {

      if (idData < length(x)) {

        i <- idData + 1

      } else {

        i <- idData

      }

      if (idData < length(y)) {

        j <- idData + 1

      } else {

        j <- idData

      }

      k <- idData + 1


      while ((i <= length(x) | j <= length(y)) & k <= length(BF10u)) {

        if (oneSided == FALSE) {

          BF <- BayesFactor::ttestBF(x = x[1:i], y= y[1:j], paired = paired, rscale="ultrawide", nullInterval = nullInterval)
          BF10u[k] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]

        } else {

          BF10u[k] <- .oneSidedTtestBFRichard(x = x[1:i], y= y[1:j], paired = paired, r="ultrawide", oneSided=oneSided)
        }

        k <- k + 1

        if (i < length(x)) {

          i <- i + 1
        }
        if (j < length(y)) {

          j <- j + 1
        }

        if ( ! .shouldContinue(callback()))
          return()
      }


      BF10u <- BF10u[is.finite(BF10u)]

      if ( ! .shouldContinue(callback()))
        return()


      if (idData < length(x)) {

        i <- idData + 1

      } else {

        i <- idData

      }

      if (idData < length(y)) {

        j <- idData + 1

      } else {

        j <- idData

      }

      k <- idData + 1


      while ((i <= length(x) | j <= length(y)) & k <= length(BF10w)) {

        if (oneSided == FALSE) {

          BF <- BayesFactor::ttestBF(x = x[1:i], y= y[1:j], paired = paired, rscale= "wide", nullInterval = nullInterval)
          BF10w[k] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]

        } else {

          BF10w[k] <- .oneSidedTtestBFRichard(x = x[1:i], y= y[1:j], paired = paired, r="wide", oneSided=oneSided)
        }

        k <- k + 1

        if (i < length(x)) {

          i <- i + 1
        }
        if (j < length(y)) {

          j <- j + 1
        }

        if ( ! .shouldContinue(callback()))
          return()
      }

      BF10w <- BF10w[is.finite(BF10w)]

      if ( ! .shouldContinue(callback()))
        return()

    }

  } else if (!is.null(y) && !paired) {

    idData <- 1

    xx <- numeric()
    yy <- numeric()

    BF10 <- vector("numeric", nrow(subDataSet))
    BF10w <- vector("numeric", nrow(subDataSet))
    BF10u <- vector("numeric", nrow(subDataSet))

    for (i in seq_len(nrow(subDataSet))) {

      if (subDataSet[i, 2] == level1) {

        xx <- c(xx, subDataSet[i, 1])

      } else if (subDataSet[i, 2] == level2) {

        yy <- c(yy, subDataSet[i, 1])

      }

      if (length(xx) > 1 && length(yy) > 1 && (sd(xx) > 0 || sd(yy) > 0)) {

        bfObject <- .generalTtestBF(x = xx, y = yy, paired = paired, oneSided = oneSided, options = options)
        BF10[i] <- bfObject[["bf"]]
        # if (oneSided == FALSE) {
        #
        # 	BF <- BayesFactor::ttestBF(x = xx, y= yy, paired = paired, rscale= r, nullInterval = nullInterval)
        # 	BF10[i] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]
        #
        # } else if (oneSided == "right") {
        #
        # 	BF10[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided= "right", r=r)
        #
        # } else if (oneSided == "left") {
        #
        # 	BF10[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided= "left", r=r)
        # }

      } else {

        BF10[i] <- 1
      }
    }


    if (plotDifferentPriors) {

      xx <- numeric()
      yy <- numeric()

      for (i in seq_len(nrow(subDataSet))) {

        if (subDataSet[i, 2] == level1) {

          xx <- c(xx, subDataSet[i, 1])

        } else if (subDataSet[i, 2] == level2) {

          yy <- c(yy, subDataSet[i, 1])

        }

        if (length(xx) > 1 && length(yy) > 1 && (sd(xx) > 0 || sd(yy) > 0)) {

          if (oneSided == FALSE) {

            BF <- BayesFactor::ttestBF(x = xx, y= yy, paired = paired, rscale= "ultrawide", nullInterval = nullInterval)
            BF10u[i] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]

          } else if (oneSided == "right") {

            BF10u[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided= "right", r="ultrawide")

          } else if (oneSided == "left") {

            BF10u[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided= "left", r="ultrawide")
          }

        } else {

          BF10u[i] <- 1
        }
      }

      xx <- numeric()
      yy <- numeric()

      for (i in seq_len(nrow(subDataSet))) {

        if (subDataSet[i, 2] == level1) {

          xx <- c(xx, subDataSet[i, 1])

        } else if (subDataSet[i, 2] == level2) {

          yy <- c(yy, subDataSet[i, 1])

        }

        if (length(xx) > 1 && length(yy) > 1 && (sd(xx) > 0 || sd(yy) > 0)) {

          if (oneSided == FALSE) {

            BF <- BayesFactor::ttestBF(x = xx, y= yy, paired = paired, rscale= "wide", nullInterval = nullInterval)
            BF10w[i] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]

          } else if (oneSided == "right") {

            BF10w[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided= "right", r="wide")

          } else if (oneSided == "left") {

            BF10w[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided= "left", r="wide")
          }

        } else {

          BF10w[i] <- 1
        }
      }
    }
  }

  ####################### scale y axis ###########################

  if (plotDifferentPriors) {

    BF <- c(BF10, BF10u, BF10w)

  } else {

    BF <- BF10

  }


  if (!BFH1H0) {

    BF <- 1 / BF
    BF10 <- 1 / BF10

    if (plotDifferentPriors) {

      BF10u  <- 1 / BF10u
      BF10w <- 1 / BF10w
    }
  }


  # y-axis labels larger than 1

  y1h <- "1"

  i <- 1

  while (eval(parse(text= y1h[i])) < max(BF)) {

    if (grepl(pattern = "e",y1h[i])) {

      newy <- paste(strsplit(y1h[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y1h[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
    } else {

      newy <- paste(y1h[i], "0", sep= "")
    }

    if (eval(parse(text=newy)) >= 10^6) {

      newy <- format(as.numeric(newy), digits= 3, scientific = TRUE)
    }

    y1h <- c(y1h, newy)
    i <- i + 1
  }


  y3h <- "3"

  i <- 1

  while (eval(parse(text= y3h[i])) < max(BF)) {

    if (grepl(pattern = "e",y3h[i])) {

      newy <- paste(strsplit(y3h[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y3h[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
    } else {

      newy <- paste(y3h[i], "0", sep= "")
    }

    if (as.numeric(newy) >= 10^6) {

      newy <- format(as.numeric(newy), digits= 3, scientific = TRUE)
    }

    y3h <- c(y3h, newy)

    i <- i + 1
  }

  if ( ! .shouldContinue(callback()))
    return()

  yhigh <- vector("numeric", length(y1h) + length(y3h))

  o <- 1
  e <- 1

  for (i in seq_along(yhigh)) {

    if (i %% 2 == 1) {

      yhigh[i] <- y1h[o]
      o <- o + 1
    }

    if (i %% 2 == 0) {

      yhigh[i] <- y3h[e]
      e <- e + 1
    }
  }

  yhighLab <- as.character(yhigh)


  # y-axis labels smaller than 1

  y1l <- "1/1"

  i <- 1

  while (eval(parse(text= y1l[i])) > min(BF)) {

    if (grepl(pattern = "e",y1l[i])) {

      newy <- paste(strsplit(y1l[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y1l[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
    } else {

      newy <- paste(y1l[i], "0", sep= "")
    }

    if (eval(parse(text= newy)) <= 10^(-6)) {

      newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
      newy <-  sub("-", "+", x = newy)
      newy <- paste0("1/", newy)
    }

    y1l <- c(y1l, newy)
    i <- i + 1
  }


  y3l <- "1/3"

  i <- 1

  while (eval(parse(text= y3l[i])) > min(BF)) {

    if (grepl(pattern = "e",y3l[i])) {

      newy <- paste(strsplit(y3l[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y3l[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
    } else {

      newy <- paste(y3l[i], "0", sep= "")
    }

    if (newy == "1/3e+9") {

      newy <- "1/3e+09"
    }

    if (eval(parse(text= newy)) <= 10^(-6) & eval(parse(text= newy)) > 10^(-9)) {

      newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
      newy <- paste(substring(newy, 1, nchar(newy)-1), as.numeric(substring(newy, nchar(newy), nchar(newy)))-1,sep="")
      newy <- sub(".33", "", newy)
      newy <-  sub("-", "+", x = newy)
      newy <- paste0("1/", newy)
    }

    y3l <- c(y3l, newy)
    i <- i + 1
  }

  if ( ! .shouldContinue(callback()))
    return()

  ylow <- vector("numeric", length(y1l) + length(y3l))
  o <- 1
  e <- 1

  for (i in seq_along(ylow)) {

    if (i %% 2 == 1) {

      ylow[i] <- y1l[o]
      o <- o + 1
    }

    if (i %% 2 == 0) {

      ylow[i] <- y3l[e]
      e <- e + 1
    }
  }

  yLab <- c(rev(ylow[-1]), yhighLab)


  # remove 3's if yLab vector is too long
  omit3s <- FALSE

  if (length(yLab) > 9) {

    omit3s <- TRUE

    ind <- which(yLab == "3")

    yLabsHigh <- yLab[ind:length(yLab)]

    if (length(yLabsHigh) > 1) {

      yLabsHigh <- yLabsHigh[seq(2, length(yLabsHigh),2)]
    } else {

      yLabsHigh <- character(0)
    }

    yLabsLow <- yLab[1:(ind-1)]
    yLabsLow <- yLabsLow[-grep(pattern = "/3", x = yLab)]

    yLab1s <- c(yLabsLow, yLabsHigh)


    if (max(BF) > eval(parse(text= yLab1s[length(yLab1s)]))) {

      for (i in 1:2) {

        if(grepl(pattern = "e",yLab1s[length(yLab1s)])){

          newy <-  paste(strsplit(yLab1s[length(yLab1s)], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[length(yLab1s)], split = "+", fixed=TRUE)[[1]][2])+1, sep="")
        } else {

          newy <- paste(yLab1s[length(yLab1s)], "0", sep= "")
        }

        if (eval(parse(text=newy)) >= 10^6) {

          newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
        }

        yLab1s <- c(yLab1s, newy)
      }
    }


    if (yLab1s[1] == "1") {

      yLab1s <- c(paste0(yLab1s[1], "/", "10"), yLab1s)
    }

    if (yLab1s[length(yLab1s)] == "1") {

      yLab1s <- c(yLab1s, "10")
    }

    if (min(BF) < eval(parse(text= yLab1s[1]))) {

      for (i in 1:2) {

        if (grepl(pattern = "e",yLab1s[1])) {

          newy <- paste(strsplit(yLab1s[1], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[1],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
        } else {

          newy <- paste(yLab1s[1], "0", sep= "")
        }

        if (eval(parse(text= newy)) <= 10^(-6)) {

          newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
          newy <-  sub("-", "+", x = newy)
          newy <- substring(newy, nchar(newy)-4, nchar(newy))
          newy <- paste0("1/", newy)
        }
      }

      yLab1s <- c(newy, yLab1s)
    }

    yLab <- yLab1s
  }

  if ( ! .shouldContinue(callback()))
    return()

  while (length(yLab) > 9) {

    ind <- which(yLab == "1")

    if (ind == 1) {

      yLabLow <- character(0)
    } else {

      yLabLow <- yLab[1:(ind-1)]
    }

    if (ind == length(yLab)) {

      yLabHigh <- character(0)
    } else {

      yLabHigh <- yLab[(ind+1):length(yLab)]
    }

    if (length(yLabLow) > 1) {

      yLabLow <- yLabLow[seq(length(yLabLow)-1, 1, -2)]
    } else {

      yLabLow <- yLabLow
    }


    if (length(yLabHigh) > 1) {

      yLabHigh <- yLabHigh[seq(2, length(yLabHigh), 2)]
    } else {

      yLabHigh <- yLabHigh
    }

    if (length(yLabLow) == 1) {

      yLabLow <- paste("1/", yLabHigh[1], sep="")
    }

    if (length(yLabHigh) == 1) {

      yLabHigh <- strsplit(x = yLabLow[1], "/", fixed=TRUE)[[1]][2]
    }

    yLab <- c(rev(yLabLow), "1", yLabHigh)
  }


  while (eval(parse(text=yLab[1])) > min(BF)) {

    for (i in 1:2) {

      interval <- as.numeric(strsplit(yLab[1], "+", fixed= TRUE)[[1]][2]) - as.numeric(strsplit(yLab[2], "+", fixed= TRUE)[[1]][2])
      pot <- as.numeric(strsplit(yLab[1], "+", fixed= TRUE)[[1]][2]) + interval

      newy <- paste(strsplit(yLab[1], "+", fixed= TRUE)[[1]][1], "+", pot, sep="")
      yLab <- c(newy, yLab)
    }
  }

  while (eval(parse(text=yLab[length(yLab)])) < max(BF)) {

    for (i in 1:2) {

      interval <- as.numeric(strsplit(yLab[length(yLab)], "+", fixed= TRUE)[[1]][2]) - as.numeric(strsplit(yLab[length(yLab)-1], "+", fixed= TRUE)[[1]][2])
      pot <- as.numeric(strsplit(yLab[length(yLab)], "+", fixed= TRUE)[[1]][2]) + interval
      newy <- paste(strsplit(yLab[length(yLab)], "+", fixed= TRUE)[[1]][1], "+", pot, sep="")
      yLab <- c( yLab, newy)
    }
  }

  if ( ! .shouldContinue(callback()))
    return()

  yAt <- vector("numeric", length(yLab))

  for (i in seq_along(yLab)) {

    yAt[i] <- log(eval(parse(text= yLab[i])))
  }


  ####################### plot ###########################

  xLab <- pretty(c(0, length(BF10)+2))
  xlim <- range(xLab)
  ylow <- log(eval(parse(text= yLab[1])))
  yhigh <- log(eval(parse(text= yLab[length(yLab)])))

  if (is.infinite(yhigh)) {

    yhigh <- 1e+308
  }


  ylim <- c(ylow, yhigh)

  plot(1,1, xlim= xlim, ylim= ylim, ylab= "", xlab="", type= "n", axes= FALSE)


  for (i in seq_along(yAt)) {

    lines(x= xlim, y= rep(yAt[i], 2), col='darkgrey', lwd= 1.3, lty=2)
  }

  lines(xlim, rep(0, 2), lwd= lwd)

  axis(1, at= xLab, labels = xLab, cex.axis= cexAxis, lwd= lwdAxis)
  axis(2, at= yAt, labels= yLab, cex.axis= cexAxis, lwd= lwdAxis)

  # enable plotting in margin
  par(xpd= TRUE)
  xx <- grconvertX(0.79, "ndc", "user")

  yAthigh <- yAt[yAt >= 0]

  if (!omit3s & eval(parse(text= yLab[1])) >= 1/300 & eval(parse(text= yLab[length(yLab)])) <= 300) {

    for (i in 1:(length(yAthigh)-1)) {
      yy <- mean(c(yAthigh[i], yAthigh[i+1]))

      if (yAthigh[i] == log(1)) {
        text(x = xx, yy,"Anecdotal", pos= 4, cex= cexText)
      }
      if (yAthigh[i] == log(3)) {
        text(x = xx, yy,"Moderate", pos= 4, cex= cexText)
      }
      if (yAthigh[i] == log(10)) {
        text(x = xx, yy,"Strong", pos= 4, cex= cexText)
      }
      if (yAthigh[i] == log(30)) {
        text(x = xx, yy,"Very strong", pos= 4, cex= cexText)
      }
      if (yAthigh[i] == log(100)) {
        text(x = xx, yy,"Extreme", pos= 4, cex= cexText)
      }
    }

    yAtlow <- rev(yAt[yAt <= 0])

    for (i in 1:(length(yAtlow)-1)) {

      yy <- mean(c(yAtlow[i], yAtlow[i+1]))

      if (yAtlow[i] == log(1)) {
        text(x = xx, yy,"Anecdotal", pos= 4, cex= cexText)
      }
      if (yAtlow[i] == log(1/3)) {
        text(x = xx, yy,"Moderate", pos= 4, cex= cexText)
      }
      if (yAtlow[i] == log(1/10)) {
        text(x = xx, yy,"Strong", pos= 4, cex= cexText)
      }
      if (yAtlow[i] == log(1/30)) {
        text(x = xx, yy,"Very strong", pos= 4, cex= cexText)
      }
      if (yAtlow[i] == log(1/100)) {
        text(x = xx, yy,"Extreme", pos= 4, cex= cexText)
      }
    }

    if ( ! .shouldContinue(callback()))
      return()

    axis(side=4, at= yAt,tick=TRUE,las=2, cex.axis= cexAxis, lwd= lwdAxis, labels=FALSE, line= -0.6)

    xx <- grconvertX(0.96, "ndc", "user")
    yy <- grconvertY(0.5, "npc", "user")

    text(xx, yy, "Evidence", srt= -90, cex= cexEvidence)
  }

  if (omit3s) {

    if (oneSided == FALSE) {

      if (BFH1H0) {

        mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYlab, line= 4.3)
      } else {

        mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYlab, line= 4.3)
      }
    }

    if (oneSided == "right") {

      if (BFH1H0) {

        mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYlab, line= 4.3)
      } else {

        mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYlab, line= 4.3)
      }
    }

    if (oneSided == "left") {

      if (BFH1H0) {

        mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYlab, line= 4.3)
      } else {

        mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYlab, line= 4.3)
      }
    }
  }

  if (omit3s == FALSE) {

    if (oneSided == FALSE) {

      if (BFH1H0) {

        mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
      } else {

        mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYlab, line= 3.1)
      }
    }

    if (oneSided == "right") {

      if (BFH1H0) {

        mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
      } else {

        mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYlab, line= 3.1)
      }
    }

    if (oneSided == "left") {

      if (BFH1H0) {

        mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
      } else {

        mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYlab, line= 3.1)
      }
    }
  }

  mtext("n", side = 1, cex = cexXlab, line= 2.5)

  xx <- grconvertX(0.1, "npc", "user")
  yy1 <- yAt[length(yAt)-1]
  yy2 <- yAt[length(yAt)]
  yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
  yya2 <- yy1 + 3/4* diff(c(yy1, yy2))

  arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd= lwd)

  xxt <- grconvertX(0.28, "npc", "user")

  if (oneSided == FALSE) {

    if (BFH1H0) {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H1"), cex= cexText)
    } else {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H0"), cex= cexText)
    }
  }

  if (oneSided == "right") {

    if (BFH1H0) {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H+"), cex= cexText)
    } else {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H0"), cex= cexText)
    }
  }

  if (oneSided == "left") {

    if (BFH1H0) {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H-"), cex= cexText)
    } else {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H0"), cex= cexText)
    }
  }


  yy1 <- yAt[2]
  yy2 <- yAt[1]
  yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
  yya2 <- yy1 + 3/4 * diff(c(yy1, yy2))

  arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd= lwd)

  if (oneSided == FALSE) {

    if (BFH1H0) {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H0"), cex= cexText)
    } else {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H1"), cex= cexText)
    }
  }

  if (oneSided == "right"){

    if (BFH1H0) {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H0"), cex= cexText)
    } else {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H+"), cex= cexText)
    }
  }

  if (oneSided == "left") {

    if (BFH1H0) {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H0"), cex= cexText)
    } else {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H-"), cex= cexText)
    }
  }


  # display BF10 value
  if (idData < length(BF10)) {

      BF10post <- as.numeric(BF10post) # TO FIX: This needs to be done to make sure that the BF shows up in the table.
      BF10e <- BF10post

  } else {

    BF10e <- 1
  }

  if (BFH1H0) {

    BF01e <- 1 / BF10e

  } else {

    BF01e <- BF10e
    BF10e <- 1 / BF01e
  }

  # display BF10 value

  offsetTopPart <- 0.06

  xx <- min(xLab)
  yy <- grconvertY(0.75 + offsetTopPart, "ndc", "user")
  yy2 <- grconvertY(0.806 + offsetTopPart, "ndc", "user")

  if (BF10e >= 1000000 | BF01e >= 1000000) {

    BF10t <- formatC(BF10e,3, format = "e")
    BF01t <- formatC(BF01e,3, format = "e")
  }

  if (BF10e < 1000000 & BF01e < 1000000) {

    BF10t <- formatC(BF10e, 3, format = "f")
    BF01t <- formatC(BF01e, 3, format = "f")
  }

  if (oneSided == FALSE) {

    text(xx, yy2, bquote(BF[10]==.(BF10t)), cex= cexTextBF, pos= 4, offset= -.2)
    text(xx, yy, bquote(BF[0][1]==.(BF01t)), cex= cexTextBF, pos= 4, offset= -.2)
  }

  if (oneSided == "right") {

    text(xx, yy2, bquote(BF["+"][0]==.(BF10t)), cex= cexTextBF, pos= 4, offset= -.2)
    text(xx, yy, bquote(BF[0]["+"]==.(BF01t)), cex= cexTextBF, pos= 4, offset= -.2)
  }

  if (oneSided == "left") {

    text(xx, yy2, bquote(BF["-"][0]==.(BF10t)), cex= cexTextBF, pos= 4, offset= -.2)
    text(xx, yy, bquote(BF[0]["-"]==.(BF01t)), cex= cexTextBF, pos= 4, offset= -.2)
  }


  # probability wheel

  if (max(nchar(BF10t), nchar(BF01t)) <= 4) {
    xx <- grconvertX(0.44, "ndc", "user")
  }

  if (max(nchar(BF10t), nchar(BF01t)) == 5) {
    xx <- grconvertX(0.44 +  0.001* 5, "ndc", "user")
  }

  if (max(nchar(BF10t), nchar(BF01t)) == 6) {
    xx <- grconvertX(0.44 + 0.001* 6, "ndc", "user")
  }

  if (max(nchar(BF10t), nchar(BF01t)) == 7) {
    xx <- grconvertX(0.44 + 0.002* max(nchar(BF10t), nchar(BF01t)), "ndc", "user")
  }

  if (max(nchar(BF10t), nchar(BF01t)) == 8) {
    xx <- grconvertX(0.44 + 0.003* max(nchar(BF10t), nchar(BF01t)), "ndc", "user")
  }

  if (max(nchar(BF10t), nchar(BF01t)) > 8) {
    xx <- grconvertX(0.445 + 0.005* max(nchar(BF10t), nchar(BF01t)), "ndc", "user")
  }

  yy <- grconvertY(0.788 + offsetTopPart, "ndc", "user")


  # make sure that colored area is centered

  radius <- grconvertX(0.2, "ndc", "user") - grconvertX(0.16, "ndc", "user")
  A <- radius^2*pi
  alpha <- 2 / (BF01e + 1) * A / radius^2
  startpos <- pi/2 - alpha/2

  if ( ! .shouldContinue(callback()))
    return()

  # draw probability wheel

  plotrix::floating.pie(xx, yy,c(BF10e, 1),radius= radius, col=c("darkred", "white"), lwd=2,startpos = startpos)

  yy <- grconvertY(0.865 + offsetTopPart, "ndc", "user")
  yy2 <- grconvertY(0.708 + offsetTopPart, "ndc", "user")

  if (oneSided == FALSE) {

    text(xx, yy, "data|H1", cex= 1.1)
    text(xx, yy2, "data|H0", cex=  1.1)
  }

  if (oneSided == "right") {

    text(xx, yy, "data|H+", cex=  1.1)
    text(xx, yy2, "data|H0", cex=  1.1)
  }

  if (oneSided == "left") {

    text(xx, yy, "data|H-", cex=  1.1)
    text(xx, yy2, "data|H0", cex=  1.1)
  }

  if (length(BF10) <= 60) {

    points(log(BF10), pch=21, bg="grey", cex= cexPoints, lwd = 1.3) # user prior
  } else {

    lines(log(BF10), col="black", lwd = 2.7) # user prior
  }

  if (plotDifferentPriors) {

    if (length(BF10) <= 60) {

      points(log(BF10u), pch=21, bg= "white", cex= 0.7, lwd= 1.3) # "ultrawide" prior
      points(log(BF10w), pch=21, bg= "black", cex= 0.7, lwd= 1.3) # "wide" prior

    } else {

      greycol <- rgb(0,0,0, alpha=0.95)
      greycol2 <- rgb(0,0,0, alpha=0.5)
      lines(log(BF10u), col= greycol2, cex= 0.7, lwd= 1.3, lty= 1) # "ultrawide" prior
      lines(log(BF10w), col= greycol, cex= 0.7, lwd= 1.3, lty=3) # "wide" prior
    }
  }

  BFevidence <- BF10e

  if (evidenceText) {

    if (BF10e < 1) {
      BFevidence <- 1 / BF10e
    }
    if (BFevidence >= 1 & BFevidence <= 3) {
      lab <- "Anecdotal"
    }
    if (BFevidence > 3 & BFevidence <= 10) {
      lab <- "Moderate"
    }
    if (BFevidence > 10 & BFevidence <= 30) {
      lab <- "Strong"
    }
    if (BFevidence > 30 & BFevidence <= 100) {
      lab <- "Very strong"
    }
    if (BFevidence > 100) {
      lab <- "Extreme"
    }
    xxT <- max(xLab)
    yyT <- grconvertY(0.775 + offsetTopPart, "ndc", "user")

    if (BF10e >= 1) {

      if (oneSided == FALSE) {
        text(xxT, yyT, paste(gettext("Evidence for H1:\n"), lab), cex= 1.4, pos= 2, offset= -.2)
      }
      if (oneSided == "right") {
        text(xxT, yyT, paste(gettext("Evidence for H+:\n"), lab), cex= 1.4, pos= 2, offset= -.2)
      }
      if (oneSided == "left") {
        text(xxT, yyT, paste(gettext("Evidence for H-:\n"), lab), cex= 1.4, pos= 2, offset= -.2)
      }
    }

    if (BF10e < 1) {
      text(xxT, yyT, paste("Evidence for H0:\n", lab), cex= 1.4, pos= 2, offset= -.2)
    }

  } else {

    # add legend
    xx <- grconvertX(0.56, "ndc", "user")
    yy <- grconvertY(0.872 + offsetTopPart, "ndc", "user")

    BFind <- sort(c(BF10[length(x)], BF10u[length(x)], BF10w[length(x)]), decreasing = TRUE, index.return=TRUE)$ix
    legend <- c("user prior", "ultrawide prior", "wide prior")

    if (length(BF10) <= 60) {

      pt.bg <-  c("grey", "white", "black")
      pt.cex <-  c(cexPoints, 0.7, 0.7)
      legend(xx, yy, legend = legend[BFind], pch=rep(21,3), pt.bg= pt.bg[BFind], bty= "n", cex= cexLegend, lty=rep(NULL,3), pt.lwd=rep(1.3,3), pt.cex= pt.cex[BFind])
    } else {

      xx <- grconvertX(0.55, "ndc", "user")
      lty <- c(1, 1, 3)
      lwd <- c(2.7, 1.3, 1.3)
      col <- c("black", greycol2, greycol)
      legend(xx, yy, legend = legend[BFind], lty= lty[BFind], bty= "n", cex= cexLegend, lwd= lwd[BFind], col= col[BFind], seg.len= .7)
    }
  }
}


.plotBF.robustnessCheck.ttest <- function(x= NULL, y= NULL, paired= FALSE, BF10post, callback=function(...) 0, formula= NULL, data= NULL, rscale= 1, oneSided= FALSE, lwd= 2, cexPoints= 1.4, cexAxis= 1.2,
                                          cexYXlab= 1.5,  cexText=1.2, cexLegend= 1.4, lwdAxis= 1.2, cexEvidence= 1.6, BFH1H0 = TRUE, dontPlotData= FALSE, additionalInformation=FALSE) {


  #### settings ####
  if (rscale == "medium") {
    r <- sqrt(2) / 2
  }
  if (rscale == "wide") {
    r <- 1
  }
  if (rscale == "ultrawide") {
    r <- sqrt(2)
  }
  if (mode(rscale) == "numeric") {
    r <- rscale
  }

  if (oneSided == FALSE) {
    nullInterval <- NULL
  }
  if (oneSided == "right") {
    nullInterval <- c(0, Inf)
  }
  if (oneSided == "left") {
    nullInterval <- c(-Inf, 0)
  }

  if (additionalInformation) {
    par(mar= c(5.6, 6.5, 7, 7) + 0.1, las=1)
  } else {
    par(mar= c(5.6, 6.5, 4, 7) + 0.1, las=1)
  }

  if (dontPlotData) {

    plot(1, type='n', xlim=0:1, ylim=0:1, bty='n', axes=FALSE, xlab="", ylab="")

    axis(1, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, xlab="")
    axis(2, at=0:1, labels=FALSE, cex.axis=cexAxis, lwd=lwdAxis, ylab="")

    if (oneSided == FALSE) {

      if (BFH1H0) {

        mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      } else {

        mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      }
    }

    if (oneSided == "right") {

      if (BFH1H0) {

        mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      } else {

        mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      }
    }

    if (oneSided == "left") {

      if (BFH1H0) {

        mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      } else {

        mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      }
    }

    mtext("Cauchy prior width", side = 1, cex = cexYXlab, line= 2.5)

    return()
  }

  #### get BFs ###
  if(r > 1.5)
  {
    rValues <- seq(0.0005, 2, length.out = 535)
  }
  else
  {
    rValues <- seq(0.0005, 1.5, length.out = 400)
  }

  # BF10
  BF10 <- vector("numeric", length(rValues))

  for (i in seq_along(rValues)) {

    if (oneSided == FALSE) {

      BF <- BayesFactor::ttestBF(x=x, y=y, paired=paired, nullInterval=nullInterval, rscale=rValues[i])
      BF10[i] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, "bf"]

    } else {

      BF10[i] <- .oneSidedTtestBFRichard(x=x, y=y, paired=paired, oneSided=oneSided, r=rValues[i])
    }

    if ( ! .shouldContinue(callback()))
      return()
  }

	# sometimes BayesFactor fails and returns NaN, e.g. BayesFactor::ttest.tstat(t = 10, n1 = 5, n2 = 0, nullInterval = NULL, rscale = 0.0005)
	# we'll remove up to 5% of the NaN's and otherwise just return an error for the plot
	validValues <- is.finite(BF10)
	if (sum(validValues) < (0.95 * length(BF10)))
		stop("could not calculate enough valid Bayes Factors for different values of the prior")

	BF10 <- BF10[validValues]
	rValues <- rValues[validValues]

  # maximum BF value
  maxBF10 <- max(BF10)
  maxBFrVal <- rValues[which.max(BF10)]
  BF10maxText <- .clean(maxBF10)

  # BF10 "medium" prior
  if (oneSided == FALSE) {

    BF10m <- BayesFactor::ttestBF(x=x, y=y, paired=paired, nullInterval=nullInterval, rscale= "medium")
    BF10m <- BayesFactor::extractBF(BF10m, logbf = FALSE, onlybf = F)[1, "bf"]

  } else {

    BF10m <- .oneSidedTtestBFRichard(x=x, y=y, paired=paired, oneSided=oneSided, r="medium")
  }

  BF10mText <- BF10m

  # BF10 "wide" prior
  if (oneSided == FALSE) {

    BF10w <- BayesFactor::ttestBF(x = x, y=y, paired= paired, nullInterval= nullInterval, rscale= "wide")
    BF10w <- BayesFactor::extractBF(BF10w, logbf = FALSE, onlybf = F)[1, "bf"]

  } else {

    BF10w <- .oneSidedTtestBFRichard(x=x, y=y, paired=paired, oneSided=oneSided, r="wide")
  }

  BF10wText <- BF10w

  # BF10 "ultrawide" prior
  if (oneSided == FALSE) {

    BF10ultra <- BayesFactor::ttestBF(x = x, y=y, paired= paired, nullInterval= nullInterval, rscale= "ultrawide")
    BF10ultra <- BayesFactor::extractBF(BF10ultra, logbf = FALSE, onlybf = F)[1, "bf"]

  } else {

    BF10ultra <- .oneSidedTtestBFRichard(x=x, y=y, paired=paired, oneSided=oneSided, r="ultrawide")
  }

  BF10ultraText <- BF10ultra

  # BF10 user prior
  BF10post <- as.numeric(BF10post)

  BF10user <- BF10post
  BF10userText <- BF10user

  if ( ! .shouldContinue(callback()))
    return()

  ####################### scale y axis ###########################

  BF <- c(BF10, BF10m, BF10w, BF10ultra, BF10user, maxBF10)

  if (!BFH1H0) {

    BF <- 1 / BF
    BF10 <- 1 / BF10
    BF10m  <- 1 / BF10m
    BF10w <- 1 / BF10w
    BF10ultra <- 1 / BF10ultra
    maxBF10 <- 1 / maxBF10
    # BF10user <- 1 / BF10user
  }

  # y-axis labels larger than 1
  y1h <- "1"
  i <- 1

  while (eval(parse(text= y1h[i])) < max(BF10)) {

    if (grepl(pattern = "e",y1h[i])) {

      newy <- paste(strsplit(y1h[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y1h[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
    } else {

      newy <- paste(y1h[i], "0", sep= "")
    }

    if (eval(parse(text=newy)) >= 10^6) {

      newy <- format(as.numeric(newy), digits= 3, scientific = TRUE)
    }

    y1h <- c(y1h, newy)
    i <- i + 1
  }

  y3h <- "3"
  i <- 1

  while (eval(parse(text= y3h[i])) < max(BF10)) {

    if (grepl(pattern = "e",y3h[i])) {

      newy <- paste(strsplit(y3h[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y3h[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
    } else {

      newy <- paste(y3h[i], "0", sep= "")
    }

    if (as.numeric(newy) >= 10^6) {

      newy <- format(as.numeric(newy), digits= 3, scientific = TRUE)
    }

    y3h <- c(y3h, newy)
    i <- i + 1
  }

  yhigh <- vector("numeric", length(y1h) + length(y3h))
  o <- 1
  e <- 1

  for (i in seq_along(yhigh)) {

    if (i %% 2 == 1) {

      yhigh[i] <- y1h[o]
      o <- o + 1
    }

    if (i %% 2 == 0) {

      yhigh[i] <- y3h[e]
      e <- e + 1
    }
  }

  yhighLab <- as.character(yhigh)

  # y-axis labels smaller than 1
  y1l <- "1/1"
  i <- 1

  while (eval(parse(text= y1l[i])) > min(BF10)) {

    if (grepl(pattern = "e",y1l[i])) {

      newy <- paste(strsplit(y1l[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y1l[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
    } else {

      newy <- paste(y1l[i], "0", sep= "")
    }

    if (eval(parse(text= newy)) <= 10^(-6)) {

      newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
      newy <-  sub("-", "+", x = newy)
      newy <- paste0("1/", newy)
    }

    y1l <- c(y1l, newy)
    i <- i + 1
  }

  y3l <- "1/3"
  i <- 1

  while (eval(parse(text= y3l[i])) > min(BF10)) {

    if (grepl(pattern = "e",y3l[i])) {

      newy <- paste(strsplit(y3l[i], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(y3l[i],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
    } else {

      newy <- paste(y3l[i], "0", sep= "")
    }

    if (newy == "1/3e+9") {
      newy <- "1/3e+09"
    }

    if (eval(parse(text= newy)) <= 10^(-6) & eval(parse(text= newy)) > 10^(-9)) {

      newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
      newy <- paste(substring(newy, 1, nchar(newy)-1), as.numeric(substring(newy, nchar(newy), nchar(newy)))-1,sep="")
      newy <- sub(".33", "", newy)
      newy <-  sub("-", "+", x = newy)
      newy <- paste0("1/", newy)
    }

    y3l <- c(y3l, newy)
    i <- i + 1
  }

  ylow <- vector("numeric", length(y1l) + length(y3l))
  o <- 1
  e <- 1

  if ( ! .shouldContinue(callback()))
    return()

  for (i in seq_along(ylow)) {

    if (i %% 2 == 1) {
      ylow[i] <- y1l[o]
      o <- o + 1
    }
    if (i %% 2 == 0) {
      ylow[i] <- y3l[e]
      e <- e + 1
    }
  }

  yLab <- c(rev(ylow[-1]), yhighLab)

  # remove 3's if yLab vector is too long
  omit3s <- FALSE

  if (length(yLab) > 9) {

    omit3s <- TRUE

    ind <- which(yLab == "3")

    yLabsHigh <- yLab[ind:length(yLab)]

    if (length(yLabsHigh) > 1) {

      yLabsHigh <- yLabsHigh[seq(2, length(yLabsHigh),2)]
    } else {

      yLabsHigh <- character(0)
    }

    yLabsLow <- yLab[1:(ind-1)]
    yLabsLow <- yLabsLow[-grep(pattern = "/3", x = yLab)]

    yLab1s <- c(yLabsLow, yLabsHigh)

    if (max(BF10) > eval(parse(text= yLab1s[length(yLab1s)]))) {

      for (i in 1:2) {

        if (grepl(pattern = "e",yLab1s[length(yLab1s)])) {

          newy <-  paste(strsplit(yLab1s[length(yLab1s)], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[length(yLab1s)],
                                                                                                                     split = "+", fixed=TRUE)[[1]][2])+1, sep="")
        } else {

          newy <- paste(yLab1s[length(yLab1s)], "0", sep= "")
        }

        if (eval(parse(text=newy)) >= 10^6) {

          newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
        }

        yLab1s <- c(yLab1s, newy)
      }
    }

    if (max(BF10) > eval(parse(text= yLab1s[length(yLab1s)-1]))) {

      if (grepl(pattern = "e",yLab1s[length(yLab1s)])) {

        newy <-  paste(strsplit(yLab1s[length(yLab1s)], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[length(yLab1s)],
                                                                                                                   split = "+", fixed=TRUE)[[1]][2])+1, sep="")
      } else {

        newy <- paste(yLab1s[length(yLab1s)], "0", sep= "")
      }

      if (eval(parse(text=newy)) >= 10^6) {

        newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
      }

      yLab1s <- c(yLab1s, newy)
    }

    if (yLab1s[1] == "1") {

      yLab1s <- c(paste0(yLab1s[1], "/", "10"), yLab1s)
    }
    if (yLab1s[length(yLab1s)] == "1") {

      yLab1s <- c(yLab1s, "10")
    }

    if (min(BF10) < eval(parse(text= yLab1s[1]))) {

      for (i in 1:2) {

        if (grepl(pattern = "e",yLab1s[1])) {

          newy <- paste(strsplit(yLab1s[1], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[1],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
        } else {

          newy <- paste(yLab1s[1], "0", sep= "")
        }

        if (eval(parse(text= newy)) <= 10^(-6)) {

          newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
          newy <-  sub("-", "+", x = newy)
          newy <- substring(newy, nchar(newy)-4, nchar(newy))
          newy <- paste0("1/", newy)
        }
      }

      yLab1s <- c(newy, yLab1s)
    }

    if (min(BF10) < eval(parse(text= yLab1s[2]))) {

      if (grepl(pattern = "e",yLab1s[1])) {

        newy <- paste(strsplit(yLab1s[1], split = "+", fixed=TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[1],split = "+", fixed=TRUE)[[1]][2])+1, sep="")
      } else {

        newy <- paste(yLab1s[1], "0", sep= "")
      }

      if (eval(parse(text= newy)) <= 10^(-6)) {

        newy <- format(eval(parse(text=newy)), digits= 3, scientific = TRUE)
        newy <-  sub("-", "+", x = newy)
        newy <- substring(newy, nchar(newy)-4, nchar(newy))
        newy <- paste0("1/", newy)
      }


      yLab1s <- c(newy, yLab1s)
    }

    yLab <- yLab1s
  }

  if ( ! .shouldContinue(callback()))
    return()

  while (length(yLab) > 9) {

    ind <- which(yLab == "1")

    if (ind == 1) {

      yLabLow <- character(0)
    } else {

      yLabLow <- yLab[1:(ind-1)]
    }

    if (ind == length(yLab)) {

      yLabHigh <- character(0)
    } else {

      yLabHigh <- yLab[(ind+1):length(yLab)]
    }

    if (length(yLabLow) > 1) {

      yLabLow <- yLabLow[seq(length(yLabLow)-1, 1, -2)]
    } else {

      yLabLow <- yLabLow
    }


    if (length(yLabHigh) > 1) {

      yLabHigh <- yLabHigh[seq(2, length(yLabHigh), 2)]
    } else {

      yLabHigh <- yLabHigh
    }

    if (length(yLabLow) == 1) {

      yLabLow <- paste("1/", yLabHigh[1], sep="")
    }
    if (length(yLabHigh) == 1) {

      yLabHigh <- strsplit(x = yLabLow[1], "/", fixed=TRUE)[[1]][2]
    }

    yLab <- c(rev(yLabLow), "1", yLabHigh)
  }

  if ( ! .shouldContinue(callback()))
    return()


  while (eval(parse(text=yLab[2])) > min(BF10)) {

    interval <- as.numeric(strsplit(format(eval(parse(text=yLab[1])), digits=3, scientific=TRUE), "-", fixed= TRUE)[[1]][2]) - as.numeric(strsplit(format(eval(parse(text=yLab[2])), digits=3, scientific=TRUE), "-", fixed= TRUE)[[1]][2])
    pot <- as.numeric(strsplit(format(eval(parse(text=yLab[1])), digits=3, scientific=TRUE), "-", fixed= TRUE)[[1]][2]) + interval

    if (nchar(pot) == 1)
      pot <- paste("0", pot, sep="")

    newy <- paste("1/1e", "+", pot, sep="")
    yLab <- c(newy, yLab)

    # interval <- as.numeric(strsplit(yLab[1], "+", fixed= TRUE)[[1]][2]) - as.numeric(strsplit(yLab[2], "+", fixed= TRUE)[[1]][2])
    # pot <- as.numeric(strsplit(yLab[1], "+", fixed= TRUE)[[1]][2]) + interval
    #
    # newy <- paste(strsplit(yLab[1], "+", fixed= TRUE)[[1]][1], "+", pot, sep="")
    # yLab <- c(newy, yLab)
  }


  while (eval(parse(text=yLab[length(yLab)-1])) < max(BF10)) {

    interval <- as.numeric(strsplit(format(eval(parse(text=yLab[length(yLab)])), digits=3, scientific=TRUE), "+", fixed= TRUE)[[1]][2]) - as.numeric(strsplit(format(eval(parse(text=yLab[length(yLab)-1])), digits=3, scientific=TRUE), "+", fixed= TRUE)[[1]][2])
    # pot <- as.numeric(strsplit(yLab[length(yLab)], "+", fixed= TRUE)[[1]][2]) + interval
    pot <- as.numeric(strsplit(format(eval(parse(text=yLab[length(yLab)])), digits=3, scientific=TRUE), "+", fixed= TRUE)[[1]][2]) + interval

    if (nchar(pot) == 1)
      pot <- paste("0", pot, sep="")

    newy <- paste(strsplit(format(eval(parse(text=yLab[length(yLab)])), digits=3, scientific=TRUE), "+", fixed= TRUE)[[1]][1], "+", pot, sep="")
    yLab <- c( yLab, newy)
  }


  yAt <- vector("numeric", length(yLab))

  for (i in seq_along(yLab)) {

    yAt[i] <- log(eval(parse(text= yLab[i])))
  }


  ####################### plot ###########################

  if(r > 1.5)
  {
    xLab <- c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2)
  }
  else
  {
    xLab <- c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)
  }

  xlim <- range(xLab)
  ylow <- log(eval(parse(text= yLab[1])))
  yhigh <- log(eval(parse(text= yLab[length(yLab)])))
  ylim <- c(ylow, yhigh)

  plot(1,1, xlim= xlim, ylim= ylim, ylab= "", xlab="", type= "n", axes= FALSE)

  for (i in seq_along(yAt)) {

    lines(x= xlim, y= rep(yAt[i], 2), col='darkgrey', lwd= 1.3, lty=2)
  }

  lines(xlim, rep(0, 2), lwd= lwd)

  axis(1, at= xLab, labels = xLab, cex.axis= cexAxis, lwd= lwdAxis)
  axis(2, at= yAt, labels= yLab, cex.axis= cexAxis, lwd= lwdAxis)

  # enable plotting in margin
  par(xpd= TRUE)
  xx <- grconvertX(0.79, "ndc", "user")

  yAthigh <- yAt[yAt >= 0]

  if (!omit3s & eval(parse(text= yLab[1])) >= 1/300 & eval(parse(text= yLab[length(yLab)])) <= 300) {

    for (i in 1:(length(yAthigh)-1)) {
      yy <- mean(c(yAthigh[i], yAthigh[i+1]))

      if (yAthigh[i] == log(1)) {
        text(x = xx, yy,"Anecdotal", pos= 4, cex= cexText)
      }
      if (yAthigh[i] == log(3)) {
        text(x = xx, yy,"Moderate", pos= 4, cex= cexText)
      }
      if (yAthigh[i] == log(10)) {
        text(x = xx, yy,"Strong", pos= 4, cex= cexText)
      }
      if (yAthigh[i] == log(30)) {
        text(x = xx, yy,"Very strong", pos= 4, cex= cexText)
      }
      if (yAthigh[i] == log(100)) {
        text(x = xx, yy,"Extreme", pos= 4, cex= cexText)
      }
    }

    yAtlow <- rev(yAt[yAt <= 0])

    for (i in 1:(length(yAtlow)-1)) {

      yy <- mean(c(yAtlow[i], yAtlow[i+1]))

      if (yAtlow[i] == log(1)) {
        text(x = xx, yy,"Anecdotal", pos= 4, cex= cexText)
      }
      if (yAtlow[i] == log(1/3)) {
        text(x = xx, yy,"Moderate", pos= 4, cex= cexText)
      }
      if (yAtlow[i] == log(1/10)) {
        text(x = xx, yy,"Strong", pos= 4, cex= cexText)
      }
      if (yAtlow[i] == log(1/30)) {
        text(x = xx, yy,"Very strong", pos= 4, cex= cexText)
      }
      if (yAtlow[i] == log(1/100)) {
        text(x = xx, yy,"Extreme", pos= 4, cex= cexText)
      }
    }

    axis(side=4, at= yAt,tick=TRUE,las=2, cex.axis= cexAxis, lwd= lwdAxis, labels=FALSE, line= -0.6)

    xx <- grconvertX(0.96, "ndc", "user")
    yy <- grconvertY(0.5, "npc", "user")
    text(xx, yy, "Evidence", srt= -90, cex= cexEvidence)
  }

  if (omit3s) {

    if (eval(parse(text= yLab[1])) <= 1/10^6) {

      line <- 4.75

    } else {

      line <- 4.3
    }

    if (oneSided == FALSE) {

      if (BFH1H0) {

        mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYXlab, line= line)
      } else {

        mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYXlab, line= line)
      }
    }

    if (oneSided == "right") {

      if (BFH1H0) {

        mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYXlab, line= line)
      } else {

        mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYXlab, line= line)
      }
    }

    if (oneSided == "left") {

      if (BFH1H0) {

        mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYXlab, line= line)
      } else {

        mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYXlab, line= line)
      }
    }
  }

  if (omit3s == FALSE) {

    if (oneSided == FALSE) {

      if (BFH1H0) {

        mtext(text = expression(BF[1][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      } else {

        mtext(text = expression(BF[0][1]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      }
    }

    if (oneSided == "right") {

      if (BFH1H0) {

        mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      } else {

        mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      }
    }

    if (oneSided == "left") {

      if (BFH1H0) {

        mtext(text = expression(BF["-"][0]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      } else {

        mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYXlab, line= 3.1)
      }
    }
  }

  mtext("Cauchy prior width", side = 1, cex = cexYXlab, line= 2.5)

  xx <- grconvertX(0.1, "npc", "user")
  yy1 <- yAt[length(yAt)-1]
  yy2 <- yAt[length(yAt)]
  yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
  yya2 <- yy1 + 3/4* diff(c(yy1, yy2))

  arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd= lwd)

  xxt <- grconvertX(0.28, "npc", "user")

  if (oneSided == FALSE) {

    if (BFH1H0) {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H1"), cex= cexText)
    } else {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H0"), cex= cexText)
    }
  }

  if (oneSided == "right") {

    if (BFH1H0) {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H+"), cex= cexText)
    } else {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H0"), cex= cexText)
    }
  }

  if (oneSided == "left") {

    if (BFH1H0) {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H-"), cex= cexText)
    } else {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H0"), cex= cexText)
    }
  }

  yy1 <- yAt[2]
  yy2 <- yAt[1]
  yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
  yya2 <- yy1 + 3/4 * diff(c(yy1, yy2))

  arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd= lwd)

  if (oneSided == FALSE) {

    if (BFH1H0) {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H0"), cex= cexText)
    } else {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H1"), cex= cexText)
    }
  }

  if (oneSided == "right") {

    if (BFH1H0) {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H0"), cex= cexText)
    } else {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H+"), cex= cexText)
    }
  }

  if (oneSided == "left") {

    if (BFH1H0) {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H0"), cex= cexText)
    } else {

      text(xxt, mean(c(yya1, yya2)), labels = gettext("Evidence for H-"), cex= cexText)
    }
  }

  if ( ! .shouldContinue(callback()))
    return()

  # display BF10
  lines(rValues,log(BF10), col="black", lwd = 2.7)

  if (additionalInformation) {
      # display "wide", user, "ultrawide" prior and max BFs
      points(r, log(BF10user), pch=21, bg="grey", cex= cexPoints, lwd = 1.3) # user prior
      points(1, log(BF10w), pch=21, bg= "black", cex= 1.1, lwd= 1.3) # "wide" prior
      points(sqrt(2), log(BF10ultra), pch=21, bg= "white", cex= 1.1, lwd= 1.3) # "ultrawide" prior
      points(maxBFrVal, log(maxBF10), pch=21, bg="red", cex=1.1, lwd=1.3)  # max BF value

      #### add legend
      # BF values

      # BFuser

      if (BFH1H0) {

        BF01userText <- 1 / BF10userText

      } else {

        BF10userText <- 1 / BF10userText
        BF01userText <- 1 / BF10userText
      }

      if (BF10userText >= 1000000 | BF01userText >= 1000000) {

        BF10usert <- format(BF10userText, digits= 4, scientific = TRUE)
        BF01usert <- format(BF01userText, digits= 4, scientific = TRUE)
      }
      if (BF10userText < 1000000 & BF01userText < 1000000) {

        BF10usert <- formatC(BF10userText, 3, format = "f")
        BF01usert <- formatC(BF01userText, 3, format = "f")
      }

      if (oneSided == FALSE) {

        if( BF10userText >= BF01userText) {
          userBF <- bquote(BF[10]==.(BF10usert))
        } else {
          userBF <- bquote(BF[0][1]==.(BF01usert))
        }
      }
      if (oneSided == "right") {

        if (BF10userText >= BF01userText) {
          userBF <- bquote(BF["+"][0]==.(BF10usert))
        } else {
          userBF <- bquote(BF[0]["+"]==.(BF01usert))
        }
      }
      if (oneSided == "left") {

        if (BF10userText >= BF01userText) {
          userBF <- bquote(BF["-"][0]==.(BF10usert))
        } else {
          userBF <- bquote(BF[0]["-"]==.(BF01usert))
        }
      }

      # BFwide
      BF01wText <- 1 / BF10wText

      if (BF10wText >= 1000000 | BF01wText >= 1000000) {
        BF10wt <- format(BF10wText, digits= 4, scientific = TRUE)
        BF01wt <- format(BF01wText, digits= 4, scientific = TRUE)
      }
      if (BF10wText < 1000000 & BF01wText < 1000000) {
        BF10wt <- formatC(BF10wText, 3, format = "f")
        BF01wt <- formatC(BF01wText, 3, format = "f")
      }

      if (oneSided == FALSE) {

        if (BF10wText >= BF01wText) {
          wBF <- bquote(BF[10]==.(BF10wt))
        } else {
          wBF <- bquote(BF[0][1]==.(BF01wt))
        }
      }
      if (oneSided == "right") {

        if (BF10wText >= BF01wText) {
          wBF <- bquote(BF["+"][0]==.(BF10wt))
        } else {
          wBF <- bquote(BF[0]["+"]==.(BF01wt))
        }
      }
      if (oneSided == "left") {

        if (BF10wText >= BF01wText) {
          wBF <- bquote(BF["-"][0]==.(BF10wt))
        } else {
          wBF <- bquote(BF[0]["-"]==.(BF01wt))
        }
      }

      # BFultrawide
      BF01ultraText <- 1 / BF10ultraText

      if (BF10ultraText >= 1000000 | BF01ultraText >= 1000000) {

        BF10ultrat <- format(BF10ultraText, digits= 4, scientific = TRUE)
        BF01ultrat <- format(BF01ultraText, digits= 4, scientific = TRUE)
      }
      if (BF10ultraText < 1000000 & BF01ultraText < 1000000) {

        BF10ultrat <- formatC(BF10ultraText, 3, format = "f")
        BF01ultrat <- formatC(BF01ultraText, 3, format = "f")
      }

      if (oneSided == FALSE) {

        if (BF10ultraText >= BF01ultraText) {
          ultraBF <- bquote(BF[10]==.(BF10ultrat))
        } else {
          ultraBF <- bquote(BF[0][1]==.(BF01ultrat))
        }
      }

      if (oneSided == "right") {

        if (BF10ultraText >= BF01ultraText) {
          ultraBF <- bquote(BF["+"][0]==.(BF10ultrat))
        } else{
          ultraBF <- bquote(BF[0]["+"]==.(BF01ultrat))
        }
      }

      if (oneSided == "left") {

        if (BF10ultraText >= BF01ultraText) {
          ultraBF <- bquote(BF["-"][0]==.(BF10ultrat))
        } else {
          ultraBF <- bquote(BF[0]["-"]==.(BF01ultrat))
        }
      }

      # maxBF
      if (BF10maxText >= 1000000) {
          BF10maxt <- format(BF10maxText, digits = 3, scientific = TRUE)
      }

      if (BF10maxText < 1000000) {
          BF10maxt <- formatC(BF10maxText, digits = 3, format = "f", drop0trailing = TRUE)
      }

      maxBFrValt <- formatC(maxBFrVal, digits = 4, format = "f", drop0trailing = TRUE )
      maxBF <- bquote(.(BF10maxt) ~ .('at r') == .(maxBFrValt))

      if (oneSided == FALSE) {
          maxBF10LegendText <- bquote(max~BF[1][0]*":")
      } else if (oneSided == "right") {
          maxBF10LegendText <- bquote(max~BF["+"][0]*":")
      } else if (oneSided == "left") {
          maxBF10LegendText <- bquote(max~BF["-"][0]*":")
      }

      xx <- grconvertX(0.2, "ndc", "user")
      yy <- grconvertY(0.999, "ndc", "user")

      BFind <- sort(c(BF10userText, BF10ultraText, BF10wText, BF10maxText), decreasing = TRUE, index.return=TRUE)$ix
      BFsort <- sort(c(BF10userText, BF10ultraText, BF10wText, BF10maxText), decreasing = TRUE, index.return=TRUE)$x

      legend <- c("user prior:", "ultrawide prior:", "wide prior:", as.expression(maxBF10LegendText))
      pt.bg <-  c("grey", "white", "black", "red")
      pt.cex <-  c(cexPoints, 1.1, 1.1, 1.1)

      legend(xx, yy, legend = legend[BFind], pch=rep(21,4), pt.bg= pt.bg[BFind], bty= "n", cex= cexLegend, lty=rep(NULL,4), pt.lwd=rep(1.3,4), pt.cex= pt.cex[BFind])

      xx <- grconvertX(0.47, "ndc", "user")
      y1 <- grconvertY(0.946, "ndc", "user")
      y2 <- grconvertY(0.890, "ndc", "user")
      y3 <- grconvertY(0.843, "ndc", "user")
      y4 <- grconvertY(0.790, "ndc", "user")
      yy <- c(y1, y2, y3, y4)

      text(xx, yy[BFsort== BF10userText], userBF, cex= 1.3,pos = 4)
      text(xx, yy[BFsort== BF10ultraText], ultraBF, cex= 1.3, pos= 4)
      text(xx, yy[BFsort== BF10wText], wBF, cex= 1.3, pos= 4)
      text(xx, yy[BFsort == BF10maxText], maxBF, cex = 1.3, pos = 4)
  }
}

# only used by bain!
.plot2GroupMeansBayesIndTtest <- function(v1=NULL, v2=NULL, nameV1=NULL, nameV2=NULL, groupingName=NULL, dependentName=NULL, descriptivesPlotsCredibleInterval=.95) {

	v1 <- na.omit(v1)
	v2 <- na.omit(v2)

	posteriorSummary1 <- .posteriorSummaryGroupMean(variable=v1, descriptivesPlotsCredibleInterval=descriptivesPlotsCredibleInterval)
	posteriorSummary2 <- .posteriorSummaryGroupMean(variable=v2, descriptivesPlotsCredibleInterval=descriptivesPlotsCredibleInterval)
	summaryStat <- data.frame(	groupingVariable=c(nameV1, nameV2), dependent=c(posteriorSummary1$median, posteriorSummary2$median),
								ciLower=c(posteriorSummary1$ciLower, posteriorSummary2$ciLower), ciUpper=c(posteriorSummary1$ciUpper,
								posteriorSummary2$ciUpper),
								group = 1)

	pd <- ggplot2::position_dodge(.2)

	p <-	ggplot2::ggplot(summaryStat, ggplot2::aes(x=groupingVariable, y=dependent, group=group)) +
			ggplot2::geom_errorbar(ggplot2::aes(ymin=ciLower, ymax=ciUpper), colour="black", width=.2, position=pd) +
			ggplot2::geom_line(position=pd, size = .7) +
			ggplot2::geom_point(position=pd, size=4) +
			ggplot2::ylab(dependentName) +
			ggplot2::xlab(groupingName) +
			.base_breaks_y2(summaryStat, NULL) +
			.base_breaks_x(summaryStat$groupingVariable)

	p <- JASPgraphs::themeJasp(p)

	return(p)
}

.plotGroupMeanBayesOneSampleTtest <- function(variable=1:10, variableName="test1", testValueOpt=0, descriptivesPlotsCredibleInterval=.95) {

  variable <- na.omit(variable)

  testValue <- data.frame("testValue" = testValueOpt) # default zero
  posteriorSummary <- .posteriorSummaryGroupMean(variable=variable, descriptivesPlotsCredibleInterval=descriptivesPlotsCredibleInterval)
  summaryStat <- data.frame(groupingVariable=variableName, dependent=posteriorSummary$median, ciLower=posteriorSummary$ciLower, ciUpper=posteriorSummary$ciUpper)

  pd <- ggplot2::position_dodge(.2)

  p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x=groupingVariable, y=dependent, group=1)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=ciLower, ymax=ciUpper), colour="black", width=.2, position=pd) +
    ggplot2::geom_line(position=pd, size = .7) +
    ggplot2::geom_point(position=pd, size=4) +
    ggplot2::geom_hline(data = testValue, ggplot2::aes(yintercept=testValue), linetype="dashed") +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL) +
    .base_breaks_y2(summaryStat, testValueOpt)

    p <- JASPgraphs::themeJasp(p) + ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())

  return(p)
}

.base_breaks_y2 <- function(x, testValue){

  values <- c(testValue, x$ciLower, x$ciUpper)
  ci.pos <- c(min(values), max(values))
  b <- pretty(ci.pos)
  d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
  list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1),
       ggplot2::scale_y_continuous(breaks=c(min(b), testValue, max(b))))
}
