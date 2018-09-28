#some pretty samplers that know how many rows you would like to see in your computed column!
#having compact aligned code makes it easier to see mistakes

normalDist   <- function(mean, sd)          { return( rnorm(   n=.dataSetRowCount(), mean=mean, sd=sd))             }
expDist      <- function(rate)              { return( rexp(    n=.dataSetRowCount(), rate=rate))                    }
betaDist     <- function(alpha, beta)       { return( rbeta(   n=.dataSetRowCount(), shape1=alpha, shape2=beta))    }
gammaDist    <- function(shape, scale)      { return( rgamma(  n=.dataSetRowCount(), shape=shape, scale=scale))     }
unifDist     <- function(min, max)          { return( runif(   n=.dataSetRowCount(), min=min, max=max))             }
tDist        <- function(df, ncp)           { return( rt(      n=.dataSetRowCount(), df=df, ncp=ncp))               }
chiSqDist    <- function(df, ncp)           { return( rchisq(  n=.dataSetRowCount(), df=df, ncp=ncp))               }
binomDist    <- function(trials, prob)      { return( rbinom(  n=.dataSetRowCount(), size=trials, prob=prob))       }
poisDist     <- function(lambda)            { return( rpois(   n=.dataSetRowCount(), lambda=lambda))                }
geomDist     <- function(prob)              { return( rgeom(   n=.dataSetRowCount(), prob=prob))                    }
fDist        <- function(df1, df2, ncp)     { return( rf(      n=.dataSetRowCount(), df1=df1, df2=df2, ncp=ncp))    }
negBinomDist <- function(targetTrial, prob) { return( rnbinom( n=.dataSetRowCount(), size=targetTrial, prob=prob))  }
logNormDist  <- function(meanLog, sdLog)    { return( rlnorm(  n=.dataSetRowCount(), meanlog=meanLog, sdlog=sdLog)) }
weibullDist  <- function(shape, scale)      { return( rweibull(n=.dataSetRowCount(), shape=shape, scale=scale))     }

# TODO(Alexander/Joris): default arguments for function calls
integerDist <- function(categories, replace=TRUE, prob=NULL) {
  return(sample.int(n=categories, size=.dataSetRowCount(), replace=replace, prob=prob))
}
