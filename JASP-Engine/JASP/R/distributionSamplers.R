#some pretty samplers that know how many rows you would like to see in your computed column!

normalDst <- function(mean, sd)     { return(rnorm( .dataSetRowCount(), mean,         sd))          }
expDst    <- function(rate)         { return(rexp(  .dataSetRowCount(), rate))                      }
betaDst   <- function(alpha, beta)  { return(rbeta( .dataSetRowCount(), shape1=alpha, shape2=beta)) }
gammaDst  <- function(shape, scale) { return(rgamma(.dataSetRowCount(), shape,        scale))       }
unifDst   <- function(min, max)     { return(runif( .dataSetRowCount(), min,          max))         }
tDst      <- function(df, ncp)      { return(rt(    .dataSetRowCount(), df,           ncp))         }
chiSqDst  <- function(df, ncp)      { return(rchisq(.dataSetRowCount(), df,           ncp))         }
