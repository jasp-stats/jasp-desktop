#some customised transform functions for compute column and filter

fishZ        <- function(x)              { return(atanh(x))                   }
invFishZ     <- function(x)              { return(tanh(x))                    }