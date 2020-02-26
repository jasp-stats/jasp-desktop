#
# Copyright (C) 2013-2020 University of Amsterdam
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

# Function commonly used in the various procedures within the SEM module

lavBootstrap <- function(fit, samples = 1000) {
  # Run bootstrap, track progress with progress bar
  # Notes: faulty runs are simply ignored
  # recommended: add a warning if not all boot samples are successful
  # fit <- semBootstrap(fit, samples = 1000)
  # if (nrow(fit@boot$coef) < 1000) 
  #  tab$addFootnote(gettextf("Not all bootstrap samples were successful: CI based on %.0f samples.", nrow(fit@boot$coef)), 
  #                  "<em>Note.</em>")
  # using internal bootstrap in lavaan to perform callbacks
  
  
  coef_with_callback <- function(lav_object) {
    # Progress bar is ticked every time coef() is evaluated, which happens once on the main object:
    # https://github.com/yrosseel/lavaan/blob/77a568a574e4113245e2f6aff1d7c3120a26dd90/R/lav_bootstrap.R#L107
    # and then every time on a successful bootstrap:
    # https://github.com/yrosseel/lavaan/blob/77a568a574e4113245e2f6aff1d7c3120a26dd90/R/lav_bootstrap.R#L375
    # i.e., samples + 1 times
    progressbarTick()
    return(lavaan::coef(lav_object))
  }
  startProgressbar(samples + 1)
  bootres <- lavaan:::bootstrap.internal(object = fit, R = samples, FUN = coef_with_callback)
  
  # Add the bootstrap samples to the fit object
  fit@boot       <- list(coef = bootres)
  fit@Options$se <- "bootstrap"
  
  return(fit)
}

