#
# Copyright (C) 2013-2016 University of Amsterdam
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

# Vovk-Sellke-Bayarri-Berger Maximum p-Ratio calculation.
# Sellke, T., Bayarri, M.J., Berger, J.O. (2001) Calibration of p Values for
# Testing Precise Null Hypotheses. The American Statistician. 55(1) 62-71
.VovkSellkeMPR <- function(p){
  # requires TWO-SIDED p-value! otherwise use .SellkeOROneSided
  return(ifelse(p >= 1/exp(1), 1, 1/(-exp(1)*p*log(p))))
}

# Type I error probability / posterior probability from same paper
.VovkSellkeMPROneSided <- function(p){
  stop("This function should not be used. In for reference.")
  pTwoSided <- ifelse(p>=0.5,(1-p)*2,p*2)
  return(.SellkeMPR(pTwoSided)*2*(1-p))
}
