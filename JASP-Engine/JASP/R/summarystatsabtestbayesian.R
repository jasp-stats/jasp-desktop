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


SummaryStatsABTestBayesian <- function(jaspResults, dataset = NULL, options) {
  
  ready <- (options$n1 != 0 && options$y1 != 0 && options$n2 != 0 && options$y2 != 0)
  
  ### READ DATA                ###
  if (ready)
    dataset <- list(y1 = options$y1, n1 = options$n1, y2 = options$y2, n2 = options$n2)
  
  ### This function is shared with frequencies a/b test
  .abTestMain(jaspResults, dataset, options, ready)
}