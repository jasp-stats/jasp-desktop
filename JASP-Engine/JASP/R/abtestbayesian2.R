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


ABTestBayesian <- function(jaspResults, dataset, options) {

  ready <- (length(options$variables) > 0)

  if (ready) {
    dataset <- .abReadData(dataset, options)

    .abCheckErrors(dataset, options)
  }




}

.abReadData <- function(dataset, options) {
  # Get the relevant data
  #
  # Args:
  #   dataset
  #   options: a list of user options
  #
  # Return:
  #   The (numeric) columns

  if (!is.null(dataset))
    return(dataset)

    n1 <- .readDataSetToEnd(columns.as.numeric = c(options$n1), exclude.na.listwise = c(options$n1))
    y1 <- .readDataSetToEnd(columns.as.numeric = c(options$y1), exclude.na.listwise = c(options$y1))
    n2 <- .readDataSetToEnd(columns.as.numeric = c(options$n2), exclude.na.listwise = c(options$n2))
    y2 <- .readDataSetToEnd(columns.as.numeric = c(options$y2), exclude.na.listwise = c(options$y2))

    .data = list()
    .data[["n1"]] <- n1[[.v(options$n1)]]
    .data[["y1"]] <- y1[[.v(options$y1)]]
    .data[["n2"]] <- n2[[.v(options$n2)]]
    .data[["y2"]] <- y2[[.v(options$y2)]]

    return(.data)
}

.abCheckErrors <- function(dataset, options) {
  
}
