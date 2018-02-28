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


ReinforcementLearningR11tLearning <- function (
    dataset = NULL,
    options,
    perform = "run",
    callback = function(...) list(status = "ok"),
    state = NULL,
    ...
) {

    table <- list()
    table[["title"]] <- "R11t Learning"
    table[["schema"]] <- list(fields = list())
    table[["data"]] <- list()

    results <- list()
    results[[".meta"]] <- list(list(name = "table", type = "table"))
    results[["table"]] <- table

    return (list(results = results, status = "complete"))
}
