#
# Copyright (C) 2016 University of Amsterdam
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

.pValueFromT <- function(t, n1, n2 = FALSE, alternative = "two.sided", var.equal = TRUE) {
	# Note the hack by setting n2=FALSE
	# Output: number in [0, 1]
	# Note: always true: var.equal, we do not have enough info for different variances.
	#       In that case we also need s1 and s2

	result <- NA

	if (n2 > 0) {
		# If n2 > 0, then two-sample
		someDf <- n1 + n2 - 2
	} else {
		# If n2 <= 0, then one-sample
		someDf <- n1 - 1
	}

	if (alternative=="two.sided") {
		# mu \neq 0
		result <- 2 * pt(-abs(t), df = someDf)
	} else if (alternative == "less") {
		# mu < 0
		result <- pt(t, df = someDf)
	} else if (alternative == "greater") {
		# mu > 0
		result <- pt(t, df = someDf, lower.tail = FALSE)
	}

	return(result)
}
