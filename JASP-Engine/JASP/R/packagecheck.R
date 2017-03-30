#
# Copyright (C) 2013-2015 University of Amsterdam
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

.checkPackages <- function() {

	expected <- matrix(nrow=0, ncol=2, dimnames=list(NULL, c("Package", "Version")))

	expected <- rbind(expected, c("acepack", "1.3-3.3"))
	expected <- rbind(expected, c("afex", "0.14-2"))
	expected <- rbind(expected, c("BayesFactor", "0.9.12"))
	expected <- rbind(expected, c("car", "2.0-22"))
	expected <- rbind(expected, c("coda", "0.17-1"))
	expected <- rbind(expected, c("coin", "1.0-24"))
	expected <- rbind(expected, c("colorspace", "1.2-4"))
	expected <- rbind(expected, c("contfrac", "1.1-9"))
	expected <- rbind(expected, c("corpcor", "1.6.7"))
	expected <- rbind(expected, c("d3Network", "0.5.1"))
	expected <- rbind(expected, c("dichromat", "2.0-0"))
	expected <- rbind(expected, c("digest", "0.6.6"))
	expected <- rbind(expected, c("effects", "3.0-3"))
	expected <- rbind(expected, c("ellipse", "0.3-8"))
	expected <- rbind(expected, c("elliptic", "1.3-5"))
	expected <- rbind(expected, c("estimability", "1.1"))
	expected <- rbind(expected, c("fdrtool", "1.2.13"))
	expected <- rbind(expected, c("Formula", "1.1-2"))
	expected <- rbind(expected, c("ggm", "2.2"))
	expected <- rbind(expected, c("ggplot2", "1.0.0"))
	expected <- rbind(expected, c("glasso", "1.8"))
	expected <- rbind(expected, c("gnm", "1.0-8"))
	expected <- rbind(expected, c("gtable", "0.1.2"))
	expected <- rbind(expected, c("gtools", "3.4.1"))
	expected <- rbind(expected, c("Hmisc", "3.14-6"))
	expected <- rbind(expected, c("huge", "1.2.6"))
	expected <- rbind(expected, c("hypergeo", "1.2-9"))
	expected <- rbind(expected, c("igraph", "0.7.1"))
	expected <- rbind(expected, c("jpeg", "0.1-8"))
	expected <- rbind(expected, c("labeling", "0.3"))
	expected <- rbind(expected, c("latticeExtra", "0.6-26"))
	expected <- rbind(expected, c("lavaan", "0.5-17"))
	expected <- rbind(expected, c("logspline", "2.1.5"))
	expected <- rbind(expected, c("lme4", "1.1-7"))
	expected <- rbind(expected, c("lsmeans", "2.17"))
	expected <- rbind(expected, c("matrixcalc", "1.0-3"))
	expected <- rbind(expected, c("MatrixModels", "0.4-0"))
	expected <- rbind(expected, c("minqa", "1.2.4"))
	expected <- rbind(expected, c("mnormt", "1.5-1"))
	expected <- rbind(expected, c("modeltools", "0.2-21"))
	expected <- rbind(expected, c("munsell", "0.4.2"))
	expected <- rbind(expected, c("multcomp", "1.3-9"))
	expected <- rbind(expected, c("mvtnorm", "1.0-1"))
	expected <- rbind(expected, c("nloptr", "1.0.4"))
	expected <- rbind(expected, c("pbapply", "1.1-1"))
	expected <- rbind(expected, c("pbivnorm", "0.5-1"))
	expected <- rbind(expected, c("pbkrtest", "0.4-2"))
	expected <- rbind(expected, c("pixmap", "0.4-11"))
	expected <- rbind(expected, c("plotrix", "3.5-10"))
	expected <- rbind(expected, c("plyr", "1.8.1"))
	expected <- rbind(expected, c("png", "0.1-7"))
	expected <- rbind(expected, c("proto", "0.3-10"))
	expected <- rbind(expected, c("psych", "1.6.12"))
	expected <- rbind(expected, c("qgraph", "1.3"))
	expected <- rbind(expected, c("quadprog", "1.5-5"))
	expected <- rbind(expected, c("qvcalc", "0.8-8"))
	expected <- rbind(expected, c("RColorBrewer", "1.1-2"))
	expected <- rbind(expected, c("Rcpp", "0.11.3"))
	expected <- rbind(expected, c("RcppEigen", "0.3.2.2.0"))
	expected <- rbind(expected, c("relimp", "1.0-3"))
	expected <- rbind(expected, c("reshape2", "1.4.1"))
	expected <- rbind(expected, c("RInside", "0.2.11"))
	expected <- rbind(expected, c("RJSONIO", "1.3-0"))
	expected <- rbind(expected, c("rtiff", "1.4.4"))
	expected <- rbind(expected, c("sandwich", "2.3-2"))
	expected <- rbind(expected, c("scales", "0.2.4"))
	expected <- rbind(expected, c("sem", "3.1-5"))
	expected <- rbind(expected, c("semTools", "0.4-6"))
	expected <- rbind(expected, c("sendplot", "4.0.0"))
	expected <- rbind(expected, c("sna", "2.3-2"))
	expected <- rbind(expected, c("stringr", "0.6.2"))
	expected <- rbind(expected, c("TH.data", "1.0-6"))
	expected <- rbind(expected, c("vcd", "1.3-2"))
	expected <- rbind(expected, c("vcdExtra", "0.6-3"))
	expected <- rbind(expected, c("whisker", "0.3-2"))
	expected <- rbind(expected, c("zoo", "1.7-11"))

	expected.package.names <- expected[,1]
	dimnames(expected) <- list(expected.package.names, c("Package", "Version"))

	installed <- installed.packages()
	installed.package.names <- dimnames(installed)[[1]]

	messages <- c()

	for (package.name in expected.package.names) {
	
		if (package.name %in% installed.package.names) {
		
			installed.version <- installed[package.name, "Version"]
			expected.version  <- expected[package.name, "Version"]
		
			if (installed.version != expected.version)
				messages <- c(messages, paste("Package ", package.name, " is not the correct version; expected: ", expected.version, ", installed: ", installed.version, sep=""))
		
		} else {
		
			messages <- c(messages, paste("Package ", package.name, " not installed!", sep=""))
		}
	
	}
	
	.initPackages(installed.package.names)
	
	if (length(messages) == 0) {
	
		list(official=TRUE)
	
	} else {
	
		list(official=FALSE, messages=messages)
	}
}


.initPackages <- function(installed) {
	
	# First call to BayesFactor initializes its functions and speeds up subsequent calls.
	if ('BayesFactor' %in% installed) {
		try(BayesFactor::BFInfo(print=FALSE), silent=TRUE)
	}
	
}

