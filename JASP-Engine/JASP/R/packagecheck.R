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
	
#--auto-generated
	expected <- rbind(expected, c('BAS', '1.4.6'))
	expected <- rbind(expected, c('BDgraph', '2.40'))
	expected <- rbind(expected, c('BMS', '0.3.4'))
	expected <- rbind(expected, c('BayesFactor', '0.9.12-2'))
	expected <- rbind(expected, c('Formula', '1.2-2'))
	expected <- rbind(expected, c('GPArotation', '2014.11-1'))
	expected <- rbind(expected, c('Hmisc', '4.0-3'))
	expected <- rbind(expected, c('KernSmooth', '2.23-15'))
	expected <- rbind(expected, c('MASS', '7.3-45'))
	expected <- rbind(expected, c('Matrix', '1.2-8'))
	expected <- rbind(expected, c('MatrixModels', '0.4-1'))
	expected <- rbind(expected, c('R6', '2.2.2'))
	expected <- rbind(expected, c('RColorBrewer', '1.1-2'))
	expected <- rbind(expected, c('Rcpp', '0.12.13'))
	expected <- rbind(expected, c('RcppEigen', '0.3.3.3.0'))
	expected <- rbind(expected, c('SparseM', '1.77'))
	expected <- rbind(expected, c('SuppDists', '1.1-9.4'))
	expected <- rbind(expected, c('TH.data', '1.0-8'))
	expected <- rbind(expected, c('TTR', '0.23-2'))
	expected <- rbind(expected, c('abind', '1.4-5'))
	expected <- rbind(expected, c('acepack', '1.4.1'))
	expected <- rbind(expected, c('afex', '0.18-0'))
	expected <- rbind(expected, c('arm', '1.9-3'))
	expected <- rbind(expected, c('backports', '1.1.1'))
	expected <- rbind(expected, c('base', '3.3.3'))
	expected <- rbind(expected, c('base64enc', '0.1-3'))
	expected <- rbind(expected, c('boot', '1.3-18'))
	expected <- rbind(expected, c('ca', '0.70'))
	expected <- rbind(expected, c('car', '2.1-5'))
	expected <- rbind(expected, c('checkmate', '1.8.4'))
	expected <- rbind(expected, c('class', '7.3-14'))
	expected <- rbind(expected, c('cluster', '2.0.5'))
	expected <- rbind(expected, c('coda', '0.19-1'))
	expected <- rbind(expected, c('codetools', '0.2-15'))
	expected <- rbind(expected, c('coin', '1.2-1'))
	expected <- rbind(expected, c('colorspace', '1.3-2'))
	expected <- rbind(expected, c('compiler', '3.3.3'))
	expected <- rbind(expected, c('contfrac', '1.1-11'))
	expected <- rbind(expected, c('conting', '1.6'))
	expected <- rbind(expected, c('corpcor', '1.6.9'))
	expected <- rbind(expected, c('curl', '2.8.1'))
	expected <- rbind(expected, c('d3Network', '0.5.2.1'))
	expected <- rbind(expected, c('data.table', '1.10.4'))
	expected <- rbind(expected, c('datasets', '3.3.3'))
	expected <- rbind(expected, c('deSolve', '1.20'))
	expected <- rbind(expected, c('dichromat', '2.0-0'))
	expected <- rbind(expected, c('digest', '0.6.12'))
	expected <- rbind(expected, c('ellipse', '0.3-8'))
	expected <- rbind(expected, c('elliptic', '1.3-7'))
	expected <- rbind(expected, c('estimability', '1.2'))
	expected <- rbind(expected, c('evaluate', '0.10.1'))
	expected <- rbind(expected, c('fdrtool', '1.2.15'))
	expected <- rbind(expected, c('foreign', '0.8-67'))
	expected <- rbind(expected, c('ggm', '2.3'))
	expected <- rbind(expected, c('ggplot2', '2.2.1'))
	expected <- rbind(expected, c('glasso', '1.8'))
	expected <- rbind(expected, c('gnm', '1.0-8'))
	expected <- rbind(expected, c('grDevices', '3.3.3'))
	expected <- rbind(expected, c('graphics', '3.3.3'))
	expected <- rbind(expected, c('grid', '3.3.3'))
	expected <- rbind(expected, c('gridExtra', '2.3'))
	expected <- rbind(expected, c('gtable', '0.2.0'))
	expected <- rbind(expected, c('gtools', '3.5.0'))
	expected <- rbind(expected, c('highr', '0.6'))
	expected <- rbind(expected, c('hmeasure', '1.0'))
	expected <- rbind(expected, c('htmlTable', '1.9'))
	expected <- rbind(expected, c('htmltools', '0.3.6'))
	expected <- rbind(expected, c('htmlwidgets', '0.9'))
	expected <- rbind(expected, c('huge', '1.2.7'))
	expected <- rbind(expected, c('hypergeo', '1.2-13'))
	expected <- rbind(expected, c('igraph', '1.1.2'))
	expected <- rbind(expected, c('irlba', '2.2.1'))
	expected <- rbind(expected, c('jpeg', '0.1-8'))
	expected <- rbind(expected, c('jsonlite', '1.5'))
	expected <- rbind(expected, c('knitr', '1.17'))
	expected <- rbind(expected, c('labeling', '0.3'))
	expected <- rbind(expected, c('lattice', '0.20-34'))
	expected <- rbind(expected, c('latticeExtra', '0.6-28'))
	expected <- rbind(expected, c('lavaan', '0.5-23.1097'))
	expected <- rbind(expected, c('lazyeval', '0.2.0'))
	expected <- rbind(expected, c('lme4', '1.1-14'))
	expected <- rbind(expected, c('lmerTest', '2.0-33'))
	expected <- rbind(expected, c('lmtest', '0.9-35'))
	expected <- rbind(expected, c('logspline', '2.1.9'))
	expected <- rbind(expected, c('lsmeans', '2.27-2'))
	expected <- rbind(expected, c('magrittr', '1.5'))
	expected <- rbind(expected, c('markdown', '0.8'))
	expected <- rbind(expected, c('matrixcalc', '1.0-3'))
	expected <- rbind(expected, c('methods', '3.3.3'))
	expected <- rbind(expected, c('mgcv', '1.8-17'))
	expected <- rbind(expected, c('mi', '1.0'))
	expected <- rbind(expected, c('mime', '0.5'))
	expected <- rbind(expected, c('minqa', '1.2.4'))
	expected <- rbind(expected, c('mnormt', '1.5-5'))
	expected <- rbind(expected, c('modeltools', '0.2-21'))
	expected <- rbind(expected, c('multcomp', '1.4-7'))
	expected <- rbind(expected, c('munsell', '0.4.3'))
	expected <- rbind(expected, c('mvtnorm', '1.0-6'))
	expected <- rbind(expected, c('network', '1.13.0'))
	expected <- rbind(expected, c('nlme', '3.1-131'))
	expected <- rbind(expected, c('nloptr', '1.0.4'))
	expected <- rbind(expected, c('nnet', '7.3-12'))
	expected <- rbind(expected, c('numDeriv', '2016.8-1'))
	expected <- rbind(expected, c('parallel', '3.3.3'))
	expected <- rbind(expected, c('pbapply', '1.3-3'))
	expected <- rbind(expected, c('pbivnorm', '0.6.0'))
	expected <- rbind(expected, c('pbkrtest', '0.4-7'))
	expected <- rbind(expected, c('pkgconfig', '2.0.1'))
	expected <- rbind(expected, c('plotrix', '3.6-6'))
	expected <- rbind(expected, c('plyr', '1.8.4'))
	expected <- rbind(expected, c('png', '0.1-7'))
	expected <- rbind(expected, c('psych', '1.7.8'))
	expected <- rbind(expected, c('qgraph', '1.4.4'))
	expected <- rbind(expected, c('quadprog', '1.5-5'))
	expected <- rbind(expected, c('quantmod', '0.4-10'))
	expected <- rbind(expected, c('quantreg', '5.33'))
	expected <- rbind(expected, c('qvcalc', '0.9-1'))
	expected <- rbind(expected, c('relimp', '1.0-5'))
	expected <- rbind(expected, c('reshape2', '1.4.2'))
	expected <- rbind(expected, c('rjson', '0.2.15'))
	expected <- rbind(expected, c('rlang', '0.1.2'))
	expected <- rbind(expected, c('rpart', '4.1-10'))
	expected <- rbind(expected, c('sandwich', '2.4-0'))
	expected <- rbind(expected, c('scales', '0.5.0'))
	expected <- rbind(expected, c('sem', '3.1-9'))
	expected <- rbind(expected, c('semTools', '0.4-14'))
	expected <- rbind(expected, c('sna', '2.4'))
	expected <- rbind(expected, c('spatial', '7.3-11'))
	expected <- rbind(expected, c('splines', '3.3.3'))
	expected <- rbind(expected, c('statnet.common', '4.0.0'))
	expected <- rbind(expected, c('stats', '3.3.3'))
	expected <- rbind(expected, c('stats4', '3.3.3'))
	expected <- rbind(expected, c('stringi', '1.1.5'))
	expected <- rbind(expected, c('stringr', '1.2.0'))
	expected <- rbind(expected, c('survival', '2.40-1'))
	expected <- rbind(expected, c('tcltk', '3.3.3'))
	expected <- rbind(expected, c('tibble', '1.3.4'))
	expected <- rbind(expected, c('tools', '3.3.3'))
	expected <- rbind(expected, c('tseries', '0.10-42'))
	expected <- rbind(expected, c('utils', '3.3.3'))
	expected <- rbind(expected, c('vcd', '1.4-3'))
	expected <- rbind(expected, c('vcdExtra', '0.7-1'))
	expected <- rbind(expected, c('viridis', '0.4.0'))
	expected <- rbind(expected, c('viridisLite', '0.2.0'))
	expected <- rbind(expected, c('whisker', '0.3-2'))
	expected <- rbind(expected, c('xtable', '1.8-2'))
	expected <- rbind(expected, c('xts', '0.10-0'))
	expected <- rbind(expected, c('yaml', '2.1.14'))
	expected <- rbind(expected, c('zoo', '1.8-0'))
#--auto-generated

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


.initPackages <- function(installedPackages) {
	
	packages <- c("BayesFactor") # Add any package that needs pre-loading
	
	for (package in packages) {
		if (package %in% installedPackages && base::isNamespaceLoaded(package) == FALSE) {
			try(base::loadNamespace(package), silent=TRUE)
		}
	}
	
}

