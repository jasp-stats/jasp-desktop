# Function to create covariance matrix:
.covTable <- function(fit,title = "Covariance matrix", include = c("observed","fitted","residual"))
{
	observedCov <- inspect(fit, "sampstat")$cov
	fittedCov <- fitted(fit)$cov
	residualCov <- observedCov - fittedCov
	
	varNames <- colnames(observedCov)
	
	covList <- list(
		observed = observedCov,
		fitted = fittedCov,
		residual = residualCov)
	
	corList <- list(
		observed = cov2cor(observedCov),
		fitted = cov2cor(fittedCov),
		residual = cov2cor(observedCov) - cov2cor(fittedCov))
	
	n <- ncol(covList[[1]])
	
	
	matList <- mapply(cov = covList, cor = corList, type = names(covList), FUN=function(cov,cor, type){
		cov[upper.tri(cov,diag=FALSE)] <- cor[upper.tri(cor,diag=FALSE)] 
		cbind(..sortingDummy = seq_len(NROW(cov)), ..varName = rownames(cov), ..type = type, as.data.frame(cov),stringsAsFactors = FALSE)
	}, SIMPLIFY = FALSE)
	
	matDF <- do.call(rbind, matList)
	matDF <- matDF[matDF$..type %in% include,]
	matDF$..type <- as.character(	matDF$..type)
	matDF <- matDF[order(matDF$..sortingDummy),]
	matDF <- matDF[,-1]
	
	# Empty object:
	covariances <- list()
	
	# Fill:
	covariances[["title"]] <- title
 # covariances[["cases"]] <- matDF$..varName
#	covariances[["cases"]] <- rep("",length(matDF$..varName))
	
	fields <- list(list(name = "Variable", title = "", type="text"),list(name = "Type", title = "", type="text"))
	
	# Enter fields:
	for (i in 1:n)
	{
		fields[[i+2]] <- list(name = varNames[i], type="number", format="dp:3")
	}
	
	# Enter rows:
	rows <- list()
	
	for (i in seq_len(NROW(matDF)))
	{
		variable.name <- varNames[i]
		row <- matDF[i,,drop=FALSE]
	names(row)[1:2] <- c("Variable", "Type")
		rows[[i]] <- as.list(row)
	}

	schema <- list(fields=fields)
	
	covariances[["schema"]] <- schema
	covariances[["data"]] <- rows	
	return(covariances)
}

.is.raw.letter <- function(ch) {

	(ch >= 0x61 && ch <= 0x7A) || (ch >= 0x41 && ch <= 0x5A)
}

.is.alpha.numeric <- function(ch) {

	(ch >= 0x61 && ch <= 0x7A) || (ch >= 0x41 && ch <= 0x5A) || (ch >= 0x30 && ch <= 0x39)
}

.extractVariables <- function(model) {

	reserved.words <- c("c", "start", "equal", "NA")
	
	bytes <- c(charToRaw(model), 0)
	
	variables <- c()
	
	none <- 0
	in.double.quote <- 1
	in.single.quote <- 2
	in.unquoted <- 3
	in.comment <- 4
	
	parse.state <- none
	token.start <- 1
	
	sq <- charToRaw("'")
	dq <- charToRaw('"')
	hash <- charToRaw('#')
	nl <- charToRaw('\n')

	i <- 1
 	while (i <= length(bytes)) {
	
		ch <- bytes[i]

		if (parse.state == none) {

			if (.is.raw.letter(ch))
			{
				token.start <- i
				parse.state <- in.unquoted
				
			} else if (ch == sq) {
			
				token.start <- i
				parse.state <- in.single.quote
				
			} else if (ch == dq) {
			
				token.start <- i
				parse.state <- in.double.quote
			
			} else if (ch == hash) {
			
				parse.state <- in.comment
			}
		
		} else if (parse.state == in.single.quote) {
		
			if (ch == sq) {
			
				variable <- substr(model, token.start, i)
				variables <- c(variables, variable)
				parse.state <- none
			}
		
		} else if (parse.state == in.double.quote) {
		
			if (ch == dq) {
			
				variable <- substr(model, token.start, i)
				variables <- c(variables, variable)
				parse.state <- none
			}
		
		} else if (parse.state == in.unquoted) {
		
			if (.is.alpha.numeric(ch) == FALSE) {
			
				variable <- substr(model, token.start, i - 1)
				variables <- c(variables, variable)
				parse.state <- none
				i <- i - 1
			}
			
		} else if (parse.state == in.comment) {
		
			if (ch == nl)
				parse.state <- none
		
		}
	
		i <- i + 1
	}
	
	variables <- unique(variables)
	variables <- variables[ ! (variables %in% reserved.words)]
	
	if (length(variables) > 0) {
	
		for (i in 1:length(variables))
		{
			variable <- variables[i]
			if ((regexpr("'.*'", variable) == 1) || (regexpr("\".*\"", variable) == 1))
				variable <- substr(variable, 2, nchar(variable) - 1)
			variables[i] <- variable
		}
	
	}
	
	variables <- variables[ ! (variables %in% reserved.words)]
	
	variables
}

.translateModel <- function(model, variables) {

	if (length(variables) == 0)
		return(model)

	variables <- variables[order(nchar(variables), decreasing=TRUE)]
	with.s.quotes <- paste("'", variables, "'", sep="")
	with.d.quotes <- paste('"', variables, '"', sep="")
	
	new.names <- .v(variables)
	
	for (i in 1:length(variables))
		model <- gsub(with.d.quotes[i], new.names[i], model)
	for (i in 1:length(variables))
		model <- gsub(with.s.quotes[i], new.names[i], model)
	for (i in 1:length(variables))
		model <- gsub(variables[i], new.names[i], model)
		
	model
}

### SEM Function:
SEMSimple <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	variables <- .extractVariables(options$model)
	model <- .translateModel(options$model, variables)

	if (is.null(dataset)) {
	
		if (perform == "run") {
			dataset <- read.dataset.to.end(all.columns=TRUE)
		} else {
			dataset <- read.dataset.header(all.columns=TRUE)
		}
	}
	
	
	errorMessage <- ""
	### RUN SEM ###
	if (perform == "run") {
		semResults <- try(lavaan:::lavaan(
			model = model, 
			data = dataset, 
			auto.delta = options$addScalingParameters, 
			auto.th = options$addThresholds,
			orthogonal = options$assumeFactorsUncorrelated,
			auto.cov.y = options$correlateDependentVariables,
			auto.cov.lv.x = options$correlateExogenousLatents,
			mimic = ifelse(options$emulation=="none","lavaan",options$emulation),
			se = options$errorCalculation,
			bootstrap = options$errorCalculationBootstrapSamples,
			estimator = ifelse(options$estimator == "automatic", "default", options$estimator),
			std.lv = options$factorStandardisation == "residualVariance",
			auto.fix.first = options$factorStandardisation == "factorLoadings",
			fixed.x = options$fixExogenousCovariates,
			int.lv.free = !options$fixLatentInterceptsToZero,
			int.ov.free = !options$fixManifestInterceptsToZero,
			meanstructure = options$includeMeanStructure,
			auto.fix.single = options$omitResidualSingleIndicator,
			auto.var = options$residualVariances
	#		group = options$groupingVariable
			))

		# Check if worked:
		if (is(semResults,"try-error"))	{
			errorMessage <- as.character(semResults)
			# Better Error messages:
			if (errorMessage == "Error in start.idx[i]:end.idx[i] : NA/NaN argument\n") {
				errorMessage <- "Model misspecified"
			}
			semResults <- NULL
		}
	} else {
		semResults <- NULL
	}

	### Output object:
	results <- list()

	### ANOVA table ###
	an0va <- list()
	an0va[["title"]] <- "Chi Square Test Statistic (unscaled)"
	# an0va[["cases"]] <- c("Saturated", "Model")
	an0va[["schema"]] <- list(
		fields = list(
			list(name="Model", title = "", type="string"),
			list(name="DF", type="number", format="dp:0"),
			list(name="AIC", type="number", format="dp:1"),
			list(name="BIC", type="number", format="dp:1"),
			list(name="Chisq", type="number", format="dp:3"),
			list(name="Chisq diff", type="number", format="dp:3"),
			list(name="Pr(>Chisq)", type="number", format="dp:3;p:0.001")
		)
	)
	an0va[["data"]] <- list()
	if (!is.null(semResults))
	{
		sem_anova <- lavaan:::anova(semResults)
		for (i in seq_len(NROW(sem_anova)))
		{
			an0va[["data"]][[i]] <- c(Model = rownames(sem_anova)[i],as.list(sem_anova[i,]))
			an0va[["data"]][[i]][is.na(an0va[["data"]][[i]])] <- '.'
			names(an0va[["data"]][[i]]) <- gsub("Df","DF",	names(an0va[["data"]][[i]]))
		}
	}
	if (errorMessage!="" & perform == "run" & options$model != "") an0va[['error']] <- list(errorType="badData", errorMessage=errorMessage)
	results[['fit']] <- an0va


	### PARAMETER ESTIMATES ####
	parEstimates <- list()
	parEstimates[["title"]] <- "Parameter Estimates"
	parEstimates[["schema"]] <- list(
		fields = list(
			list(name="lhs", type="character"),
			list(name="op", type="character"),
			list(name="rhs", type="character"),
			list(name="label", type="character"),
			list(name="est", type="number", format = "dp:3"),
			list(name="se", type="number", format = "dp:3"),
			list(name="z", type="number", format = "dp:3"),
			list(name="pvalue", type="number", format = "dp:3;p:.001"),
			list(name="ci.lower", type="number", format = "dp:3"),
			list(name="ci.upper", type="number", format = "dp:3"),
			list(name="std.lv", type="number", format = "dp:3"),
			list(name="std.all", type="number", format = "dp:3"),
			list(name="std.nox", type="number", format = "dp:3")
		))
	parEstimates[["data"]] <- list()
	if (errorMessage!="" & perform == "run" & options$model != "") parEstimates[['error']] <- list(errorType="badData")
	
	if (!is.null(semResults)) {
		sem_parest <- lavaan:::parameterEstimates(semResults, standardized = TRUE)
		# parEstimates[["cases"]] <- rep("",NROW(sem_parest))
		for (i in seq_len(NROW(sem_parest)))
		{
			estimates <- sem_parest[i,]
			estimates["lhs"] <- .unv(estimates["lhs"])
			estimates["rhs"] <- .unv(estimates["rhs"])
			estimates[is.na(estimates)] <- '.'
			parEstimates[["data"]][[i]] <- as.list(estimates)
		}
	} else {
		parEstimates[["cases"]] <- NULL
	}
	results[["parameterEstimates"]] <- parEstimates


	### MODIFICATION INDICES ###
## SORT THEM
	if (options$output$modificationIndices) {

		modIndices <- list()
		modIndices[["title"]] <- "Modification Indices"
		
		modIndices[["schema"]] <- list(
			fields = list(
				list(name="lhs", type="character"),
				list(name="op", type="character"),
				list(name="rhs", type="character"),
				list(name="mi", type="number", format = "dp:3"),
				list(name="epc", type="number", format = "dp:3"),
				list(name="sepc.lv", type="number", format = "dp:3"),
				list(name="sepc.all", type="number", format = "dp:3"),
				list(name="sepc.nox", type="number", format = "dp:3")
			)
		)
	
		modIndices[["data"]] <- list()
		if (!is.null(semResults))
		{
			# Extract modidffication indices:
			sem_modind <- lavaan:::modificationIndices(semResults)
			### Remove NA:
			sem_modind <- sem_modind[!is.na(sem_modind$mi),,drop=FALSE]

			## Sort:
			sem_modind <- sem_modind[order(sem_modind$mi,decreasing = TRUE),,drop=FALSE]

			### Remove low indices:
			if (options$output$modificationIndicesHideLowIndices)
			{
				sem_modind <- sem_modind[sem_modind$mi > options$output$modificationIndicesHideLowIndicesThreshold,,  drop = FALSE]
			}

			modIndices[["cases"]] <- rep("",nrow(sem_modind))
			for (i in seq_len(nrow(sem_modind)))
			{
				modIndices[["data"]][[i]] <- as.list(sem_modind[i,])
				modIndices[["data"]][[i]][is.na(modIndices[["data"]][[i]])] <- '.'
			}
		} else {
			modIndices[["cases"]] <- NULL
		}
	if (errorMessage!="" & perform == "run" & options$model != "") modIndices[['error']] <- list(errorType="badData")
		results[["modificationIndices"]] <- modIndices
	}


	## FIT MEASURES ###
	if (options$output$additionalFitMeasures){
		fitMeasures <- list()
		fitMeasures[["title"]] <- "Fit Measures"
		fitMeasures[["schema"]] <- list(fields = list(
			list(name="Type", title = "", type="string"),
			list(name="Measure", type="number", format="dp:3")))


		cases <- c("fmin", "chisq", "df", "pvalue", "baseline.chisq", "baseline.df", 
						"baseline.pvalue", "cfi", "tli", "nnfi", "rfi", "nfi", "pnfi", 
						"ifi", "rni", "logl", "unrestricted.logl", "npar", "aic", "bic", 
						"ntotal", "bic2", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", 
						"rmsea.pvalue", "rmr", "rmr_nomean", "srmr", "srmr_nomean", "cn_05", 
						"cn_01", "gfi", "agfi", "pgfi", "mfi", "ecvi")

		if (!is.null(semResults))
		{
			sem_fitm <- unlist(lavaan:::fitMeasures(semResults))
			#data <- lapply(as.list(unname(sem_fitm)) , function(x) {names(x) <- "Measure";x})
		} #else data <- list()

		for (i in seq_along(cases))
		{
			entry <- list(Type = cases[i], Measure = ".")
			if (!is.null(semResults))
			{
				entry$Measure <- sem_fitm[[i]]
			}
			fitMeasures[["data"]][[i]] <- entry
		}
		if (errorMessage!="" & perform == "run" & options$model != "") fitMeasures[['error']] <- list(errorType="badData")
		results[["fitMeasures"]] <- fitMeasures
	}

   ### Mardia coefficient
	if (options$output$mardiasCoefficients){
		mardiasCoefficient <- list()
		mardiasCoefficient[["title"]] <- "Mardia's coefficients"
		mardiasCoefficient[["schema"]] <- list(fields = list(
			list(name="Type", title = "", type="string"),
			list(name="Coefficient", type="number", format="dp:3"),
			list(name="z", type="number", format="dp:3"),
			list(name="Chisq", type="number", format="dp:3"),
			list(name="DF", type="number", format="dp:0"),
			list(name="p-value", type="number", format="dp:3;p:0.001")
))


		if (!is.null(semResults))
		{
			varNames <- lavaanNames(semResults, type = "ov")

			mardiaSkew <- unname(semTools:::mardiaSkew(dataset[,varNames]))
			mardiaKurtosis <- unname(semTools:::mardiaKurtosis(dataset[,varNames]))
			mardiasCoefficient[["data"]] <- list(
			list(Type = "Skewness", Coefficient = mardiaSkew[1], z = ".", Chisq = mardiaSkew[2], DF = mardiaSkew[3], "p-value" = mardiaSkew[4]),
			list(Type = "Kurtosis", Coefficient = mardiaKurtosis[1], z = mardiaKurtosis[2], Chisq = ".", DF = ".", "p-value" = mardiaKurtosis[3]))
		}
		if (errorMessage!="" & perform == "run" & options$model != "") mardiasCoefficient[['error']] <- list(errorType="badData")
		results[["mardiasCoefficient"]] <- mardiasCoefficient
	}

	### Covariance table:
	# Only if observed, fitted or residual 
	if( options$output$observedCovarianceCorrelations | options$output$fittedCovarianceCorrelations | options$output$residualCovarianceCorrelations) {
		if (!is.null(semResults))
		{
			results[["covcor"]] <- .covTable(semResults, "Covariances (lower triangle) / correlations (upper triangle)", include = c("observed","fitted","residual")[ c(options$output$observedCovarianceCorrelations , options$output$fittedCovarianceCorrelations ,options$output$residualCovarianceCorrelations) ] )
		}
	}
	
	return(results)
}

