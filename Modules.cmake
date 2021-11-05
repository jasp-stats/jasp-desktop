cmake_minimum_required(VERSION 3.21)


# This should happen before the configuration
# add_custom_command(TARGET R-Interface
# 	PRE_BUILD
# 	COMMAND ${CMAKE_SOURCE_DIR}/${_Rscript_EXE} -e 'install.packages\("Rcpp", repos="http://cran.r-project.org"\)'
# 	COMMAND ${CMAKE_SOURCE_DIR}/${_Rscript_EXE} -e 'install.packages\("RInside", repos="http://cran.r-project.org"\)'
# 	COMMENT "=== Installing RInside and Rcpp ...")

set(JASP_COMMON_MODULES
	jaspBase
	jaspDescriptives
	jaspAnova
	jaspFactor
	jaspFrequencies
	jaspRegression
	jaspTTests
	jaspMixedModels
)

set(JASP_EXTRA_MODULES
	jaspAudit
	jaspBain
	jaspNetwork
	jaspSem
	jaspMachineLearning
	jaspSummaryStatistics
	jaspMetaAnalysis
	jaspDistributions
	jaspEquivalenceTTests
	jaspJags
	jaspReliability
	jaspVisualModeling
	jaspLearnBayes
	jaspProphet
	jaspProcessControl
	jaspCircular
	)


if (${INSTALL_R_MODULES})

	message(STATUS "[JASP]: Installing JASP's R Modules...")

	foreach(MODULE IN JASP_COMMON_MODULES)

		add_custom_command(TARGET R-Interface
			POST_BUILD 
			COMMAND ${CMAKE_SOURCE_DIR}/${_Rscript_EXE} -e 'remotes::install_github\("jasp-stats/${MODULE}"\)'
			COMMENT "=== Installing ${MODULE} ...")

	endforeach()

endif()