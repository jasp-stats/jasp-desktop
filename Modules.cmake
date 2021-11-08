cmake_minimum_required(VERSION 3.21)

# TODOs:
# - [ ] Make sure that RInside and Rcpp are installed prior to the configuration
# - [ ] Install JAGS as a Framework

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