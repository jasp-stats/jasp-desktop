cmake_minimum_required(VERSION 3.21)

list(APPEND CMAKE_MESSAGE_CONTEXT Modules)

# TODOs:
# - [ ] Make sure that RInside and Rcpp are installed prior to the configuration
# - [ ] Install JAGS as a Framework
# - [ ] Install the jaspBase
# - [ ] Setup the RENV
# - [ ] Install the common modules

set(R_REQUIRED_MODULES "devtools")

set(JASP_COMMON_MODULES
    "jaspDescriptives"
    "jaspAnova"
    # "jaspFactor"
    # "jaspFrequencies"
    # "jaspRegression"
    # "jaspTTests"
    # "jaspMixedModels"
)

set(JASP_EXTRA_MODULES
    # "jaspAudit"
    # "jaspBain"
    # "jaspNetwork"
    # "jaspSem"
    # "jaspMachineLearning"
    # "jaspSummaryStatistics"
    # "jaspMetaAnalysis"
    "jaspDistributions"
    # "jaspEquivalenceTTests"
    # "jaspJags"
    # "jaspReliability"
    # "jaspVisualModeling"
    # "jaspLearnBayes"
    # "jaspProphet"
    # "jaspProcessControl"
    # "jaspCircular"
)

# TODO: Standardize these names, either use PATH, ROOT, or DIR, but everywhere

set(MODULES_SOURCE_PATH ${PROJECT_SOURCE_DIR}/Modules
    CACHE PATH "Location of JASP Modules")

set(MODULES_RENV_PATH "${PROJECT_BINARY_DIR}/Desktop/Modules"
    CACHE PATH "Location of the renv libraries")
set(MODULES_RENV_ROOT_PATH "${MODULES_RENV_PATH}/renv-root"
    CACHE PATH "Location of renv root directories")
set(MODULES_RENV_CACHE_PATH "${MODULES_RENV_PATH}/renv-cache"
    CACHE PATH "Location of renv cache directories")

make_directory(${MODULES_RENV_PATH})
make_directory(${MODULES_RENV_ROOT_PATH})
make_directory(${MODULES_RENV_CACHE_PATH})

cmake_print_variables(MODULES_RENV_PATH)
cmake_print_variables(MODULES_RENV_ROOT_PATH)
cmake_print_variables(MODULES_RENV_CACHE_PATH)

# message(
#   STATUS
#     ${_Rscript_EXE}
#     -e
#     'Sys.setenv\(R_REMOTES_NO_ERRORS_FROM_WARNINGS=TRUE, RENV_PATHS_ROOT="${MODULES_RENV_PATH}", RENV_PATHS_CACHE="${MODULES_RENV_CACHE_PATH}"\)'
# )

# execute_process(
#   COMMAND
#     ${_Rscript_EXE} -e
#     'Sys.setenv\(R_REMOTES_NO_ERRORS_FROM_WARNINGS=TRUE, RENV_PATHS_ROOT="${MODULES_RENV_PATH}", RENV_PATHS_CACHE="${MODULES_RENV_CACHE_PATH}"\)'
#     COMMAND_ERROR_IS_FATAL ANY COMMAND_ECHO NONE
#   OUTPUT_QUIET ERROR_QUIET)

# , JAGS_ROOT="$JAGS_ROOT"$$JAGS_MAC_HELPER\)'

add_custom_target(Modules)

list(APPEND CMAKE_MESSAGE_INDENT "  ")
message(STATUS "Installing Required R Modules...")

foreach(MODULE ${R_REQUIRED_MODULES})

  add_custom_command(
    TARGET Modules
    PRE_BUILD
    COMMAND ${_Rscript_EXE} -e
            'install.packages\("${MODULE}", repos="http://cran.r-project.org"\)'
    COMMENT "------ Installing ${MODULE}...")

endforeach()

add_custom_command(
  TARGET Modules POST_BUILD
  COMMAND ${_Rscript_EXE} -e 'remotes::install_github\("jasp-stats/jaspBase"\)'
  COMMENT "------ Installing jaspBase...")

message(STATUS "Installing JASP's Common Modules...")
foreach(MODULE ${JASP_COMMON_MODULES})

  add_custom_command(
    TARGET Modules
    POST_BUILD
    COMMAND
      ${_Rscript_EXE} -e
      'jaspBase::installJaspModule\("${MODULES_SOURCE_PATH}/${MODULE}", libPathsToUse=NULL, repos="http://cran.r-project.org", moduleLibrary="${MODULES_RENV_PATH}/${MODULE}", onlyModPkg=FALSE\)'
    COMMENT "------ Installing ${MODULE}...")

endforeach()

message(STATUS "Installing JASP's Extra Modules...")
foreach(MODULE ${JASP_EXTRA_MODULES})

  add_custom_command(
    TARGET Modules
    POST_BUILD
    COMMAND
      ${_Rscript_EXE} -e
      'jaspBase::installJaspModule\("${MODULES_SOURCE_PATH}/${MODULE}", libPathsToUse=NULL, repos="http://cran.r-project.org", moduleLibrary="${MODULES_RENV_PATH}/${MODULE}", onlyModPkg=FALSE\)'
    COMMENT "------ Installing ${MODULE}...")

endforeach()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
