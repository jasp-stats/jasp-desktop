cmake_minimum_required(VERSION 3.21)

list(APPEND CMAKE_MESSAGE_CONTEXT Modules)

# TODOs:
# - [ ] Install JAGS as a Framework
# - [ ] Double check the GITHUB_PAT stuff

# We can perhaps install a lot of the packages that we need before
# getting into JASP stuff, if they don't cause any clash
set(R_REQUIRED_MODULES "devtools")

set(JASP_COMMON_MODULES
    "jaspDescriptives"
    # "jaspAnova"
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
    # "jaspDistributions"
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

set(MODULES_RENV_PATH "${CMAKE_CURRENT_LIST_DIR}"
    CACHE PATH "Location of the renv libraries")
set(MODULES_RENV_ROOT_PATH "${MODULES_RENV_PATH}/renv-root"
    CACHE PATH "Location of renv root directories")
set(MODULES_RENV_CACHE_PATH "${MODULES_RENV_PATH}/renv-cache"
    CACHE PATH "Location of renv cache directories")
set(JASPENGINE_PATH "${PROJECT_BINARY_DIR}/Desktop/"
    CACHE PATH "Location of the JASPEngine")

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
add_dependencies(JASP Modules)

message(STATUS "Installing Required R Modules...")

foreach(MODULE ${R_REQUIRED_MODULES})

  add_custom_command(
    TARGET Modules
    PRE_BUILD
    COMMAND ${_Rscript_EXE} -e
            'install.packages\("${MODULE}", repos="http://cran.r-project.org"\)'
    COMMENT "------ Installing ${MODULE}...")

endforeach()

# I need to make sure that this goes before other jaspModules.
# I am still not sure if it's better to use the execute_process or
# create a custom_command
# message(CHECK_START "Checking for 'jaspBase'")
# if(NOT EXISTS ${_R_Library_HOME}/jaspBase)

message(CHECK_START "Installing the 'jaspBase'")
execute_process(
  COMMAND
    ${_Rscript_EXE} -e
    "install.packages('${PROJECT_SOURCE_DIR}/Engine/jaspBase/', type='source', repos=NULL)"
  OUTPUT_QUIET ERROR_QUIET COMMAND_ERROR_IS_FATAL ANY COMMAND_ECHO NONE)
message(CHECK_PASS "successful.")
# else()
#   message(CHECK_PASS "found.")
# endif()

message(STATUS "Configuring JASP's Common Modules...")
foreach(MODULE ${JASP_COMMON_MODULES})

  # We can technically create a new install_module.R for each Renv
  # even better, we can have different templates for each module, and use those
  # to set them up correctly
  make_directory(${MODULES_RENV_PATH}/${MODULE})
  configure_file(${MODULES_SOURCE_PATH}/install_module.R.in
                 ${MODULES_RENV_PATH}/${MODULE}/install_module.R)

  add_custom_command(
    TARGET Modules
    POST_BUILD
    COMMAND ${_Rscript_EXE} -e ${MODULES_RENV_PATH}/${MODULE}/install_module.R
    WORKING_DIRECTORY ${MODULES_RENV_PATH}/${MODULE}
    COMMENT "------ Installing ${MODULE}...")

endforeach()

message(STATUS "Configuring JASP's Extra Modules...")
foreach(MODULE ${JASP_EXTRA_MODULES})

  make_directory(${MODULES_RENV_PATH}/${MODULE})
  configure_file(${MODULES_SOURCE_PATH}/install_module.R.in
                 ${MODULES_RENV_PATH}/${MODULE}/install_module.R)

  add_custom_command(
    TARGET Modules
    POST_BUILD
    COMMAND ${_Rscript_EXE} -e ${MODULES_RENV_PATH}/${MODULE}/install_module.R
    WORKING_DIRECTORY ${MODULES_RENV_PATH}/${MODULE}
    COMMENT "------ Installing ${MODULE}...")

endforeach()
