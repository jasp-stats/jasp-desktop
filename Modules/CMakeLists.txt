cmake_minimum_required(VERSION 3.21)

list(APPEND CMAKE_MESSAGE_CONTEXT Modules)

set(JASP_COMMON_MODULES
    "jaspDescriptives"
    # "jaspAnova"
    # "jaspFactor"
    # "jaspFrequencies"
    # "jaspRegression"
    # "jaspTTests"
    # "jaspMixedModels"
)

list(REVERSE JASP_COMMON_MODULES)
set(JASP_COMMON_MODULES_COPY ${JASP_COMMON_MODULES})
list(POP_FRONT JASP_COMMON_MODULES_COPY FIRST_COMMON_MODULE)

set(JASP_EXTRA_MODULES
    # "jaspAudit"
    # "jaspBain"
    # "jaspNetwork"
    # "jaspSem"
    # "jaspMachineLearning"
    # "jaspSummaryStatistics"
    "jaspMetaAnalysis"
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

list(REVERSE JASP_EXTRA_MODULES)
set(JASP_EXTRA_MODULES_COPY ${JASP_EXTRA_MODULES})
list(POP_FRONT JASP_EXTRA_MODULES_COPY)

# TODO: Standardize these names, either use PATH, ROOT, or DIR, but everywhere

set(MODULES_SOURCE_PATH
    ${PROJECT_SOURCE_DIR}/Modules
    CACHE PATH "Location of JASP Modules")

set(MODULES_RENV_PATH
    "${CMAKE_BINARY_DIR}/Desktop/Modules"
    CACHE PATH "Location of the renv libraries")
set(MODULES_RENV_ROOT_PATH
    "${MODULES_RENV_PATH}/renv-root"
    CACHE PATH "Location of renv root directories")
set(MODULES_RENV_CACHE_PATH
    "${MODULES_RENV_PATH}/renv-cache"
    CACHE PATH "Location of renv cache directories")
set(JASP_ENGINE_PATH
    "${PROJECT_BINARY_DIR}/Desktop/"
    CACHE PATH "Location of the JASPEngine")

make_directory(${MODULES_RENV_PATH})
make_directory(${MODULES_RENV_ROOT_PATH})
make_directory(${MODULES_RENV_CACHE_PATH})

cmake_print_variables(MODULES_RENV_PATH)
cmake_print_variables(MODULES_RENV_ROOT_PATH)
cmake_print_variables(MODULES_RENV_CACHE_PATH)

add_custom_target(Modules)
add_dependencies(Modules ${JASP_COMMON_MODULES} ${JASP_EXTRA_MODULES})

message(STATUS "Installing Required R Modules...")

# This happens during the configuration!
message(CHECK_START "Installing the 'jaspBase'")
execute_process(
  COMMAND
    ${_Rscript_EXE} -e
    "install.packages('${PROJECT_SOURCE_DIR}/Engine/jaspBase/', type='source', repos='${R_REPOSITORY}')"
  OUTPUT_QUIET
  ERROR_QUIET
  COMMAND_ERROR_IS_FATAL
  ANY
  COMMAND_ECHO
  NONE)
message(CHECK_PASS "successful.")

message(STATUS "Configuring Common Modules...")
foreach(MODULE ${JASP_COMMON_MODULES})

  # We can technically create a new install-module.R for each Renv
  # even better, we can have different templates for each module, and use those
  # to set them up correctly
  make_directory(${MODULES_RENV_PATH}/${MODULE})
  configure_file(${CMAKE_CURRENT_LIST_DIR}/install-module.R.in
                 ${MODULES_RENV_ROOT_PATH}/install-${MODULE}.R)

  add_custom_target(
    ${MODULE}
    COMMAND ${_Rscript_EXE} ${MODULES_RENV_ROOT_PATH}/install-${MODULE}.R
    BYPRODUCTS ${MODULES_RENV_PATH}/${MODULE}/
    COMMENT "------ Installing ${MODULE}...")

  list(POP_FRONT JASP_COMMON_MODULES_COPY PREVIOUS_COMMON_MODULE)

  if(PREVIOUS_COMMON_MODULE)
    add_dependencies(${MODULE} ${PREVIOUS_COMMON_MODULE})
  endif()

  add_dependencies(Modules ${MODULE})

endforeach()

message(STATUS "Configuring Extra Modules...")
foreach(MODULE ${JASP_EXTRA_MODULES})

  make_directory(${MODULES_RENV_PATH}/${MODULE})
  configure_file(${CMAKE_CURRENT_LIST_DIR}/install-module.R.in
                 ${MODULES_RENV_ROOT_PATH}/install-${MODULE}.R)

  add_custom_target(
    ${MODULE}
    COMMAND ${_Rscript_EXE} ${MODULES_RENV_ROOT_PATH}/install-${MODULE}.R
    BYPRODUCTS ${MODULES_RENV_PATH}/${MODULE}/
    COMMENT "------ Installing ${MODULE}...")

  list(POP_FRONT JASP_EXTRA_MODULES_COPY PREVIOUS_EXTRA_MODULE)

  if(PREVIOUS_EXTRA_MODULE)
    add_dependencies(${MODULE} ${PREVIOUS_EXTRA_MODULE} ${FIRST_COMMON_MODULE})
  endif()

  add_dependencies(Modules ${MODULE})

  # We can add other specific dependencies here:
  if(${MODULE} STREQUAL "jaspMetaAnalysis")
    add_dependencies(${MODULE} jags-install)
  endif()

endforeach()

# Dependencies of Modules
#
# - If a module needs a dependency, we can download/configure/install it
#   in the Dependencies.cmake and make sure that their dependencies are
#   build before the installation of the module.
#
# add_dependencies(jaspMetaAnalysis jags-install)
