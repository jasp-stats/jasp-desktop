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

list(REVERSE JASP_EXTRA_MODULES)
set(JASP_EXTRA_MODULES_COPY ${JASP_EXTRA_MODULES})
list(POP_FRONT JASP_EXTRA_MODULES_COPY)

# TODO: Standardize these names, either use PATH, ROOT, or DIR, but everywhere

set(MODULES_SOURCE_PATH
    ${PROJECT_SOURCE_DIR}/Modules
    CACHE PATH "Location of JASP Modules")

if(NOT EXISTS ${MODULES_SOURCE_PATH})
  message(WARNING "Modules sources are not available. If you are planning
       to install them during the build, make sure that they are avialble in
       the jasp-desktop folder.")
endif()

set(MODULES_BINARY_PATH
    "${CMAKE_BINARY_DIR}/Modules"
    CACHE PATH "Location of the renv libraries")
set(MODULES_RENV_ROOT_PATH
    "${MODULES_BINARY_PATH}/renv-root"
    CACHE PATH "Location of renv root directories")
set(MODULES_RENV_CACHE_PATH
    "${MODULES_BINARY_PATH}/renv-cache"
    CACHE PATH "Location of renv cache directories")
set(JASP_ENGINE_PATH
    "${CMAKE_BINARY_DIR}/Desktop/"
    CACHE PATH "Location of the JASPEngine")

set(INSTALL_MODULE_TEMPLATE_FILE
    "${PROJECT_SOURCE_DIR}/Modules/install-module.R.in"
    CACHE FILEPATH "Location of the install-module.R.in")

make_directory(${MODULES_BINARY_PATH})
make_directory(${MODULES_RENV_ROOT_PATH})
make_directory(${MODULES_RENV_CACHE_PATH})

cmake_print_variables(MODULES_BINARY_PATH)
cmake_print_variables(MODULES_RENV_ROOT_PATH)
cmake_print_variables(MODULES_RENV_CACHE_PATH)

# Moving the .R files into the Modules/ folder
# TODO:
# - [ ] They still need to be installed as well.
#   - They are most likely being installed during the Module transfer anyway
file(COPY ${CMAKE_SOURCE_DIR}/R-Interface/jaspResults/R/writeImage.R
     DESTINATION ${MODULES_BINARY_PATH})
file(COPY ${CMAKE_SOURCE_DIR}/R-Interface/jaspResults/R/zzzWrappers.R
     DESTINATION ${MODULES_BINARY_PATH})
file(COPY ${CMAKE_SOURCE_DIR}/R-Interface/R/workarounds.R
     DESTINATION ${MODULES_BINARY_PATH})
file(COPY ${CMAKE_SOURCE_DIR}/R-Interface/R/symlinkTools.R
     DESTINATION ${MODULES_BINARY_PATH})

if(INSTALL_R_MODULES)

  # Cleaning the renv-path on Windows only, for now.
  # It takes somes times on macOS, but if we can build and
  # cache it, let's do it.
  set_property(
    DIRECTORY
    APPEND
    PROPERTY ADDITIONAL_CLEAN_FILES
             $<PLATFORM_ID:Windows,${MODULES_BINARY_PATH}>)

  add_custom_target(Modules)

  add_dependencies(Modules ${JASP_COMMON_MODULES} ${JASP_EXTRA_MODULES})

  message(STATUS "Installing Required R Modules...")

  # This happens during the configuration!
  message(CHECK_START "Installing the 'jaspBase' and its dependencies...")
  file(
    WRITE ${CMAKE_BINARY_DIR}/Modules/renv-root/install-jaspBase.R
    "
    install.packages(c('ggplot2', 'gridExtra', 'gridGraphics',
                        'jsonlite', 'modules', 'officer', 'pkgbuild',
                        'plyr', 'qgraph', 'ragg', 'R6', 'renv',
                        'rjson', 'rvg', 'svglite', 'systemfonts', 'withr'), type='binary', repos='${R_REPOSITORY}')
    install.packages('${PROJECT_SOURCE_DIR}/Engine/jaspBase/', type='source', repos=NULL)
    ")
  execute_process(
    WORKING_DIRECTORY ${_R_HOME}
    COMMAND ./R --slave --no-restore --no-save
            --file=${CMAKE_BINARY_DIR}/Modules/renv-root/install-jaspBase.R
    OUTPUT_QUIET
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
    make_directory(${MODULES_BINARY_PATH}/${MODULE})
    configure_file(${INSTALL_MODULE_TEMPLATE_FILE}
                   ${MODULES_RENV_ROOT_PATH}/install-${MODULE}.R)

    add_custom_target(
      ${MODULE}
      WORKING_DIRECTORY ${_R_HOME}
      COMMAND ./R --slave --no-restore --no-save
              --file=${MODULES_RENV_ROOT_PATH}/install-${MODULE}.R
      BYPRODUCTS ${MODULES_BINARY_PATH}/${MODULE}
                 ${MODULES_BINARY_PATH}/${MODULE}_md5sums.rds
                 ${MODULES_RENV_ROOT_PATH}/install-${MODULE}.R
      COMMENT "------ Installing ${MODULE}...")

    list(POP_FRONT JASP_COMMON_MODULES_COPY PREVIOUS_COMMON_MODULE)

    if(PREVIOUS_COMMON_MODULE)
      add_dependencies(${MODULE} ${PREVIOUS_COMMON_MODULE})
    endif()

    add_dependencies(Modules ${MODULE})

  endforeach()

  message(STATUS "Configuring Extra Modules...")
  foreach(MODULE ${JASP_EXTRA_MODULES})

    make_directory(${MODULES_BINARY_PATH}/${MODULE})
    configure_file(${INSTALL_MODULE_TEMPLATE_FILE}
                   ${MODULES_RENV_ROOT_PATH}/install-${MODULE}.R)

    add_custom_target(
      ${MODULE}
      WORKING_DIRECTORY ${_R_HOME}
      COMMAND ./R --slave --no-restore --no-save
              --file=${MODULES_RENV_ROOT_PATH}/install-${MODULE}.R
      BYPRODUCTS ${MODULES_BINARY_PATH}/${MODULE}
                 ${MODULES_BINARY_PATH}/${MODULE}_md5sums.rds
                 ${MODULES_RENV_ROOT_PATH}/install-${MODULE}.R
      COMMENT "------ Installing ${MODULE}...")

    list(POP_FRONT JASP_EXTRA_MODULES_COPY PREVIOUS_EXTRA_MODULE)

    if(PREVIOUS_EXTRA_MODULE)
      add_dependencies(${MODULE} ${PREVIOUS_EXTRA_MODULE}
                       ${FIRST_COMMON_MODULE})
    endif()

    add_dependencies(Modules ${MODULE})

    # We can add other specific dependencies here:
    if((${MODULE} STREQUAL "jaspMetaAnalysis")
       AND (INSTALL_JASP_REQUIRED_LIBRARIES))
      add_dependencies(${MODULE} jags-install)
    endif()

  endforeach()

endif()
