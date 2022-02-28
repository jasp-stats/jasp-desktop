# This file contains codes necessary for setting up JASP Modules, it prepares
#   - `jaspBase` if it is not installed
#   - For every Module,
#     - We create an installer file from the `install-module.R.in` template
#     - Runs them *one by one* at Build time, and if successful, the
#       `install-module.R` creates an empty file `<module>-installed-successfully.log`.
#       I can also use the `.mds` file but for now, I have it like this.
#         - `symlinktools.R` is being called right after successful installation as well
#     - After a successful installation, we run the `Patch.R` on each Modules
#       to patch and sign the installed libraries.
#
#
# Todos:
#   - [ ] If we end up using the install components, we need to make sure that
#   the `install` command for each Module is more granular, and only targets
#   the pieces of the `renv-cache` that belongs to the Module.
#   - [ ] Move the installation of jaspBase dependencies to R.cmake

list(APPEND CMAKE_MESSAGE_CONTEXT Modules)

# Ninja supports JOB_POOL therefore setting up the sequential building of
# modules is much nicer, and easier. Without Ninja, I have to make a chain
# of them, and make them depend on each other, and some of the biolerplate
# below is deal with that, because we cannot use Ninja all the time, and
# I'm not yet fully convinced that it can handle everything we need. I remember
# it having some issues with Resources, etc.
if(CMAKE_GENERATOR STREQUAL "Ninja")
  set_property(GLOBAL PROPERTY JOB_POOLS sequential=1)
endif()

set(JASP_COMMON_MODULES
    "jaspDescriptives"
    # "jaspAnova"
    # "jaspFactor"
    # "jaspFrequencies"
    # "jaspRegression"
    # "jaspTTests"
    # "jaspMixedModels"
)

# list(REVERSE JASP_COMMON_MODULES)
# set(JASP_COMMON_MODULES_COPY ${JASP_COMMON_MODULES})
# list(POP_FRONT JASP_COMMON_MODULES_COPY FIRST_COMMON_MODULE)

# Possible Bug:
#
# - The order is kind of important here, because `jaspProphet` and `jaspCircular`
# are linking against broken libraries. It is also partially caused by having
# the `.patched.log` but I'm not so sure.
set(JASP_EXTRA_MODULES
    # "jaspProphet"
    # "jaspCircular"
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
    # "jaspProcessControl"
)

# list(REVERSE JASP_EXTRA_MODULES)
# set(JASP_EXTRA_MODULES_COPY ${JASP_EXTRA_MODULES})
# list(POP_FRONT JASP_EXTRA_MODULES_COPY)

if("jaspMetaAnalysis" IN_LIST JASP_EXTRA_MODULES)
  if(LINUX)
    if(LINUX_LOCAL_BUILD)
      set(jags_HOME ${R_OPT_PATH}/jags)
    else()
      # Flatpak
      set(jags_HOME /usr)
    endif()
  else()
    # On macOS and Windows jags will live inside R.framework/ or R/
    set(jags_HOME ${R_OPT_PATH}/jags)
  endif()
else()
  set(jags_HOME "")
endif()
message(STATUS "If necessary, 'jags' will be installed at ${jags_HOME}")

if(NOT EXISTS ${MODULES_SOURCE_PATH})
  message(WARNING "Modules sources are not available. If you are planning
       to install them during the build, make sure that they are avialble in
       the jasp-desktop folder.")
endif()

# Setting JASP_ENGINE_PATH like this doesn't work with APP_BUNDLE

set(INSTALL_MODULE_TEMPLATE_FILE
    "${PROJECT_SOURCE_DIR}/Modules/install-module.R.in"
    CACHE FILEPATH "Location of the install-module.R.in")

make_directory(${MODULES_BINARY_PATH})
make_directory(${MODULES_RENV_ROOT_PATH})
make_directory(${MODULES_RENV_CACHE_PATH})

cmake_print_variables(MODULES_BINARY_PATH)
cmake_print_variables(MODULES_RENV_ROOT_PATH)
cmake_print_variables(MODULES_RENV_CACHE_PATH)

file(COPY ${CMAKE_SOURCE_DIR}/R-Interface/jaspResults/R/writeImage.R
     DESTINATION ${MODULES_BINARY_PATH})
file(COPY ${CMAKE_SOURCE_DIR}/R-Interface/jaspResults/R/zzzWrappers.R
     DESTINATION ${MODULES_BINARY_PATH})
file(COPY ${CMAKE_SOURCE_DIR}/R-Interface/R/workarounds.R
     DESTINATION ${MODULES_BINARY_PATH})
file(COPY ${CMAKE_SOURCE_DIR}/R-Interface/R/symlinkTools.R
     DESTINATION ${MODULES_BINARY_PATH})

if(LINUX)
  set(R_PKG_TYPE "source")
else()
  set(R_PKG_TYPE "binary")
endif()

if(INSTALL_R_MODULES)

  # Cleaning the renv-path on Windows only, for now.
  # It takes somes times on macOS, but if we can build and
  # cache it, let's do it.
  set_property(
    DIRECTORY
    APPEND
    PROPERTY ADDITIONAL_CLEAN_FILES
             $<$<PLATFORM_ID:Windows>:${MODULES_BINARY_PATH}>)

  add_custom_target(Modules)

  add_dependencies(Modules ${JASP_COMMON_MODULES} ${JASP_EXTRA_MODULES})

  message(STATUS "Installing Required R Modules...")

  # This happens during the configuration!
  file(
    WRITE ${MODULES_RENV_ROOT_PATH}/install-jaspBase.R
    "
    install.packages(c('ggplot2', 'gridExtra', 'gridGraphics',
                        'jsonlite', 'modules', 'officer', 'pkgbuild',
                        'plyr', 'qgraph', 'ragg', 'R6', 'renv',
                        'rjson', 'rvg', 'svglite', 'systemfonts',
                        'withr', 'testthat',
                        'data.table', 'httr', 'lifecycle',
                        'pkgload', 'remotes', 'stringi', 'stringr',
                        'vdiffr'), type='${R_PKG_TYPE}', repos='${R_REPOSITORY}' ${USE_LOCAL_R_LIBS_PATH})
    install.packages('${PROJECT_SOURCE_DIR}/Engine/jaspBase/', type='source', repos=NULL ${USE_LOCAL_R_LIBS_PATH})
    if ('jaspBase' %in% installed.packages()) {
      cat(NULL, file='${MODULES_BINARY_PATH}/jaspBase-installed-successfully.log')
    }
    ")

  file(
    WRITE ${MODULES_RENV_ROOT_PATH}/install-jaspGraphs.R
    "
    install.packages('${PROJECT_SOURCE_DIR}/Engine/jaspGraphs/', type='source', repos=NULL ${USE_LOCAL_R_LIBS_PATH})
    if ('jaspGraphs' %in% installed.packages()) {
      cat(NULL, file='${MODULES_BINARY_PATH}/jaspGraphs-installed-successfully.log')
    }
    ")

  file(
    WRITE ${MODULES_RENV_ROOT_PATH}/install-jaspTools.R
    "
    install.packages('${PROJECT_SOURCE_DIR}/Tools/jaspTools/', type='source', repos=NULL ${USE_LOCAL_R_LIBS_PATH})
    if ('jaspTools' %in% installed.packages()) {
      cat(NULL, file='${MODULES_BINARY_PATH}/jaspTools-installed-successfully.log')
    }
    ")

  # I'm using a custom_command here to make sure that jaspBase is installed once
  # and only once before everything else. So, `install-jaspBase.R` creates an empty
  # file, i.e., `jaspBase-installed-successfully.log` and all other Modules look for
  # it. If they find it, they proceed, if not, they trigger this custom command.
  # TODO:
  #   - [ ] The following commands can be turned into a function or a macro, but
  #         for now, I would like to keep a granular control over differnt steps
  add_custom_command(
    WORKING_DIRECTORY ${R_HOME_PATH}
    OUTPUT ${MODULES_BINARY_PATH}/jaspBase-installed-successfully.log
    # BYPRODUCTS ${MODULES_BINARY_PATH}/jaspBase-installed-successfully.log
    JOB_POOL sequential
    COMMAND ${R_EXECUTABLE} --slave --no-restore --no-save
            --file=${MODULES_RENV_ROOT_PATH}/install-jaspBase.R
    COMMAND
      ${CMAKE_COMMAND} -D
      NAME_TOOL_PREFIX_PATCHER=${PROJECT_SOURCE_DIR}/Tools/macOS/install_name_prefix_tool.sh
      -D PATH=${R_HOME_PATH}/library -D R_HOME_PATH=${R_HOME_PATH} -D
      R_DIR_NAME=${R_DIR_NAME} -D SIGNING=${IS_SIGNING} -D
      CODESIGN_TIMESTAMP_FLAG=${CODESIGN_TIMESTAMP_FLAG} -P
      ${PROJECT_SOURCE_DIR}/Tools/CMake/Patch.cmake
    COMMENT "------ Installing 'jaspBase'")

  add_custom_command(
    WORKING_DIRECTORY ${R_HOME_PATH}
    DEPENDS ${MODULES_BINARY_PATH}/jaspBase-installed-successfully.log
    OUTPUT ${MODULES_BINARY_PATH}/jaspGraphs-installed-successfully.log
    # BYPRODUCTS ${MODULES_BINARY_PATH}/jaspGraphs-installed-successfully.log
    JOB_POOL sequential
    COMMAND ${R_EXECUTABLE} --slave --no-restore --no-save
            --file=${MODULES_RENV_ROOT_PATH}/install-jaspGraphs.R
    COMMAND
      ${CMAKE_COMMAND} -D
      NAME_TOOL_PREFIX_PATCHER=${PROJECT_SOURCE_DIR}/Tools/macOS/install_name_prefix_tool.sh
      -D PATH=${R_HOME_PATH}/library -D R_HOME_PATH=${R_HOME_PATH} -D
      R_DIR_NAME=${R_DIR_NAME} -D SIGNING=${IS_SIGNING} -D
      CODESIGN_TIMESTAMP_FLAG=${CODESIGN_TIMESTAMP_FLAG} -P
      ${PROJECT_SOURCE_DIR}/Tools/CMake/Patch.cmake
    COMMENT "------ Installing 'jaspGraphs'")

  add_custom_command(
    WORKING_DIRECTORY ${R_HOME_PATH}
    DEPENDS ${MODULES_BINARY_PATH}/jaspBase-installed-successfully.log
            ${MODULES_BINARY_PATH}/jaspGraphs-installed-successfully.log
    OUTPUT ${MODULES_BINARY_PATH}/jaspTools-installed-successfully.log
    # BYPRODUCTS ${MODULES_BINARY_PATH}/jaspTools-installed-successfully.log
    JOB_POOL sequential
    COMMAND ${R_EXECUTABLE} --slave --no-restore --no-save
            --file=${MODULES_RENV_ROOT_PATH}/install-jaspTools.R
    COMMAND
      ${CMAKE_COMMAND} -D
      NAME_TOOL_PREFIX_PATCHER=${PROJECT_SOURCE_DIR}/Tools/macOS/install_name_prefix_tool.sh
      -D PATH=${R_HOME_PATH}/library -D R_HOME_PATH=${R_HOME_PATH} -D
      R_DIR_NAME=${R_DIR_NAME} -D SIGNING=${IS_SIGNING} -D
      CODESIGN_TIMESTAMP_FLAG=${CODESIGN_TIMESTAMP_FLAG} -P
      ${PROJECT_SOURCE_DIR}/Tools/CMake/Patch.cmake
    COMMENT "------ Installing 'jaspTools'")

  message(STATUS "Configuring Common Modules...")
  foreach(MODULE ${JASP_COMMON_MODULES})

    # We can technically create a new install-module.R for each Renv
    # even better, we can have different templates for each module, and use those
    # to set them up correctly
    make_directory(${MODULES_BINARY_PATH}/${MODULE})
    configure_file(${INSTALL_MODULE_TEMPLATE_FILE}
                   ${MODULES_RENV_ROOT_PATH}/install-${MODULE}.R @ONLY)

    add_custom_target(
      ${MODULE}
      JOB_POOL sequential
      WORKING_DIRECTORY ${R_HOME_PATH}
      DEPENDS ${MODULES_BINARY_PATH}/jaspBase-installed-successfully.log
              ${MODULES_BINARY_PATH}/jaspGraphs-installed-successfully.log
              ${MODULES_BINARY_PATH}/jaspTools-installed-successfully.log
      COMMAND ${R_EXECUTABLE} --slave --no-restore --no-save
              --file=${MODULES_RENV_ROOT_PATH}/install-${MODULE}.R
      COMMAND
        ${CMAKE_COMMAND} -D
        NAME_TOOL_PREFIX_PATCHER=${PROJECT_SOURCE_DIR}/Tools/macOS/install_name_prefix_tool.sh
        -D PATH=${MODULES_BINARY_PATH}/${MODULE} -D R_HOME_PATH=${R_HOME_PATH}
        -D R_DIR_NAME=${R_DIR_NAME} -D
        MODULES_BINARY_PATH=${MODULES_BINARY_PATH} -D MODULE=${MODULE} -D
        SIGNING=${IS_SIGNING} -P ${PROJECT_SOURCE_DIR}/Tools/CMake/Patch.cmake
      COMMAND
        ${CMAKE_COMMAND} -D PATH=${MODULES_BINARY_PATH}/${MODULE} -D
        MODULES_BINARY_PATH=${MODULES_BINARY_PATH} -P
        ${PROJECT_SOURCE_DIR}/Tools/CMake/Symlink.cmake
      BYPRODUCTS ${MODULES_BINARY_PATH}/${MODULE}
                 ${MODULES_BINARY_PATH}/${MODULE}_md5sums.rds
                 ${MODULES_BINARY_PATH}/${MODULE}-installed-successfully.log
                 ${MODULES_RENV_ROOT_PATH}/install-${MODULE}.R
      COMMENT "------ Installing '${MODULE}'")

    # install(
    #   DIRECTORY ${MODULES_BINARY_PATH}/${MODULE}
    #   DESTINATION ${CMAKE_INSTALL_PREFIX}/Modules/
    #   COMPONENT ${MODULE})

    # To fix the Rpath stuff
    if(APPLE)
      add_dependencies(${MODULE} JASPEngine)
    endif()

    add_dependencies(Modules ${MODULE})

  endforeach()

  message(STATUS "Configuring Extra Modules...")
  foreach(MODULE ${JASP_EXTRA_MODULES})

    make_directory(${MODULES_BINARY_PATH}/${MODULE})
    configure_file(${INSTALL_MODULE_TEMPLATE_FILE}
                   ${MODULES_RENV_ROOT_PATH}/install-${MODULE}.R @ONLY)

    add_custom_target(
      ${MODULE}
      JOB_POOL sequential
      WORKING_DIRECTORY ${R_HOME_PATH}
      DEPENDS
        ${MODULES_BINARY_PATH}/jaspBase-installed-successfully.log
        ${MODULES_BINARY_PATH}/jaspGraphs-installed-successfully.log
        ${MODULES_BINARY_PATH}/jaspTools-installed-successfully.log
        $<$<STREQUAL:"${MODULE}","jaspMetaAnalysis">:${jags_HOME}/lib/pkgconfig/jags.pc>
      COMMAND ${R_EXECUTABLE} --slave --no-restore --no-save
              --file=${MODULES_RENV_ROOT_PATH}/install-${MODULE}.R
      COMMAND
        ${CMAKE_COMMAND} -D
        NAME_TOOL_PREFIX_PATCHER=${PROJECT_SOURCE_DIR}/Tools/macOS/install_name_prefix_tool.sh
        -D PATH=${MODULES_BINARY_PATH}/${MODULE} -D R_HOME_PATH=${R_HOME_PATH}
        -D R_DIR_NAME=${R_DIR_NAME} -D
        MODULES_BINARY_PATH=${MODULES_BINARY_PATH} -D MODULE=${MODULE} -D
        SIGNING=${IS_SIGNING} -P ${PROJECT_SOURCE_DIR}/Tools/CMake/Patch.cmake
      COMMAND
        ${CMAKE_COMMAND} -D PATH=${MODULES_BINARY_PATH}/${MODULE} -D
        MODULES_BINARY_PATH=${MODULES_BINARY_PATH} -P
        ${PROJECT_SOURCE_DIR}/Tools/CMake/Symlink.cmake
      BYPRODUCTS ${MODULES_BINARY_PATH}/${MODULE}
                 ${MODULES_BINARY_PATH}/${MODULE}_md5sums.rds
                 ${MODULES_BINARY_PATH}/${MODULE}-installed-successfully.log
                 ${MODULES_RENV_ROOT_PATH}/install-${MODULE}.R
      COMMENT "------ Installing '${MODULE}'")

    # install(
    #   DIRECTORY ${MODULES_BINARY_PATH}/${MODULE}
    #   DESTINATION ${CMAKE_INSTALL_PREFIX}/Modules/
    #   COMPONENT ${MODULE})

    # To fix the Rpath stuff
    if(APPLE)
      add_dependencies(${MODULE} JASPEngine)
    endif()

    # Making sure that CMake doesn't parallelize the installation of the modules

    add_dependencies(Modules ${MODULE})

    # We can add other specific dependencies here:
    if(${MODULE} STREQUAL "jaspMetaAnalysis")
      # ----- jags -----
      #
      # - JAGS needs GNU Bison v3, https://www.gnu.org/software/bison.
      # - With this, we can build JAGS, and link it, or even place it inside the the `R.framework`
      #   - `--prefix=${R_OPT_PATH}/jags`, with this, we inherit the R
      # - You can run `make jags-build` or `make jags-install` to just play with JAGS target
      #

      if(NOT EXISTS ${jags_HOME})
        make_directory(${jags_HOME})
      endif()

      if(WIN32)

        # Manually copying the entire JAGS from MSYS2 into the R/opt/jags
        # if later, we need any other libraries, we can use this method,
        # automate it a bit nicer, and ship our external libraries
        add_custom_command(
          OUTPUT ${jags_HOME}/lib/pkgconfig/jags.pc
          # bin
          COMMAND ${CMAKE_COMMAND} -E make_directory ${jags_HOME}/bin
          COMMAND ${CMAKE_COMMAND} -E copy ${MINGW_LIBJAGS_BAT}
                  ${jags_HOME}/bin/
          COMMAND ${CMAKE_COMMAND} -E copy ${MINGW_LIBJAGS} ${jags_HOME}/bin/
          COMMAND ${CMAKE_COMMAND} -E copy ${MINGW_LIBJAGS_JRMATH}
                  ${jags_HOME}/bin/
          # libexe
          COMMAND ${CMAKE_COMMAND} -E make_directory ${jags_HOME}/libexe
          COMMAND ${CMAKE_COMMAND} -E copy ${MINGW_LIBJAGS_JAGS_TERMINAL_EXE}
                  ${jags_HOME}/libexe/
          # headers
          COMMAND ${CMAKE_COMMAND} -E make_directory ${jags_HOME}/include/JAGS
          COMMAND ${CMAKE_COMMAND} -E copy_directory
                  ${MINGW_LIBJAGS_HEADERS_PATH}/ ${jags_HOME}/include/JAGS
          # libs
          COMMAND ${CMAKE_COMMAND} -E make_directory ${jags_HOME}/lib
          COMMAND ${CMAKE_COMMAND} -E copy_directory
                  ${MINGW_LIBJAGS_LIBRARIES_PATH}/ ${jags_HOME}/lib/JAGS
          COMMAND ${CMAKE_COMMAND} -E copy_directory
                  ${MINGW_LIBJAGS_PKGCONFIG_PATH}/ ${jags_HOME}/lib/pkgconfig
          COMMAND ${CMAKE_COMMAND} -E copy ${MINGW_LIBJAGS_LIBJAGS_A}
                  ${jags_HOME}/lib
          COMMAND ${CMAKE_COMMAND} -E copy ${MINGW_LIBJAGS_LIBJAGS_LA}
                  ${jags_HOME}/lib
          COMMAND ${CMAKE_COMMAND} -E copy ${MINGW_LIBJAGS_LIBJRMATH_A}
                  ${jags_HOME}/lib
          COMMAND ${CMAKE_COMMAND} -E copy ${MINGW_LIBJAGS_LIBJRMATH_LA}
                  ${jags_HOME}/lib)

      else()

        # ----- Downloading and Building jags

        fetchcontent_declare(
          jags
          HG_REPOSITORY "http://hg.code.sf.net/p/mcmc-jags/code-0"
          HG_TAG "release-4_3_0")

        message(CHECK_START "Downloading 'jags'")

        fetchcontent_makeavailable(jags)

        if(jags_POPULATED)

          message(CHECK_PASS "successful.")

          add_custom_command(
            WORKING_DIRECTORY ${jags_SOURCE_DIR}
            OUTPUT ${jags_HOME}/lib/pkgconfig/jags.pc
            COMMAND ${ACLOCAL}
            COMMAND ${AUTORECONF} -fi
            COMMAND ./configure --disable-dependency-tracking
                    --prefix=${jags_HOME}
            COMMAND ${MAKE}
            COMMAND ${MAKE} install
            COMMAND
              ${CMAKE_COMMAND} -D
              NAME_TOOL_PREFIX_PATCHER=${PROJECT_SOURCE_DIR}/Tools/macOS/install_name_prefix_tool.sh
              -D PATH=${jags_HOME} -D R_HOME_PATH=${R_HOME_PATH} -D
              R_DIR_NAME=${R_DIR_NAME} -D SIGNING=${IS_SIGNING} -D
              CODESIGN_TIMESTAMP_FLAG=${CODESIGN_TIMESTAMP_FLAG} -P
              ${PROJECT_SOURCE_DIR}/Tools/CMake/Patch.cmake
            COMMENT "----- Preparing 'jags'")

        else()

          message(CHECK_FAIL "failed.")

        endif()

      endif()

      set(jags_INCLUDE_DIRS ${jags_HOME}/include)
      set(jags_LIBRARY_DIRS ${jags_HOME}/lib)
      set(jags_PKG_CONFIG_PATH ${jags_HOME}/lib/pkgconfig/)

      # install-jaspMetaAnalysis.R needs to be reconfigured again
      configure_file(${INSTALL_MODULE_TEMPLATE_FILE}
                     ${MODULES_RENV_ROOT_PATH}/install-${MODULE}.R @ONLY)

    endif()

  endforeach()

endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
