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

set(JASP_TEST_BUILD		OFF					CACHE BOOL		"Do a quick build with just descriptives and testmodule")
set(JASP_TEST_MODULE	"jaspDescriptives"	CACHE STRING	"Which module other than jaspTestModule would you like to test?")

if(NOT JASP_TEST_BUILD)
set(JASP_COMMON_MODULES
        "jaspDistributions"

)

set(JASP_EXTRA_MODULES
    )
else() #it IS a test build
	message(STATUS "JASP_TEST_BUILD is enabled, building with minimal modules")
	set(JASP_COMMON_MODULES
		${JASP_TEST_MODULE}
		"jaspTestModule"
	)
endif()

list(
  JOIN
  JASP_COMMON_MODULES
  "\",\n\t\t\t\""
  JASP_COMMON_MODULES_QUOTED)

list(
  JOIN
  JASP_EXTRA_MODULES
  "\",\n\t\t\t\""
  JASP_EXTRA_MODULES_QUOTED)

configure_file(${CMAKE_SOURCE_DIR}/Desktop/modules/activemodules.h.in
               ${CMAKE_SOURCE_DIR}/Desktop/modules/activemodules.h @ONLY)
message(STATUS "activemodules.h is successfully generated...")

if(("jaspMetaAnalysis" IN_LIST JASP_EXTRA_MODULES) OR ("jaspJags" IN_LIST JASP_EXTRA_MODULES))
  if(LINUX)

    if(LINUX_LOCAL_BUILD)
      set(jags_HOME /usr/local)
    endif()

    if(FLATPAK_USED)
      set(jags_HOME /app)
    endif()

    message(CHECK_START "Looking for libjags.so")

    find_file(LIBJAGS libjags.so HINTS ${jags_HOME}/lib)
    if(EXISTS ${LIBJAGS})
      message(CHECK_PASS "found")
      message(STATUS "  ${LIBJAGS}")
    else()
      message(CHECK_FAIL "not found")
      message(
        WARNING
          "JAGS is required for building on Linux but wasnt found, perhaps JASP builds, otherwise follow the build instruction before you continue."
      )
    endif()

  else()
    # On macOS and Windows jags will live inside R.framework/ or R/
    set(jags_HOME ${R_OPT_PATH}/jags)
    if(WIN32)
      set(jags_VERSION_H_PATH ${jags_HOME}/include/version.h)
    else()
      set(jags_VERSION_H_PATH ${jags_HOME}/include/JAGS/version.h)
    endif()
  endif()
else()
  set(jags_HOME "")
endif()
message(STATUS "If necessary, 'jags' will be installed at ${jags_HOME}")

if(NOT EXISTS ${MODULES_SOURCE_PATH})
  message(WARNING "Modules sources are not available. If you are planning
	   to install them during the build, make sure that they are available in
       the jasp-desktop folder.")
endif()

# Setting JASP_ENGINE_PATH like this doesn't work with APP_BUNDLE

set(INSTALL_MODULE_TEMPLATE_FILE
    "${PROJECT_SOURCE_DIR}/Modules/install-module.R.in"
    CACHE FILEPATH "Location of the install-module.R.in")

if(APPLE AND (NOT EXISTS ${R_HOME_PATH}/bin/Modules))
  # This is added because packages installed by Renv needs to be at
  # @executable_path/../ relative to the R binary, which is in `/bin/exec/R`
  # So, by adding this, both R binary and JASP can find their libraries
  message(STATUS "${R_HOME_PATH}/bin")
  execute_process(
    # COMMAND_ECHO STDOUT
    ERROR_QUIET OUTPUT_QUIET
    WORKING_DIRECTORY ${R_HOME_PATH}/bin
    COMMAND ln -sf ../../../../../../Modules Modules)
endif()

message(STATUS "Installing Required R Modules...")

execute_process(
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/R-Interface
  COMMAND ${CMAKE_COMMAND} -E copy_if_different R/workarounds.R
          ${MODULES_BINARY_PATH}/Tools/
  COMMAND ${CMAKE_COMMAND} -E copy_if_different R/symlinkTools.R
          ${MODULES_BINARY_PATH}/Tools/)

add_custom_target(
  jaspModuleInstaller
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/R-Interface
  DEPENDS ${JASPMODULEINSTALLER_LIBRARY}/jaspModuleInstaller)

configure_file("${PROJECT_SOURCE_DIR}/Modules/install-jaspModuleInstaller.R.in"
                   ${SCRIPT_DIRECTORY}/install-jaspModuleInstaller.R @ONLY)

add_dependencies(jaspModuleInstaller JASPEngine)

# always run a cmake command, based on https://stackoverflow.com/a/31518137
# we want the R code to install jaspModuleInstaller to always run, because it generates
# a status object that indicates which modules need reinstallation
if(EXISTS ${JASPMODULEINSTALLER_LIBRARY}/fakeFile.R)
    message(FATAL_ERROR "File \"${JASPMODULEINSTALLER_LIBRARY}/fakeFile.R\" found, \
        this file should never be created, please remove it manually!")
endif()

if(APPLE)
	add_custom_command(
	  WORKING_DIRECTORY ${R_HOME_PATH}
          OUTPUT
              ${JASPMODULEINSTALLER_LIBRARY}/fakeFile.R          # never exists, thus ensures this is run
              ${JASPMODULEINSTALLER_LIBRARY}/jaspModuleInstaller # created by this command
          USES_TERMINAL
          COMMAND ${CMAKE_COMMAND}  -E env "JASP_R_HOME=${R_HOME_PATH}" ${R_EXECUTABLE} --no-echo --vanilla --file=${SCRIPT_DIRECTORY}/install-jaspModuleInstaller.R
          COMMENT "------ Installing 'jaspModuleInstaller'")
else()

        add_custom_command(
          WORKING_DIRECTORY ${R_HOME_PATH}
          OUTPUT
            ${JASPMODULEINSTALLER_LIBRARY}/fakeFile.R          # never exists, thus ensures this is run
            ${JASPMODULEINSTALLER_LIBRARY}/jaspModuleInstaller # created by this command
          USES_TERMINAL
          COMMAND ${R_EXECUTABLE} --no-echo --vanilla --file=${SCRIPT_DIRECTORY}/install-jaspModuleInstaller.R
          COMMENT "------ Installing 'jaspModuleInstaller'")

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

  message(STATUS "Configuring Common Modules...")
  foreach(MODULE ${JASP_COMMON_MODULES})

    # We can technically create a new install-module.R for each Renv
    # even better, we can have different templates for each module, and use those
    # to set them up correctly
    make_directory(${MODULES_BINARY_PATH}/${MODULE})
    configure_file(${INSTALL_MODULE_TEMPLATE_FILE}
		           ${SCRIPT_DIRECTORY}/install-${MODULE}.R @ONLY)
			   
if(APPLE)			   
    add_custom_target(
      ${MODULE}
      USES_TERMINAL
      WORKING_DIRECTORY ${R_HOME_PATH}
          DEPENDS  ${JASPMODULEINSTALLER_LIBRARY}/jaspModuleInstaller
	  COMMAND  ${CMAKE_COMMAND}  -E env "JASP_R_HOME=${R_HOME_PATH}" ${R_EXECUTABLE} --slave --no-restore --no-save --file=${SCRIPT_DIRECTORY}/install-${MODULE}.R
      # COMMAND
      #   ${CMAKE_COMMAND} -D PATH=${MODULES_BINARY_PATH}/${MODULE} -D
      #   MODULES_BINARY_PATH=${MODULES_BINARY_PATH} -P
      #   ${PROJECT_SOURCE_DIR}/Tools/CMake/Symlink.cmake
      BYPRODUCTS ${MODULES_BINARY_PATH}/${MODULE}
                 ${MODULES_BINARY_PATH}/${MODULE}_md5sums.rds
                 ${MODULES_BINARY_PATH}/${MODULE}-installed-successfully.log
      COMMENT "------ Installing '${MODULE}'")

    add_dependencies(${MODULE} JASPEngine)

else()
	add_custom_target(
      ${MODULE}
      USES_TERMINAL
      WORKING_DIRECTORY ${R_HOME_PATH}
      DEPENDS ${JASPMODULEINSTALLER_LIBRARY}/jaspModuleInstaller
      COMMAND ${R_EXECUTABLE} --slave --no-restore --no-save
	          --file=${SCRIPT_DIRECTORY}/install-${MODULE}.R
      BYPRODUCTS ${MODULES_BINARY_PATH}/${MODULE}
                 ${MODULES_BINARY_PATH}/${MODULE}_md5sums.rds
                 ${MODULES_BINARY_PATH}/${MODULE}-installed-successfully.log
      COMMENT "------ Installing '${MODULE}'")
endif()
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
		           ${SCRIPT_DIRECTORY}/install-${MODULE}.R @ONLY)

if(APPLE)			   
    add_custom_target(
      ${MODULE}
      USES_TERMINAL
      WORKING_DIRECTORY ${R_HOME_PATH}
      DEPENDS JASPEngine ${JASPMODULEINSTALLER_LIBRARY}/jaspModuleInstaller
        $<$<STREQUAL:"${MODULE}","jaspMetaAnalysis">:${jags_VERSION_H_PATH}>
        $<$<STREQUAL:"${MODULE}","jaspJags">:${jags_VERSION_H_PATH}>
		COMMAND ${CMAKE_COMMAND}  -E env "JASP_R_HOME=${R_HOME_PATH}" ${R_EXECUTABLE} --slave --no-restore --no-save --file=${SCRIPT_DIRECTORY}/install-${MODULE}.R
      # COMMAND
      #   ${CMAKE_COMMAND} -D PATH=${MODULES_BINARY_PATH}/${MODULE} -D
      #   MODULES_BINARY_PATH=${MODULES_BINARY_PATH} -P
      #   ${PROJECT_SOURCE_DIR}/Tools/CMake/Symlink.cmake
      BYPRODUCTS ${MODULES_BINARY_PATH}/${MODULE}
                 ${MODULES_BINARY_PATH}/${MODULE}_md5sums.rds
                 ${MODULES_BINARY_PATH}/${MODULE}-installed-successfully.log
				 ${SCRIPT_DIRECTORY}/install-${MODULE}.R
      COMMENT "------ Installing '${MODULE}'")
else()
	add_custom_target(
      ${MODULE}
      USES_TERMINAL
      WORKING_DIRECTORY ${R_HOME_PATH}
      DEPENDS ${JASPMODULEINSTALLER_LIBRARY}/jaspModuleInstaller $<$<STREQUAL:"${MODULE}","jaspMetaAnalysis">:${jags_VERSION_H_PATH}> $<$<STREQUAL:"${MODULE}","jaspJags">:${jags_VERSION_H_PATH}>
      COMMAND ${R_EXECUTABLE} --slave --no-restore --no-save
	          --file=${SCRIPT_DIRECTORY}/install-${MODULE}.R

      BYPRODUCTS ${MODULES_BINARY_PATH}/${MODULE}
                 ${MODULES_BINARY_PATH}/${MODULE}_md5sums.rds
                 ${MODULES_BINARY_PATH}/${MODULE}-installed-successfully.log
				 ${SCRIPT_DIRECTORY}/install-${MODULE}.R
      COMMENT "------ Installing '${MODULE}'")
endif()
    # install(
    #   DIRECTORY ${MODULES_BINARY_PATH}/${MODULE}
    #   DESTINATION ${CMAKE_INSTALL_PREFIX}/Modules/
    #   COMPONENT ${MODULE})

    # Making sure that CMake doesn't parallelize the installation of the modules

    add_dependencies(Modules ${MODULE})

    # We can add other specific dependencies here:
	if((${MODULE} STREQUAL "jaspMetaAnalysis") OR (${MODULE} STREQUAL "jaspJags"))
      # ----- jags -----
      #
      # - JAGS needs GNU Bison v3, https://www.gnu.org/software/bison.
      # - With this, we can build JAGS, and link it, or even place it inside the the `R.framework`
      #   - `--prefix=${R_OPT_PATH}/jags`, with this, we inherit the R
      # - You can run `make jags-build` or `make jags-install` to just play with JAGS target
      #

      if((NOT EXISTS ${jags_HOME}) AND (NOT LINUX))
        message(STATUS "Creating ${jags_HOME}")
        make_directory("${jags_HOME}")
      endif()

      if(WIN32)

        if(NOT TARGET jags)

          message(STATUS "Downloading `jags`")
          fetchcontent_declare(
            jags_win
            URL "https://static.jasp-stats.org/development/JAGS-4.3.1-Windows.zip"
            URL_HASH
              SHA256=4b168ddcc29a22c02e5c8dd61e3240ec8f940fee239b1563f63fc5b0bea60796
          )

          fetchcontent_makeavailable(jags_win)

          if(jags_win_POPULATED)

            message(CHECK_PASS "successful")

            add_custom_command(
              OUTPUT ${jags_VERSION_H_PATH}
              # bin
              COMMAND ${CMAKE_COMMAND} -E make_directory ${jags_HOME}/x64
              COMMAND ${CMAKE_COMMAND} -E make_directory ${jags_HOME}/include
              COMMAND ${CMAKE_COMMAND} -E copy_directory
                      ${jags_win_SOURCE_DIR}/x64/ ${jags_HOME}/x64
              COMMAND ${CMAKE_COMMAND} -E copy_directory
                      ${jags_win_SOURCE_DIR}/include/ ${jags_HOME}/include)

            add_custom_target(
              jags
              JOB_POOL sequential
              DEPENDS ${jags_VERSION_H_PATH})

          else()

            message(CHECK_FAIL "failed")

          endif()

          # Manually copying the entire JAGS from MSYS2 into the R/opt/jags
          # if later, we need any other libraries, we can use this method,
          # automate it a bit nicer, and ship our external libraries
          # add_custom_command(
          #   OUTPUT ${jags_VERSION_H_PATH}
          #   # bin
          #   COMMAND ${CMAKE_COMMAND} -E make_directory ${jags_HOME}/x64
          #   COMMAND ${CMAKE_COMMAND} -E make_directory ${jags_HOME}/x64/bin
          #   COMMAND ${CMAKE_COMMAND} -E copy ${RTOOLS_LIBJAGS_BAT} ${jags_HOME}/x64/bin/
          #   COMMAND ${CMAKE_COMMAND} -E copy ${RTOOLS_LIBJAGS} ${jags_HOME}/x64/bin/
          #   COMMAND ${CMAKE_COMMAND} -E copy ${RTOOLS_LIBJAGS_JRMATH} ${jags_HOME}/x64/bin/
          #   COMMAND ${CMAKE_COMMAND} -E copy ${RTOOLS_LIBJAGS_JAGS_TERMINAL_EXE} ${jags_HOME}/x64/bin
          #   COMMAND ${CMAKE_COMMAND} -E copy ${RTOOLS_LIB_BLAS} ${jags_HOME}/x64/bin
          #   COMMAND ${CMAKE_COMMAND} -E copy ${RTOOLS_LIB_LAPACK} ${jags_HOME}/x64/bin
          #   # headers
          #   COMMAND ${CMAKE_COMMAND} -E make_directory ${jags_HOME}/include
          #   COMMAND ${CMAKE_COMMAND} -E copy_directory ${RTOOLS_LIBJAGS_HEADERS_PATH}/ ${jags_HOME}/include
          #   # libs
          #   COMMAND ${CMAKE_COMMAND} -E make_directory ${jags_HOME}/x64/lib
          #   COMMAND ${CMAKE_COMMAND} -E copy ${RTOOLS_LIBJAGS_LIBJAGS_A} ${jags_HOME}/x64/lib
          #   COMMAND ${CMAKE_COMMAND} -E copy ${RTOOLS_LIBJAGS_LIBJAGS_LA} ${jags_HOME}/x64/lib
          #   COMMAND ${CMAKE_COMMAND} -E copy ${RTOOLS_LIBJAGS_LIBJRMATH_A} ${jags_HOME}/x64/lib
          #   COMMAND ${CMAKE_COMMAND} -E copy ${RTOOLS_LIBJAGS_LIBJRMATH_LA} ${jags_HOME}/x64/lib
          #   COMMAND ${CMAKE_COMMAND} -E copy ${RTOOLS_LIB_BLAS_DLL_A} ${jags_HOME}/x64/lib
          #   COMMAND ${CMAKE_COMMAND} -E copy ${RTOOLS_LIB_LAPACK_DLL_A} ${jags_HOME}/x64/lib
          #   # modules
          #   COMMAND ${CMAKE_COMMAND} -E make_directory ${jags_HOME}/x64/modules
          #   COMMAND ${CMAKE_COMMAND} -E copy_directory ${RTOOLS_LIBJAGS_MODULES_PATH} ${jags_HOME}/x64/modules
          #   # pkgconfig
          #   COMMAND ${CMAKE_COMMAND} -E copy_directory ${RTOOLS_LIBJAGS_PKGCONFIG_PATH}/ ${jags_HOME}/lib/pkgconfig
          #   )

        endif()

      elseif(APPLE)

        # ----- Downloading and Building jags
        if(NOT TARGET jags)

          fetchcontent_declare(
            jags
            URL "http://static.jasp-stats.org/JAGS-4.3.1.tar.gz"
            URL_HASH
              SHA256=f9258355b5e9eb13bd33c5fa720f0cbebacea7d0a4a42b71b0fb14501ee14229
          )

          message(CHECK_START "Downloading 'jags'")

          fetchcontent_makeavailable(jags)

          if(jags_POPULATED)

            message(CHECK_PASS "successful.")

            set(JAGS_F77_FLAG "F77=${FORTRAN_EXECUTABLE}")
            set(JAGS_CFLAGS
                "-g -O2 -arch ${CMAKE_OSX_ARCHITECTURES} -mmacosx-version-min=${CMAKE_OSX_DEPLOYMENT_TARGET}"
            )
            set(JAGS_EXTRA_FLAGS_1 "--with-sysroot=${CMAKE_OSX_SYSROOT}")
            set(JAGS_EXTRA_FLAGS_2 "--target=${CONFIGURE_HOST_FLAG}")
            set(JAGS_CXXFLAGS "${JAGS_CFLAGS}")

            add_custom_command(
              JOB_POOL sequential
              WORKING_DIRECTORY ${jags_SOURCE_DIR}
              OUTPUT ${jags_VERSION_H_PATH}
              COMMAND
                export CFLAGS=${READSTAT_CFLAGS} && export
                CXXFLAGS=${READSTAT_CXXFLAGS} && ${JAGS_F77_FLAG} ./configure
                --disable-dependency-tracking --prefix=${jags_HOME}
                ${JAGS_EXTRA_FLAGS_1} ${JAGS_EXTRA_FLAGS_2}
              COMMAND ${MAKE}
              COMMAND ${MAKE} install
              COMMAND
                ${CMAKE_COMMAND} -D
                NAME_TOOL_PREFIX_PATCHER=${PROJECT_SOURCE_DIR}/Tools/macOS/install_name_prefix_tool.sh
                -D PATH=${jags_HOME} -D R_HOME_PATH=${R_HOME_PATH} -D
                R_DIR_NAME=${R_DIR_NAME} -D
                SIGNING_IDENTITY=${APPLE_CODESIGN_IDENTITY} -D
                SIGNING=${IS_SIGNING} -D
                CODESIGN_TIMESTAMP_FLAG=${CODESIGN_TIMESTAMP_FLAG} -P
                ${PROJECT_SOURCE_DIR}/Tools/CMake/Patch.cmake
              COMMENT "----- Preparing 'jags'")

            add_custom_target(
              jags
              JOB_POOL sequential
              DEPENDS ${jags_VERSION_H_PATH})

          else()

            message(CHECK_FAIL "failed.")

          endif()

        endif()

      elseif(LINUX)

        # On Linux,
        #   we only set the jags_HOME to the /usr/local/ or /app in case of FLATPAK

      endif()

      set(jags_INCLUDE_DIRS ${jags_HOME}/include)
      set(jags_LIBRARY_DIRS ${jags_HOME}/lib)
      set(jags_PKG_CONFIG_PATH ${jags_HOME}/lib/pkgconfig/)

      # The install-jaspMetaAnalysis.R and/or install-jaspJags.R need to be reconfigured
      # for jags flags to be included as well
      configure_file(${INSTALL_MODULE_TEMPLATE_FILE}
		             ${SCRIPT_DIRECTORY}/install-${MODULE}.R @ONLY)

    endif()

  endforeach()

endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
