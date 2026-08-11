	# R.cmake handles the process of downloading, patching, locating and pathing the R
# instance in different platforms. There is a lot that is going on here, so, if you
# don't know what you are doing, you might very well start breaking things!
#
# The general flow of setting R is as follow:
#
#   - Downloading or locating the R instance, e.g., R.framework,
#   - Copying the R instance to the build folder, after patching and preparing it (only on Windows and macOS)
#   - Installing RInside, and Rcpp
#   - Interpolating all the necessary paths and passing them to the rest of the CMake
#
# on macOS,
#   - Because we are cross-building, I am downloading the right Fortran, place it inside the
#     R.framework, and make sure that R can find it. Most of this is being done in the
#     PatchR.cmake were I modify the `etc/Makeconf`. On ARM, R uses the Fortran 11, or so; and
#     on x86_64, it is using the Fortran 8.
#
# Notes:
#   - Be aware that at some point, R will move to use a different Fortran, and
#     when that happens, someone needs to make sure that the right Fortran is being
#     download, unpacked, and placed in the right location. You can find the
#     appropriate version in `etc/Makeconf` and the binary here,
#     https://github.com/fxcoudert/gfortran-for-macOS/releases
#   - On GitHub Action,
#     - You probably want to unpack the `https://static.jasp-stats.org/development/gfortran-8.2-Mojave.dmg`
#       into a `.tar.gz`. I think this might elimite some possible issues with the unpacking on
#       their environment. If you have decided to do this, make sure that the structure of the
#       archive is similiar and things land where they are expected.
#
# Todos:
#
#   - [ ] All the code inside the if(APPLE), and if(WIN32) should be turned into
#       a CMake module. I leave this for later cleanup
#

set(JASP_STATIC_IS_DOWN_AGAIN OFF CACHE BOOL "Turn ON to try to get R from CRAN instead")
set(STATIC_DEVELOPMENT_REPOSITORY "https://static.jasp-stats.org/development/")

if(APPLE)
    set(XQUARTZ_VERSION "2.8.5")
endif()

if(NOT JASP_STATIC_IS_DOWN_AGAIN)
    set(R_BINARY_REPOSITORY "${STATIC_DEVELOPMENT_REPOSITORY}")
    set(XQUARTZ_REPOSITORY "${STATIC_DEVELOPMENT_REPOSITORY}")
    set(GFORTRAN_REPOSITORY "${STATIC_DEVELOPMENT_REPOSITORY}")
else()
    # TODO: GFORTRAN_REPOSITORY should be set somewhere else...
    set(GFORTRAN_REPOSITORY "${STATIC_DEVELOPMENT_REPOSITORY}")

    if(APPLE)
		set(R_BINARY_ARCH "x86_64")
		if(CMAKE_OSX_ARCHITECTURES STREQUAL "arm64")
			set(R_BINARY_ARCH "arm64")
		endif()
		
		set(R_BINARY_REPOSITORY		"${R_REPOSITORY}/bin/macosx/big-sur-${R_BINARY_ARCH}/base/")
        set(XQUARTZ_REPOSITORY		"https://github.com/XQuartz/XQuartz/releases/download/XQuartz-${XQUARTZ_VERSION}/")
		set(R_BINARY_TYPE			"mac.binary.${R_BINARY_ARCH}")
    elseif(WINDOWS)
		set(R_BINARY_REPOSITORY		"${R_REPOSITORY}/bin/windows/base/")
		set(R_BINARY_TYPE			"win.binary")
    endif()
endif()


set(AVAILABLE_R_VERSIONS
    "R-4.1.2"
    "R-4.1.2-arm64"
    "R-4.1.2-win"
    "R-4.1.3"
    "R-4.1.3-arm64"
    "R-4.1.3-win"
    "R-4.2.1"
    "R-4.2.1-arm64"
    "R-4.2.1-win"
    "R-4.2.2"
    "R-4.2.2-arm64"
    "R-4.2.2-win"
    "R-4.3.0"
    "R-4.3.0-arm64"
    "R-4.3.0-win"
	"R-4.3.1"
	"R-4.3.1-arm64"
	"R-4.3.1-win"
	"R-4.3.2"
	"R-4.3.2-x86_64"
	"R-4.3.2-arm64"
	"R-4.3.2-win"
	"R-4.3.3"
	"R-4.3.3-x86_64"
	"R-4.3.3-arm64"
	"R-4.3.3-win"
	"R-4.4.0"
	"R-4.4.0-x86_64"
	"R-4.4.0-arm64"
	"R-4.4.0-win"
  "R-4.4.1"
	"R-4.4.1-x86_64"
	"R-4.4.1-arm64"
	"R-4.4.1-win"
  "R-4.4.2"
	"R-4.4.2-x86_64"
	"R-4.4.2-arm64"
	"R-4.4.2-win"
 "R-4.4.3"
 	"R-4.4.3-x86_64"
  	"R-4.4.3-arm64"
   	"R-4.4.3-win"
"R-4.5.0"
	  "R-4.5.0-x86_64"
	  "R-4.5.0-arm64"
	  "R-4.5.0-win"
"R-4.5.1"
	  "R-4.5.1-x86_64"
	  "R-4.5.1-arm64"
	  "R-4.5.1-win"
"R-4.5.2"
	  "R-4.5.2-x86_64"
	  "R-4.5.2-arm64"
	  "R-4.5.2-win"
)

set(R_BINARY_HASHES
    # sha1sums
    # 4.1.2
    "61d3909bc070f7fb86c5a2bd67209fda9408faaa"
    "69e8845ffa134c822d4bdcf458220e841a9eeaa5"
    "c72e68bc50e84bea68a2379073c9fedbdfaeda0c"
    # 4.1.3
    "45121f2c830b0cd7d180aee3fc4cd80d0de1e582"
    "dad405d4f58349403c4976ba50e944502070b209"
    "d4068fdc75334c850d5948a0dc8356d34d3512e1"
    # 4.2.1
    "f83a6c96cedd19193255f94cb01381a273073a3a"
    "05370dd000f0fded68594fc95334808ee25a8e91"
    "37cfb7702a7be00abd64bef8e2ae4252821e5cfc"
    # 4.2.2
    "99b8d184f855e630ac950ca4e62cb7fc9a1f7b2e"
    "c3bb657ca6912b9b98e254f63434a365da26848f"
    "f6ddcf0591d0d47034cce8dacf9b9f3ef6547fae"
    # 4.3.0
    "d28e528c8e3ee761aa4b871a8d444a1bfbee9bd3"
    "8ee0276daa9841993f218ebd2a8a7aa86c00d470"
    "6054e6909d3f92015252f3bcee379d7f7e808bf3"
	# 4.3.1
    "1af8f055a601d5de5dfefdb3956ecc8f745c2401"
    "14c018ff54f7f5bb37c1d96b33207343b83e9345"
    "302489ab7ffc3f45a127688fe0d7c567a7f1200d"
	# 4.3.2
	"3d68ea6698add258bd7a4a5950152f4072eee8b2"
	"3d68ea6698add258bd7a4a5950152f4072eee8b2"
	"763be9944ad00ed405972c73e9960ce4e55399d4"
	"7965f49cc3ba08d5aaeb7d853f470cf30cc03915"
	# 4.3.3
	"cbff7e4657eb67d3a35f4c74772680aa3c6b8d4b"
	"cbff7e4657eb67d3a35f4c74772680aa3c6b8d4b"
	"37dc051e0a241eeef8e8207b2710067767781e6c"
	"2bdc0bf43810f6253a101efc0dfbab837c2aebf2"
	  
	# 4.4.0
	"13c1ae112666425ddc9bbb6327b66d2bcb26eba8"
	"13c1ae112666425ddc9bbb6327b66d2bcb26eba8"
	"45e08d760f10c939b1a341223562bf0ac7051332"
	"b447e5ad8b87857ec263591b246c19f3271ba7b7"

  # 4.4.1
  "e66eb09244121d7db7f8fb41d3c06a7579fc93b5"
  "e66eb09244121d7db7f8fb41d3c06a7579fc93b5"
  "616560b17092bbdd8b814d9ed92d098e52204830"
  "2b99600ca0b4280b4439947c6be71f3dff380e15"

  # 4.4.2
  "f49ad56ce3a0ac569fd8f9668749bc861b965b5e"
  "f49ad56ce3a0ac569fd8f9668749bc861b965b5e"
  "7832cb5d6cd686fd3cc54c8ab4c93c464540a944"
  "acf05881e15100144fd70c7df98dc10e57216224"
    # 4.4.3
  "2391e3c97b3c9f3d36001a3a3eb314a6e6efc819"
  "2391e3c97b3c9f3d36001a3a3eb314a6e6efc819"
  "c32bed5f8f0a7ddd31a8c5598a5a60f8b6c89073"
  "791361bb061421ca178f4c5124cc1ee114810a4b"
  # 4.5.0
  "d1121c69451118c6e43d66b643c589008340f3e7"
  "d1121c69451118c6e43d66b643c589008340f3e7"
  "a47d9579664f0ca878b83d90416d66af2581ef9c"
  "ed8be81b82f849e43cd85482753b0948acac0e19"
  # 4.5.1
  "5384a1b3458a28030fc043e64c113e3af40f4c58"
  "5384a1b3458a28030fc043e64c113e3af40f4c58"
  "0db802faf0e544168794a6d648c73a48c2b51a5d"
  "612d85a0913dda78d95acf21a63489bc3b68352a"
  #4.5.2
  "0184504a11da63b26cc31f91a812e5456d523e0e"
  "0184504a11da63b26cc31f91a812e5456d523e0e"
  "1cc0b3d78bc3b3857c6bf3128a9d414b130d938e"
  "3e9a1cf2f48cab87f62a9f3b374c1faab78e89c6"
)


list(APPEND CMAKE_MESSAGE_CONTEXT R)

# dont forget check and upgrande Rtools version if major_minor version changed.
set(R_VERSION "4.6.0")
set(R_VERSION_MAJOR_MINOR "4.6")
set(CURRENT_R_VERSION ${R_VERSION_MAJOR_MINOR})

if(CMAKE_OSX_ARCHITECTURES STREQUAL "arm64")
  set(R_DIR_NAME "${R_VERSION_MAJOR_MINOR}-arm64")
else()
  set(R_DIR_NAME "${R_VERSION_MAJOR_MINOR}-x86_64")
endif()

if(WIN32)
  if(CMAKE_SIZEOF_VOID_P EQUAL 8) # 64 bits
    set(R_DIR_NAME "x64")
  elseif(CMAKE_SIZEOF_VOID_P EQUAL 4) # 32 bits
    set(R_DIR_NAME "i386")
  endif()
endif()

# ------ Preparing Renv Paths
#
set(MODULES_SOURCE_PATH		${PROJECT_SOURCE_DIR}/Modules			CACHE PATH "Location of JASP Modules")
set(MODULES_BINARY_PATH		${CMAKE_BINARY_DIR}/Modules				CACHE PATH "Location of the renv libraries")
set(MODULES_RENV_ROOT_PATH  ${PROJECT_BINARY_DIR}/_cache/renv-root  CACHE PATH "Location of renv root directories")
set(SCRIPT_DIRECTORY		${PROJECT_BINARY_DIR}/_scripts			CACHE PATH "Location of R scripts for building")

if(FLATPAK_USED)
  set(MODULES_RENV_CACHE_PATH "/app/lib64/renv-cache" CACHE PATH "Location of renv cache directories")
else()
  set(MODULES_RENV_CACHE_PATH "${MODULES_BINARY_PATH}/renv-cache" CACHE PATH "Location of renv cache directories")
endif()


set(JASP_ENGINE_PATH
    "${CMAKE_BINARY_DIR}/Desktop/"
    CACHE PATH "Location of the JASPEngine")

make_directory("${MODULES_BINARY_PATH}")
make_directory("${MODULES_RENV_ROOT_PATH}")
make_directory("${MODULES_RENV_CACHE_PATH}")

cmake_print_variables(MODULES_BINARY_PATH)
cmake_print_variables(MODULES_RENV_ROOT_PATH)
cmake_print_variables(MODULES_RENV_CACHE_PATH)

# ------

if(APPLE)

  set(R_FRAMEWORK_PATH	"${CMAKE_BINARY_DIR}/Frameworks")
  set(R_HOME_PATH		"${R_FRAMEWORK_PATH}/R.framework/Versions/${R_DIR_NAME}/Resources")
  set(R_LIBRARY_PATH	"${R_HOME_PATH}/library")
  set(R_OPT_PATH		"${R_HOME_PATH}/opt")
  set(R_EXECUTABLE		"${R_HOME_PATH}/bin/R")
  set(R_INCLUDE_PATH	"${R_HOME_PATH}/include")
  set(RCPP_PATH			"${R_LIBRARY_PATH}/Rcpp")
  set(RINSIDE_PATH		"${R_LIBRARY_PATH}/RInside")
  set(RENV_PATH			"${R_LIBRARY_PATH}/renv")
  set(ENV{JASP_R_HOME}	${R_HOME_PATH})
  #set(ENV{R_HOME} ${R_HOME_PATH}) #enabling this breaks the output from R because it will add a warning about: `WARNING: ignoring environment value of R_HOME`

  cmake_print_variables(R_FRAMEWORK_PATH)
  cmake_print_variables(R_HOME_PATH)
  cmake_print_variables(R_LIBRARY_PATH)
  cmake_print_variables(R_OPT_PATH)
  cmake_print_variables(R_EXECUTABLE)
  cmake_print_variables(RCPP_PATH)
  cmake_print_variables(RINSIDE_PATH)
  cmake_print_variables(RENV_PATH)

  if(INSTALL_R_FRAMEWORK AND (NOT EXISTS
                              ${CMAKE_BINARY_DIR}/Frameworks/R.framework))

    set(XQUARTZ_URL "${XQUARTZ_REPOSITORY}XQuartz-${XQUARTZ_VERSION}.pkg")

    if(CMAKE_OSX_ARCHITECTURES STREQUAL "arm64")

      set(R_VERSION_NAME "R-${R_VERSION}-${CMAKE_OSX_ARCHITECTURES}")
      set(R_PACKAGE_NAME "${R_VERSION_NAME}.pkg")
      set(R_DOWNLOAD_URL "${R_BINARY_REPOSITORY}/${R_PACKAGE_NAME}")

      list(
        FIND
        AVAILABLE_R_VERSIONS
        "${R_VERSION_NAME}"
        HASH_INDEX)
      list(
        GET
        R_BINARY_HASHES
        ${HASH_INDEX}
        R_PACKAGE_HASH)

    else()

		if(JASP_STATIC_IS_DOWN_AGAIN)
			set(R_VERSION_NAME "R-${R_VERSION}-x86_64")
		else()
			set(R_VERSION_NAME "R-${R_VERSION}")
		endif()
		
		set(R_PACKAGE_NAME "${R_VERSION_NAME}.pkg")
		set(R_DOWNLOAD_URL "${R_BINARY_REPOSITORY}/${R_PACKAGE_NAME}")
		
		list(FIND AVAILABLE_R_VERSIONS	${R_VERSION_NAME} HASH_INDEX)
		list(GET R_BINARY_HASHES ${HASH_INDEX} R_PACKAGE_HASH)

    endif()

    if(NOT EXISTS ${CMAKE_SOURCE_DIR}/Frameworks/R.framework)

      fetchcontent_declare(
        r_pkg
        URL ${R_DOWNLOAD_URL}
        URL_HASH SHA1=${R_PACKAGE_HASH}
        DOWNLOAD_NO_EXTRACT ON
        DOWNLOAD_NAME ${R_PACKAGE_NAME})

      message(CHECK_START "Downloading '${R_PACKAGE_NAME}'")

      fetchcontent_makeavailable(r_pkg)

      if(r_pkg_POPULATED)

        message(CHECK_PASS "done.")

        set(r_pkg_r_home
            ${r_pkg_SOURCE_DIR}/R.framework/Versions/${R_DIR_NAME}/Resources)

        message(CHECK_START "Unpacking '${R_PACKAGE_NAME}'")
        execute_process(WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
                        COMMAND xar -xf ${R_PACKAGE_NAME})
        message(CHECK_PASS "done.")

        message(CHECK_START "Unpacking the payloads")
        execute_process(WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
                        COMMAND tar -xf R-fw.pkg/Payload)

        if(CMAKE_OSX_ARCHITECTURES STREQUAL "arm64")

          execute_process(WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
                          COMMAND tar -xf tcltk.pkg/Payload -C ${r_pkg_r_home}/)

          execute_process(
            WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
            COMMAND tar -xf texinfo.pkg/Payload -C ${r_pkg_r_home}/)

          # Downloading the gfortran

          message(CHECK_START "Downloading gfortran")

          fetchcontent_declare(
            gfortran_tar_gz
            URL "${GFORTRAN_REPOSITORY}gfortran-14.2-arm64.tar.xz"
            URL_HASH
              SHA256=77f5eb33b961eba4f9ffac015ca96dbabfd745fff6bf8883bc34c41b7208291d
            DOWNLOAD_NO_EXTRACT ON
            DOWNLOAD_NAME gfortran.tar.gz)

          fetchcontent_makeavailable(gfortran_tar_gz)

          if(gfortran_tar_gz_POPULATED)

            message(CHECK_PASS "done.")

            execute_process(WORKING_DIRECTORY ${gfortran_tar_gz_SOURCE_DIR}
                            COMMAND tar xzf gfortran.tar.gz -C ${r_pkg_r_home}/)

            set(GFORTRAN_PATH ${R_OPT_PATH}/R/arm64/bin)

          else()

            message(CHECK_FAIL "unsuccessful")

          endif()

        else()

          make_directory(${r_pkg_r_home}/opt)
          execute_process(
            WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
            COMMAND tar -xf tcltk.pkg/Payload --strip-components=2 -C
                    ${r_pkg_r_home}/opt)

          execute_process(
            WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
            COMMAND tar -xf texinfo.pkg/Payload --strip-components=2 -C
                    ${r_pkg_r_home}/opt)

          # Downloading the gfortran
          message(CHECK_START "Downloading gfortran")


	  fetchcontent_declare(
            gfortran_tar_gz
            URL "${GFORTRAN_REPOSITORY}gfortran-14.2-intel.tar.xz"
            URL_HASH
              SHA256=fae5e451ace56b97c02e909cd864095bf2a749b71db61454f4fdfd8908a71919
            DOWNLOAD_NO_EXTRACT ON
            DOWNLOAD_NAME gfortran.tar.gz)

          fetchcontent_makeavailable(gfortran_tar_gz)

          if(gfortran_tar_gz_POPULATED)

            message(CHECK_PASS "done.")

            execute_process(WORKING_DIRECTORY ${gfortran_tar_gz_SOURCE_DIR}
                            COMMAND tar xzf gfortran.tar.gz -C ${r_pkg_r_home}/)

            set(GFORTRAN_PATH ${R_OPT_PATH}/R/x86_64/bin)
          else()

            message(CHECK_FAIL "unsuccessful")

          endif()

        endif()

        message(CHECK_START
                "Copying the 'R.framework' to the jasp-desktop/Frameworks.")

        make_directory(${CMAKE_BINARY_DIR}/Frameworks)
        execute_process(
          WORKING_DIRECTORY ${r_pkg_SOURCE_DIR}
          COMMAND cp -Rpf R.framework ${CMAKE_BINARY_DIR}/Frameworks)

        message(CHECK_PASS "done.")
      else()
        message(CHECK_FAIL "failed.")
      endif()

      fetchcontent_declare(
        xquartz_pkg
        URL ${XQUARTZ_URL}
        URL_HASH
            SHA256=e89538a134738dfa71d5b80f8e4658cb812e0803115a760629380b851b608782
        DOWNLOAD_NO_EXTRACT ON
        DOWNLOAD_NAME XQuartz.pkg)

      message(CHECK_START "Downloading XQuartz.pkg")

      fetchcontent_makeavailable(xquartz_pkg)

      if(xquartz_pkg_POPULATED)
        message(CHECK_START "Unpacking XQuartz.pkg")
        execute_process(WORKING_DIRECTORY ${xquartz_pkg_SOURCE_DIR}
                          COMMAND xar -xf XQuartz.pkg)
        message(CHECK_PASS "done.")

        message(CHECK_START "Unpacking XQuartzComponent.pkg/Payload")
        execute_process(WORKING_DIRECTORY ${xquartz_pkg_SOURCE_DIR}
                          COMMAND tar -xf XQuartzComponent.pkg/Payload)
        message(CHECK_PASS "done.")

        set(XQUARTZ_LIB ${CMAKE_BINARY_DIR}/Frameworks/R.framework/Resources/opt/X11/lib)
        make_directory(${XQUARTZ_LIB})
        execute_process(
          WORKING_DIRECTORY ${xquartz_pkg_SOURCE_DIR}
          COMMAND sh -c "cp -pf opt/X11/lib/{libSM.6.dylib,libICE.6.dylib,libX11.6.dylib,libXext.6.dylib,libXrender.1.dylib,libXt.6.dylib,libXmu.6.dylib,libxcb.1.dylib,libXau.6.dylib} ${XQUARTZ_LIB}")

        message(CHECK_PASS "done.")
      else()
        message(CHECK_FAIL "failed.")
      endif()

      message(CHECK_START "Locating the 'gfortran'")

      find_program(
        FORTRAN_EXECUTABLE
        NAMES gfortran
        PATHS ${GFORTRAN_PATH}
        NO_DEFAULT_PATH
        DOC "'gfortran' is needed for building some of the R packages")

      if(NOT FORTRAN_EXECUTABLE)
        message(CHECK_FAIL "not found")
        message(FATAL_ERROR "Please install 'gfortran' before continuing.")
      else()
        message(CHECK_PASS "found")
        message(STATUS "  ${FORTRAN_EXECUTABLE}")
      endif()

      # --------------------------------------------------------
      # Patching R.framework and everything related to it ------
      #
      # A this point, R.framework is unpacked, and prepared and
      # has been copied into the build directory.
      # --------------------------------------------------------

      # Patching R's pathing variables, R_HOME, etc. -----------
      message(CHECK_START "Patching bin/R and etc/Makeconf, and library paths")

      include(${CMAKE_SOURCE_DIR}/Tools/CMake/PatchR.cmake)
      cmake_print_variables(r_pkg_r_home)
      cmake_print_variables(R_HOME_PATH)
      patch_r()

      message(CHECK_PASS "done.")

      message(CHECK_START "Patching and signing all the first-party libraries")

      # Patch and sign all first party libraries
	  execute_process(
		#COMMAND_ECHO STDOUT
		#ERROR_QUIET 
		OUTPUT_QUIET
		WORKING_DIRECTORY ${R_HOME_PATH}
		COMMAND
		  ${CMAKE_COMMAND} -D
		  NAME_TOOL_PREFIX_PATCHER=${PROJECT_SOURCE_DIR}/Tools/macOS/install_name_prefix_tool.sh
		  -D PATH=${R_HOME_PATH} -D R_HOME_PATH=${R_HOME_PATH}
		  -D R_DIR_NAME=${R_DIR_NAME}
		  -D SIGNING_IDENTITY=${APPLE_CODESIGN_IDENTITY} -D SIGNING=1
		  -D CODESIGN_TIMESTAMP_FLAG=${CODESIGN_TIMESTAMP_FLAG} -P
		  ${PROJECT_SOURCE_DIR}/Tools/CMake/Patch.cmake)

      # R binary should be patched as well
      message(CHECK_START "Patching /bin/exec/R")
      execute_process(
        #COMMAND_ECHO STDOUT
        #ERROR_QUIET
        OUTPUT_QUIET
        WORKING_DIRECTORY ${R_HOME_PATH}
        COMMAND
          bash ${PROJECT_SOURCE_DIR}/Tools/macOS/install_name_prefix_tool.sh
          "${R_HOME_PATH}/bin/exec/R"
          "/Library/Frameworks/R.framework/Versions/${R_DIR_NAME}/Resources/lib"
          "@executable_path/../Frameworks/R.framework/Versions/${R_DIR_NAME}/Resources/lib"
      )
      message(CHECK_PASS "successful")

      message(CHECK_START "Signing '${R_HOME_PATH}/bin/exec/R'")

      set(SIGNING_RESULT "timeout")
      while((${SIGNING_RESULT} MATCHES "timeout") OR (${SIGNING_RESULT} STREQUAL "1"))
            execute_process(
                #COMMAND_ECHO STDOUT
                #ERROR_QUIET
                OUTPUT_QUIET
                TIMEOUT 30
                WORKING_DIRECTORY ${R_HOME_PATH}
                COMMAND
                  codesign --force --verbose --deep ${CODESIGN_TIMESTAMP_FLAG} --sign
                  ${APPLE_CODESIGN_IDENTITY} 
                  "${R_HOME_PATH}/bin/exec/R"
                RESULT_VARIABLE SIGNING_RESULT
                OUTPUT_VARIABLE SIGNING_OUTPUT
                ERROR_VARIABLE SIGNING_ERROR)
      endwhile()

      if(NOT (SIGNING_RESULT MATCHES "timeout"))
        message(CHECK_PASS "successful")
      else()
        message(CHECK_FAIL "unsuccessful")
      endif()

      execute_process(
        # COMMAND_ECHO STDOUT
        ERROR_QUIET OUTPUT_QUIET
        WORKING_DIRECTORY ${R_HOME_PATH}/bin
        COMMAND ln -s ../../../../../../Frameworks Frameworks)

      execute_process(WORKING_DIRECTORY ${R_OPT_PATH}/R/arm64/gfortran
                      COMMAND ln -sfn ${CMAKE_OSX_SYSROOT} SDK)

      # ------------------------

      message(CHECK_PASS "done.")

    endif()

	message(CHECK_START "Locating the 'gfortran'")

	find_program(
	  FORTRAN_EXECUTABLE
	  NAMES gfortran
	  PATHS ${GFORTRAN_PATH}
	  NO_DEFAULT_PATH
	  DOC "'gfortran' is needed for building some of the R packages")

	if(NOT FORTRAN_EXECUTABLE)
	  message(CHECK_FAIL "not found")
	  message(FATAL_ERROR "Please install 'gfortran' before continuing.")
	else()
	  message(CHECK_PASS "found")
	  message(STATUS "FORTRAN_EXECUTABLE=${FORTRAN_EXECUTABLE}")
	endif()
	
  endif()

  #
  # I copy the `R.frameworks` inside the build folder as well,
  # so we will have a similar debug and bundle builds and our
  # paths are not altered that we have to take care of them later.
  # This also leaves the original `R.framework` intact. This is
  # important because, as we are starting to mess with it and installing
  # `jags`, etc., we want to have it to be build dependent when we are
  # experimenting and not always tapping into one instance of it.
  #
  # Another reason for having the Framework being copied into the build
  # folder is that it allows us to leave the CMake in charge of the
  # multi-architecture build.
  #
  if(NOT EXISTS ${CMAKE_BINARY_DIR}/Frameworks/R.framework)
    message(
      FATAL_ERROR
        "CMake cannot locate 'R.framework' inside this build folder.
         You can use `cmake .. -DINSTALL_R_FRAMEWORK=ON` to ask CMake to install
         it for you.")
  endif()

  set_property(
    DIRECTORY
    APPEND
    PROPERTY ADDITIONAL_CLEAN_FILES ${R_FRAMEWORK_PATH}/R.framework)

  message(CHECK_START "Checking for 'R.framework'")
  find_library(
    _R_Framework
    NAMES R
    PATHS ${R_FRAMEWORK_PATH}
    NO_DEFAULT_PATH NO_CACHE REQUIRED)

  if(_R_Framework)
    message(CHECK_PASS "found.")
  else()
    message(CHECK_FAIL "not found in ${R_FRAMEWORK_PATH}")
  endif()

  message(CHECK_START "Checking for 'libR'")
  find_library(
    _LIB_R
    NAMES R
    PATHS ${R_HOME_PATH}/lib
    NO_DEFAULT_PATH NO_CACHE REQUIRED)

  if(_LIB_R)
    message(CHECK_PASS "found.")
    message(STATUS "  ${_LIB_R}")
  else()
    message(CHECK_FAIL "not found in ${R_HOME_PATH}/lib")
  endif()

elseif(WIN32)

  set(R_HOME_PATH		"${CMAKE_BINARY_DIR}/R")
  set(R_BIN_PATH		"${R_HOME_PATH}/bin")
  set(R_LIB_PATH		"${R_HOME_PATH}/bin/${R_DIR_NAME}")
  set(R_LIBRARY_PATH	"${R_HOME_PATH}/library")
  set(R_OPT_PATH		"${R_HOME_PATH}/opt")
  set(R_EXECUTABLE		"${R_HOME_PATH}/bin/R")
  set(R_INCLUDE_PATH	"${R_HOME_PATH}/include")

  # This will be added to the install.packages calls
  set(USE_LOCAL_R_LIBS_PATH ", lib='${R_LIBRARY_PATH}'")

  cmake_print_variables(R_HOME_PATH)
  cmake_print_variables(R_LIB_PATH)
  cmake_print_variables(R_LIBRARY_PATH)
  cmake_print_variables(R_OPT_PATH)
  cmake_print_variables(R_EXECUTABLE)

  message(CHECK_START "Checking for R/")

  if(NOT EXISTS ${CMAKE_BINARY_DIR}/R)

    message(CHECK_FAIL "not found.")

    message(CHECK_START "Downloading R-${R_VERSION}-win.exe")

    set(R_VERSION_NAME "R-${R_VERSION}-win")
    set(R_PACKAGE_NAME "${R_VERSION_NAME}.exe")
    set(R_DOWNLOAD_URL "${R_BINARY_REPOSITORY}/${R_PACKAGE_NAME}")

    list(
      FIND
      AVAILABLE_R_VERSIONS
      "${R_VERSION_NAME}"
      HASH_INDEX)
    list(
      GET
      R_BINARY_HASHES
      ${HASH_INDEX}
      R_PACKAGE_HASH)

    fetchcontent_declare(
      r_win_exe
      URL ${R_DOWNLOAD_URL}
      URL_HASH SHA1=${R_PACKAGE_HASH}
      DOWNLOAD_NO_EXTRACT ON
      DOWNLOAD_NAME ${R_PACKAGE_NAME})

    fetchcontent_makeavailable(r_win_exe)

    if(r_win_exe_POPULATED)

      message(CHECK_PASS "successful.")

      message(CHECK_START "Unpacking and preparing the R instance")

      execute_process(
        WORKING_DIRECTORY ${r_win_exe_SOURCE_DIR}
        COMMAND ${R_PACKAGE_NAME} /CURRENTUSER /verysilent /sp
                /DIR=${r_win_exe_BINARY_DIR}/R)

      file(COPY ${r_win_exe_BINARY_DIR}/R DESTINATION ${CMAKE_BINARY_DIR})

      if(EXISTS ${CMAKE_BINARY_DIR}/R)
        message(CHECK_PASS "successful")
      else()
        message(CHECK_FAIL "failed")
        message(
          FATAL_ERROR
            "CMake has failed to prepare the R environment in the build folder."
        )
      endif()

      # TODOs:
      #   - [ ] I think we should probably remove a few auxiliary files, e.g. uninstall stuff

    else()

      message(CHECK_FAIL "failed.")

    endif()

  else()

    message(CHECK_PASS "found.")

  endif()

elseif(LINUX)

  message(CHECK_START "Looking for R")

  # If not custom path is not defined, we are looking if R_HOME is set
  if(CUSTOM_R_PATH STREQUAL "")

    set(R_HOME_PATH $ENV{R_HOME})

    if("${R_HOME_PATH}" STREQUAL "")

        if(NOT EXISTS "/usr/lib/R/bin/R")
          message(CHECK_FAIL "unsuccessful.")
          message(FATAL_ERROR "R is not installed in your system. Please install R and try again, or set R_HOME properly.")
        else()
            set($ENV{R_HOME} /usr/lib/R)
            set(R_HOME_PATH /usr/lib/R)

            message(CHECK_PASS "successful")
            message(STATUS "R_HOME is ${R_HOME_PATH}")
        endif()

    else()

      message(CHECK_PASS "successful")
      message(STATUS "R_HOME is ${R_HOME_PATH}")

    endif()

  else()

    if(EXISTS ${CUSTOM_R_PATH})

      set(R_HOME_PATH ${CUSTOM_R_PATH})
      message(CHECK_PASS "successful")
      message(STATUS "Using a custom R installation, ${R_HOME_PATH}")

    else()

      message(CHECK_FAIL "unsuccessful")
      message(FATAL_ERROR "${CUSTOM_R_PATH} does not exist.")

    endif()

  endif()

  if(LINUX_LOCAL_BUILD)
    message(
      STATUS
        "JASP is configured to install all its R depdendencies inside the build folder. If this is not what you want, make sure that 'LINUX_LOCAL_BUILD' parametere is set to OFF, e.g., 'cmake .. -DLINUX_LOCAL_BUILD=OFF'"
    )

    set(R_LIBRARY_PATH "${CMAKE_BINARY_DIR}/R/library")
    set(R_OPT_PATH "${CMAKE_BINARY_DIR}/R/opt")
    make_directory(${R_LIBRARY_PATH})
    make_directory(${R_OPT_PATH})
  else() # Flatpak
    message(
      WARNING
        "JASP is configured to install all its depdendencies into the ${R_HOME_PATH}/library. CMake may not be able to continue if the user does not have the right permission to right into ${R_HOME_PATH}/library folder."
    )

    set(R_LIBRARY_PATH "${R_HOME_PATH}/library")
    set(R_OPT_PATH "${R_HOME_PATH}/opt")
  endif()

  set(R_EXECUTABLE "${R_HOME_PATH}/bin/R")

  set(USE_LOCAL_R_LIBS_PATH ", lib='${R_LIBRARY_PATH}'")

  message(CHECK_START "Looking for R.h")
  # ask R where it thinks it's include folder is
  execute_process(COMMAND command -E env "JASP_R_HOME=${R_HOME_PATH}" ${R_EXECUTABLE} --slave --no-restore --no-save -e "cat(R.home(\"include\"))" OUTPUT_VARIABLE R_INCLUDE_PATH)
  # if R returns a nonexisting directory, try some fallback locations
  if(NOT EXISTS ${R_INCLUDE_PATH})
    message(STATUS "R return an invalid include directory, trying fallbacks")
    set(R_INCLUDE_PATH "${R_HOME_PATH}/include")
    if(NOT EXISTS ${R_INCLUDE_PATH})
      find_file(
        _R_H
        NAMES R.h
        PATHS /usr/include /usr/include/R /usr/share/R/include)
      if(_R_H)
        get_filename_component(R_INCLUDE_PATH ${_R_H} DIRECTORY)
        message(CHECK_PASS "found")
        message(STATUS "  ${_R_H}")
      else()
        message(CHECK_FAIL "not found")
        message(FATAL_ERROR "R.h is necessary for building R-Interface library.")
      endif()
    endif()
  endif()

  cmake_print_variables(R_HOME_PATH)
  cmake_print_variables(R_LIBRARY_PATH)
  cmake_print_variables(R_OPT_PATH)
  cmake_print_variables(R_EXECUTABLE)
  cmake_print_variables(RCPP_PATH)
  cmake_print_variables(RINSIDE_PATH)
  cmake_print_variables(RENV_PATH)

  message(CHECK_START "Checking for 'libR'")
  find_library(
    _LIB_R
    NAMES R
    PATHS ${R_HOME_PATH}/lib
    NO_DEFAULT_PATH NO_CACHE REQUIRED)

  if(_LIB_R)
    message(CHECK_PASS "found")
    message(STATUS "  ${_LIB_R}")
  else()
    message(CHECK_FAIL "not found in ${R_HOME_PATH}/lib")
  endif()

endif()

set(RENV_LIBRARY                        "${CMAKE_BINARY_DIR}/_cache/R/renv_library")
set(R_CPP_INCLUDES_LIBRARY              "${CMAKE_BINARY_DIR}/Modules/Tools/R_cpp_includes_library")
set(JASP_MODULE_BUNDLE_MANAGER_LIBRARY  "${CMAKE_BINARY_DIR}/Modules/Tools/jaspModuleBundleManager_library")

SET(RENV_SANDBOX                "${CMAKE_BINARY_DIR}/_cache/R/renv_sandbox")
file(MAKE_DIRECTORY ${RENV_SANDBOX})
# TODO: it could be nice to ship the sandbox so it can be used to install dynamic modules
# also, the sandbox paths may need to be adjusted on windows (they are symlinks)

message(STATUS "Setting up renv, Rcpp, RInside, and jaspModuleBundleManager, etc")
message(STATUS "RENV_LIBRARY           = ${RENV_LIBRARY}")
message(STATUS "R_CPP_INCLUDES_LIBRARY = ${R_CPP_INCLUDES_LIBRARY}")


if(FLATPAK_USED)
execute_process(
  WORKING_DIRECTORY ${MODULES_BINARY_PATH}/../
  COMMAND bash -c "rm -r ${MODULES_BINARY_PATH}  && ln -s /app/Modules/ ${MODULES_BINARY_PATH}"
)

else()
##################
# renv bootstrap  
configure_file(${PROJECT_SOURCE_DIR}/Modules/install-renv.R.in
                ${SCRIPT_DIRECTORY}/install-renv.R @ONLY)

              
execute_process(
  COMMAND_ECHO STDOUT
  #ERROR_QUIET OUTPUT_QUIET
  WORKING_DIRECTORY ${R_HOME_PATH}
  COMMAND 
    ${R_EXECUTABLE} --slave --no-restore --no-save --file=${SCRIPT_DIRECTORY}/install-renv.R
)

if(APPLE)
  # Patch renv
  message(CHECK_START "Patching ${RENV_LIBRARY}")
  execute_process(
    COMMAND_ECHO STDOUT
    #ERROR_QUIET OUTPUT_QUIET
    WORKING_DIRECTORY ${R_HOME_PATH}
    COMMAND
      ${CMAKE_COMMAND} -D
      NAME_TOOL_PREFIX_PATCHER=${PROJECT_SOURCE_DIR}/Tools/macOS/install_name_prefix_tool.sh
      -D PATH=${RENV_LIBRARY} -D R_HOME_PATH=${R_HOME_PATH} -D
      R_DIR_NAME=${R_DIR_NAME} -D SIGNING_IDENTITY=${APPLE_CODESIGN_IDENTITY}
      -D SIGNING=1 -D CODESIGN_TIMESTAMP_FLAG=${CODESIGN_TIMESTAMP_FLAG} -P
      ${PROJECT_SOURCE_DIR}/Tools/CMake/Patch.cmake
  )
endif()

##################
# install rest of the tools  
configure_file(${PROJECT_SOURCE_DIR}/Modules/install-tools.R.in
                ${SCRIPT_DIRECTORY}/install-tools.R @ONLY)

execute_process(
  COMMAND_ECHO STDOUT
  #ERROR_QUIET OUTPUT_QUIET
  WORKING_DIRECTORY ${R_HOME_PATH}
  COMMAND 
    ${R_EXECUTABLE} --slave --no-restore --no-save --file=${SCRIPT_DIRECTORY}/install-tools.R
    
)

if(APPLE)
  message(CHECK_START "Patching ${CMAKE_BINARY_DIR}/Modules/Tools/")
  execute_process(
    COMMAND_ECHO STDOUT
    #ERROR_QUIET OUTPUT_QUIET
    WORKING_DIRECTORY ${R_HOME_PATH}
    COMMAND
      ${CMAKE_COMMAND} -D
      NAME_TOOL_PREFIX_PATCHER=${PROJECT_SOURCE_DIR}/Tools/macOS/install_name_prefix_tool.sh
      -D PATH=${CMAKE_BINARY_DIR}/Modules/Tools/ -D R_HOME_PATH=${R_HOME_PATH} -D
      R_DIR_NAME=${R_DIR_NAME} -D SIGNING_IDENTITY=${APPLE_CODESIGN_IDENTITY}
      -D SIGNING=1 -D CODESIGN_TIMESTAMP_FLAG=${CODESIGN_TIMESTAMP_FLAG} -P
      ${PROJECT_SOURCE_DIR}/Tools/CMake/Patch.cmake
  )
endif()

endif()
          
include(FindRPackagePath)

find_package_path(RCPP_PATH       ${R_CPP_INCLUDES_LIBRARY} "Rcpp")
find_package_path(RINSIDE_PATH    ${R_CPP_INCLUDES_LIBRARY} "RInside")

set(RENV_PATH "${RENV_LIBRARY}/renv")

message(STATUS "RENV_PATH              = ${RENV_PATH}")
message(STATUS "RCPP_PATH              = ${RCPP_PATH}")
message(STATUS "RINSIDE_PATH           = ${RINSIDE_PATH}")

set_property(DIRECTORY APPEND PROPERTY ADDITIONAL_CLEAN_FILES _cache)
set_property(DIRECTORY APPEND PROPERTY ADDITIONAL_CLEAN_FILES ${R_CPP_INCLUDES_LIBRARY})

# if(NOT EXISTS ${RENV_PATH})
#     message(FATAL_ERROR "'renv' installation has failed!")
# endif()
if(NOT EXISTS ${RCPP_PATH})
    message(FATAL_ERROR "'Rcpp' installation has failed!")
endif()
if(NOT EXISTS ${RINSIDE_PATH})
    message(FATAL_ERROR "'RInside' installation has failed!")
endif()

if(APPLE OR LINUX)
  message(CHECK_START "Checking for 'libRInside'")
  find_library(
    _LIB_RINSIDE
    NAMES "libRInside.a"
    PATHS ${RINSIDE_PATH}/lib
    NO_DEFAULT_PATH NO_CACHE REQUIRED)

  if(_LIB_RINSIDE)
    message(CHECK_PASS "found")
    message(STATUS "  ${_LIB_RINSIDE}")
  else()
    message(CHECK_FAIL "not found in ${RINSIDE_PATH}/lib")
  endif()
endif()

# ----- jags -----
#
# - JAGS needs GNU Bison v3, https://www.gnu.org/software/bison.
# - With this, we can build JAGS, and link it, or even place it inside the the `R.framework`
#   - `--prefix=${R_OPT_PATH}/jags`, with this, we inherit the R
# - You can run `make jags-build` or `make jags-install` to just play with JAGS target
#

set(jags_HOME ${R_OPT_PATH}/jags)
if(WIN32)
  set(jags_VERSION_H_PATH ${jags_HOME}/include/version.h)
else()
  set(jags_VERSION_H_PATH ${jags_HOME}/include/JAGS/version.h)
endif()

if((NOT EXISTS ${jags_HOME}) AND (NOT LINUX))
  message(STATUS "Creating ${jags_HOME}")
  make_directory("${jags_HOME}")
endif()

if(WIN32)

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
list(POP_BACK CMAKE_MESSAGE_CONTEXT)
