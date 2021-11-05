cmake_minimum_required(VERSION 3.21)

message(STATUS "[JASP]: Configuring the R-Interface...")

find_package(Boost COMPONENTS nowide filesystem system date_time timer chrono REQUIRED)

pkg_check_modules(LIBJSON REQUIRED IMPORTED_TARGET jsoncpp)

if (BUILD_WITH_SYSTEM_R)
	message(STATUS "[R-Interface]: Building with system R...")
	pkg_check_modules(LIBR REQUIRED IMPORTED_TARGET libR)

	include_directories(${LIBR_INCLUDE_DIRS})
	include_directories(${_Rcpp_HOME}/include)
	include_directories(${_RInside_HOME}/include)

else()
	message(STATUS "[R-Interface]: Building with R.framework...")
	message(STATUS "[R-Interface]: Setting the headers...")

	include_directories(${CMAKE_SOURCE_DIR}/${_R_HOME}/include)
	include_directories(${CMAKE_SOURCE_DIR}/${_Rcpp_HOME}/include)
	include_directories(${CMAKE_SOURCE_DIR}/${_RInside_HOME}/include)

	# include_directories(/Users/amabdol/Projects/JASP/Frameworks/R.framework/Versions/4.1/Resources/library/Rcpp/include)
	# include_directories(/Users/amabdol/Projects/JASP/Frameworks/R.framework/Versions/4.1/Resources/library/RInside/include)
	# include_directories(RInside)

	message(STATUS "[R-Interface]: Rcpp_INCLUDE_DIRS is set to ${_Rcpp_HOME}/include")
	message(STATUS "[R-Interface]: RInside_INCLUDE_DIRS is set to ${_RInside_HOME}/include")
endif()

include_directories(jaspResults)
include_directories(${PROJECT_SOURCE_DIR}/Common)
include_directories(${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder)

include_directories(${Boost_INCLUDE_DIRS})
include_directories(${LIBJSON_INCLUDE_DIRS})

add_definitions(-DJASP_R_INTERFACE_LIBRARY)
add_definitions(-DQT_NO_DEPRECATED_WARNINGS)


SET(HEADER_FILES
	# RInside/Callbacks.h
	# RInside/MemBuf.h
	# RInside/RInside.h
	# RInside/RInsideAutoloads.h
	# RInside/RInsideCommon.h
	# RInside/RInsideConfig.h
	# RInside/RInsideEnvVars.h
	# jaspResults/src/jaspColumn.h
	# jaspResults/src/jaspContainer.h
	# jaspResults/src/jaspHtml.h
	# jaspResults/src/jaspJson.h
	# jaspResults/src/jaspList.h
	# jaspResults/src/jaspModuleRegistration.h
	# jaspResults/src/jaspObject.h
	# jaspResults/src/jaspPlot.h
	# jaspResults/src/jaspQmlSource.h
	# jaspResults/src/jaspResults.h
	# jaspResults/src/jaspState.h
	# jaspResults/src/jaspTable.h
	jasprcpp.h
	jasprcpp_interface.h
)

SET(SOURCE_FILES
	# RInside/MemBuf.cpp
	# RInside/RInside.cpp
	jaspResults/src/jaspColumn.cpp
	jaspResults/src/jaspContainer.cpp
	jaspResults/src/jaspHtml.cpp
	jaspResults/src/jaspJson.cpp
	jaspResults/src/jaspObject.cpp
	jaspResults/src/jaspPlot.cpp
	jaspResults/src/jaspQmlSource.cpp
	jaspResults/src/jaspResults.cpp
	jaspResults/src/jaspState.cpp
	jaspResults/src/jaspTable.cpp
	jasprcpp.cpp
)

qt_add_library(
	R-Interface
	${HEADER_FILES}
	${SOURCE_FILES}
)

# Making sure that Xcode could find libR-Interface.a during the archiving process
set_target_properties(R-Interface PROPERTIES
	ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/)

add_dependencies(R-Interface Common)

if (BUILD_WITH_SYSTEM_R)

	target_link_libraries(
		R-Interface PUBLIC
		Common
		
		PkgConfig::LIBR

		${_LIB_RInside}
	)

else()

	target_link_libraries(
		R-Interface PUBLIC 
		Common

		${_R_Framework}

		${Boost_LIBRARIES}

		${_LIB_RInside}
	)

	if (${INSTALL_R_MODULES})

		add_custom_command(TARGET R-Interface
			POST_BUILD 
			COMMAND ${CMAKE_SOURCE_DIR}/${_Rscript_EXE} -e 'install.packages\("Rcpp", repos="http://cran.r-project.org"\)'
			COMMAND ${CMAKE_SOURCE_DIR}/${_Rscript_EXE} -e 'install.packages\("RInside", repos="http://cran.r-project.org"\)'
			COMMAND ${CMAKE_SOURCE_DIR}/${_Rscript_EXE} -e 'install.packages\("rjags", repos="http://cran.r-project.org"\)'
			COMMAND ${CMAKE_SOURCE_DIR}/${_Rscript_EXE} -e 'remotes::install_github\("jasp-stats/jaspDescriptives"\)'
			COMMENT "=== Installing R Modules ...")

	endif()


endif()


# qt_internal_add_cmake_library(R-Interface11'.'6
#     OUTPUT_DIRECTORY ".."
#     SOURCES
#     DEFINES
#         BUILDING_JASP
#         CURRENT_R_VERSION=\"4.1\"
#         GITHUB_PAT_DEFINE=\"$ENV{GITHUB_PAT_DEF}\"
#         GIT_CURRENT_BRANCH=\"(\"git\" rev-parse --abbrev-ref HEAD)\"
#         GIT_CURRENT_COMMIT=\"(\"git\" rev-parse --verify HEAD)\"
#         JASP_LIBJSON_STATIC
#         JASP_R_INTERFACE_LIBRARY
#         JASP_VERSION_BUILD=0
#         JASP_VERSION_MAJOR=0
#         JASP_VERSION_MINOR=16
#         JASP_VERSION_REVISION=0
#         PRINT_ENGINE_MESSAGES
#         QT_DEPRECATED_WARNINGS
#         QT_NO_FOREACH
#     INCLUDE_DIRECTORIES
#         $ENV{R_HOME}/include
#         $ENV{R_HOME}/library/Rcpp/include
#         ../../jasp-required-files/boost_1_71_0
#         ../Common
#         ../Common/jaspColumnEncoder
#         RInside
# )

#### Keys ignored in scope 1:.:.:R-Interface.pro:<TRUE>:
# DEST_DIR_AUX_R = "$$winPathFix($$OUT_PWD/$$DESTDIR)"
# DISTFILES = "jaspResults/R/RcppExports.R" "jaspResults/R/zzaLoadModule.R" "jaspResults/R/zzzWrappers.R" "jaspResults/R/writeImage.R" "jaspResults/DESCRIPTION" "jaspResults/NAMESPACE" "jaspResults/man/jaspList.Rd" "jaspResults/man/jaspTable.Rd" "jaspResults/man/jaspResultsClass.Rd" "jaspResults/man/jaspPlot.Rd" "jaspResults/man/jaspObject.Rd" "jaspResults/man/jaspHtml.Rd" "jaspResults/man/jaspContainer.Rd" "jaspResults/man/jaspState.Rd" "jaspResults/man/jaspResults-package.Rd"
# INSTALLS = "auxillaryRFiles"
# SRC_SYMLINKTOOL = "$$winPathFix($${PWD}/R/symlinkTools.R)"
# SRC_WORKAROUNDS = "$$winPathFix($${PWD}/R/workarounds.R)"
# SRC_WRAPPERS = "$$winPathFix($${PWD}/jaspResults/R/zzzWrappers.R)"
# SRC_WRITE_IMAGE = "$$winPathFix($${PWD}/jaspResults/R/writeImage.R)"
# TEMPLATE = "lib"
# auxillaryRFiles.files = "$${PWD}/jaspResults/R/writeImage.R" "$${PWD}/jaspResults/R/zzzWrappers.R" "$${PWD}/R/workarounds.R"
# auxillaryRFiles.path = "$$INSTALLPATH"

## Scopes:
#####################################################################

# qt_internal_extend_target(R-Interface11'.'6 CONDITION UNIX
#     COMPILE_OPTIONS
#         -Werror=return-type
# )

#### Keys ignored in scope 2:.:.:R-Interface.pro:UNIX:
# QMAKE_CLEAN = "$$OUT_PWD/$$DESTDIR/'lib'$$JASP_R_INTERFACE_TARGET'*.a'"

# qt_internal_extend_target(R-Interface11'.'6 CONDITION windows
#     DEFINES
#         BOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED
#         BOOST_USE_WINDOWS_H
#         NOMINMAX
#         WIN32_LEAN_AND_MEAN
#     INCLUDE_DIRECTORIES
#         ../../boost_1_71_0
#     PUBLIC_LIBRARIES
#         # Remove: L$ENV{R_HOME}/bin/
#         R
#     COMPILE_OPTIONS
#         -Og
#         -municode
# )

#### Keys ignored in scope 3:.:.:R-Interface.pro:windows:
# QMAKE_CLEAN = "$$OUT_PWD/$$DESTDIR/$$JASP_R_INTERFACE_TARGET'*.lib'" "$$OUT_PWD/$$DESTDIR/$$JASP_R_INTERFACE_TARGET'*.dll'"

# qt_internal_extend_target(R-Interface11'.'6 CONDITION MACOS
#     INCLUDE_DIRECTORIES
#         ../../boost_1_71_0
# )

#### Keys ignored in scope 4:.:.:R-Interface.pro:MACOS:
# QMAKE_CLEAN = "$$OUT_PWD/$$DESTDIR/'lib'$$JASP_R_INTERFACE_TARGET'*.dylib'"

#### Keys ignored in scope 8:.:.:R-Interface.pro:WIN32:
# copyRFiles.commands = "['quote', ['cmd', '/c', 'xcopy', '/I', '/Y', '$${SRC_WRITE_IMAGE}', '$${DEST_DIR_AUX_R}']]" "$$escape_expand(\n\t)" "['quote', ['cmd', '/c', 'xcopy', '/I', '/Y', '$${SRC_WRAPPERS}', '$${DEST_DIR_AUX_R}']]" "$$escape_expand(\n\t)" "['quote', ['cmd', '/c', 'xcopy', '/I', '/Y', '$${SRC_WORKAROUNDS}', '$${DEST_DIR_AUX_R}']]" "$$escape_expand(\n\t)" "['quote', ['cmd', '/c', 'xcopy', '/I', '/Y', '$${SRC_SYMLINKTOOL}', '$${DEST_DIR_AUX_R}']]" "$$escape_expand(\n\t)"

#### Keys ignored in scope 10:.:.:R-Interface.pro:NOT PWD___equals____ss_{OUT_PWD}:
# POST_TARGETDEPS = "copyRFiles"
# QMAKE_EXTRA_TARGETS = "copyRFiles"

#### Keys ignored in scope 12:.:..:../JASP.pri:GITHUB_PAT_DEF_ISEMPTY:
# GITHUB_PAT_DEF = "$$(GITHUB_PAT)"

#### Keys ignored in scope 13:.:..:../JASP.pri:EXISTS /app/lib/_x_:
# GITHUB_PAT = "$$system(echo $${GITHUB_PAT} | base64 -d)"
# GITHUB_PAT_DEF = "$$system(echo $${GITHUB_PAT_DEF} | base64 -d)"

#### Keys ignored in scope 16:.:..:../JASP.pri:_GIT_LOCATION_ISEMPTY:
# _GIT_LOCATION = "C:Program FilesGit"

# qt_internal_extend_target(R-Interface11'.'6 CONDITION (EXISTS /app/lib/_x_) AND (LINUX)
#     DEFINES
#         FLATPAK_USED
# )

# qt_internal_extend_target(R-Interface11'.'6 CONDITION (NOT (EXISTS /app/lib/_x_)) AND (LINUX AND CONFIG(debug,debug OR release))
#     DEFINES
#         JASP_DEBUG
#         LINUX_NOT_FLATPAK
# )

# qt_internal_extend_target(R-Interface11'.'6 CONDITION CMAKE_BUILD_TYPE STREQUAL Debug AND (MACOS OR windows)
#     DEFINES
#         JASP_DEBUG
# )

#### Keys ignored in scope 24:.:..:../JASP.pri:QT_ARCH___contains___i386:
# ARCH = "i386"
# BOOST_ARCH = "x32"

#### Keys ignored in scope 25:.:..:../JASP.pri:else:
# ARCH = "x64"
# BOOST_ARCH = "x64"

#### Keys ignored in scope 26:.:..:../JASP.pri:CONFIG(ReleaseBuild):
# BOOST_POSTFIX = "-vc142-mt-$${BOOST_ARCH}-1_71"

#### Keys ignored in scope 27:.:..:../JASP.pri:CONFIG(DebugBuild):
# BOOST_POSTFIX = "-vc142-mt-gd-$${BOOST_ARCH}-1_71"

# qt_internal_extend_target(R-Interface11'.'6 CONDITION _ss_JASPTIMER_USED
#     DEFINES
#         PROFILE_JASP
# )

# qt_internal_extend_target(R-Interface11'.'6 CONDITION (MACOS) AND (CONFIG(debug))
#     COMPILE_OPTIONS
#         -fstandalone-debug
# )

# qt_internal_extend_target(R-Interface11'.'6 CONDITION LINUX
#     DEFINES
#         'R_HOME=\\\"\\\"'
#     INCLUDE_DIRECTORIES
#         $ENV{R_HOME}/library/include
#         $ENV{R_HOME}/site-library/Rcpp/include
#         /usr/include/R
#         /usr/share/R/include
#     LINK_OPTIONS
#         "-fuse-ld=gold"
# )

#### Keys ignored in scope 38:.:..:../R_HOME.pri:QMAKE_HOST.arch___contains___x86_64:
# _R_HOME = "/app/lib64/R"

#### Keys ignored in scope 39:.:..:../R_HOME.pri:else:
# _R_HOME = "/app/lib/R"

#### Keys ignored in scope 42:.:..:../R_HOME.pri:_R_HOME_ISEMPTY:
# _R_HOME = "/usr/lib64/R"

#### Keys ignored in scope 44:.:..:../R_HOME.pri:_R_HOME_ISEMPTY:
# _R_HOME = "/usr/lib/R"

#### Keys ignored in scope 46:.:..:../R_HOME.pri:_R_HOME_ISEMPTY:
# _R_HOME = "$$JASP_REQUIRED_FILES/Frameworks/R.framework/Versions/$$CURRENT_R_VERSION/Resources"

#### Keys ignored in scope 49:.:..:../R_HOME.pri:JASP_BUILDROOT_DIR_ISEMPTY:
# _R_HOME = "$$OUT_PWD/../R"

#### Keys ignored in scope 50:.:..:../R_HOME.pri:else:
# _R_HOME = "$${JASP_BUILDROOT_DIR}/R"

#### Keys ignored in scope 51:.:..:../R_HOME.pri:_RLibrary_ISEMPTY:
# _RLibrary = "$$_R_HOME/library"

#### Keys ignored in scope 54:.:..:../R_HOME.pri:defineReplace(winPathFix):
# THE_PATH = "$$1"

#### Keys ignored in scope 58:.:..:../R_HOME.pri:GETTEXT_LOCATION_ISEMPTY:
# GETTEXT_LOCATION = "/usr/local/bin"

#### Keys ignored in scope 60:.:..:../R_HOME.pri:GETTEXT_LOCATION_ISEMPTY:
# GETTEXT_LOCATION = "$${_GIT_LOCATION}\usr\bin"

#### Keys ignored in scope 62:.:..:../R_INSTALL_CMDS.pri:JASP_BUILDROOT_DIR_ISEMPTY:
# JASP_BUILDROOT_DIR = "$$OUT_PWD/.."

#### Keys ignored in scope 69:.:..:../R_INSTALL_CMDS.pri:JASP_LIBRARY_DIR_ISEMPTY:
# JASP_LIBRARY_DIR = "$$ROOT_LIBRARY_DIR"

#### Keys ignored in scope 72:.:..:../R_INSTALL_CMDS.pri:LOAD_WORKAROUND_ISEMPTY:
# LOAD_WORKAROUND = "false"

#### Keys ignored in scope 73:.:..:../R_INSTALL_CMDS.pri:_ss_LOAD_WORKAROUND:
# WORKAROUND_LOADER = "source" "(" "\'../workarounds.R\'" ")" ";"

#### Keys ignored in scope 74:.:..:../R_INSTALL_CMDS.pri:defineReplace(runRCommandForInstall):
# CMD = "$$1"
