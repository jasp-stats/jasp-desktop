cmake_minimum_required(VERSION 3.21)

message(STATUS "[JASP]: Configuring the Engine")

#####################################################################
## JASPEngine Binary:
#####################################################################

set(HEADER_FILES
	engine.h
	otoolstuff.h
	rbridge.h
)

set(SOURCE_FILES
	engine.cpp 
	main.cpp
	otoolstuff.cpp 
	rbridge.cpp 
)

find_package(PkgConfig REQUIRED)
pkg_check_modules(LIBARCHIVE REQUIRED IMPORTED_TARGET libarchive)
pkg_check_modules(LIBJSON REQUIRED IMPORTED_TARGET jsoncpp)

# TODO: I don't think Engine needs all of these, but for now, I leave them here
find_package(Boost COMPONENTS nowide filesystem system date_time timer chrono REQUIRED)

find_package(Qt6 REQUIRED
				 COMPONENTS
				 Core
			)

include_directories(${Boost_INCLUDE_DIRS})
include_directories(${LIBJSON_INCLUDE_DIRS})
include_directories(${LIBARCHIVE_INCLUDE_DIRS})

# TODO: This should be done better
include_directories(${PROJECT_SOURCE_DIR}/R-Interface)
include_directories(${PROJECT_SOURCE_DIR}/Common/)
include_directories(${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder)

# configure_file(appinfo.cpp.in appinfo.cpp)

if (BUILD_WITH_SYSTEM_R)
	message(STATUS "[Engine]: Building with system R..")
	pkg_check_modules(LIBR REQUIRED IMPORTED_TARGET libR)
	include_directories(${LIBR_INCLUDE_DIRS})

	include_directories(${_Rcpp_HOME}/include)
	include_directories(${_RInside_HOME}/include)

else()
	message(STATUS "[Engine]: Looking for the libR, here: " ${_R_HOME})
	find_library(LIBR
			NAMES R
			PATHS ${_R_HOME}
			REQUIRED
		)

	include_directories(${_R_HOME}/include)
	include_directories(${_R_HOME}/library/Rcpp/include)
endif()

# qt_internal_add_executable(JASPEngine
#     INSTALL_DIRECTORY "$$INSTALLPATH"
#     OUTPUT_DIRECTORY ".."
#     SOURCES

#     DEFINES
#         BUILDING_JASP
#         CURRENT_R_VERSION=\"4.1\"
#         GITHUB_PAT_DEFINE=\"$ENV{GITHUB_PAT_DEF}\"
#         GIT_CURRENT_BRANCH=\"(\"git\" rev-parse --abbrev-ref HEAD)\"
#         GIT_CURRENT_COMMIT=\"(\"git\" rev-parse --verify HEAD)\"
#         JASP_LIBJSON_STATIC
#         JASP_VERSION_BUILD=0
#         JASP_VERSION_MAJOR=0
#         JASP_VERSION_MINOR=16
#         JASP_VERSION_REVISION=0
#         PRINT_ENGINE_MESSAGES
#         QT_NO_FOREACH
#     INCLUDE_DIRECTORIES
#         $ENV{R_HOME}/include
#         $ENV{R_HOME}/library/Rcpp/include
#         ../../jasp-required-files/boost_1_71_0
#         ../Common
#         ../Common/jaspColumnEncoder
#     PUBLIC_LIBRARIES
#         # Remove: L..
#         Common
#         R-Interface11'.'6
# )

#### Keys ignored in scope 1:.:.:Engine.pro:<TRUE>:
# BUILDING_JASP_ENGINE = "true"
# DEPENDPATH = ".."
# PRE_TARGETDEPS = "../Common"
# QMAKE_CLEAN = "$$OUT_PWD/../R/library/*"
# TEMPLATE = "app"

## Scopes:
#####################################################################

# qt_internal_extend_target(JASPEngine CONDITION LINUX
#     DEFINES
#         'R_HOME=\\\"$ENV{R_HOME}\\\"'
#     INCLUDE_DIRECTORIES
#         $ENV{R_HOME}/library/include
#         $ENV{R_HOME}/site-library/Rcpp/include
#         /usr/include/R
#         /usr/share/R/include
#     PUBLIC_LIBRARIES
#         archive
#         boost_filesystem
#         boost_system
#         rt
#     LINK_OPTIONS
#         "-fuse-ld=gold"
# )

# qt_internal_extend_target(JASPEngine CONDITION windows
#     DEFINES
#         BOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED BOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED
#         BOOST_USE_WINDOWS_H BOOST_USE_WINDOWS_H
#         NOMINMAX NOMINMAX
#         WIN32_LEAN_AND_MEAN
#     INCLUDE_DIRECTORIES
#         ../../boost_1_71_0
#     PUBLIC_LIBRARIES
#         archive.dll
#         libboost_date_time
#         libboost_filesystem
#         libboost_system
#         ole32
#         oleaut32
# )

# qt_internal_extend_target(JASPEngine CONDITION MACOS
#     INCLUDE_DIRECTORIES
#         ../../boost_1_71_0
#     PUBLIC_LIBRARIES
#         archive
#         boost_filesystem-mt
#         boost_system-mt
#         z
#     COMPILE_OPTIONS
#         -Wno-c++11-extensions -Wno-c++11-extensions
#         -Wno-c++11-extra-semi -Wno-c++11-extra-semi
#         -Wno-c++11-long-long -Wno-c++11-long-long
#         -Wno-deprecated-declarations
#         -stdlib=libc++ -stdlib=libc++
# )

add_definitions(-DBUILDING_JASP_ENGINE)
add_definitions(-DR_HOME=${_R_HOME})

qt_add_executable(JASPEngine
	${HEADER_FILES}
	${SOURCE_FILES}
)

if (BUILD_WITH_SYSTEM_R)


	target_link_libraries(
		JASPEngine PUBLIC 
			Common
			R-Interface
		)

else()

	target_link_libraries(
		JASPEngine PUBLIC 
			# JASP Libraries
			Common
			R-Interface
			
			# CMake and PkgConfig
			PkgConfig::LIBJSON
			${Boost_LIBRARIES}

			# Local Frameworks, and Libraries
			${LIBR}
		)
	
endif()

#### Keys ignored in scope 4:.:.:Engine.pro:MACOS:
# QMAKE_CXXFLAGS_WARN_ON = "-Wno-unused-parameter" "-Wno-unused-local-typedef" "-Wno-unused-parameter" "-Wno-unused-local-typedef"

# qt_internal_extend_target(JASPEngine CONDITION (LINUX) AND (EXISTS /app/lib/_x_)
#     PUBLIC_LIBRARIES
#         # Remove: L/app/lib
# )

# qt_internal_extend_target(JASPEngine CONDITION _ss_JASPTIMER_USED
#     DEFINES
#         PROFILE_JASP
# )

# qt_internal_extend_target(JASPEngine CONDITION _ss_JASPTIMER_USED AND windows
#     PUBLIC_LIBRARIES
#         libboost_chrono
#         libboost_timer
# )

# qt_internal_extend_target(JASPEngine CONDITION LINUX AND _ss_JASPTIMER_USED
#     PUBLIC_LIBRARIES
#         boost_chrono
#         boost_timer
# )

# qt_internal_extend_target(JASPEngine CONDITION MACOS AND _ss_JASPTIMER_USED
#     PUBLIC_LIBRARIES
#         boost_chrono-mt
#         boost_timer-mt
# )

# qt_internal_extend_target(JASPEngine CONDITION UNIX
#     PUBLIC_LIBRARIES
#         # Remove: L$ENV{R_HOME}/lib
#         R
#     COMPILE_OPTIONS
#         -Werror=return-type
# )

#### Keys ignored in scope 13:.:.:Engine.pro:else:
# InstallJASPRPackage.commands = "" "$${INSTALL_R_PKG_CMD_PREFIX}$$PWD/jaspBase$${INSTALL_R_PKG_CMD_POSTFIX}"
# POST_TARGETDEPS = "InstallJASPRPackage"
# QMAKE_EXTRA_TARGETS = "InstallJASPRPackage"

#### Keys ignored in scope 14:.:.:Engine.pro:WIN32:
# InstallJASPRPackage.depends = "RemoveJASPRPkgLock"
# POST_TARGETDEPS = "RemoveJASPRPkgLock"
# QMAKE_EXTRA_TARGETS = "RemoveJASPRPkgLock"
# RemoveJASPRPkgLock = "$${PKG_LOCK_CMD_PREFIX}00LOCK-jaspBase$${PKG_LOCK_CMD_INFIX}00LOCK-jaspBase$${PKG_LOCK_CMD_POSTFIX}"

#### Keys ignored in scope 16:.:..:../JASP.pri:GITHUB_PAT_DEF_ISEMPTY:
# GITHUB_PAT_DEF = "$$(GITHUB_PAT)"

#### Keys ignored in scope 20:.:..:../JASP.pri:_GIT_LOCATION_ISEMPTY:
# _GIT_LOCATION = "C:Program FilesGit"

# qt_internal_extend_target(JASPEngine CONDITION (EXISTS /app/lib/_x_) AND (LINUX)
#     DEFINES
#         FLATPAK_USED
# )

# qt_internal_extend_target(JASPEngine CONDITION (NOT (EXISTS /app/lib/_x_)) AND (LINUX AND CONFIG(debug,debug OR release))
#     DEFINES
#         JASP_DEBUG
#         LINUX_NOT_FLATPAK
# )

# qt_internal_extend_target(JASPEngine CONDITION CMAKE_BUILD_TYPE STREQUAL Debug AND (MACOS OR windows)
#     DEFINES
#         JASP_DEBUG
# )

#### Keys ignored in scope 28:.:..:../JASP.pri:QT_ARCH___contains___i386:
# ARCH = "i386"
# BOOST_ARCH = "x32"

#### Keys ignored in scope 29:.:..:../JASP.pri:else:
# ARCH = "x64"
# BOOST_ARCH = "x64"

#### Keys ignored in scope 30:.:..:../JASP.pri:CONFIG(ReleaseBuild):
# BOOST_POSTFIX = "-vc142-mt-$${BOOST_ARCH}-1_71"

#### Keys ignored in scope 31:.:..:../JASP.pri:CONFIG(DebugBuild):
# BOOST_POSTFIX = "-vc142-mt-gd-$${BOOST_ARCH}-1_71"

# qt_internal_extend_target(JASPEngine CONDITION (MACOS) AND (CONFIG(debug))
#     COMPILE_OPTIONS
#         -fstandalone-debug
# )

#### Keys ignored in scope 42:.:..:../R_HOME.pri:QMAKE_HOST.arch___contains___x86_64:
# _R_HOME = "/app/lib64/R"

#### Keys ignored in scope 43:.:..:../R_HOME.pri:else:
# _R_HOME = "/app/lib/R"

#### Keys ignored in scope 46:.:..:../R_HOME.pri:_R_HOME_ISEMPTY:
# _R_HOME = "/usr/lib64/R"

#### Keys ignored in scope 48:.:..:../R_HOME.pri:_R_HOME_ISEMPTY:
# _R_HOME = "/usr/lib/R"

#### Keys ignored in scope 50:.:..:../R_HOME.pri:_R_HOME_ISEMPTY:
# _R_HOME = "$$JASP_REQUIRED_FILES/Frameworks/R.framework/Versions/$$CURRENT_R_VERSION/Resources"

#### Keys ignored in scope 53:.:..:../R_HOME.pri:JASP_BUILDROOT_DIR_ISEMPTY:
# _R_HOME = "$$OUT_PWD/../R"

#### Keys ignored in scope 54:.:..:../R_HOME.pri:else:
# _R_HOME = "$${JASP_BUILDROOT_DIR}/R"

#### Keys ignored in scope 55:.:..:../R_HOME.pri:_RLibrary_ISEMPTY:
# _RLibrary = "$$_R_HOME/library"

#### Keys ignored in scope 58:.:..:../R_HOME.pri:defineReplace(winPathFix):
# THE_PATH = "$$1"

#### Keys ignored in scope 62:.:..:../R_HOME.pri:GETTEXT_LOCATION_ISEMPTY:
# GETTEXT_LOCATION = "/usr/local/bin"

#### Keys ignored in scope 64:.:..:../R_HOME.pri:GETTEXT_LOCATION_ISEMPTY:
# GETTEXT_LOCATION = "$${_GIT_LOCATION}\usr\bin"

#### Keys ignored in scope 66:.:..:../R_INSTALL_CMDS.pri:JASP_BUILDROOT_DIR_ISEMPTY:
# JASP_BUILDROOT_DIR = "$$OUT_PWD/.."

#### Keys ignored in scope 73:.:..:../R_INSTALL_CMDS.pri:JASP_LIBRARY_DIR_ISEMPTY:
# JASP_LIBRARY_DIR = "$$ROOT_LIBRARY_DIR"

#### Keys ignored in scope 76:.:..:../R_INSTALL_CMDS.pri:LOAD_WORKAROUND_ISEMPTY:
# LOAD_WORKAROUND = "false"

#### Keys ignored in scope 77:.:..:../R_INSTALL_CMDS.pri:_ss_LOAD_WORKAROUND:
# WORKAROUND_LOADER = "source" "(" "\'../workarounds.R\'" ")" ";"

#### Keys ignored in scope 78:.:..:../R_INSTALL_CMDS.pri:defineReplace(runRCommandForInstall):
# CMD = "$$1"
