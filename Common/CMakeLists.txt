cmake_minimum_required(VERSION 3.21)

message(STATUS "[JASP]: Configuring Common...")

find_package(PkgConfig REQUIRED)
pkg_check_modules(LIBARCHIVE REQUIRED IMPORTED_TARGET libarchive)
pkg_check_modules(LIBJSON REQUIRED IMPORTED_TARGET jsoncpp)

if (BUILD_WITH_SYSTEM_R)
	message(STATUS "[Common]: Building with system R..")
	pkg_check_modules(LIBR REQUIRED IMPORTED_TARGET libR)
	include_directories(${LIBR_INCLUDE_DIRS})

	include_directories(${_Rcpp_HOME}/include)
	include_directories(${_RInside_HOME}/include)

else()
	find_library(LIBR
			NAMES R
			PATHS "${_R_HOME}"
			REQUIRED
		)

	include_directories(${_R_HOME}/include)
	include_directories(${_R_HOME}/library/Rcpp/include)
endif()

find_package(Boost COMPONENTS nowide filesystem system date_time timer chrono REQUIRED)

include_directories(${Boost_INCLUDE_DIRS})
include_directories(${LIBJSON_INCLUDE_DIRS})
include_directories(${LIBARCHIVE_INCLUDE_DIRS})

include_directories(jaspColumnEncoder)

configure_file(appinfo.cpp.in appinfo.cpp)
message(STATUS "[Common]: appinfo.cpp is successfully generated...")

SET(HEADER_FILES

	appinfo.h
	column.h
	columns.h
	common.h
	datablock.h
	dataset.h
	dirs.h
	enginedefinitions.h
	filereader.h
	ipcchannel.h
	jsonredirect.h
	label.h
	labels.h
	log.h
	processinfo.h
	r_functionwhitelist.h
	sharedmemory.h
	tempfiles.h
	timers.h
	utilenums.h
	utils.h
	version.h
)

SET(SOURCE_FILES

	# This is generated from appinfo.cpp.in
	${CMAKE_CURRENT_BINARY_DIR}/appinfo.cpp
	column.cpp 
	columns.cpp 
	datablock.cpp 
	dataset.cpp 
	dirs.cpp 
	enginedefinitions.cpp 
	filereader.cpp 
	ipcchannel.cpp 
	label.cpp 
	labels.cpp 
	log.cpp 
	processinfo.cpp 
	r_functionwhitelist.cpp 
	sharedmemory.cpp 
	tempfiles.cpp 
	timers.cpp 
	utilenums.cpp 
	utils.cpp 
	version.cpp 
	jaspColumnEncoder/columnencoder.cpp 
	jaspColumnEncoder/columntype.cpp 
)

qt_add_library(Common
	${HEADER_FILES}
	${SOURCE_FILES}
	STATIC
	)

if (BUILD_WITH_SYSTEM_R)

	target_link_libraries(
		Common
			${Boost_LIBRARIES}
			PkgConfig::LIBARCHIVE
			PkgConfig::LIBJSON
			# PkgConfig::LIBR
		)

else()

	target_link_libraries(
	Common
		${Boost_LIBRARIES}
		PkgConfig::LIBARCHIVE
		PkgConfig::LIBJSON
		# ${LIBR}
	)

endif()


# qt_add_library(Common
#     OUTPUT_DIRECTORY ".."
#     SOURCES
	# DEFINES
	#     BUILDING_JASP
	#     CURRENT_R_VERSION=\"4.1\"
	#     GITHUB_PAT_DEFINE=\"$ENV{GITHUB_PAT_DEF}\"
	#     GIT_CURRENT_BRANCH=\"(\"git\" rev-parse --abbrev-ref HEAD)\"
	#     GIT_CURRENT_COMMIT=\"(\"git\" rev-parse --verify HEAD)\"
	#     JASP_LIBJSON_STATIC
	#     JASP_VERSION_BUILD=0
	#     JASP_VERSION_MAJOR=0
	#     JASP_VERSION_MINOR=16
	#     JASP_VERSION_REVISION=0
	#     PRINT_ENGINE_MESSAGES
	#     QT_NO_FOREACH
	# INCLUDE_DIRECTORIES
	#     ../../jasp-required-files/boost_1_71_0
	#     /
	#     jaspColumnEncoder

#### Keys ignored in scope 1:.:.:Common.pro:<TRUE>:
# TEMPLATE = "lib"

## Scopes:
#####################################################################

# if(__Apple__)
	
# endif()

# qt_add_library(Common CONDITION MACOS
#     DEFINES
#         BOOST_INTERPROCESS_SHARED_DIR_FUNC
#     COMPILE_OPTIONS
#         -Wno-c++11-extensions
#         -Wno-c++11-extra-semi
#         -Wno-c++11-long-long
#         -Wno-deprecated-declarations
#         -stdlib=libc++
# )

# qt_add_library(Common CONDITION windows
#     DEFINES
#         BOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED BOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED
#         BOOST_USE_WINDOWS_H BOOST_USE_WINDOWS_H
#         NOMINMAX NOMINMAX
#         WIN32_LEAN_AND_MEAN
#     PUBLIC_LIBRARIES
#         archive.dll
#         ole32
#         oleaut32
# )

# qt_add_library(Common CONDITION DEFINES___contains___JASP_LIBJSON_STATIC
#     SOURCES

# )

#### Keys ignored in scope 6:.:..:../JASP.pri:GITHUB_PAT_DEF_ISEMPTY:
# GITHUB_PAT_DEF = "$$(GITHUB_PAT)"

#### Keys ignored in scope 7:.:..:../JASP.pri:EXISTS /app/lib/_x_:
# GITHUB_PAT = "$$system(echo $${GITHUB_PAT} | base64 -d)"
# GITHUB_PAT_DEF = "$$system(echo $${GITHUB_PAT_DEF} | base64 -d)"

#### Keys ignored in scope 10:.:..:../JASP.pri:_GIT_LOCATION_ISEMPTY:
# _GIT_LOCATION = "C:Program FilesGit"

# qt_internal_extend_target(Common CONDITION (EXISTS /app/lib/_x_) AND (LINUX)
#     DEFINES
#         FLATPAK_USED
# )

# qt_internal_extend_target(Common CONDITION (NOT (EXISTS /app/lib/_x_)) AND (LINUX AND CONFIG(debug,debug OR release))
#     DEFINES
#         JASP_DEBUG
#         LINUX_NOT_FLATPAK
# )

# qt_internal_extend_target(Common CONDITION CMAKE_BUILD_TYPE STREQUAL Debug AND (MACOS OR windows)
#     DEFINES
#         JASP_DEBUG
# )

#### Ignored by pro2cmake.py
#### Keys ignored in scope 18:.:..:../JASP.pri:QT_ARCH___contains___i386:
# ARCH = "i386"
# BOOST_ARCH = "x32"

#### Keys ignored in scope 19:.:..:../JASP.pri:else:
# ARCH = "x64"
# BOOST_ARCH = "x64"

#### Keys ignored in scope 20:.:..:../JASP.pri:CONFIG(ReleaseBuild):
# BOOST_POSTFIX = "-vc142-mt-$${BOOST_ARCH}-1_71"

#### Keys ignored in scope 21:.:..:../JASP.pri:CONFIG(DebugBuild):
# BOOST_POSTFIX = "-vc142-mt-gd-$${BOOST_ARCH}-1_71"

# qt_internal_extend_target(Common CONDITION UNIX
#     COMPILE_OPTIONS
#         -Werror=return-type
# )

# qt_internal_extend_target(Common CONDITION _ss_JASPTIMER_USED
#     DEFINES
#         PROFILE_JASP
# )

# qt_internal_extend_target(Common CONDITION (MACOS) AND (CONFIG(debug))
#     COMPILE_OPTIONS
#         -fstandalone-debug
# )

# qt_internal_extend_target(Common CONDITION LINUX
#     LINK_OPTIONS
#         "-fuse-ld=gold"
# )
