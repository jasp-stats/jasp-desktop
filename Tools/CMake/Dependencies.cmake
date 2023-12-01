# Dependencies.cmake tries to provide any dependencies that cannot be
# installed, or configured using Conan. At the moment, this only includes
# ReadStat which doesn't have a proper CMake or PkgConfig.
#
# Notes:
#
# - FetchContent variables will be lower cased, that's why we have
#   weird variable names readstat_POPULATED.
# - "CMake 3.22 updated the FindPkgConfig module to allow passing
#   arbitrary arguments to the pkgconfig executable." This could come handy
#   later on when dealing with some of the more annoying dependencies
#
# On macOS,
#   We can cross compiler, so, here I take care that right ARCH and target are set.
#   I'm not fully trusting this and rather have a native build, but this seems to be
#   working!
#
# On Linux,
#   We can build for Flatpak, as well as a local build, ...

list(APPEND CMAKE_MESSAGE_CONTEXT Dependencies)

add_custom_target(Dependencies)

if(APPLE)

  add_dependencies(Dependencies readstat)

  # ----- readstat -----
  #
  # This might look a bit unusual but it is fine. I had to do it this
  # way because CMake was not being very smart about it, and was
  # reconfiguring the entire readstat everytime during build even if
  # it was already built!

  fetchcontent_declare(
    readstat
	URL "https://github.com/WizardMac/ReadStat/releases/download/v1.1.9/readstat-1.1.9.tar.gz"
    URL_HASH
	  SHA256=3a232b9e852d10173e2f25da9155afe2e129a30d1fc6c9aac142cdc5cbfe527e)

  message(CHECK_START "Downloading 'readstat'")

  fetchcontent_makeavailable(readstat)

  if(USE_CONAN)
	  #If we are using conan we want to avoid readstat using the system libiconv and zlib
	  #using the configure options seems to be ignored for some reason, so instead here we just get the information from conan
	  #We then make sure that readstat finds zlib and iconv headers and libs instead of macosx sdk ones
	  if(CMAKE_BUILD_TYPE STREQUAL "Debug")
		set(Iconv_DIR ${libiconv_PACKAGE_FOLDER_DEBUG})
		set(Zlib_DIR ${zlib_PACKAGE_FOLDER_DEBUG})
		set(zlib_LIB_DIRS ${zlib_LIB_DIRS_DEBUG})
	else()
		set(Iconv_DIR ${libiconv_PACKAGE_FOLDER_RELEASE})
		set(Zlib_DIR ${zlib_PACKAGE_FOLDER_RELEASE})
	endif()
	set(EXTRA_INCLUDE "-I${Iconv_DIR}/include -I${Zlib_DIR}/include")
	set(EXTRA_LIBS_LINK "-L${Iconv_DIR}/lib -liconv -L${Zlib_DIR}/lib -lz")

	#message(STATUS "Using EXTRA_LIBS_LINK: '${EXTRA_LIBS_LINK}'")
  else()
	set(EXTRA_INCLUDE "")
	set(EXTRA_LIBS_LINK "")
  endif()

  if(readstat_POPULATED)

    message(CHECK_PASS "successful.")

	set(READSTAT_CFLAGS "${EXTRA_INCLUDE} -g -O2 -Wno-strict-prototypes -arch ${CMAKE_OSX_ARCHITECTURES} -mmacosx-version-min=${CMAKE_OSX_DEPLOYMENT_TARGET}")
	set(READSTAT_CXXFLAGS "${READSTAT_CFLAGS}")

    add_custom_command(
      WORKING_DIRECTORY ${readstat_SOURCE_DIR}
      OUTPUT ${readstat_BINARY_DIR}/include/readstat.h
             ${readstat_BINARY_DIR}/lib/libreadstat.a
      COMMAND
	    export CFLAGS=${READSTAT_CFLAGS} && export CXXFLAGS=${READSTAT_CXXFLAGS}
		&& export LIBS=${EXTRA_LIBS_LINK}
		&& ./configure --enable-shared=no --enable-static --prefix=${readstat_BINARY_DIR}
		COMMAND ${MAKE}
      COMMAND ${MAKE} install
	  COMMENT "----- Preparing 'readstat'"
	  USES_TERMINAL)

    add_custom_target(readstat DEPENDS ${readstat_BINARY_DIR}/include/readstat.h)

    set(LIBREADSTAT_INCLUDE_DIRS ${readstat_BINARY_DIR}/include)
    set(LIBREADSTAT_LIBRARY_DIRS ${readstat_BINARY_DIR}/lib)
    set(LIBREADSTAT_LIBRARIES ${LIBREADSTAT_LIBRARY_DIRS}/libreadstat.a)

  else()

    message(CHECK_FAIL "failed.")

  endif()

endif()

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
