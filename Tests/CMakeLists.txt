if(BUILD_TESTS)
  # add_subdirectory(test-input)

  if(WIN32)
    add_subdirectory(Windows)
  endif()

  if(APPLE)
    add_subdirectory(macOS)
  endif()

endif()
