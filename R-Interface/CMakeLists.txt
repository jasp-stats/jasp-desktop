list(APPEND CMAKE_MESSAGE_CONTEXT R-Interface)

file(GLOB SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/jaspResults/src/*.cpp")
list(APPEND SOURCE_FILES "${CMAKE_CURRENT_LIST_DIR}/jasprcpp.cpp")

add_library(R-Interface ${HEADER_FILES} ${SOURCE_FILES})

add_dependencies(R-Interface Common)

target_include_directories(
  R-Interface
  PUBLIC # JASP
         jaspResults
         ${PROJECT_SOURCE_DIR}/Common
         ${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder
         # R
         ${_R_HOME}/include
         ${_Rcpp_HOME}/include
         ${_RInside_HOME}/include)

target_link_libraries(
  R-Interface
  PUBLIC Common
         ${_R_Framework}
         ${_LIB_RInside}
         $<$<BOOL:${JASP_USES_QT_HERE}>:Qt::Core>)

target_compile_definitions(
  R-Interface
  PUBLIC JASP_R_INTERFACE_LIBRARY
         QT_NO_DEPRECATED_WARNINGS
         $<$<BOOL:${JASP_USES_QT_HERE}>:JASP_USES_QT_HERE>
         $<$<BOOL:${PRINT_ENGINE_MESSAGES}>:PRINT_ENGINE_MESSAGES>)

if(IWYU_EXECUTABLE AND RUN_IWYU)
  set_target_properties(R-Interface PROPERTIES CXX_INCLUDE_WHAT_YOU_USE
                                               ${IWYU_EXECUTABLE})
endif()

if(WIN32)
  target_compile_definitions(
    R-Interface
    PUBLIC BOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED
           BOOST_USE_WINDOWS_H
           NOMINMAX
           WIN32_LEAN_AND_MEAN)
  target_compile_options(R-Interface -Og -municode)
endif()

# Amir: There are some auxillary files here, and I guess I will take care of
# them later

list(POP_BACK CMAKE_MESSAGE_CONTEXT)
