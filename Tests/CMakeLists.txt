if(BUILD_TESTS)

find_package(Qt6 REQUIRED COMPONENTS Test QuickTest Qml Core)

qt_add_executable(
  JASPTest
  ${SYSTEM_TYPE}
  testall.h
  testall.cpp
  $<$<PLATFORM_ID:Darwin>:${_R_Framework}>
  $<$<PLATFORM_ID:Windows>:${CMAKE_SOURCE_DIR}/Desktop/JASP.exe.manifest>
  testinfo.h
)

qt_add_executable(
  JASPTestDebugData
  ${SYSTEM_TYPE}
  $<$<PLATFORM_ID:Darwin>:${_R_Framework}>
  $<$<PLATFORM_ID:Windows>:${CMAKE_SOURCE_DIR}/Desktop/JASP.exe.manifest>
  testdebugdata.h testdebugdata.cpp
)

qt_add_executable(
  JASPTestEngine
  ${SYSTEM_TYPE}
  $<$<PLATFORM_ID:Darwin>:${_R_Framework}>
  $<$<PLATFORM_ID:Windows>:${CMAKE_SOURCE_DIR}/Desktop/JASP.exe.manifest>
  testengine.h testengine.cpp
)

qt_add_executable(
  JASPQuickTest
  ${SYSTEM_TYPE}
  testqml.h
  testqml.cpp
  $<$<PLATFORM_ID:Darwin>:${_R_Framework}>
  $<$<PLATFORM_ID:Windows>:${CMAKE_SOURCE_DIR}/Desktop/JASP.exe.manifest>
)

qt_add_executable(
  JASPTestCsvPrev
  ${SYSTEM_TYPE}
  testcsvpreviewmodel.h
  testcsvpreviewmodel.cpp
  $<$<PLATFORM_ID:Darwin>:${_R_Framework}>
  $<$<PLATFORM_ID:Windows>:${CMAKE_SOURCE_DIR}/Desktop/JASP.exe.manifest>
)

qt_add_executable(
  JASPTestColumnEncoderContext
  ${SYSTEM_TYPE}
  testcolumnencodercontext.h
  testcolumnencodercontext.cpp
  $<$<PLATFORM_ID:Darwin>:${_R_Framework}>
  $<$<PLATFORM_ID:Windows>:${CMAKE_SOURCE_DIR}/Desktop/JASP.exe.manifest>
)

qt_add_executable(
  JASPTestDbMigration
  ${SYSTEM_TYPE}
  testdatabasemigration.h
  testdatabasemigration.cpp
  $<$<PLATFORM_ID:Darwin>:${_R_Framework}>
  $<$<PLATFORM_ID:Windows>:${CMAKE_SOURCE_DIR}/Desktop/JASP.exe.manifest>
)


qt_add_executable(
  JASPTestParsedArgs
  ${SYSTEM_TYPE}
  testparsedarguments.h
  testparsedarguments.cpp
  $<$<PLATFORM_ID:Darwin>:${_R_Framework}>
  $<$<PLATFORM_ID:Windows>:${CMAKE_SOURCE_DIR}/Desktop/JASP.exe.manifest>
)

add_dependencies(JASPTest           JASPDesktopLib)
add_dependencies(JASPTestDebugData  JASPDesktopLib)
add_dependencies(JASPTestEngine     JASPDesktopLib)
add_dependencies(JASPQuickTest      JASPDesktopLib)
add_dependencies(JASPTestCsvPrev    JASPDesktopLib)
add_dependencies(JASPTestDbMigration JASPDesktopLib)
add_dependencies(JASPTestColumnEncoderContext Common)
add_dependencies(JASPTestParsedArgs   JASPDesktopLib)

target_include_directories(
  JASPTest
  PUBLIC
  JASPDesktopLib
)

target_include_directories(
  JASPTestDebugData
  PUBLIC
  JASPDesktopLib

  PRIVATE
  Qt6::Core
)

target_include_directories(
  JASPTestEngine
  PUBLIC
  JASPDesktopLib
)

target_include_directories(
  JASPQuickTest
  PUBLIC
  JASPDesktopLib
)

target_include_directories(
  JASPTestCsvPrev
  PUBLIC
  JASPDesktopLib
)

target_include_directories(
  JASPTestDbMigration
  PUBLIC
  JASPDesktopLib
)

target_include_directories(
  JASPTestColumnEncoderContext
  PUBLIC
  ${PROJECT_SOURCE_DIR}/Common
)

target_include_directories(
  JASPTestParsedArgs
  PUBLIC
  JASPDesktopLib
)

target_link_libraries(
  JASPTest
  PUBLIC
  JASPDesktopLib
  Qt::Test

  PRIVATE
  Qt6::Core
)

target_link_libraries(
  JASPTestDebugData
  PUBLIC
  JASPDesktopLib
  Qt::Test

  PRIVATE
  Qt6::Core
)

target_link_libraries(
  JASPQuickTest
  PUBLIC
  JASPDesktopLib
  Qt::QuickTest

  PRIVATE
  Qt6::Core
  Qt6::Qml

  Qt::WebEngineQuick
  Qt::WebChannel
  Qt::WebChannelQuick
)

target_link_libraries(
  JASPTestEngine
  PUBLIC
  JASPDesktopLib
  Qt::Test
)

target_link_libraries(
  JASPTestCsvPrev
  PUBLIC
  JASPDesktopLib
  Qt::Test
)

target_link_libraries(
  JASPTestDbMigration
  PUBLIC
  JASPDesktopLib
  Qt::Test
)

target_link_libraries(
  JASPTestColumnEncoderContext
  PUBLIC
  Common
  Qt::Test
)

target_link_libraries(
  JASPTestParsedArgs
  PUBLIC
  JASPDesktopLib
  Qt::Test

  PRIVATE
  Qt6::Core
)

target_compile_definitions(
  JASPTest
  PUBLIC TESTLIBRARY_DIR=${CMAKE_SOURCE_DIR}/Tests/TestLibrary
)

target_compile_definitions(
  JASPTestDebugData
  PUBLIC TESTLIBRARY_DIR=${CMAKE_SOURCE_DIR}/Tests/TestLibrary
)

target_compile_definitions(
  JASPTestEngine
  PUBLIC TESTLIBRARY_DIR=${CMAKE_SOURCE_DIR}/Tests/TestLibrary
)

target_compile_definitions(
  JASPTestCsvPrev
  PUBLIC TESTLIBRARY_DIR=${CMAKE_SOURCE_DIR}/Tests/TestLibrary
)

target_compile_definitions(
  JASPTestParsedArgs
  PUBLIC TESTLIBRARY_DIR=${CMAKE_SOURCE_DIR}/Tests/TestLibrary
)



target_compile_definitions(
  JASPQuickTest
  PUBLIC 
  TESTLIBRARY_DIR=${CMAKE_SOURCE_DIR}/Tests/TestLibrary
  -DQUICK_TEST_SOURCE_DIR="${CMAKE_CURRENT_SOURCE_DIR}/qmlTests/"
)

enable_testing(true)

add_test(NAME testDataImport               COMMAND JASPTest)
add_test(NAME testParsedArgs               COMMAND JASPTestParsedArgs)
add_test(NAME testCsvPreview               COMMAND JASPTestCsvPrev)
add_test(NAME testDbMigration              COMMAND JASPTestDbMigration)
add_test(NAME testDebugData                COMMAND JASPTestDebugData)
add_test(NAME testEngine                   COMMAND JASPTestEngine)
add_test(NAME testQuick                    COMMAND JASPQuickTest)
add_test(NAME testColumnEncoderContext     COMMAND JASPTestColumnEncoderContext)



endif()
