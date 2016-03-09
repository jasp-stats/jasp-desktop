
Unit Tests
==========

Guide to build Unit test project and run (for Linux)
----------------------------------------------------
(JASP project must be built before building this)

1) use Qt creator and open the file Test.pro

2) use qmake from terminal

    qmake Test.pro
    make 
    ./UnitTest

Adding new unit tests to the project
------------------------------------

To add new test (for example Tester), include "AutomatedTests.h" in the header file and the last line in the file should be 
    DECLARE_TEST(Tester)

Update the Test.pro file to include the source file and the header file

The test will run automatically when the project is built and run.


Unit Tests in the project
-------------------------

1) Opening of Text files (tests the AsyncLoader class)
2) OSF login authentication


