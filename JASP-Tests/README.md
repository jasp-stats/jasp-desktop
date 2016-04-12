
Unit Tests
==========

Guide to build Unit test project and run 
-----------------------------------------

1) use Qt creator and build the project JASP.pro

2) use qmake from terminal (in a different build directory)

    qmake ../jasp-desktop/JASP.pro
    make 
    ./JASPTests

Adding new unit tests to the project
------------------------------------

To add new test (for example Tester), include "AutomatedTests.h" in the header file and the last line in the file should be 
    DECLARE_TEST(Tester)

Update the JASP-Tests-app.pro file to include the source file and the header file

The test will run automatically when the project is built and run.


Unit Tests in the project
-------------------------

1) Opening of Text files (tests the AsyncLoader class)

2) Testing of OSF login and file opening

3) CSV importer 

4) SPSS importer

