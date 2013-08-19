
QT       -= core
QT       -= gui

windows:CONFIG += c++11

DESTDIR = ..
TARGET = JASPEngine
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

DEPENDPATH = ..

PRE_TARGETDEPS += ../libJASP-Common.a

unix:INCLUDEPATH += /opt/local/include
windows:INCLUDEPATH += C:/progra~1/boost/boost_1_53_0

INCLUDEPATH += R-3.0.0/include \
	R-3.0.0/library/Rinside/include \
	R-3.0.0/library/Rcpp/include

LIBS += -L.. -lJASP-Common

unix {

R_HOME = R-3.0.0

## include headers and libraries for R
RCPPFLAGS =             $$system($$R_HOME/bin/R CMD config --cppflags)
RLDFLAGS =              $$system($$R_HOME/bin/R CMD config --ldflags)
RBLAS =                 $$system($$R_HOME/bin/R CMD config BLAS_LIBS)
RLAPACK =               $$system($$R_HOME/bin/R CMD config LAPACK_LIBS)

## if you need to set an rpath to R itself, also uncomment
#RRPATH =               -Wl,-rpath,$$R_HOME/lib

## include headers and libraries for Rcpp interface classes
RCPPINCL =              $$system($$R_HOME/bin/Rscript -e \'Rcpp:::CxxFlags\(\)\')
RCPPLIBS =              $$system($$R_HOME/bin/Rscript -e \'Rcpp:::LdFlags\(\)\')

## for some reason when building with Qt we get this each time
## so we turn unused parameter warnings off
RCPPWARNING =           -Wno-unused-parameter
## include headers and libraries for RInside embedding classes
RINSIDEINCL =           $$system($$R_HOME/bin/Rscript -e \'RInside:::CxxFlags\(\)\')
RINSIDELIBS =           $$system($$R_HOME/bin/Rscript -e \'RInside:::LdFlags\(\)\')

message($$system(pwd))

}

win32 {

LIBS += -lole32 -loleaut32

## comment this out if you need a different version of R,
## and set set R_HOME accordingly as an environment variable
## R_HOME =                $$system(R RHOME)
#R_HOME = Y:\\Documents\\build-JASP-win-Desktop_Qt_5_0_2_MinGW_32bit-Debug\\R-3.0.0
R_HOME = Y:/Documents/build-JASPEngine-win-Rtools-Debug/R-3.0.0

## include headers and libraries for R
RCPPFLAGS =             $$system($$R_HOME/bin/R CMD config --cppflags)
RLDFLAGS =              $$system($$R_HOME/bin/R CMD config --ldflags)
RBLAS =                 $$system($$R_HOME/bin/R CMD config BLAS_LIBS)
RLAPACK =               $$system($$R_HOME/bin/R CMD config LAPACK_LIBS)

## if you need to set an rpath to R itself, also uncomment
#RRPATH =               -Wl,-rpath,$$R_HOME/lib

## include headers and libraries for Rcpp interface classes
RCPPINCL =              $$system($$R_HOME/bin/Rscript -e Rcpp:::CxxFlags\(\))
RCPPLIBS =              $$system($$R_HOME/bin/Rscript -e Rcpp:::LdFlags\(\))

LIBS += -L$$R_HOME/library/RInside/lib/i386 -lRInside -L$$R_HOME/library/Rcpp/lib/i386 -lRcpp -L$$R_HOME/bin/i386 -lR

## for some reason when building with Qt we get this each time
## so we turn unused parameter warnings off
RCPPWARNING =           -Wno-unused-parameter
## include headers and libraries for RInside embedding classes
RINSIDEINCL =           $$system($$R_HOME/bin/Rscript -e RInside:::CxxFlags\(\))
RINSIDELIBS =           $$system($$R_HOME/bin/Rscript -e RInside:::LdFlags\(\))

}

## compiler etc settings used in default make rules
QMAKE_CXXFLAGS +=       $$RCPPWARNING $$RCPPFLAGS $$RCPPINCL $$RINSIDEINCL
QMAKE_LFLAGS +=         $$RLDFLAGS $$RBLAS $$RLAPACK $$RCPPLIBS $$RINSIDELIBS

RPackage.commands = $$OUT_PWD/../R-3.0.0/bin/R CMD INSTALL $$PWD/JASP
QMAKE_EXTRA_TARGETS += RPackage
PRE_TARGETDEPS += RPackage

SOURCES += main.cpp \
    engine.cpp \
    rcppbridge.cpp

HEADERS += \
    engine.h \
	analysistask.h \
	rcppbridge.h

RESOURCES +=
