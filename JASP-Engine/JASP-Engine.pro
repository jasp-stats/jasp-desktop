
QT -= core
QT -= gui

windows:CONFIG += c++11

DESTDIR = ..
TARGET = JASPEngine
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

DEPENDPATH = ..

PRE_TARGETDEPS += ../libJASP-Common.a

LIBS += -L.. -lJASP-Common

windows:LIBS += -lboost_filesystem-mt -lboost_system-mt -larchive.dll
   macx:LIBS += -lboost_filesystem-mt -lboost_system-mt -larchive -lz
  linux:LIBS += -lboost_filesystem    -lboost_system    -larchive

macx {

    INCLUDEPATH += ../../boost_1_54_0

    R_HOME = $$OUT_PWD/../../Frameworks/R.framework/Versions/3.1/Resources
    R_EXE  = $$R_HOME/bin/R
}

linux {

    R_HOME = /usr/lib/R
    R_EXE  = $$R_HOME/bin/R
}

windows {

    COMPILER_DUMP = $$system(g++ -dumpmachine)
    contains(COMPILER_DUMP, x86_64-w64-mingw32) {

        ARCH = x64

    } else {

        ARCH = i386
    }

    INCLUDEPATH += ../../boost_1_54_0
    R_HOME = $$OUT_PWD/../R
    R_EXE  = $$R_HOME/bin/$$ARCH/R
}

QMAKE_CXXFLAGS += -Wno-c++11-extensions
QMAKE_CXXFLAGS += -Wno-unused-parameter
QMAKE_CXXFLAGS += -Wno-c++11-long-long
QMAKE_CXXFLAGS += -Wno-c++11-extra-semi

win32:QMAKE_CXXFLAGS += -DBOOST_USE_WINDOWS_H

INCLUDEPATH += \
    $$R_HOME/include \
    $$R_HOME/library/RInside/include \
    $$R_HOME/library/Rcpp/include

linux:INCLUDEPATH += \
    /usr/share/R/include \
    $$R_HOME/site-library/RInside/include \
    $$R_HOME/site-library/Rcpp/include

macx:LIBS += \
    -L$$R_HOME/library/RInside/lib -lRInside \
    -L$$R_HOME/lib -lR

linux:LIBS += \
    -L$$R_HOME/library/RInside/lib \
    -L$$R_HOME/site-library/RInside/lib -lRInside \
    -L$$R_HOME/lib -lR \
    -lrt

win32:LIBS += \
    -L$$R_HOME/library/RInside/lib/$$ARCH -lRInside \
    -L$$R_HOME/bin/$$ARCH -lR

win32:LIBS += -lole32 -loleaut32

RPackage.commands = $$R_EXE CMD INSTALL --library=$$OUT_PWD/.. $$PWD/JASP

QMAKE_EXTRA_TARGETS += RPackage
PRE_TARGETDEPS += RPackage

SOURCES += main.cpp \
    engine.cpp \
    rbridge.cpp

HEADERS += \
    engine.h \
    rbridge.h
