#-------------------------------------------------
#
# Project created by QtCreator 2016-04-05T15:17:11
#
#-------------------------------------------------

QT       -= core gui

DESTDIR = ..
TARGET = icu-connector
TEMPLATE = lib
CONFIG += staticlib

linux:CONFIG += c++11

#
# Include ICU headers:
# On Mac use our own except from IBM source code,
# and link with OS supplied lib. (See jasp-desktop.pro)
# See blog.lukhnos.org/6441462604/using-os-xs-built-in-icu-library-in-your-own
# Some (system) dependent header for ICU are supplied with QT sources,
# so we put the QT directory ahead of the ICU excerpt.
mac: INCLUDEPATH += /Users/aknight1/Qt/5.2.1/Src/qtwebkit/Source/WebKit/mac/icu
mac: INCLUDEPATH +=../../icu-src/include
mac: QMAKE_CXXFLAGS += -DU_NOEXCEPT= -DU_DISABLE_RENAMING
#
# Linux buids use the system supplied ICU implementation.
# unicode on the system library path.
#


#
# Boost: For Mac And 'doze only.
#
macx:INCLUDEPATH += ../../boost_1_54_0

QMAKE_CXXFLAGS += -Wno-c++11-extensions
QMAKE_CXXFLAGS += -Wno-unused-parameter
QMAKE_CXXFLAGS += -Wno-c++11-long-long
QMAKE_CXXFLAGS += -Wno-c++11-extra-semi

SOURCES += icuconnector.cpp \
    spsscpconvert.cpp

HEADERS += icuconnector.h \
    icu-names.h \
    spsscpconvert.h
