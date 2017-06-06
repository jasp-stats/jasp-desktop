
QT += core gui webkit webkitwidgets svg network printsupport xml

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

windows:CONFIG += c++11
linux:CONFIG += c++11
linux:CONFIG += -pipe

DESTDIR = ..
OBJECTS_DIR = ../JASP-Desktop
MOC_DIR = ../JASP-Desktop
RCC_DIR = ../JASP-Desktop
UI_DIR	= ../JASP-Desktop

TARGET = JASP-Desktop
TEMPLATE = lib
CONFIG += staticlib


DEPENDPATH = ..

CONFIG -= app_bundle

INCLUDEPATH += ../JASP-Desktop/ \
        ../JASP-Common/ \
        ../JASP-Engine/

INCLUDEPATH += ../JASP-Common/
INCLUDEPATH += ../icu-connector/

   macx:INCLUDEPATH += ../../boost_1_64_0
windows:INCLUDEPATH += ../../boost_1_64_0

PRE_TARGETDEPS += ../libJASP-Common.a

LIBS += -L.. -lJASP-Common
 macx:LIBS += -licu-connector

windows:LIBS += -lboost_filesystem-mgw48-mt-1_64 -lboost_system-mgw48-mt-1_64 -larchive.dll
   macx:LIBS += -lboost_filesystem-clang-mt-1_64 -lboost_system-clang-mt-1_64 -larchive -lz
  linux:LIBS += -lboost_filesystem    -lboost_system    -larchive

windows:LIBS += -lole32 -loleaut32
  linux:LIBS += -lrt

QMAKE_CXXFLAGS_WARN_ON += -Wno-unused-parameter -Wno-unused-local-typedef
macx:QMAKE_CXXFLAGS += -Wno-c++11-extensions
macx:QMAKE_CXXFLAGS += -Wno-c++11-long-long
macx:QMAKE_CXXFLAGS += -Wno-c++11-extra-semi

QMAKE_CXXFLAGS += -DBOOST_USE_WINDOWS_H

linux {
	_R_HOME = $$(R_HOME)
	isEmpty(_R_HOME):_R_HOME = /usr/lib/R
	QMAKE_CXXFLAGS += -D\'R_HOME=\"$$_R_HOME\"\'
}


include(../JASP-Desktop/JASP-Desktop.pri)
