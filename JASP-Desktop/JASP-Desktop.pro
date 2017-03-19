
QT += core gui webkit webkitwidgets svg network printsupport xml

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

windows:CONFIG += c++11
linux:CONFIG += c++11


DESTDIR = ..

windows:TARGET = JASP
   macx:TARGET = JASP
  linux:TARGET = jasp

TEMPLATE = app

DEPENDPATH = ..

CONFIG -= app_bundle

INCLUDEPATH += ../JASP-Common/

   macx:INCLUDEPATH += ../../boost_1_54_0
windows:INCLUDEPATH += ../../boost_1_54_0

PRE_TARGETDEPS += ../libJASP-Common.a

LIBS += -L.. -lJASP-Common

windows:LIBS += -lboost_filesystem-mt -lboost_system-mt -larchive.dll
   macx:LIBS += -lboost_filesystem-mt -lboost_system-mt -larchive -lz
  linux:LIBS += -lboost_filesystem    -lboost_system    -larchive

windows:LIBS += -lole32 -loleaut32
  linux:LIBS += -lrt

QMAKE_CXXFLAGS_WARN_ON += -Wno-unused-parameter -Wno-unused-local-typedef
macx:QMAKE_CXXFLAGS += -Wno-c++11-extensions
macx:QMAKE_CXXFLAGS += -Wno-c++11-long-long
macx:QMAKE_CXXFLAGS += -Wno-c++11-extra-semi

windows:QMAKE_CXXFLAGS += -DBOOST_USE_WINDOWS_H

linux {
        _R_HOME = $$(R_HOME)
        isEmpty(_R_HOME):_R_HOME = /usr/lib/R
        QMAKE_CXXFLAGS += -D\'R_HOME=\"$$_R_HOME\"\'
}

include(JASP-Desktop.pri)

HEADERS +=

SOURCES +=
