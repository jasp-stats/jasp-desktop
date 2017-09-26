QT += core gui webkit webkitwidgets svg network printsupport xml

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

CONFIG += c++11

DESTDIR = ..

windows:TARGET = JASP
   macx:TARGET = JASP
  linux:TARGET = jasp

TEMPLATE = app

DEPENDPATH = ..

CONFIG -= app_bundle

INCLUDEPATH += ../JASP-Common/

   macx:INCLUDEPATH += ../../boost_1_64_0
windows:INCLUDEPATH += ../../boost_1_64_0

PRE_TARGETDEPS += ../libJASP-Common.a

LIBS += -L.. -lJASP-Common

windows:LIBS += -lboost_filesystem-mgw48-mt-1_64 -lboost_system-mgw48-mt-1_64 -larchive.dll
   macx:LIBS += -lboost_filesystem-clang-mt-1_64 -lboost_system-clang-mt-1_64 -larchive -lz
  linux:LIBS += -lboost_filesystem    -lboost_system    -larchive

windows:LIBS += -lole32 -loleaut32
  linux:LIBS += -lrt

QMAKE_CXXFLAGS_WARN_ON += -Wno-unused-parameter -Wno-unused-local-typedef
macx:QMAKE_CXXFLAGS += -Wno-c++11-extensions
macx:QMAKE_CXXFLAGS += -Wno-c++11-long-long
macx:QMAKE_CXXFLAGS += -Wno-c++11-extra-semi
macx:QMAKE_CXXFLAGS += -stdlib=libc++

windows:QMAKE_CXXFLAGS += -DBOOST_USE_WINDOWS_H

linux {
    _R_HOME = $$(R_HOME)
    isEmpty(_R_HOME):_R_HOME = /usr/lib/R
    QMAKE_CXXFLAGS += -D\'R_HOME=\"$$_R_HOME\"\'
}

include(JASP-Desktop.pri)

# List all pri files in the analysis
defineReplace(list_pri_files) {
    FILES = $$files($$1)
    PRI_FILES =
    for(file, $$list($$FILES)) {
        exists($$file)
        {
            PRI_FILES *= $$find(file, .*\.pri)
            PRI_FILES *= $$list_pri_files($$file/*)
        }
    }
    return($$PRI_FILES)
}

# Directory containing the analysis forms
ANALYSIS_DIR = $$PWD/analysisforms
# Directory containing the modules
MODULES_DIR = $$list_pri_files($$ANALYSIS_DIR)

# Include all the module pri files
for(file, $$list($$MODULES_DIR)) {
    include($$file)
}
