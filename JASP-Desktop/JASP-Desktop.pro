QT += core gui webenginewidgets webchannel svg network printsupport xml

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

#exists(/app/lib/*) should only be true when building flatpak
exists(/app/lib/*)	{ target.path += /app/bin }
else			{
	target.path += /usr/bin
}

INSTALLS += target

   macx:INCLUDEPATH += ../../boost_1_64_0
windows:INCLUDEPATH += ../../boost_1_64_0

LIBS += -L.. -lJASP-Common

windows:CONFIG(ReleaseBuild) {
    LIBS += -llibboost_filesystem-vc141-mt-1_64 -llibboost_system-vc141-mt-1_64 -larchive.dll
}

windows:CONFIG(DebugBuild) {
    LIBS += -llibboost_filesystem-vc141-mt-gd-1_64 -llibboost_system-vc141-mt-gd-1_64 -larchive.dll
}

   macx:LIBS += -lboost_filesystem-clang-mt-1_64 -lboost_system-clang-mt-1_64 -larchive -lz
windows:LIBS += -lole32 -loleaut32

linux {
	exists(/app/lib/*)	{ LIBS += -larchive -lrt -L/app/lib -lboost_filesystem -lboost_system
	} else				{ LIBS += -larchive -lrt -ljsoncpp -lboost_filesystem -lboost_system }
}

macx:QMAKE_CXXFLAGS_WARN_ON += -Wno-unused-parameter -Wno-unused-local-typedef
macx:QMAKE_CXXFLAGS += -Wno-c++11-extensions
macx:QMAKE_CXXFLAGS += -Wno-c++11-long-long
macx:QMAKE_CXXFLAGS += -Wno-c++11-extra-semi
macx:QMAKE_CXXFLAGS += -stdlib=libc++

windows:QMAKE_CXXFLAGS += -DBOOST_USE_WINDOWS_H -DNOMINMAX -D__WIN32__ -DBOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED

linux {
    _R_HOME = $$(R_HOME)
    isEmpty(_R_HOME):_R_HOME = /usr/lib/R
    QMAKE_CXXFLAGS += -D\'R_HOME=\"$$_R_HOME\"\'
}

macx | windows | exists(/app/lib/*) { DEFINES += JASP_LIBJSON_STATIC
} else	{ linux:LIBS += -ljsoncpp }

INCLUDEPATH += $$PWD/../JASP-Common/

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


exists(/app/lib/*) {
	flatpak_desktop.files = ../Tools/flatpak/jasp.desktop
	flatpak_desktop.path = /app/share/applications/
	INSTALLS += flatpak_desktop

	flatpak_icon.files = ../Tools/flatpak/jasp.svg
	flatpak_icon.path = /app/share/icons/hicolor/scalable/apps
	INSTALLS += flatpak_icon
} else {
	CONFIG(debug, debug|release) {  DEFINES+=JASP_DEBUG }
}

