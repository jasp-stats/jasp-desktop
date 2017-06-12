
#QT -= core
QT -= gui
CURRENT_R_VERSION = 3.3

windows:CONFIG += c++11
macx:CONFIG += c++11
linux:CONFIG += c++11
linux:CONFIG += -pipe

DESTDIR = ..
TARGET = JASPEngine
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

DEPENDPATH = ..

PRE_TARGETDEPS += ../libJASP-Common.a

LIBS += -L.. -lJASP-Common

windows:LIBS += -lboost_filesystem-mgw48-mt-1_64 -lboost_system-mgw48-mt-1_64 -larchive.dll
   macx:LIBS += -lboost_filesystem-clang-mt-1_64 -lboost_system-clang-mt-1_64 -larchive -lz
  linux:LIBS += -lboost_filesystem    -lboost_system    -larchive

_R_HOME = $$(R_HOME)

 ! isEmpty(_R_HOME) : message(using R_HOME of $$_R_HOME)

macx {

	INCLUDEPATH += ../../boost_1_64_0

	isEmpty(_R_HOME):_R_HOME = $$OUT_PWD/../../Frameworks/R.framework/Versions/$$CURRENT_R_VERSION/Resources
	R_EXE  = $$_R_HOME/bin/R
}

linux {

	isEmpty(_R_HOME):_R_HOME = /usr/lib/R
	R_EXE  = $$_R_HOME/bin/R
}

windows {

	COMPILER_DUMP = $$system(g++ -dumpmachine)
	contains(COMPILER_DUMP, x86_64-w64-mingw32) {

		ARCH = x64

	} else {

		ARCH = i386
	}

	INCLUDEPATH += ../../boost_1_64_0

	isEmpty(_R_HOME):_R_HOME = $$OUT_PWD/../R
	R_EXE  = $$_R_HOME/bin/$$ARCH/R
}

QMAKE_CXXFLAGS_WARN_ON += -Wno-unused-parameter -Wno-unused-local-typedef
macx:QMAKE_CXXFLAGS += -Wno-c++11-extensions
macx:QMAKE_CXXFLAGS += -Wno-c++11-long-long
macx:QMAKE_CXXFLAGS += -Wno-c++11-extra-semi
macx:QMAKE_CXXFLAGS += -stdlib=libc++

win32:QMAKE_CXXFLAGS += -DBOOST_USE_WINDOWS_H

INCLUDEPATH += \
	$$_R_HOME/include \
	$$_R_HOME/library/Rcpp/include

linux:INCLUDEPATH += \
	/usr/share/R/include \
	$$_R_HOME/site-library/Rcpp/include

macx:LIBS += \
	-L$$_R_HOME/lib -lR

linux:LIBS += \
	-L$$_R_HOME/lib -lR \
	-lrt

win32:LIBS += \
	-L$$_R_HOME/bin/$$ARCH -lR

win32:LIBS += -lole32 -loleaut32

mkpath($$OUT_PWD/../R/library)

InstallJASPRPackage.commands += \"$$R_EXE\" CMD INSTALL --library=$$OUT_PWD/../R/library $$PWD/JASP

QMAKE_EXTRA_TARGETS += InstallJASPRPackage
PRE_TARGETDEPS      += InstallJASPRPackage

SOURCES += main.cpp \
	engine.cpp \
	rbridge.cpp \
        RInside/MemBuf.cpp \
        RInside/RInside.cpp
  
HEADERS += \
	engine.h \
	rbridge.h \
        RInside/Callbacks.h \
        RInside/MemBuf.h \
        RInside/RInside.h \
        RInside/RInsideAutoloads.h \
        RInside/RInsideCommon.h \
        RInside/RInsideConfig.h \
        RInside/RInsideEnvVars.h
