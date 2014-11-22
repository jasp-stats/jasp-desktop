
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

INCLUDEPATH += ..

LIBS += -L.. -lJASP-Common

include(../common.pri)

linux {
	target.path = $$PREFIX/bin
	INSTALLS += target
}

macx {

	INCLUDEPATH += ../../boost_1_54_0

	R_HOME = $$OUT_PWD/../../Frameworks/R.framework/Versions/3.1/Resources
	R_EXE  = $$R_HOME/bin/R
	R_LIB  = $$R_HOME/library
}

linux {

	isEmpty(RSCRIPT) { RSCRIPT = $$system(which Rscript) }

	use_jasps_own_r_binary_package {
		R_HOME = $$JASPS_OWN_R_BINARY_PACKAGE
		R_LIB = $$R_HOME/library
		R_EXE  = $$R_HOME/bin/R

		QMAKE_CXXFLAGS += -I$$R_HOME/include
		LIBS           += -Wl,--export-dynamic -fopenmp  -L$$R_HOME/lib -lR -lpcre -llzma -lbz2 -lz -lrt -ldl -lm
		LIBS           += -lblas
		LIBS           += -Wl,-rpath,$$R_HOME/lib
	} else {
		R_HOME = $$system( $$RSCRIPT -e \'cat(R.home())\' )
		R_LIB  = $$system( $$RSCRIPT -e \'cat(.libPaths()[1])\' )
		R_EXE  = $$R_HOME/bin/R

		QMAKE_CXXFLAGS += $$system( $$R_EXE CMD config --cppflags )
		LIBS           += $$system( $$R_EXE CMD config --ldflags )
		LIBS           += $$system( $$R_EXE CMD config BLAS_LIBS )
		LIBS           += -Wl,-rpath,$$R_HOME/lib
        }
}

windows {

	COMPILER_DUMP = $$system(g++ -dumpmachine)
	contains(COMPILER_DUMP, x86_64-w64-mingw32) {

		ARCH = x64
		INCLUDEPATH += ../../boost_1_54_0

	} else {

		ARCH = i386
		INCLUDEPATH += ../../boost_1_53_0
	}

	R_HOME = $$OUT_PWD/../R
	R_EXE  = $$R_HOME/bin/$$ARCH/R
	R_LIB  = $$R_HOME/library
}

QMAKE_CXXFLAGS += -Wno-c++11-extensions
QMAKE_CXXFLAGS += -Wno-unused-parameter
QMAKE_CXXFLAGS += -Wno-c++11-long-long
QMAKE_CXXFLAGS += -Wno-c++11-extra-semi

QMAKE_CXXFLAGS += -DBOOST_USE_WINDOWS_H

win32,macx:INCLUDEPATH += \
	$$R_HOME/include \
	$$R_LIB/RInside/include \
	$$R_LIB/Rcpp/include

macx:LIBS += \
	-L$$R_HOME/library/RInside/lib -lRInside \
	-L$$R_HOME/lib -lR

win32:LIBS += \
	-L$$R_HOME/library/RInside/lib/$$ARCH -lRInside \
	-L$$R_HOME/bin/$$ARCH -lR

win32:LIBS += -lole32 -loleaut32

# We build the JASP package in a phony directory, then copy it at install time
# to $$JASP_R_LIBRARY. This way, we can call `make` without super-user privileges even if
# we want to install into a site-wide directory
JASP_R_LIB_BUILD = $$PWD/lib
JaspRLib.target = $$JASP_R_LIB_BUILD
JaspRLib.commands = $$QMAKE_MKDIR $$JASP_R_LIB_BUILD

RPACKAGE = $$PWD/lib/JASP
RPackage.target   = $$RPACKAGE
RPackage.commands = $$R_EXE CMD INSTALL --library=$$JASP_R_LIB_BUILD $$PWD/JASP && touch -c $$RPACKAGE
RPackage.depends = JaspRLib

RLibRelocate.files = $$JASP_R_LIB_BUILD/*
RLibRelocate.path  = $$JASP_R_LIBRARY

QMAKE_EXTRA_TARGETS += RPackage JaspRLib
PRE_TARGETDEPS += $$RPACKAGE
INSTALLS += RLibRelocate

include(Dependencies.pri)

SOURCES += main.cpp \
	engine.cpp \
	rcppbridge.cpp

HEADERS += \
	engine.h \
	analysistask.h \
	rcppbridge.h

RESOURCES +=
