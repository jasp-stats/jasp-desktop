
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

include(../common.pri)

LIBS += -L.. -lJASP-Common

macx {

	INCLUDEPATH += ../../boost_1_54_0

	R_HOME = $$OUT_PWD/../../Frameworks/R.framework/Versions/3.1/Resources
	R_EXE  = $$R_HOME/bin/R
}

linux {

	isEmpty(RSCRIPT) {
		RSCRIPT = $$PWD/Rscript-wrapper
		system( perl -pe\'s/^R_HOME_DIR=.*\$/R_HOME_DIR=\"\\\$(dirname \"\\\$(dirname \"\\\$(readlink -f \"\\\$0\")\" )\" )\"/\' -i ../R/bin/R )
		system( perl -pe\'s+^R_INCLUDE_DIR=.*\$+R_INCLUDE_DIR=\"\\\${R_HOME_DIR}/include\"+\' -i ../R/bin/R )
	}
	R_HOME = $$system( $$RSCRIPT -e \'cat(R.home())\' )
	R_LIB  = $$system( $$RSCRIPT -e \'cat(.libPaths()[1])\' )
	R_EXE  = $$R_HOME/bin/R

	QMAKE_CXXFLAGS += $$system( $$R_EXE CMD config --cppflags )
	LIBS           += $$system( $$R_EXE CMD config --ldflags )
	LIBS           += $$system( $$R_EXE CMD config BLAS_LIBS )
	LIBS           += -Wl,-rpath,$$R_HOME/lib
	LIBS           += -Wl,-rpath,$$R_HOME/library/RInside/lib
	# workaround for https://github.com/RcppCore/Rcpp/issues/178 in case
	# R compiles with --export-dynamic
        LIBS           += -Wl,--no-export-dynamic
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

QMAKE_CXXFLAGS += -DBOOST_USE_WINDOWS_H

INCLUDEPATH += \
	$$R_HOME/include \
	$$R_HOME/library/RInside/include \
	$$R_HOME/library/Rcpp/include

unix:LIBS += \
	-L$$R_HOME/library/RInside/lib -lRInside

macx:LIBS += \
	-L$$R_HOME/lib -lR

win32:LIBS += \
	-L$$R_HOME/library/RInside/lib/$$ARCH -lRInside \
	-L$$R_HOME/bin/$$ARCH -lR

win32:LIBS += -lole32 -loleaut32

RPackage.commands = $$R_EXE CMD INSTALL --library=$$R_HOME/library $$PWD/JASP
RPackage.path = $$R_HOME/library
QMAKE_EXTRA_TARGETS += RPackage
INSTALLS += RPackage

SOURCES += main.cpp \
	engine.cpp \
	rcppbridge.cpp

HEADERS += \
	engine.h \
	analysistask.h \
	rcppbridge.h

RESOURCES +=
