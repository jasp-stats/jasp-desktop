
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

LIBS += -L.. -lJASP-Common

macx {

	INCLUDEPATH += ../../boost_1_54_0

	R_HOME = $$OUT_PWD/../../Frameworks/R.framework/Versions/3.1/Resources
	R_EXE  = $$R_HOME/bin/R
	R_LIB  = $$R_HOME/library
}

linux {

	RSCRIPT = $$(RSCRIPT)
	isEmpty(RSCRIPT) { RSCRIPT = $$system(which Rscript) }

	R_HOME = $$system( $$RSCRIPT -e \'cat(R.home())\' )
	R_EXE  = $$R_HOME/bin/R
	R_LIB  = $$system( $$RSCRIPT -e \'cat(.libPaths()[1])\' )

	QMAKE_CXXFLAGS += $$system( $$R_EXE CMD config --cppflags )
	LIBS           += $$system( $$R_EXE CMD config --ldflags )
	LIBS           += $$system( $$R_EXE CMD config BLAS_LIBS )
	QMAKE_CXXFLAGS += $$system( $$RSCRIPT -e \'cat(Rcpp:::CxxFlags())\' )
	LDFLAGS        += $$system( $$RSCRIPT -e \'cat(Rcpp:::LdFlags())\' )
	LIBS           += -Wl,-rpath,$$R_HOME/lib

	# RInside doesn't expose its build options in a sane way,
	# so lets add them manually
	INCLUDEPATH += $$R_LIB/RInside/include
	LIBS        += -L$$R_LIB/RInside/lib -lRInside -Wl,-rpath,$$R_LIB/RInside/lib
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
# to $$R_LIB. This way, we can call `make` without super-user privileges even if
# we want to install into a site-wide directory
RPACKAGE = $$PWD/lib/JASP
RPackage.target   = $$RPACKAGE
RPackage.commands = mkdir -p $$PWD/lib && $$R_EXE CMD INSTALL --library=$$PWD/lib $$PWD/JASP
RPackageRelocate.files = $$RPACKAGE
RPackageRelocate.path = $$R_LIB

QMAKE_EXTRA_TARGETS += RPackage
PRE_TARGETDEPS += $$RPACKAGE
INSTALLS += RPackageRelocate

SOURCES += main.cpp \
	engine.cpp \
	rcppbridge.cpp

HEADERS += \
	engine.h \
	analysistask.h \
	rcppbridge.h

RESOURCES +=
