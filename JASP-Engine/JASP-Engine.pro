
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

JASP_R_LIBRARY = $$PREFIX/lib/JASP/R/library

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
	LIBS           += -Wl,-rpath,$$R_HOME/lib

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
JaspRLib.commands = mkdir -p $$JASP_R_LIB_BUILD

RPACKAGE = $$PWD/lib/JASP
RPackage.target   = $$RPACKAGE
RPackage.commands = $$R_EXE CMD INSTALL --library=$$JASP_R_LIB_BUILD $$PWD/JASP
RPackage.depends = JaspRLib

RLibRelocate.files = $$JASP_R_LIB_BUILD/*
RLibRelocate.path  = $$JASP_R_LIBRARY
DEFINES           += JASP_R_LIBRARY=$$JASP_R_LIBRARY

QMAKE_EXTRA_TARGETS += RPackage JaspRLib
PRE_TARGETDEPS += $$RPACKAGE
INSTALLS += RLibRelocate JASPEngine

with_dependencies {
	CRAN = http://cran.r-project.org/src/contrib
	ARCHIVES = $$PWD/archives

	RCPP                    = Rcpp_0.11.3.tar.gz
	ArchiveRcpp.target      = $$ARCHIVES/$$RCPP
	ArchiveRcpp.commands    = cd $$ARCHIVES && wget $$CRAN/$$RCPP
	Rcpp.target             = $$JASP_R_LIB_BUILD/Rcpp
	Rcpp.commands           = $$R_EXE CMD INSTALL --library=$$JASP_R_LIB_BUILD $$ArchiveRcpp.target
	Rcpp.depends            = ArchiveRcpp
	QMAKE_EXTRA_TARGETS     += Rcpp ArchiveRcpp
	RPackage.depends        += Rcpp

	RINSIDE                 = RInside_0.2.11.tar.gz
	ArchiveRInside.target   = $$ARCHIVES/$$RINSIDE
	ArchiveRInside.commands = cd $$ARCHIVES && wget $$CRAN/$$RINSIDE
	RInside.target          = $$JASP_R_LIB_BUILD/RInside
	# FIXME: this seems to put $$JASP_R_LIB_BUILD into .libPaths() when running an RInside interpreter
	# maybe we should give up on this trick of relocating from the build dir $$JASP_R_LIB_BUILD to
	# the library dir $$JASP_R_LIBRARY
	RInside.commands        = $$R_EXE CMD INSTALL --library=$$JASP_R_LIB_BUILD $$ArchiveRInside.target
	RInside.depends         = ArchiveRInside Rcpp
	QMAKE_EXTRA_TARGETS     += RInside ArchiveRInside
	RPackage.depends        += RInside

	# RInside doesn't expose its build options in a sane way,
	# so lets add them manually
	INCLUDEPATH             += $$JASP_R_LIB_BUILD/RInside/include
	# note that we link NOW against $$JASP_R_LIB_BUILD, but at runtime we need to use -rpath $$JASP_R_LIBRARY
	LIBS                    += -L$$JASP_R_LIB_BUILD/RInside/lib -lRInside -Wl,-rpath,$$JASP_R_LIBRARY/RInside/lib

	# similarly, we cannot query Rcpp:::*Flags until we've installed it. But since we're in
	# control of this packages' compilation, we might as well add them verbatim
	INCLUDEPATH             += $$JASP_R_LIB_BUILD/Rcpp/include
} else {
	QMAKE_CXXFLAGS += $$system( $$RSCRIPT -e \'cat(Rcpp:::CxxFlags())\' )
	LDFLAGS        += $$system( $$RSCRIPT -e \'cat(Rcpp:::LdFlags())\' )

	# RInside doesn't expose its build options in a sane way,
	# so lets add them manually
	INCLUDEPATH += $$R_LIB/RInside/include
	LIBS        += -L$$R_LIB/RInside/lib -lRInside -Wl,-rpath,$$R_LIB/RInside/lib
}
SOURCES += main.cpp \
	engine.cpp \
	rcppbridge.cpp

HEADERS += \
	engine.h \
	analysistask.h \
	rcppbridge.h

RESOURCES +=
