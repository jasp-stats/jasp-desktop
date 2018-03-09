
QT += core
QT -= gui

include(../JASP.pri)

CONFIG += c++11
linux:CONFIG += -pipe

DESTDIR = ..
TARGET = JASPEngine
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

exists(/app/lib/*)	{ INSTALLPATH = /app/bin } else	{
	INSTALLPATH = /usr/bin
}

target.path = $$INSTALLPATH
INSTALLS += target

analysis_jsons.path = $$INSTALLPATH
analysis_jsons.files = ../Resources
INSTALLS += analysis_jsons

DEPENDPATH = ..
PRE_TARGETDEPS += ../JASP-Common
LIBS +=  -l$$JASP_R_INTERFACE_NAME -L.. -lJASP-Common

windows:CONFIG(ReleaseBuild) {
	LIBS += -llibboost_filesystem-vc141-mt-1_64 -llibboost_system-vc141-mt-1_64 -larchive.dll
}

windows:CONFIG(DebugBuild) {
	LIBS += -llibboost_filesystem-vc141-mt-gd-1_64 -llibboost_system-vc141-mt-gd-1_64 -larchive.dll
}

macx:LIBS += -lboost_filesystem-clang-mt-1_64 -lboost_system-clang-mt-1_64 -larchive -lz

linux {
	exists(/app/lib/*)	{ LIBS += -larchive -L/app/lib -lboost_filesystem -lboost_system }
	else				{ LIBS += -larchive -lboost_filesystem -lboost_system }
}


_R_HOME = $$(R_HOME)

 ! isEmpty(_R_HOME) : message(using R_HOME of $$_R_HOME)

macx {
	INCLUDEPATH += ../../boost_1_64_0

	isEmpty(_R_HOME):_R_HOME = $$OUT_PWD/../../Frameworks/R.framework/Versions/$$CURRENT_R_VERSION/Resources
	R_EXE  = $$_R_HOME/bin/R
}

linux {
	LIBS += -L$$_R_HOME/lib -lR -lrt

	isEmpty(_R_HOME):_R_HOME = /usr/lib/R
	R_EXE  = $$_R_HOME/bin/R
}

windows {
	contains(QT_ARCH, i386) {
		ARCH = i386
	} else {
		ARCH = x64
	}

	INCLUDEPATH += ../../boost_1_64_0

	isEmpty(_R_HOME):_R_HOME = $$OUT_PWD/../R
	R_EXE  = $$_R_HOME/bin/$$ARCH/R
}

macx | windows | exists(/app/lib/*) { DEFINES += JASP_LIBJSON_STATIC
} else { linux { LIBS += -ljsoncpp} }

INCLUDEPATH += $$PWD/../JASP-Common/

macx:QMAKE_CXXFLAGS_WARN_ON += -Wno-unused-parameter -Wno-unused-local-typedef
macx:QMAKE_CXXFLAGS += -Wno-c++11-extensions
macx:QMAKE_CXXFLAGS += -Wno-c++11-long-long
macx:QMAKE_CXXFLAGS += -Wno-c++11-extra-semi
macx:QMAKE_CXXFLAGS += -stdlib=libc++

win32:QMAKE_CXXFLAGS += -DBOOST_USE_WINDOWS_H -DNOMINMAX -D__WIN32__ -DBOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED

win32:LIBS += -lole32 -loleaut32

mkpath($$OUT_PWD/../R/library)

exists(/app/lib/*) {
	#for flatpak we can just use R's own library as it is contained anyway
	InstallJASPRPackage.commands		= \"$$R_EXE\" CMD INSTALL $$PWD/JASP
	InstallJASPgraphsRPackage.commands	= \"$$R_EXE\" CMD INSTALL $$PWD/JASPgraphs
} else {
	InstallJASPRPackage.commands		= \"$$R_EXE\" CMD INSTALL --library=$$OUT_PWD/../R/library $$PWD/JASP
	InstallJASPgraphsRPackage.commands	= \"$$R_EXE\" CMD INSTALL --library=$$OUT_PWD/../R/library $$PWD/JASPgraphs
	CONFIG(debug, debug|release) {  DEFINES+=JASP_DEBUG }
}

QMAKE_EXTRA_TARGETS += InstallJASPRPackage
PRE_TARGETDEPS      += InstallJASPRPackage
QMAKE_EXTRA_TARGETS += InstallJASPgraphsRPackage
PRE_TARGETDEPS      += InstallJASPgraphsRPackage


QMAKE_CLEAN += $$OUT_PWD/../R/library/*

SOURCES += main.cpp \
	engine.cpp \
    rbridge.cpp \
    r_functionwhitelist.cpp

HEADERS += \
	engine.h \
    rbridge.h \
    r_functionwhitelist.h


OTHER_FILES  += \
	JASP/R/ancova.R \
	JASP/R/ancovabayesian.R \
	JASP/R/ancovamultivariate.R \
	JASP/R/anova.R \
	JASP/R/anovabayesian.R \
	JASP/R/anovamultivariate.R \
	JASP/R/anovaoneway.R \
	JASP/R/anovarepeatedmeasures.R \
	JASP/R/anovarepeatedmeasuresbayesian.R \
	JASP/R/base64.R \
	JASP/R/binomialtest.R \
	JASP/R/binomialtestbayesian.R \
	JASP/R/common.R \
	JASP/R/commonbayesianlinearmodels.R \
	JASP/R/commonerrorcheck.R \
	JASP/R/commonglm.R \
	JASP/R/commonmessages.R \
	JASP/R/commonMPR.R \
	JASP/R/commonsummarystats.R \
	JASP/R/commonsummarystatsttestbayesian.R \
	JASP/R/commonTTest.R \
	JASP/R/contingencytables.R \
	JASP/R/contingencytablesbayesian.R \
	JASP/R/correlation.R \
	JASP/R/correlationbayesian.R \
	JASP/R/correlationbayesianpairs.R \
	JASP/R/correlationpartial.R \
	JASP/R/descriptives.R \
	JASP/R/exploratoryfactoranalysis.R \
	JASP/R/packagecheck.R \
	JASP/R/principalcomponentanalysis.R \
	JASP/R/r11tlearn.R \
	JASP/R/regressionlinear.R \
	JASP/R/regressionlinearbayesian.R \
	JASP/R/regressionlogistic.R \
	JASP/R/regressionloglinear.R \
	JASP/R/regressionloglinearbayesian.R \
	JASP/R/reliabilityanalysis.R \
	JASP/R/semsimple.R \
	JASP/R/summarystatsbinomialtestbayesian.R \
	JASP/R/summarystatscorrelationbayesianpairs.R \
	JASP/R/summarystatsregressionlinearbayesian.R \
	JASP/R/summarystatsttestbayesianindependentsamples.R \
	JASP/R/summarystatsttestbayesianonesample.R \
	JASP/R/summarystatsttestbayesianpairedsamples.R \
	JASP/R/ttestbayesianindependentsamples.R \
	JASP/R/ttestbayesianonesample.R \
	JASP/R/ttestbayesianpairedsamples.R \
	JASP/R/ttestindependentsamples.R \
	JASP/R/ttestonesample.R \
	JASP/R/ttestpairedsamples.R \
	JASP/R/networkanalysis.R
