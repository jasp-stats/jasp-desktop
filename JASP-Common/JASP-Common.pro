
QT       -= gui

DESTDIR = ..
TARGET = JASP-Common
TEMPLATE = lib
CONFIG += staticlib

windows:CONFIG += c++11

unix:INCLUDEPATH += ../../boost_1_54_0

windows {

	COMPILER_DUMP = $$system(g++ -dumpmachine)
	contains(COMPILER_DUMP, x86_64-w64-mingw32) {

		INCLUDEPATH += ../../boost_1_54_0
	}
	else {

		INCLUDEPATH += ../../boost_1_53_0
	}
}

windows:LIBS += -L.. -lJASP-Common -lole32 -loleaut32

QMAKE_CXXFLAGS += -Wno-c++11-extensions
QMAKE_CXXFLAGS += -Wno-unused-parameter
QMAKE_CXXFLAGS += -Wno-c++11-long-long
QMAKE_CXXFLAGS += -Wno-c++11-extra-semi

QMAKE_CXXFLAGS += -DBOOST_USE_WINDOWS_H

SOURCES += \
    datasetloader.cpp \
    dataset.cpp \
    analysisloader.cpp \
    analysis.cpp \
    columns.cpp \
    column.cpp \
    analyses.cpp \
    analysispart.cpp \
    lib_json/json_writer.cpp \
    lib_json/json_valueiterator.inl \
    lib_json/json_value.cpp \
    lib_json/json_reader.cpp \
    lib_json/json_internalmap.inl \
    lib_json/json_internalarray.inl \
	datablock.cpp \
    sharedmemory.cpp \
    options/optionboolean.cpp \
    options/optionintegerarray.cpp \
    options/optioninteger.cpp \
    options/optionlist.cpp \
    analyses/ttestonesample.cpp \
    options/optionnumber.cpp \
    analyses/ttestindependentsamples.cpp \
    analyses/descriptives.cpp \
    analyses/anovaoneway.cpp \
    analyses/anova.cpp \
    analyses/anovamultivariate.cpp \
    analyses/ttestpairedsamples.cpp \
    analyses/anovabayesian.cpp \
    analyses/ttestbayesianonesample.cpp \
    options/optionstring.cpp \
    analyses/ancova.cpp \
    analyses/ancovamultivariate.cpp \
    analyses/regressionlinear.cpp \
    analyses/correlation.cpp \
    ipcchannel.cpp \
    options/options.cpp \
    options/option.cpp \
    options/optionstable.cpp \
    analyses/crosstabs.cpp \
    analyses/correlationpartial.cpp \
    analyses/semsimple.cpp \
    csv.cpp \
    process.cpp \
    analyses/ttestbayesianindependentsamples.cpp \
    analyses/ttestbayesianpairedsamples.cpp \
    base64.cpp \
    base64/cdecode.cpp \
    base64/cencode.cpp \
    label.cpp \
    labels.cpp \
    options/optionvariable.cpp \
    options/optionterms.cpp \
    options/optionvariablesgroups.cpp \
    options/optionterm.cpp \
    options/optionvariables.cpp \
    analyses/anovarepeatedmeasures.cpp \
	analyses/anovarepeatedmeasuresshort.cpp \
    analyses/ancovabayesian.cpp \
    analyses/anovarepeatedmeasuresbayesian.cpp \
    analyses/correlationbayesian.cpp \
    analyses/crosstabsbayesian.cpp

HEADERS +=\
    datasetloader.h \
    dataset.h \
    analysisloader.h \
    analysis.h \
    columns.h \
    column.h \
    analyses.h \
    analysispart.h \
    lib_json/writer.h \
    lib_json/value.h \
    lib_json/reader.h \
    lib_json/json.h \
    lib_json/json_batchallocator.h \
    lib_json/forwards.h \
    lib_json/features.h \
    lib_json/config.h \
    lib_json/autolink.h \
    datablock.h \
    sharedmemory.h \
    options/optionboolean.h \
    options/optioni.h \
    options/optionintegerarray.h \
    options/optioninteger.h \
	options/optionlist.h \
    rinterface.h \
    options/optionnumber.h \
    analyses/ttestindependentsamples.h \
    analyses/ttestonesample.h \
    analyses/descriptives.h \
    analyses/anovaoneway.h \
    analyses/anova.h \
    analyses/anovamultivariate.h \
    analyses/ttestpairedsamples.h \
    analyses/anovabayesian.h \
    analyses/ttestbayesianonesample.h \
    common.h \
    options/optionstring.h \
    analyses/ancova.h \
    analyses/ancovamultivariate.h \
    analyses/regressionlinear.h \
    analyses/correlation.h \
    ipcchannel.h \
    options/options.h \
    options/option.h \
    options/optionstable.h \
    analyses/crosstabs.h \
    analyses/correlationpartial.h \
    analyses/semsimple.h \
    csv.h \
    process.h \
    analyses/ttestbayesianindependentsamples.h \
    analyses/ttestbayesianpairedsamples.h \
    base64/cencode.h \
    base64/cdecode.h \
    base64.h \
    label.h \
    labels.h \
    options/optionvariables.h \
    options/optionvariable.h \
    options/optionterms.h \
    options/optionvariablesgroups.h \
    options/optionterm.h \
    analyses/anovarepeatedmeasures.h \
	analyses/anovarepeatedmeasuresshort.h \
    analyses/ancovabayesian.h \
    analyses/anovarepeatedmeasuresbayesian.h \
    analyses/correlationbayesian.h \
    analyses/crosstabsbayesian.h

unix:!symbian {
    maemo5 {
        target.path = /opt/usr/lib
    } else {
        target.path = /usr/lib
    }
    INSTALLS += target
}

OTHER_FILES += \
    analyses/frequencies.R \
    analyses/makefile \
    analyses/ttestonesample.R \
    analyses/ttestindependentsamples.R \
    analyses/ttestbayesonesample.R

RESOURCES +=
