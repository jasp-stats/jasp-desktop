
QT       -= gui

DESTDIR = ..
TARGET = JASP-Common
TEMPLATE = lib
CONFIG += staticlib

windows:CONFIG += c++11

macx:INCLUDEPATH += ../../boost_1_54_0
windows:INCLUDEPATH += ../../boost_1_54_0

windows:LIBS += -L.. -lJASP-Common -lole32 -loleaut32

QMAKE_CXXFLAGS += -Wno-c++11-extensions
QMAKE_CXXFLAGS += -Wno-unused-parameter
QMAKE_CXXFLAGS += -Wno-c++11-long-long
QMAKE_CXXFLAGS += -Wno-c++11-extra-semi

QMAKE_CXXFLAGS += -DBOOST_USE_WINDOWS_H

SOURCES += \
	analyses.cpp \
	analysis.cpp \
	analysisloader.cpp \
	base64.cpp \
	base64/cdecode.cpp \
	base64/cencode.cpp \
	column.cpp \
	columns.cpp \
	datablock.cpp \
	dataset.cpp \
	datasetloader.cpp \
	dirs.cpp \
	importers/csv.cpp \
	importers/csvimporter.cpp \
	importers/spssimporter.cpp \
	ipcchannel.cpp \
	label.cpp \
	labels.cpp \
	lib_json/json_internalarray.inl \
	lib_json/json_internalmap.inl \
	lib_json/json_reader.cpp \
	lib_json/json_value.cpp \
	lib_json/json_valueiterator.inl \
	lib_json/json_writer.cpp \
	options/option.cpp \
	options/optionboolean.cpp \
	options/optioninteger.cpp \
	options/optionintegerarray.cpp \
	options/optionlist.cpp \
	options/optionnumber.cpp \
	options/options.cpp \
	options/optionstable.cpp \
	options/optionstring.cpp \
	options/optionterm.cpp \
	options/optionterms.cpp \
	options/optionvariable.cpp \
	options/optionvariables.cpp \
	options/optionvariablesgroups.cpp \
	process.cpp \
	sharedmemory.cpp \
	tempfiles.cpp \
	utils.cpp

HEADERS += \
	analyses.h \
	analysis.h \
	analysisloader.h \
	base64.h \
	base64/cdecode.h \
	base64/cencode.h \
	column.h \
	columns.h \
	common.h \
	datablock.h \
	dataset.h \
	datasetloader.h \
	dirs.h \
	importers/csv.h \
	importers/csvimporter.h \
	importers/spssimporter.h \
	ipcchannel.h \
	label.h \
	labels.h \
	lib_json/autolink.h \
	lib_json/config.h \
	lib_json/features.h \
	lib_json/forwards.h \
	lib_json/json_batchallocator.h \
	lib_json/json.h \
	lib_json/reader.h \
	lib_json/value.h \
	lib_json/writer.h \
	options/option.h \
	options/optionboolean.h \
	options/optioni.h \
	options/optioninteger.h \
	options/optionintegerarray.h \
	options/optionlist.h \
	options/optionnumber.h \
	options/options.h \
	options/optionstable.h \
	options/optionstring.h \
	options/optionterm.h \
	options/optionterms.h \
	options/optionvariable.h \
	options/optionvariables.h \
	options/optionvariablesgroups.h \
	process.h \
	rinterface.h \
	sharedmemory.h \
	tempfiles.h \
	utils.h \
	version.h

