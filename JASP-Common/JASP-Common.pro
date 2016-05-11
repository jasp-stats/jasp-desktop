
QT -= gui
QT += webkitwidgets printsupport

DESTDIR = ..
TARGET = JASP-Common
TEMPLATE = lib
CONFIG += staticlib

windows:CONFIG += c++11
linux:CONFIG += c++11
linux:CONFIG += -pipe

   macx:INCLUDEPATH += ../../boost_1_54_0
windows:INCLUDEPATH += ../../boost_1_54_0

windows:LIBS += -lole32 -loleaut32 -larchive.dll


QMAKE_CXXFLAGS += -Wno-c++11-extensions
QMAKE_CXXFLAGS += -Wno-unused-parameter
QMAKE_CXXFLAGS += -Wno-c++11-long-long
QMAKE_CXXFLAGS += -Wno-c++11-extra-semi

windows:QMAKE_CXXFLAGS += -DBOOST_USE_WINDOWS_H

SOURCES += \
	analysis.cpp \
	analysisloader.cpp \
	appinfo.cpp \
	base64.cpp \
	base64/cdecode.cpp \
	base64/cencode.cpp \
	column.cpp \
	columns.cpp \
	datablock.cpp \
	dataset.cpp \
	datasetpackage.cpp \
	datasetloader.cpp \
	dirs.cpp \
	exporters/jaspexporter.cpp \
	filereader.cpp \
	importers/csv.cpp \
	importers/csvimporter.cpp \
	importers/jaspimporter.cpp \
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
	processinfo.cpp \
	sharedmemory.cpp \
	tempfiles.cpp \
	utils.cpp \
	version.cpp \
	importers/spss/datainforecord.cpp \
	importers/spss/datarecords.cpp \
	importers/spss/dictionaryterminationrecord.cpp \
	importers/spss/documentrecord.cpp \
	importers/spss/extnumbercasesrecord.cpp \
	importers/spss/fileheaderrecord.cpp \
	importers/spss/floatinforecord.cpp \
	importers/spss/longvarnamesrecord.cpp \
	importers/spss/miscinforecord.cpp \
	importers/spss/missingvaluechecker.cpp \
	importers/spss/readablerecord.cpp \
	importers/spss/spssrecinter.cpp \
	importers/spss/valuelabelvarsrecord.cpp \
	importers/spss/vardisplayparamrecord.cpp \
	importers/spss/variablerecord.cpp \
	importers/spss/verylongstringrecord.cpp \
	importers/spss/integerinforecord.cpp \
	importers/spss/stringutils.cpp \
    exporters/exporter.cpp \
    exporters/resultexporter.cpp \
    exporters/dataexporter.cpp


HEADERS += \
	analysis.h \
	analysisloader.h \
	appinfo.h \
	base64.h \
	base64/cdecode.h \
	base64/cencode.h \
	boost/nowide/args.hpp \
	boost/nowide/cenv.hpp \
	boost/nowide/config.hpp \
	boost/nowide/convert.hpp \
	boost/nowide/cstdio.hpp \
	boost/nowide/cstdlib.hpp \
	boost/nowide/filebuf.hpp \
	boost/nowide/fstream.hpp \
	boost/nowide/iostream.hpp \
	boost/nowide/stackstring.hpp \
	boost/nowide/system.hpp \
	boost/nowide/windows.hpp \
	column.h \
	columns.h \
	common.h \
	datablock.h \
	dataset.h \
	datasetpackage.h \
	datasetloader.h \
	dirs.h \
	exporters/jaspexporter.h \
	filepackage.h \
	filereader.h \
	importers/csv.h \
	importers/csvimporter.h \
	importers/jaspimporter.h \
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
	libzip/archive.h \
	libzip/archive_entry.h \
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
	processinfo.h \
	sharedmemory.h \
	tempfiles.h \
	utils.h \
	version.h \
	importers/spss/datainforecord.h \
	importers/spss/datarecords.h \
	importers/spss/debug_cout.h \
	importers/spss/dictionaryterminationrecord.h \
	importers/spss/documentrecord.h \
	importers/spss/extnumbercasesrecord.h \
	importers/spss/fileheaderrecord.h \
	importers/spss/floatinforecord.h \
	importers/spss/integerinforecord.h \
	importers/spss/longvarnamesrecord.h \
	importers/spss/measures.h \
	importers/spss/miscinforecord.h \
	importers/spss/missingvaluechecker.h \
	importers/spss/readablerecord.h \
	importers/spss/spssrecinter.h \
	importers/spss/spssstream.h \
	importers/spss/systemfileformat.h \
	importers/spss/valuelabelvarsrecord.h \
	importers/spss/vardisplayparamrecord.h \
	importers/spss/variablerecord.h \
	importers/spss/verylongstringrecord.h \
	importers/spss/stringutils.h \
    exporters/exporter.h \
    exporters/resultexporter.h \
    exporters/dataexporter.h


