
QT -= gui
QT -= core

DESTDIR = ..
TARGET = JASP-Common
TEMPLATE = lib
CONFIG += staticlib
CONFIG += c++11
 
include(../JASP.pri)


macx: QMAKE_CXXFLAGS += -DBOOST_INTERPROCESS_SHARED_DIR_FUNC

windows {
	LIBS			+= -lole32 -loleaut32 -larchive.dll
	QMAKE_CXXFLAGS	+= -DBOOST_USE_WINDOWS_H -DNOMINMAX -DBOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED
}

INCLUDEPATH += $$PWD/

SOURCES += \
	appinfo.cpp \
	column.cpp \
	columnencoder.cpp \
	columns.cpp \
	datablock.cpp \
	dataset.cpp \
	dirs.cpp \
	filereader.cpp \
	ipcchannel.cpp \
	label.cpp \
	labels.cpp \
	processinfo.cpp \
	r_functionwhitelist.cpp \
	sharedmemory.cpp \
	tempfiles.cpp \
    utilenums.cpp \
	utils.cpp \
	version.cpp \
	enginedefinitions.cpp \
	timers.cpp \
    log.cpp \
    columntype.cpp

HEADERS += \
	appinfo.h \
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
	columnencoder.h \
	columns.h \
	common.h \
	datablock.h \
	dataset.h \
	dirs.h \
	filereader.h \
	ipcchannel.h \
	label.h \
	labels.h \
	libzip/archive.h \
	libzip/archive_entry.h \
	processinfo.h \
	r_functionwhitelist.h \
	sharedmemory.h \
	tempfiles.h \
    utilenums.h \
	utils.h \
	version.h \
	jsonredirect.h \
	enginedefinitions.h \
	timers.h \
	enumutilities.h \
    stringutils.h \
    log.h \
    columntype.h

#exists(/app/lib/*) should only be true when building flatpak
#macx | windows | exists(/app/lib/*)
contains(DEFINES, JASP_LIBJSON_STATIC) {

    SOURCES += \
            lib_json/json_internalarray.inl \
            lib_json/json_internalmap.inl \
            lib_json/json_reader.cpp \
            lib_json/json_value.cpp \
            lib_json/json_valueiterator.inl \
            lib_json/json_writer.cpp

    HEADERS += \
            lib_json/autolink.h \
            lib_json/config.h \
            lib_json/features.h \
            lib_json/forwards.h \
            lib_json/json_batchallocator.h \
            lib_json/json.h \
            lib_json/reader.h \
            lib_json/value.h \
            lib_json/writer.h
}


