
QT -= gui
QT -= core

DESTDIR = ..
TARGET = Common
TEMPLATE = lib
CONFIG += staticlib
CONFIG += c++11
 
include(../JASP.pri)


macx: QMAKE_CXXFLAGS += -DBOOST_INTERPROCESS_SHARED_DIR_FUNC

windows {
	LIBS			+= -lole32 -loleaut32 -larchive.dll
	QMAKE_CXXFLAGS	+= -DBOOST_USE_WINDOWS_H -DNOMINMAX -DBOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED
}

INCLUDEPATH += $$PWD/ jaspColumnEncoder

SOURCES += \
	appinfo.cpp \
	archivereader.cpp \
	column.cpp \
	jaspColumnEncoder/columnencoder.cpp \
	columns.cpp \
	datablock.cpp \
	dataset.cpp \
	dirs.cpp \
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
	jaspColumnEncoder/columntype.cpp

HEADERS += \
	appinfo.h \
	archivereader.h \
	jaspColumnEncoder/boost/nowide/args.hpp \
	jaspColumnEncoder/boost/nowide/cenv.hpp \
	jaspColumnEncoder/boost/nowide/config.hpp \
	jaspColumnEncoder/boost/nowide/convert.hpp \
	jaspColumnEncoder/boost/nowide/cstdio.hpp \
	jaspColumnEncoder/boost/nowide/cstdlib.hpp \
	jaspColumnEncoder/boost/nowide/filebuf.hpp \
	jaspColumnEncoder/boost/nowide/fstream.hpp \
	jaspColumnEncoder/boost/nowide/iostream.hpp \
	jaspColumnEncoder/boost/nowide/stackstring.hpp \
	jaspColumnEncoder/boost/nowide/system.hpp \
	jaspColumnEncoder/boost/nowide/windows.hpp \
	column.h \
	jaspColumnEncoder/columnencoder.h \
	columns.h \
	common.h \
	datablock.h \
	dataset.h \
	dirs.h \
	ipcchannel.h \
	label.h \
	labels.h \
	jaspColumnEncoder/libzip/archive.h \
	jaspColumnEncoder/libzip/archive_entry.h \
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
	jaspColumnEncoder/enumutilities.h \
	jaspColumnEncoder/stringutils.h \
	log.h \
	jaspColumnEncoder/columntype.h

#exists(/app/lib/*) should only be true when building flatpak
#macx | windows | exists(/app/lib/*)
contains(DEFINES, JASP_LIBJSON_STATIC) {

    SOURCES += \
            jaspColumnEncoder/lib_json/json_internalarray.inl \
            jaspColumnEncoder/lib_json/json_internalmap.inl \
            jaspColumnEncoder/lib_json/json_reader.cpp \
            jaspColumnEncoder/lib_json/json_value.cpp \
            jaspColumnEncoder/lib_json/json_valueiterator.inl \
            jaspColumnEncoder/lib_json/json_writer.cpp

    HEADERS += \
            jaspColumnEncoder/lib_json/autolink.h \
            jaspColumnEncoder/lib_json/config.h \
            jaspColumnEncoder/lib_json/features.h \
            jaspColumnEncoder/lib_json/forwards.h \
            jaspColumnEncoder/lib_json/json_batchallocator.h \
            jaspColumnEncoder/lib_json/json.h \
            jaspColumnEncoder/lib_json/reader.h \
            jaspColumnEncoder/lib_json/value.h \
            jaspColumnEncoder/lib_json/writer.h
}


