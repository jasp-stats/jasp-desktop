QT -= gui
QT -= core

DESTDIR = ..

TARGET = JASP-Common-mingw

TEMPLATE = lib
CONFIG += staticlib

windows:CONFIG += c++11
linux:CONFIG += c++11
macx:CONFIG += c++11

   macx:INCLUDEPATH += ../../boost_1_64_0
windows:INCLUDEPATH += ../../boost_1_64_0 ../JASP-Common

windows:LIBS += -lole32 -loleaut32 -larchive.dll

macx:QMAKE_CXXFLAGS_WARN_ON += -Wno-unused-parameter -Wno-unused-local-typedef
macx:QMAKE_CXXFLAGS += -Wno-c++11-extensions
macx:QMAKE_CXXFLAGS += -Wno-deprecated-declarations
macx:QMAKE_CXXFLAGS += -Wno-c++11-long-long
macx:QMAKE_CXXFLAGS += -Wno-c++11-extra-semi
macx:QMAKE_CXXFLAGS += -stdlib=libc++

windows:QMAKE_CXXFLAGS += -DBOOST_USE_WINDOWS_H -D__WIN32__

SOURCES += \
        ../JASP-Common/analysis.cpp \
        ../JASP-Common/analysisloader.cpp \
        ../JASP-Common/appinfo.cpp \
        ../JASP-Common/base64.cpp \
        ../JASP-Common/base64/cdecode.cpp \
        ../JASP-Common/base64/cencode.cpp \
        ../JASP-Common/column.cpp \
        ../JASP-Common/columns.cpp \
        ../JASP-Common/datablock.cpp \
        ../JASP-Common/dataset.cpp \
        ../JASP-Common/datasetpackage.cpp \
        ../JASP-Common/dirs.cpp \
        ../JASP-Common/filereader.cpp \
        ../JASP-Common/ipcchannel.cpp \
        ../JASP-Common/label.cpp \
        ../JASP-Common/labels.cpp \
        ../JASP-Common/lib_json/json_internalarray.inl \
        ../JASP-Common/lib_json/json_internalmap.inl \
        ../JASP-Common/lib_json/json_reader.cpp \
        ../JASP-Common/lib_json/json_value.cpp \
        ../JASP-Common/lib_json/json_valueiterator.inl \
        ../JASP-Common/lib_json/json_writer.cpp \
        ../JASP-Common/options/option.cpp \
        ../JASP-Common/options/optionboolean.cpp \
        ../JASP-Common/options/optioninteger.cpp \
        ../JASP-Common/options/optionintegerarray.cpp \
        ../JASP-Common/options/optionlist.cpp \
        ../JASP-Common/options/optionnumber.cpp \
        ../JASP-Common/options/options.cpp \
        ../JASP-Common/options/optionstable.cpp \
        ../JASP-Common/options/optionstring.cpp \
        ../JASP-Common/options/optionterm.cpp \
        ../JASP-Common/options/optionterms.cpp \
        ../JASP-Common/options/optionvariable.cpp \
        ../JASP-Common/options/optionvariables.cpp \
        ../JASP-Common/options/optionvariablesgroups.cpp \
        ../JASP-Common/processinfo.cpp \
        ../JASP-Common/sharedmemory.cpp \
        ../JASP-Common/tempfiles.cpp \
        ../JASP-Common/utils.cpp \
        ../JASP-Common/version.cpp

HEADERS += \
        ../JASP-Common/analysis.h \
        ../JASP-Common/analysisloader.h \
        ../JASP-Common/appinfo.h \
        ../JASP-Common/base64.h \
        ../JASP-Common/base64/cdecode.h \
        ../JASP-Common/base64/cencode.h \
        ../JASP-Common/boost/nowide/args.hpp \
        ../JASP-Common/boost/nowide/cenv.hpp \
        ../JASP-Common/boost/nowide/config.hpp \
        ../JASP-Common/boost/nowide/convert.hpp \
        ../JASP-Common/boost/nowide/cstdio.hpp \
        ../JASP-Common/boost/nowide/cstdlib.hpp \
        ../JASP-Common/boost/nowide/filebuf.hpp \
        ../JASP-Common/boost/nowide/fstream.hpp \
        ../JASP-Common/boost/nowide/iostream.hpp \
        ../JASP-Common/boost/nowide/stackstring.hpp \
        ../JASP-Common/boost/nowide/system.hpp \
        ../JASP-Common/boost/nowide/windows.hpp \
        ../JASP-Common/column.h \
        ../JASP-Common/columns.h \
        ../JASP-Common/common.h \
        ../JASP-Common/datablock.h \
        ../JASP-Common/dataset.h \
        ../JASP-Common/datasetpackage.h \
        ../JASP-Common/dirs.h \
        ../JASP-Common/filereader.h \
        ../JASP-Common/ipcchannel.h \
        ../JASP-Common/label.h \
        ../JASP-Common/labels.h \
        ../JASP-Common/lib_json/autolink.h \
        ../JASP-Common/lib_json/config.h \
        ../JASP-Common/lib_json/features.h \
        ../JASP-Common/lib_json/forwards.h \
        ../JASP-Common/lib_json/json_batchallocator.h \
        ../JASP-Common/lib_json/json.h \
        ../JASP-Common/lib_json/reader.h \
        ../JASP-Common/lib_json/value.h \
        ../JASP-Common/lib_json/writer.h \
        ../JASP-Common/libzip/archive.h \
        ../JASP-Common/libzip/archive_entry.h \
        ../JASP-Common/options/option.h \
        ../JASP-Common/options/optionboolean.h \
        ../JASP-Common/options/optioni.h \
        ../JASP-Common/options/optioninteger.h \
        ../JASP-Common/options/optionintegerarray.h \
        ../JASP-Common/options/optionlist.h \
        ../JASP-Common/options/optionnumber.h \
        ../JASP-Common/options/options.h \
        ../JASP-Common/options/optionstable.h \
        ../JASP-Common/options/optionstring.h \
        ../JASP-Common/options/optionterm.h \
        ../JASP-Common/options/optionterms.h \
        ../JASP-Common/options/optionvariable.h \
        ../JASP-Common/options/optionvariables.h \
        ../JASP-Common/options/optionvariablesgroups.h \
        ../JASP-Common/processinfo.h \
        ../JASP-Common/sharedmemory.h \
        ../JASP-Common/tempfiles.h \
        ../JASP-Common/utils.h \
        ../JASP-Common/version.h \
        ../JASP-Common/options/optionvariablei.h

