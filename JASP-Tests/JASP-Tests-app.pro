QT += core gui webkit webkitwidgets svg network testlib printsupport xml

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

windows:CONFIG += c++11
linux:CONFIG += c++11
macx:CONFIG += c++11
linux:CONFIG += -pipe

DESTDIR = ..

TARGET = JASPTests
CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app

INCLUDEPATH += ../JASP-Desktop/ \
               ../JASP-Common/ \
               ../JASP-Engine/

   macx:INCLUDEPATH += ../../boost_1_64_0
windows:INCLUDEPATH += ../../boost_1_64_0

PRE_TARGETDEPS += ../libJASP-Desktop.a
LIBS += -L.. -lJASP-Desktop


PRE_TARGETDEPS += ../libJASP-Common.a
LIBS += -L.. -lJASP-Common


windows:LIBS += -lboost_filesystem-mgw48-mt-1_64 -lboost_system-mgw48-mt-1_64 -larchive.dll
   macx:LIBS += -lboost_filesystem-clang-mt-1_64 -lboost_system-clang-mt-1_64 -larchive -lz
  linux:LIBS += -lboost_filesystem    -lboost_system    -larchive -lrt


windows:LIBS += -lole32 -loleaut32

QMAKE_CXXFLAGS_WARN_ON += -Wno-unused-parameter -Wno-unused-local-typedef
macx:QMAKE_CXXFLAGS += -Wno-c++11-extensions
macx:QMAKE_CXXFLAGS += -Wno-c++11-long-long
macx:QMAKE_CXXFLAGS += -Wno-c++11-extra-semi
macx:QMAKE_CXXFLAGS += -stdlib=libc++


QMAKE_CXXFLAGS += -DBOOST_USE_WINDOWS_H

linux {
    _R_HOME = $$(R_HOME)
    isEmpty(_R_HOME):_R_HOME = /usr/lib/R
    QMAKE_CXXFLAGS += -D\'R_HOME=\"$$_R_HOME\"\'
}


SOURCES += \
    core/main.cpp \
    core/textfileread_test.cpp \
    core/osf_test.cpp \
    core/spssimporter_test.cpp \
    core/csvimporter_test.cpp

HEADERS += \
    core/AutomatedTests.h \
    core/textfileread_test.h \
    core/osf_test.h \
    core/csviterator.h \
    core/spssimporter_test.h \
    core/csvimporter_test.h

RESOURCES_PATH = $${PWD}/../Resources

win32 {

    RESOURCES_PATH_DEST = $${OUT_PWD}/../Resources/

    RESOURCES_PATH ~= s,/,\\,g
    RESOURCES_PATH_DEST ~= s,/,\\,g

    copyres.commands  += $$quote(cmd /c xcopy /S /I /Y $${RESOURCES_PATH} $${RESOURCES_PATH_DEST})
}

macx {

    RESOURCES_PATH_DEST = $${OUT_PWD}/../../Resources/

    copyres.commands += $(MKDIR) $$RESOURCES_PATH_DEST ;
    copyres.commands += cp -R $$RESOURCES_PATH/* $$RESOURCES_PATH_DEST ;
}

linux {

    RESOURCES_PATH_DEST = $${OUT_PWD}/../Resources/

    copyres.commands += $(MKDIR) $$RESOURCES_PATH_DEST ;
    copyres.commands += cp -R $$RESOURCES_PATH/* $$RESOURCES_PATH_DEST ;
}

! equals(PWD, $${OUT_PWD}) {

    QMAKE_EXTRA_TARGETS += copyres
    POST_TARGETDEPS     += copyres
}
