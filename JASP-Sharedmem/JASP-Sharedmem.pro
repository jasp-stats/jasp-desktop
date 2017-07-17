#-------------------------------------------------
#
# Project created by QtCreator 2017-07-13T17:03:42
#
#-------------------------------------------------

QT       -= gui

TARGET = JASP-Sharedmem
TEMPLATE = lib

DEFINES += JASPSHAREDMEM_LIBRARY

# The following define makes your compiler emit warnings if you use
# any feature of Qt which as been marked as deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.
DEFINES += QT_DEPRECATED_WARNINGS

# You can also make your code fail to compile if you use deprecated APIs.
# In order to do so, uncomment the following line.
# You can also select to disable deprecated APIs only up to a certain version of Qt.
#DEFINES += QT_DISABLE_DEPRECATED_BEFORE=0x060000    # disables all the APIs deprecated before Qt 6.0.0

INCLUDEPATH += ../../boost_1_64_0

LIBS += -L.. -lJASP-Common

windows:CONFIG(ReleaseBuild) {
    LIBS += -lboost_filesystem-vc141-mt-1_64 -lboost_system-vc141-mt-1_64
}

windows:CONFIG(DebugBuild) {
    LIBS += -lboost_filesystem-vc141-mt-gd-1_64 -lboost_system-vc141-mt-gd-1_64
}

SOURCES += \
        jaspsharedmem.cpp

HEADERS += \
        jaspsharedmem.h \
        jasp-sharedmem_global.h 

unix {
    target.path = /usr/lib
    INSTALLS += target
}
