QT       -= gui

TARGET = JASP-Sharedmem
DESTDIR = ..
TEMPLATE = lib

DEFINES += JASPSHAREDMEM_LIBRARY

# The following define makes your compiler emit warnings if you use
# any feature of Qt which as been marked as deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.
DEFINES += QT_DEPRECATED_WARNINGS


INCLUDEPATH += ../../boost_1_64_0

LIBS += -L.. -lJASP-Common

windows:CONFIG(ReleaseBuild) {
    LIBS += -llibboost_filesystem-vc141-mt-1_64 -llibboost_system-vc141-mt-1_64
}

windows:CONFIG(DebugBuild) {
    LIBS += -llibboost_filesystem-vc141-mt-gd-1_64 -llibboost_system-vc141-mt-gd-1_64
}

SOURCES += \
    jaspsharedmem_impl.cpp

HEADERS += \
    jaspsharedmem_interface.h \
    jaspsharedmem_global.h \
    jaspsharedmem_impl.h

unix {
	target.path = /usr/lib
	INSTALLS += target
}
