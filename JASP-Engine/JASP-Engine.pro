
QT       -= core
QT       -= gui

windows:CONFIG += c++11

DESTDIR = ..
TARGET = JASPEngine
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

DEPENDPATH = ..

PRE_TARGETDEPS += ../libJASP-Common.a

LIBS += -L.. -lJASP-Common

unix:INCLUDEPATH += /opt/local/include
windows:INCLUDEPATH += C:/progra~1/boost/boost_1_53_0

R_HOME = $$OUT_PWD/../R-3.0.0

QMAKE_CXXFLAGS += -Wno-c++11-extensions
QMAKE_CXXFLAGS += -Wno-unused-parameter
QMAKE_CXXFLAGS += -Wno-c++11-long-long
QMAKE_CXXFLAGS += -Wno-c++11-extra-semi

INCLUDEPATH += \
	$$R_HOME/include \
	$$R_HOME/library/RInside/include \
	$$R_HOME/library/Rcpp/include

unix:LIBS += \
	-L$$R_HOME/library/RInside/lib -lRInside \
	-L$$R_HOME/library/Rcpp/lib -lRcpp \
	-L$$R_HOME/lib -lR

win32:LIBS += \
	-L$$R_HOME/library/RInside/lib/i386 -lRInside \
	-L$$R_HOME/library/Rcpp/lib/i386 -lRcpp \
	-L$$R_HOME/bin/i386 -lR

win32:LIBS += -lole32 -loleaut32


RPackage.commands = $$R_HOME/bin/R CMD INSTALL $$PWD/JASP
QMAKE_EXTRA_TARGETS += RPackage
PRE_TARGETDEPS += RPackage

SOURCES += main.cpp \
    engine.cpp \
    rcppbridge.cpp

HEADERS += \
    engine.h \
	analysistask.h \
	rcppbridge.h

RESOURCES +=
