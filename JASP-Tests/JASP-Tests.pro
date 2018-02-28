
TEMPLATE = subdirs
CONFIG += ordered
SUBDIRS += JASP-Desktop-staticlib.pro JASP-Tests-app.pro
linux: LIBS += -ljsoncpp
macx | windows { DEFINES += JASP_NOT_LINUX }
