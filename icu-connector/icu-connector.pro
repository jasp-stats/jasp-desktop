#-------------------------------------------------
#
# Project created by QtCreator 2016-04-05T15:17:11
#
#-------------------------------------------------

QT       -= core gui

DESTDIR = ..
TARGET = icu-connector
TEMPLATE = lib
CONFIG += staticlib

CONFIG += c++11

SOURCES += icuconnector.cpp \
    spsscpconvert.cpp

HEADERS += icuconnector.h \
    icu-names.h \
    spsscpconvert.h
