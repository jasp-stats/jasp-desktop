TEMPLATE = subdirs

CONFIG += ordered

DESTDIR = .

SUBDIRS += \
	JASP-Common \
        JASP-Engine

JASP-Engine.depends = JASP-Common
