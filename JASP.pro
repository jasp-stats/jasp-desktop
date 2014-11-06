
cache()

isEmpty(PREFIX) { PREFIX = /usr/local }

TEMPLATE = subdirs

DESTDIR = .

SUBDIRS += \
	JASP-Common \
	JASP-Engine \
	JASP-Desktop

JASP-Desktop.depends = JASP-Common
JASP-Engine.depends = JASP-Common
