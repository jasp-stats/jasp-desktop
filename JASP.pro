cache()

TEMPLATE = subdirs

DESTDIR = .

SUBDIRS += \
	JASP-Common \
        JASP-Engine \
        JASP-Desktop
#	JASP-Tests

linux: SUBDIRS += JASP-R-Interface

JASP-Desktop.depends = JASP-Common
JASP-Engine.depends = JASP-Common 

linux: JASP-Engine.depends += JASP-R-Interface
