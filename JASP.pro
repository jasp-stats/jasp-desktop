cache()

TEMPLATE = subdirs

DESTDIR = .

SUBDIRS += \
	JASP-Common \
        JASP-Engine \
        JASP-Desktop
linux: SUBDIRS += JASP-R-Interface
#	JASP-Tests

JASP-Desktop.depends = JASP-Common
JASP-Engine.depends = JASP-Common
