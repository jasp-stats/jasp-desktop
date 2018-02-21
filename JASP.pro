cache()

TEMPLATE = subdirs

DESTDIR = .

SUBDIRS += \
	JASP-Common \
        JASP-Engine \
        JASP-Desktop
#	JASP-Tests

unix: SUBDIRS += JASP-R-Interface

JASP-Desktop.depends = JASP-Common
JASP-Engine.depends = JASP-Common 

unix: JASP-Engine.depends += JASP-R-Interface
