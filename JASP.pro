
cache()

TEMPLATE = subdirs

DESTDIR = .

SUBDIRS += \
	JASP-Common \
	JASP-Engine \
        JASP-Desktop

JASP-Desktop.depends = JASP-Common
JASP-Engine.depends = JASP-Common
JASP-Tests.depends = JASP-Common
JASP-Tests.depends = JASP-Desktop
