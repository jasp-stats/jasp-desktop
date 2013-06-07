TEMPLATE = subdirs

CONFIG += ordered

DESTDIR = .

SUBDIRS += \
	JASP-Common \
	JASP-Desktop

JASP-Desktop.depends = JASP-Common
