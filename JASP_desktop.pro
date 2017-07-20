
cache()

TEMPLATE = subdirs

DESTDIR = .

SUBDIRS += \
        JASP-Common \
        JASP-Desktop \
        JASP-Sharedmem

JASP-Desktop.depends = JASP-Common
JASP-Sharedmem.depends = JASP-Common
