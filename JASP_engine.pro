
cache()

TEMPLATE = subdirs

DESTDIR = .

SUBDIRS += \
        JASP-Common-mingw \
        JASP-Engine

JASP-Engine.depends = JASP-Common-mingw
