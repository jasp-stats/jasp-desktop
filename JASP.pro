include(JASP.pri)

TEMPLATE = subdirs

DESTDIR = .

SUBDIRS += \
	Common \
	Engine \
	Desktop \
	Modules

unix: SUBDIRS += R-Interface

Desktop.depends  = Common
Engine.depends   = Common
Modules.depends  = Engine \
            	   Desktop

unix: Engine.depends += R-Interface
