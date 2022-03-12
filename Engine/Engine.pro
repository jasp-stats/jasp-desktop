
QT += core
QT -= gui

include(../JASP.pri)
BUILDING_JASP_ENGINE=true

CONFIG += c++17
linux:CONFIG += -pipe

DESTDIR   = ..
TARGET    = JASPEngine
CONFIG   += cmdline
CONFIG   -= app_bundle
TEMPLATE  = app

target.path  = $$INSTALLPATH
INSTALLS    += target

DEPENDPATH      = ..
PRE_TARGETDEPS += ../Common

LIBS += -L.. -l$$JASP_R_INTERFACE_NAME -lCommon

include(../R_HOME.pri) #needed to build r-packages

windows {
  LIBS 			 += -llibboost_filesystem$$BOOST_POSTFIX -llibboost_system$$BOOST_POSTFIX  -llibboost_date_time$$BOOST_POSTFIX -larchive.dll
  LIBS           += -lole32 -loleaut32
  INCLUDEPATH    += ../../boost_1_71_0
  QMAKE_CXXFLAGS += -DBOOST_USE_WINDOWS_H -DNOMINMAX -DBOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED
}

macx {
  LIBS                   += -lboost_filesystem-mt -lboost_system-mt -larchive -lz
  INCLUDEPATH 			 += ../../boost_1_71_0
  QMAKE_MACOSX_DEPLOYMENT_TARGET = 10.15

  QMAKE_CXXFLAGS_WARN_ON += -Wno-unused-parameter -Wno-unused-local-typedef
  QMAKE_CXXFLAGS         += -Wno-c++11-extensions -Wno-c++11-long-long -Wno-c++11-extra-semi -stdlib=libc++
}

linux {
    LIBS += -larchive -lrt
    exists(/app/lib/*)	{ LIBS += -L/app/lib }
    LIBS += -lboost_filesystem -lboost_system
}

$$JASPTIMER_USED {
    windows:  LIBS += -llibboost_timer$$BOOST_POSTFIX -llibboost_chrono$$BOOST_POSTFIX
    linux:    LIBS += -lboost_timer -lboost_chrono
    macx:     LIBS += -lboost_timer-mt -lboost_chrono-mt
}

unix: LIBS += -L$$_R_HOME/lib -lR

INCLUDEPATH += $$PWD/../Common/ $$PWD/../Common/jaspColumnEncoder


mkpath($$OUT_PWD/../R/library)
mkpath($$OUT_PWD/../renv-cache)

exists(/app/lib/*) {
    # org.jaspstats.JASP.json and flatpakbuilder do all this
} else {

	InstalljaspResults.commands			= ""
	InstallJASPRPackage.commands		= ""
	InstallJASPRPackage.commands		+= $${INSTALL_R_PKG_CMD_PREFIX}$$PWD/jaspBase$${INSTALL_R_PKG_CMD_POSTFIX}
	InstalljaspResults.commands			+= $${INSTALL_R_PKG_CMD_PREFIX}$$PWD/../R-Interface/jaspResults$${INSTALL_R_PKG_CMD_POSTFIX}
	
	#InstalljaspGraphsRPackage.commands  =  ""
	#InstalljaspGraphsRPackage.commands  =  $${INSTALL_R_PKG_DEPS_CMD_PREFIX}$$PWD/jaspGraphs$${INSTALL_R_PKG_DEPS_CMD_POSTFIX};	 $$escape_expand(\\n\\t) 
	#InstalljaspGraphsRPackage.commands +=       $${INSTALL_R_PKG_CMD_PREFIX}$$PWD/jaspGraphs$${INSTALL_R_PKG_CMD_POSTFIX}

    #InstalljaspGraphsRPackage.depends	= InstallJASPRPackage

	win32 {
	    RemoveJASPRPkgLock       = $${PKG_LOCK_CMD_PREFIX}00LOCK-jaspBase$${PKG_LOCK_CMD_INFIX}00LOCK-jaspBase$${PKG_LOCK_CMD_POSTFIX}
		#RemovejaspGraphsRPkgLock = $${PKG_LOCK_CMD_PREFIX}00LOCK-jaspGraphs$${PKG_LOCK_CMD_INFIX}00LOCK-jaspGraphs$${PKG_LOCK_CMD_POSTFIX}

		#InstalljaspGraphsRPackage.depends	= RemovejaspGraphsRPkgLock
		InstallJASPRPackage.depends 		= RemoveJASPRPkgLock

        #QMAKE_EXTRA_TARGETS += RemovejaspGraphsRPkgLock
		#POST_TARGETDEPS     += RemovejaspGraphsRPkgLock

        QMAKE_EXTRA_TARGETS += RemoveJASPRPkgLock
		POST_TARGETDEPS     += RemoveJASPRPkgLock
	}

	#QMAKE_EXTRA_TARGETS += InstalljaspGraphsRPackage
	#POST_TARGETDEPS     += InstalljaspGraphsRPackage

	QMAKE_EXTRA_TARGETS += InstallJASPRPackage
	QMAKE_EXTRA_TARGETS += InstalljaspResults
	POST_TARGETDEPS     += InstallJASPRPackage
	POST_TARGETDEPS     += InstalljaspResults
}

QMAKE_CLEAN += $$OUT_PWD/../R/library/* #Does this not mess up Windows somehow?

SOURCES += 	main.cpp \
 			engine.cpp \
			otoolstuff.cpp \
			rbridge.cpp

HEADERS += \
  			engine.h \
			otoolstuff.h \
			rbridge.h
