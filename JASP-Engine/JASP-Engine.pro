
QT += core
QT -= gui

include(../JASP.pri)
BUILDING_JASP_ENGINE=true

CONFIG += c++11
linux:CONFIG += -pipe

DESTDIR   = ..
TARGET    = JASPEngine
CONFIG   += cmdline
CONFIG   -= app_bundle
TEMPLATE  = app

target.path  = $$INSTALLPATH
INSTALLS    += target

DEPENDPATH      = ..
PRE_TARGETDEPS += ../JASP-Common

LIBS += -L.. -l$$JASP_R_INTERFACE_NAME -lJASP-Common

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

INCLUDEPATH += $$PWD/../JASP-Common/


mkpath($$OUT_PWD/../R/library)

exists(/app/lib/*) {
    # org.jaspstats.JASP.json and flatpakbuilder do all this
} else {
    InstallJASPRPackage.commands        =  $${INSTALL_R_PKG_CMD_PREFIX}$$PWD/jasp-r-pkg$${INSTALL_R_PKG_CMD_POSTFIX}
	InstallJASPgraphsRPackage.commands  =  $${INSTALL_R_PKG_CMD_PREFIX}$$PWD/jasp-graphs$${INSTALL_R_PKG_CMD_POSTFIX}

	win32 {
	    RemoveJASPRPkgLock       = $${PKG_LOCK_CMD_PREFIX}00LOCK-jasp-r-pkg$${PKG_LOCK_CMD_INFIX}00LOCK-jasp-r-pkg$${PKG_LOCK_CMD_POSTFIX}
		RemoveJASPgraphsRPkgLock = $${PKG_LOCK_CMD_PREFIX}00LOCK-jasp-graphs$${PKG_LOCK_CMD_INFIX}00LOCK-jasp-graphs$${PKG_LOCK_CMD_POSTFIX}

		InstallJASPgraphsRPackage.depends	= RemoveJASPgraphsRPkgLock
		InstallJASPRPackage.depends 		= RemoveJASPRPkgLock

        QMAKE_EXTRA_TARGETS += RemoveJASPgraphsRPkgLock
		POST_TARGETDEPS     += RemoveJASPgraphsRPkgLock

        QMAKE_EXTRA_TARGETS += RemoveJASPRPkgLock
		POST_TARGETDEPS     += RemoveJASPRPkgLock
	}

	QMAKE_EXTRA_TARGETS += InstallJASPgraphsRPackage
	POST_TARGETDEPS     += InstallJASPgraphsRPackage

	QMAKE_EXTRA_TARGETS += InstallJASPRPackage
	POST_TARGETDEPS     += InstallJASPRPackage
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
