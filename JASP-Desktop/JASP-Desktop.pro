QT += core gui webenginewidgets webchannel svg network printsupport xml qml quick quickwidgets

include(../JASP.pri)

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets


include(../JASP.pri)

CONFIG += c++11

DESTDIR = ..

windows:TARGET = JASP
   macx:TARGET = JASP
  linux: exists(/app/lib/*)	{ TARGET = org.jasp.JASP } else { TARGET = jasp }

TEMPLATE = app

DEPENDPATH = ..

CONFIG -= app_bundle

INCLUDEPATH += ../JASP-Common/

#exists(/app/lib/*) should only be true when building flatpak
exists(/app/lib/*)	{ target.path += /app/bin }
else			{
	target.path += /usr/bin
}

INSTALLS += target

   macx:INCLUDEPATH += ../../boost_1_64_0
windows:INCLUDEPATH += ../../boost_1_64_0

LIBS += -L.. -lJASP-Common

windows:CONFIG(ReleaseBuild) {
    LIBS += -llibboost_filesystem-vc141-mt-1_64 -llibboost_system-vc141-mt-1_64 -larchive.dll
}

windows:CONFIG(DebugBuild) {
    LIBS += -llibboost_filesystem-vc141-mt-gd-1_64 -llibboost_system-vc141-mt-gd-1_64 -larchive.dll
}

   macx:LIBS += -lboost_filesystem-clang-mt-1_64 -lboost_system-clang-mt-1_64 -larchive -lz
windows:LIBS += -lole32 -loleaut32

linux {
	exists(/app/lib/*)	{ LIBS += -larchive -lrt -L/app/lib -lboost_filesystem -lboost_system
	} else				{ LIBS += -larchive -lrt -ljsoncpp -lboost_filesystem -lboost_system }
}

macx:QMAKE_CXXFLAGS_WARN_ON += -Wno-unused-parameter -Wno-unused-local-typedef
macx:QMAKE_CXXFLAGS += -Wno-c++11-extensions
macx:QMAKE_CXXFLAGS += -Wno-c++11-long-long
macx:QMAKE_CXXFLAGS += -Wno-c++11-extra-semi
macx:QMAKE_CXXFLAGS += -stdlib=libc++

windows:QMAKE_CXXFLAGS += -DBOOST_USE_WINDOWS_H -DNOMINMAX -D__WIN32__ -DBOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED

INCLUDEPATH += $$PWD/../JASP-Common/

include(JASP-Desktop.pri)

# List all pri files in the analysis
defineReplace(list_pri_files) {
    FILES = $$files($$1)
    PRI_FILES =
    for(file, $$list($$FILES)) {
        exists($$file)
        {
            PRI_FILES *= $$find(file, .*\.pri)
            PRI_FILES *= $$list_pri_files($$file/*)
        }
    }
    return($$PRI_FILES)
}

# Directory containing the analysis forms
ANALYSIS_DIR = $$PWD/analysisforms
# Directory containing the modules
MODULES_DIR = $$list_pri_files($$ANALYSIS_DIR)

# Include all the module pri files
for(file, $$list($$MODULES_DIR)) {
    include($$file)
}


exists(/app/lib/*) {
	flatpak_desktop.files = ../Tools/flatpak/org.jasp.JASP.desktop
	flatpak_desktop.path = /app/share/applications
	INSTALLS += flatpak_desktop

	flatpak_icon.files = ../Tools/flatpak/org.jasp.JASP.svg
	flatpak_icon.path = /app/share/icons/hicolor/scalable/apps
	INSTALLS += flatpak_icon

	flatpak_appinfo.commands = "cd $$PWD/../Tools/flatpak && mkdir -p /app/share/app-info/xmls && gzip -c > /app/share/app-info/xmls/org.jasp.JASP.xml.gz < org.jasp.JASP.appdata.xml"
	QMAKE_EXTRA_TARGETS += flatpak_appinfo
	PRE_TARGETDEPS      += flatpak_appinfo

	#flatpak_appinfo_xml.files = ../Tools/flatpak.org.jasp.JASP.appdata.xml
	#flatpak_appinfo_xml.path = /app/share/appdata
	#INSTALLS += flatpak_appinfo_xml


	flatpak_appinfo_icon.files = ../Tools/flatpak/org.jasp.JASP.svg
	flatpak_appinfo_icon.path = /app/share/app-info/icons/flatpak/scalable
	INSTALLS += flatpak_appinfo_icon

	flatpak_appinfo_icon64.files = ../Tools/flatpak/64/org.jasp.JASP.png
	flatpak_appinfo_icon64.path = /app/share/app-info/icons/flatpak/64x64
	INSTALLS += flatpak_appinfo_icon64

	flatpak_appinfo_icon128.files = ../Tools/flatpak/128/org.jasp.JASP.png
	flatpak_appinfo_icon128.path = /app/share/app-info/icons/flatpak/128x128
	INSTALLS += flatpak_appinfo_icon128
}

#Lets create a nice shellscript that tells us which version of JASP and R we are building/using!
unix {
    SCRIPTFILENAME=$${OUT_PWD}/../versionScript.sh

    createVersionScript.commands += echo \"$${LITERAL_HASH}!/bin/sh\"                                                                           >  $$SCRIPTFILENAME ;
    createVersionScript.commands += echo \"JASP_VERSION_MAJOR=$$JASP_VERSION_MAJOR\"                                                            >> $$SCRIPTFILENAME ;
    createVersionScript.commands += echo \"JASP_VERSION_MINOR=$$JASP_VERSION_MINOR\"                                                            >> $$SCRIPTFILENAME ;
    createVersionScript.commands += echo \"JASP_VERSION_REVISION=$$JASP_VERSION_REVISION\"                                                      >> $$SCRIPTFILENAME ;
    createVersionScript.commands += echo \"JASP_VERSION_BUILD=$$JASP_VERSION_BUILD\n\"                                                          >> $$SCRIPTFILENAME ;
    createVersionScript.commands += echo \"JASP_VERSION=$${JASP_VERSION_MAJOR}.$${JASP_VERSION_MINOR}.$${JASP_VERSION_REVISION}.$${JASP_VERSION_BUILD}\n\"  >> $$SCRIPTFILENAME ;
    createVersionScript.commands += echo \"CURRENT_R_VERSION=$$CURRENT_R_VERSION\"                                                              >> $$SCRIPTFILENAME ;

    QMAKE_EXTRA_TARGETS += createVersionScript
    POST_TARGETDEPS     += createVersionScript
}

#ENVIRONMENT_CRYPTKEY="$(SIMPLECRYPTKEY)"
message("ENVIROMENT_CRYPTKEY: $$[ENVIRONMENT_CRYPTKEY]")
!isEmpty($$[ENVIRONMENT_CRYPTKEY]) {
    DEFINES+="ENVIRONMENT_CRYPTKEY=$$[ENVIRONMENT_CRYPTKEY]"
}

