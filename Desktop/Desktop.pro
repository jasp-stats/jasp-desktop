QT      += webengine webchannel svg network printsupport xml qml quick quickwidgets quickcontrols2
DEFINES += JASP_USES_QT_HERE

SUPPORTED_LANGUAGES = nl de es pt#ja tr

QTQUICK_COMPILER_SKIPPED_RESOURCES += html/html.qrc

include(../JASP.pri)

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets #We need this for the native dialogs (file open, message etc)

include(../R_HOME.pri)

TEMPLATE = app

CONFIG += c++11
CONFIG -= app_bundle

DESTDIR = ..

linux {
  exists(/app/lib/*) {
      TARGET = org.jaspstats.JASP
  } else {
      TARGET = jasp
}} else { #not linux
      TARGET = JASP
}

DEPENDPATH = ..
INCLUDEPATH += ../Common/
QML_IMPORT_PATH +=  components/JASP/        \
                    components/JASP/Theme/   \
                    components/JASP/Widgets/  \
                    components/JASP/Controls/


#exists(/app/lib/*) should only be true when building flatpak
exists(/app/lib/*)	{ target.path += /app/bin }
else                { target.path += /usr/bin }

INSTALLS += target

LIBS += -L.. -lCommon

windows:	LIBS += -llibboost_filesystem$$BOOST_POSTFIX -llibboost_system$$BOOST_POSTFIX -llibboost_date_time$$BOOST_POSTFIX -larchive.dll -llibreadstat -lole32 -loleaut32
macx:   	LIBS += -lboost_filesystem-mt -lboost_system-mt -larchive -lz -lreadstat -liconv

linux {
    LIBS += -larchive
    exists(/app/lib/*)	{ LIBS += -L/app/lib }
    LIBS += -lboost_filesystem -lboost_system -lrt -lreadstat -lm -lz
}

$$JASPTIMER_USED {
  windows:  LIBS += -llibboost_timer$$BOOST_POSTFIX -llibboost_chrono$$BOOST_POSTFIX
  linux:    LIBS += -lboost_timer -lboost_chrono
  macx:     LIBS += -lboost_timer-mt -lboost_chrono-mt
}

INCLUDEPATH += $$PWD/../Common/

# Additional import path used to resolve QML modules in Qt Creator's code model
QML_IMPORT_PATH = $$PWD/imports
QML_IMPORT_PATH += $$PWD/components

exists(/app/lib/*) {
  flatpak_desktop.files = ../Tools/flatpak/org.jaspstats.JASP.desktop
  flatpak_desktop.path = /app/share/applications
  INSTALLS += flatpak_desktop

  flatpak_icon.files = ../Tools/flatpak/org.jaspstats.JASP.svg
  flatpak_icon.path = /app/share/icons/hicolor/scalable/apps
  INSTALLS += flatpak_icon

  #flatpak_appinfo.commands = "cd $$PWD/../Tools/flatpak && mkdir -p /app/share/app-info/xmls && gzip -c > /app/share/app-info/xmls/org.jaspstats.JASP.xml.gz < org.jaspstats.JASP.appdata.xml"
  flatpak_appinfo.commands = "cd $$PWD/../Tools/flatpak && mkdir -p /app/share/metainfo/ && cp org.jaspstats.JASP.appdata.xml /app/share/metainfo/"
  QMAKE_EXTRA_TARGETS += flatpak_appinfo
  PRE_TARGETDEPS      += flatpak_appinfo

  #flatpak_appinfo_xml.files = ../Tools/flatpak.org.jaspstats.JASP.appdata.xml
  #flatpak_appinfo_xml.path = /app/share/appdata
  #INSTALLS += flatpak_appinfo_xml

  flatpak_appinfo_icon.files = ../Tools/flatpak/org.jaspstats.JASP.svg
  flatpak_appinfo_icon.path = /app/share/icons/hicolor/scalable/apps/
  INSTALLS += flatpak_appinfo_icon

  flatpak_appinfo_icon64.files = ../Tools/flatpak/64/org.jaspstats.JASP.png
  flatpak_appinfo_icon64.path = /app/share/icons/hicolor/64x64/apps/
  INSTALLS += flatpak_appinfo_icon64

  flatpak_appinfo_icon128.files = ../Tools/flatpak/128/org.jaspstats.JASP.png
  flatpak_appinfo_icon128.path = /app/share/icons/hicolor/128x128/apps/
  INSTALLS += flatpak_appinfo_icon128
}

#Lets create a nice shellscript that tells us which version of JASP and R we are building/using!
unix {
    SCRIPTFILENAME=$${OUT_PWD}/../versionScript.sh

    createVersionScript.commands += echo \"$${LITERAL_HASH}!/bin/sh\"                                                                               >          $$SCRIPTFILENAME ;
    createVersionScript.commands += echo \"JASP_VERSION_MAJOR=$$JASP_VERSION_MAJOR\"                                                                  >>       $$SCRIPTFILENAME ;
    createVersionScript.commands += echo \"JASP_VERSION_MINOR=$$JASP_VERSION_MINOR\"                                                                    >>     $$SCRIPTFILENAME ;
    createVersionScript.commands += echo \"JASP_VERSION_REVISION=$$JASP_VERSION_REVISION\"                                                                >>   $$SCRIPTFILENAME ;
    createVersionScript.commands += echo \"JASP_VERSION_BUILD=$$JASP_VERSION_BUILD\n\"                                                                      >> $$SCRIPTFILENAME ;
    createVersionScript.commands += echo \"JASP_VERSION=$${JASP_VERSION_MAJOR}.$${JASP_VERSION_MINOR}.$${JASP_VERSION_REVISION}.$${JASP_VERSION_BUILD}\n\"  >> $$SCRIPTFILENAME ;
    createVersionScript.commands += echo \"CURRENT_R_VERSION=$$CURRENT_R_VERSION\"                                                                          >> $$SCRIPTFILENAME ;

    QMAKE_EXTRA_TARGETS += createVersionScript
    POST_TARGETDEPS     += createVersionScript
}

#And of course also a version description to include in the Windows installer
windows {
        WIXFILENAME=$${OUT_PWD}/../jasp.wxi
        createVersionWix.commands += $$quote(echo ^<?xml version=\"1.0\" encoding=\"utf-8\"?^>^<Include^>          >   $${WIXFILENAME}) &&
        createVersionWix.commands += $$quote(echo ^<?define MajorVersion=\"$${JASP_VERSION_MAJOR}\" ?^>            >>  $${WIXFILENAME}) &&
        createVersionWix.commands += $$quote(echo ^<?define MinorVersion=\"$${JASP_VERSION_MINOR}\" ?^>            >>  $${WIXFILENAME}) &&
        createVersionWix.commands += $$quote(echo ^<?define BuildVersion=\"$${JASP_VERSION_BUILD}\" ?^>            >>  $${WIXFILENAME}) &&
        createVersionWix.commands += $$quote(echo ^<?define Revision=\"$${JASP_VERSION_REVISION}\" ?^>^</Include^> >>  $${WIXFILENAME})

        QMAKE_EXTRA_TARGETS += createVersionWix
        POST_TARGETDEPS     += createVersionWix
}

#ENVIRONMENT_CRYPTKEY is set as a "qmake internal var" in the command line
message("ENVIRONMENT_CRYPTKEY: '$$[ENVIRONMENT_CRYPTKEY]'")
COPY_CRYPT = $$[ENVIRONMENT_CRYPTKEY] # We need to copy it to make sure the equals function below actually works...
!equals(COPY_CRYPT, "") {
  !equals(COPY_CRYPT, "\"\"") { #this should be done less stupidly but I do not want to waste my time on that now
    DEFINES+="ENVIRONMENT_CRYPTKEY=$$[ENVIRONMENT_CRYPTKEY]"
    message("Key now set to: '$$COPY_CRYPT'!")
  }
}

   macx:ICON = macOS/icon.icns
windows:RC_FILE = icon.rc

HELP_PATH = $${PWD}/../Docs/help

SOURCES_TRANSLATIONS = $${PWD}/po

RESOURCES_PATH = $${PWD}/../Resources
RESOURCES_TRANSLATIONS = $${RESOURCES_PATH}/Translations

RESOURCES_DESTINATION = $${OUT_PWD}/../Resources
RESOURCES_DESTINATION_TRANSLATIONS = $$RESOURCES_DESTINATION/Translations

#Copying of resources should be done outside generate language files
win32 {

	SOURCES_TRANSLATIONS ~= s,/,\\,g

	RESOURCES_PATH ~= s,/,\\,g
	RESOURCES_TRANSLATIONS ~= s,/,\\,g

	RESOURCES_DESTINATION ~= s,/,\\,g
	RESOURCES_DESTINATION_TRANSLATIONS ~= s,/,\\,g

	EXTENSIONS=cpp,qml
	WINPWD=$$PWD
	WINPWD ~= s,/,\\,g

	delres.commands  += $$quote(IF exist \"$$RESOURCES_DESTINATION\" (rd /s /q \"$$RESOURCES_DESTINATION\";) );
	copyres.commands +=  $$quote(cmd /c xcopy /S /I /Y \"$${RESOURCES_PATH}\" \"$${RESOURCES_DESTINATION}\")
}

unix {
	delres.commands  += rm -rf $$RESOURCES_DESTINATION;
	copyres.commands += $(MKDIR) $$RESOURCES_DESTINATION ;
	copyres.commands += cp -R $$RESOURCES_PATH/* $$RESOURCES_DESTINATION ;
}

$$GENERATE_LANGUAGE_FILES { 
	
	win32 { ######################## Windows language files ##################################
		
		#Update and Cleanup .pot file
		maketranslations.commands += $$quote($${WINQTBIN}lupdate.exe -locations none -extensions $${EXTENSIONS} -recursive \"$${WINPWD}\" -ts \"$$SOURCES_TRANSLATIONS\jaspDesktop.pot\") &&
		maketranslations.commands += $$quote(\"$${GETTEXT_LOCATION}\msgattrib.exe\" --no-obsolete --no-location \"$${SOURCES_TRANSLATIONS}\jaspDesktop.pot\" -o \"$${SOURCES_TRANSLATIONS}\jaspDesktop.pot\") &&
		
		for(LANGUAGE_CODE, SUPPORTED_LANGUAGES) {
			maketranslations.commands += $$quote(echo "Generating Language Files: $${LANGUAGE_CODE}") &&
	
			#Initialize jaspDesktop-xx.po from previous language files
			!exists($$SOURCES_TRANSLATIONS/jaspDesktop-$${LANGUAGE_CODE}.po){
				maketranslations.commands += $$quote(echo "INITIALIZE JASPDESKTOP FILES WITH MSGMERGE: $${LANGUAGE_CODE} ;") &&
				maketranslations.commands += $$quote($${WINQTBIN}lupdate.exe -locations none -extensions $${EXTENSIONS} -recursive \"$${WINPWD}\" -ts \"$$SOURCES_TRANSLATIONS\jaspDesktop-$${LANGUAGE_CODE}.po\") &&
				maketranslations.commands += $$quote(\"$${GETTEXT_LOCATION}\msgmerge.exe\" \"$$SOURCES_TRANSLATIONS\jasp_$${LANGUAGE_CODE}.po\" \"$$SOURCES_TRANSLATIONS\jaspDesktop.pot\" > \"$$SOURCES_TRANSLATIONS\jaspDesktop-$${LANGUAGE_CODE}.po\" ) &&
				maketranslations.commands += $$quote(\"$${GETTEXT_LOCATION}\msgattrib.exe\" --no-obsolete --no-location \""$$SOURCES_TRANSLATIONS\jaspDesktop-$${LANGUAGE_CODE}.po\" -o \""$$SOURCES_TRANSLATIONS\jaspDesktop-$${LANGUAGE_CODE}.po\") &&
			}
			
			#Update and Cleanup .po file
			maketranslations.commands += $$quote($${WINQTBIN}lupdate.exe -locations none -extensions $${EXTENSIONS} -recursive \"$${WINPWD}\" -ts \"$$SOURCES_TRANSLATIONS\jaspDesktop-$${LANGUAGE_CODE}.po\") &&
			maketranslations.commands += $$quote(\"$${GETTEXT_LOCATION}\msgattrib.exe\" --no-obsolete --no-location \""$$SOURCES_TRANSLATIONS\jaspDesktop-$${LANGUAGE_CODE}.po\" -o \""$$SOURCES_TRANSLATIONS\jaspDesktop-$${LANGUAGE_CODE}.po\") &&
			
			#Create jaspDesktop-$${LANGUAGE_CODE}.qm
			maketranslations.commands += $$quote($${WINQTBIN}lrelease.exe \"$$SOURCES_TRANSLATIONS\jaspDesktop-$${LANGUAGE_CODE}.po\" -qm \"$$RESOURCES_TRANSLATIONS\jaspDesktop-$${LANGUAGE_CODE}.qm\") &&
			
		}#Loop over Languages
		
		#Create R-jaspGraphs.mo translation file. (Need to add GETTEXT location to PATH environment.)
		maketranslations.commands += $$quote(\"$$PWD/../Tools/translate.cmd\" \"$$_R_HOME/bin\" \"$${GETTEXT_LOCATION}\" \"$$PWD/../Tools\" \"$$PWD/../Engine/jaspGraphs\" ) &&
		
		#Create R-jaspBase.mo translation file. (Need to add GETTEXT location to PATH environment.)
		maketranslations.commands += $$quote(\"$$PWD/../Tools/translate.cmd\" \"$$_R_HOME/bin\" \"$${GETTEXT_LOCATION}\" \"$$PWD/../Tools\" \"$$PWD/../Engine/jaspBase\" ) &&
		maketranslations.commands += $$quote(copy \"$${RESOURCES_TRANSLATIONS}\*.qm\" \"$${RESOURCES_DESTINATION_TRANSLATIONS}\" ) &&
		maketranslations.depends  = copyres
		maketranslations.commands += $$quote(echo "End translation task")
	}
	
	unix { ######################## Unix language files ##################################
			
		#Update and  Cleanup .pot file
		maketranslations.commands += PATH=$$EXTENDED_PATH lupdate -locations none -extensions cpp,qml -recursive $$PWD -ts $$SOURCES_TRANSLATIONS/jaspDesktop.pot ;   
		maketranslations.commands += PATH=$$EXTENDED_PATH msgattrib --no-obsolete --no-location $$SOURCES_TRANSLATIONS/jaspDesktop.pot -o $$SOURCES_TRANSLATIONS/jaspDesktop.pot ;
	
		for(LANGUAGE_CODE, SUPPORTED_LANGUAGES) {
			maketranslations.commands += $$quote(echo "Generating language File: $${LANGUAGE_CODE}" ;) 

			#Update and Cleanup QML .po file
			maketranslations.commands += PATH=$$EXTENDED_PATH lupdate -locations none -extensions cpp,qml -recursive $$PWD -ts $$SOURCES_TRANSLATIONS/jaspDesktop-$${LANGUAGE_CODE}.po ;
			maketranslations.commands += PATH=$$EXTENDED_PATH msgattrib --no-obsolete --no-location $$SOURCES_TRANSLATIONS/jaspDesktop-$${LANGUAGE_CODE}.po -o $$SOURCES_TRANSLATIONS/jaspDesktop-$${LANGUAGE_CODE}.po ;
			
			#Create jaspDesktop-$${LANGUAGE_CODE}.qm
			maketranslations.commands += PATH=$$EXTENDED_PATH lrelease  $$SOURCES_TRANSLATIONS/jaspDesktop-$${LANGUAGE_CODE}.po -qm $$RESOURCES_TRANSLATIONS/jaspDesktop-$${LANGUAGE_CODE}.qm ;
			
		}#Loop over languages
		
		#Create jaspBase.mo translation files. (Need to add GETTEXT location to PATH environment.)
		maketranslations.commands += JASP_R_HOME=$$_R_HOME \"$$R_EXE\" -e \"rootfolder <- \'$$PWD/../Engine/jaspBase\';    source(\'$$PWD/../Tools/translate.R\');\"
		maketranslations.commands += JASP_R_HOME=$$_R_HOME \"$$R_EXE\" -e \"rootfolder <- \'$$PWD/../Engine/jaspGraphs\';  source(\'$$PWD/../Tools/translate.R\');\"
	
		#Copy to Resources
		maketranslations.commands += cp $$RESOURCES_TRANSLATIONS/*.qm $$RESOURCES_DESTINATION_TRANSLATIONS/ ;
		maketranslations.depends  = copyres
	}
}

copyres.depends = delres

! equals(PWD, $${OUT_PWD}) {
  QMAKE_EXTRA_TARGETS += delres copyres maketranslations
  POST_TARGETDEPS     += delres copyres maketranslations
}

INCLUDEPATH += $$PWD/

# Is this truly necessary? It does make the files show up nicely in the project structure, but the actual installing is done by the copyres a little I figure?
Resources.path = $$INSTALLPATH
Resources.files = ../Resources
INSTALLS += Resources

HEADERS += \
    analysis/analysisform.h \
    analysis/knownissues.h \
	analysis/term.h \
	analysis/terms.h \
    analysis/analyses.h \
    analysis/analysis.h \
    data/datasettableproxy.h \
    data/exporters/dataexporter.h \
    data/exporters/exporter.h \
    data/exporters/jaspexporter.h \
    data/exporters/resultexporter.h \
    data/importers/ods/odsimportcolumn.h \
    data/importers/ods/odsimportdataset.h \
    data/importers/ods/odssheetcell.h \
    data/importers/ods/odstypes.h \
    data/importers/ods/odsxmlcontentshandler.h \
    data/importers/ods/odsxmlhandler.h \
    data/importers/ods/odsxmlmanifesthandler.h \
    data/importers/readstat/readstat.h \
    data/importers/readstat/readstatimportcolumn.h \
    data/importers/readstat/readstatimportdataset.h \
    data/importers/readstatimporter.h \
    data/importers/codepageconvert.h \
    data/importers/convertedstringcontainer.h \
    data/importers/csv.h \
    data/importers/csvimportcolumn.h \
    data/importers/csvimporter.h \
    data/importers/importcolumn.h \
    data/importers/importdataset.h \
    data/importers/importer.h \
    data/importers/importerutils.h \
    data/importers/jaspimporter.h \
    data/importers/odsimporter.h \
    data/asyncloader.h \
    data/asyncloaderthread.h \
    data/columnsmodel.h \
    data/computedcolumn.h \
    data/computedcolumns.h \
    data/computedcolumnsmodel.h \
    data/datasetloader.h \
    data/datasetpackage.h \
    data/fileevent.h \
	analysis/variableinfo.h \
    engine/enginerepresentation.h \
    engine/enginesync.h \
    engine/rscriptstore.h \
    gui/columntypesmodel.h \
    modules/description/description.h \
    modules/description/descriptionchildbase.h \
    modules/description/entrybase.h \
    modules/upgrader/changebase.h \
    modules/upgrader/changecopy.h \
    modules/upgrader/changejs.h \
    modules/upgrader/changeremove.h \
    modules/upgrader/changerename.h \
    modules/upgrader/changesetvalue.h \
    modules/upgrader/upgrade.h \
    modules/upgrader/upgradeDefinitions.h \
    modules/upgrader/upgradechange.h \
    modules/upgrader/upgrader.h \
    modules/upgrader/upgrades.h \
    modules/upgrader/upgradestep.h \
    modules/ribbonmodeluncommon.h \
    qquick/datasetview.h \
    modules/analysisentry.h \
    modules/dynamicmodule.h \
    modules/dynamicmodules.h \
    modules/ribbonmodel.h \
    modules/analysismenumodel.h \
    osf/onlinedataconnection.h \
    osf/onlinedatamanager.h \
    osf/onlinedatanode.h \
    osf/onlinedatanodeosf.h \
    osf/onlinenode.h \
    osf/onlineusernode.h \
    osf/onlineusernodeosf.h \
    osf/osfnam.h \
    qquick/jasptheme.h \
    qquick/rcommander.h \
    utilities/appdirs.h \
    utilities/application.h \
    utilities/jsonutilities.h \
    utilities/qutils.h \
    results/resultsjsinterface.h \
    utilities/settings.h \
    utilities/simplecrypt.h \
    utilities/languagemodel.h \
    utilities/simplecryptkey.h \
    data/labelfiltergenerator.h \
    widgets/filemenu/filemenuobject.h \
    widgets/filemenu/datalibrary.h \
    widgets/filemenu/filesystem.h \
    widgets/filemenu/osffilesystem.h \
    widgets/filemenu/recentfiles.h \
    widgets/filemenu/computer.h \
    widgets/filemenu/osf.h \
    widgets/filemenu/datalibrarybreadcrumbsmodel.h \
    widgets/filemenu/datalibrarylistmodel.h \
    widgets/filemenu/computerfilesystem.h \
    widgets/filemenu/filesystementry.h \
    widgets/listmodeljagsdatainput.h \
    widgets/listmodeltermsassigned.h \
    widgets/listmodeltermsavailable.h \
    mainwindow.h \
    utilities/extractarchive.h \
    widgets/listmodel.h \
    widgets/listmodelassignedinterface.h \
    widgets/listmodelavailableinterface.h \
    widgets/listmodeldraggable.h \
    widgets/listmodelmeasurescellsassigned.h \
    widgets/listmodellayersassigned.h \
    widgets/listmodelmultinomialchi2test.h  \
    data/filtermodel.h \
    widgets/filemenu/recentfileslistmodel.h \
    widgets/filemenu/datalibraryfilesystem.h \
    widgets/filemenu/recentfilesfilesystem.h \
    widgets/filemenu/currentfilelistmodel.h \
    widgets/filemenu/currentfilefilesystem.h \
    widgets/filemenu/computerlistmodel.h \
    widgets/filemenu/osflistmodel.h \
    widgets/filemenu/osfbreadcrumbslistmodel.h \
    resultstesting/compareresults.h \
    resultstesting/resultscomparetable.h \
    widgets/filemenu/filemenu.h \
    gui/messageforwarder.h \
    widgets/filemenu/filemenulistitem.h \
    widgets/filemenu/filemenubasiclistmodel.h \
    modules/ribbonmodelfiltered.h \
    utilities/helpmodel.h \
    widgets/lavaansyntaxhighlighter.h \
    widgets/listmodelinteractionassigned.h \
    gui/preferencesmodel.h \
    widgets/filemenu/actionbuttons.h \
    widgets/filemenu/resourcebuttons.h \
    widgets/filemenu/resourcebuttonsvisible.h \
    widgets/listmodelrepeatedmeasuresfactors.h \
    widgets/interactionmodel.h \
    widgets/listmodelinteractionavailable.h \
    results/resultmenuentry.h \
    results/resultmenumodel.h \
    analysis/jaspdoublevalidator.h \
    widgets/listmodelfactorsform.h \
    gui/aboutmodel.h \
    modules/ribbonbutton.h \
    widgets/filemenu/currentdatafile.h \
    gui/jaspversionchecker.h \
    widgets/listmodeltableviewbase.h \
    widgets/sortmenumodel.h \
    widgets/sortable.h \
    widgets/listmodelfiltereddataentry.h \
    data/importers/readstat/readstat_custom_io.h \
    data/importers/readstat/readstat_windows_helper.h \
    data/datasettablemodel.h \
    data/labelmodel.h \
    results/ploteditormodel.h \
    results/ploteditoraxismodel.h \
    results/ploteditorcoordinates.h \
    widgets/listmodelinputvalue.h \
    widgets/rowcontrols.h \
    widgets/listmodelmultitermsassigned.h \
    widgets/listmodelcustomcontrasts.h \
    widgets/listmodellabelvalueterms.h \
    analysis/jaspcontrol.h \
    widgets/checkboxbase.h \
    widgets/comboboxbase.h \
    widgets/textinputbase.h \
    widgets/componentslistbase.h \
    widgets/factorsformbase.h \
    widgets/inputlistbase.h \
    widgets/sliderbase.h \
    widgets/expanderbuttonbase.h \
    widgets/variableslistbase.h \
    widgets/boundcontrolmeasurescells.h \
    widgets/boundcontrollayers.h \
    widgets/boundcontrolterms.h \
    widgets/boundcontrolmultiterms.h \
    widgets/tableviewbase.h \
    widgets/textareabase.h \
    widgets/boundcontroltextarea.h \
    widgets/boundcontrolsourcetextarea.h \
    widgets/boundcontrollavaantextarea.h \
    widgets/boundcontroljagstextarea.h \
    widgets/radiobuttonsgroupbase.h \
    widgets/jasplistcontrol.h \
    widgets/radiobuttonbase.h \
    widgets/repeatedmeasuresfactorslistbase.h \
    widgets/sourceitem.h \
    analysis/boundcontrol.h \
    analysis/boundcontrolbase.h \
    widgets/boundcontroltableview.h \
    widgets/boundcontrolcontraststableview.h \
    widgets/boundcontrolfilteredtableview.h

SOURCES += \
    analysis/analysisform.cpp \
    analysis/knownissues.cpp \
	analysis/term.cpp \
	analysis/terms.cpp \
    analysis/analyses.cpp \
    analysis/analysis.cpp \
    data/datasettableproxy.cpp \
    data/exporters/dataexporter.cpp \
    data/exporters/exporter.cpp \
    data/exporters/jaspexporter.cpp \
    data/exporters/resultexporter.cpp \
    data/importers/ods/odsimportcolumn.cpp \
    data/importers/ods/odsimportdataset.cpp \
    data/importers/ods/odssheetcell.cpp \
    data/importers/ods/odstypes.cpp \
    data/importers/ods/odsxmlcontentshandler.cpp \
    data/importers/ods/odsxmlhandler.cpp \
    data/importers/ods/odsxmlmanifesthandler.cpp \
    data/importers/readstat/readstatimportcolumn.cpp \
    data/importers/readstat/readstatimportdataset.cpp \
    data/importers/readstatimporter.cpp \
    data/importers/codepageconvert.cpp \
    data/importers/convertedstringcontainer.cpp \
    data/importers/csv.cpp \
    data/importers/csvimportcolumn.cpp \
    data/importers/csvimporter.cpp \
    data/importers/importcolumn.cpp \
    data/importers/importdataset.cpp \
    data/importers/importer.cpp \
    data/importers/jaspimporter.cpp \
    data/importers/odsimporter.cpp \
    data/asyncloader.cpp \
    data/asyncloaderthread.cpp \
    data/columnsmodel.cpp \
    data/computedcolumn.cpp \
    data/computedcolumns.cpp \
    data/computedcolumnsmodel.cpp \
    data/datasetloader.cpp \
    data/datasetpackage.cpp \
    data/fileevent.cpp \
    engine/enginerepresentation.cpp \
    engine/enginesync.cpp \
    gui/columntypesmodel.cpp \
    modules/description/description.cpp \
    modules/description/descriptionchildbase.cpp \
    modules/description/entrybase.cpp \
    modules/upgrader/changebase.cpp \
    modules/upgrader/changecopy.cpp \
    modules/upgrader/changejs.cpp \
    modules/upgrader/changeremove.cpp \
    modules/upgrader/changerename.cpp \
    modules/upgrader/changesetvalue.cpp \
    modules/upgrader/upgrade.cpp \
    modules/upgrader/upgradeDefinitions.cpp \
    modules/upgrader/upgradechange.cpp \
    modules/upgrader/upgrader.cpp \
    modules/upgrader/upgrades.cpp \
    modules/upgrader/upgradestep.cpp \
    modules/ribbonmodeluncommon.cpp \
    qquick/datasetview.cpp \
    modules/analysisentry.cpp \
    modules/dynamicmodule.cpp \
    modules/dynamicmodules.cpp \
    modules/ribbonmodel.cpp \
    modules/analysismenumodel.cpp \
    osf/onlinedataconnection.cpp \
    osf/onlinedatamanager.cpp \
    osf/onlinedatanode.cpp \
    osf/onlinedatanodeosf.cpp \
    osf/onlinenode.cpp \
    osf/onlineusernode.cpp \
    osf/onlineusernodeosf.cpp \
    osf/osfnam.cpp \
    qquick/jasptheme.cpp \
    qquick/rcommander.cpp \
    utilities/appdirs.cpp \
    utilities/application.cpp \
    utilities/jsonutilities.cpp \
    utilities/qutils.cpp \
    results/resultsjsinterface.cpp \
    utilities/settings.cpp \
    utilities/simplecrypt.cpp \
    utilities/languagemodel.cpp \
    data/labelfiltergenerator.cpp \
    widgets/filemenu/filemenuobject.cpp \
    widgets/filemenu/datalibrary.cpp \
    widgets/filemenu/filesystem.cpp \
    widgets/filemenu/osffilesystem.cpp \
    widgets/filemenu/recentfiles.cpp \
    widgets/filemenu/computer.cpp \
    widgets/filemenu/osf.cpp \
    widgets/filemenu/datalibrarybreadcrumbsmodel.cpp \
    widgets/filemenu/datalibrarylistmodel.cpp \
    widgets/filemenu/computerfilesystem.cpp \
    widgets/listmodeljagsdatainput.cpp \
    widgets/listmodeltermsassigned.cpp \
    widgets/listmodeltermsavailable.cpp \
    main.cpp \
    mainwindow.cpp \
    utilities/extractarchive.cpp \
    widgets/listmodel.cpp \
    widgets/listmodelavailableinterface.cpp \
    widgets/listmodelassignedinterface.cpp \
    widgets/listmodeldraggable.cpp \
    widgets/listmodelmeasurescellsassigned.cpp \
    widgets/listmodellayersassigned.cpp \
    widgets/listmodelmultinomialchi2test.cpp \
    data/filtermodel.cpp \
    widgets/filemenu/recentfileslistmodel.cpp \
    widgets/filemenu/datalibraryfilesystem.cpp \
    widgets/filemenu/recentfilesfilesystem.cpp \
    widgets/filemenu/currentfilelistmodel.cpp \
    widgets/filemenu/currentfilefilesystem.cpp \
    widgets/filemenu/computerlistmodel.cpp \
    widgets/filemenu/osflistmodel.cpp \
    widgets/filemenu/osfbreadcrumbslistmodel.cpp \
    resultstesting/compareresults.cpp \
    resultstesting/resultscomparetable.cpp \
    widgets/filemenu/filemenu.cpp \
    gui/messageforwarder.cpp \
    widgets/filemenu/filemenubasiclistmodel.cpp \
    modules/ribbonmodelfiltered.cpp \
    utilities/helpmodel.cpp \
    widgets/lavaansyntaxhighlighter.cpp \
    widgets/listmodelinteractionassigned.cpp \
    gui/preferencesmodel.cpp \
    widgets/filemenu/actionbuttons.cpp \
    widgets/filemenu/resourcebuttons.cpp \
    widgets/filemenu/resourcebuttonsvisible.cpp \
    widgets/listmodelrepeatedmeasuresfactors.cpp \
    widgets/interactionmodel.cpp \
    widgets/listmodelinteractionavailable.cpp \
    results/resultmenumodel.cpp \
    results/resultmenuentry.cpp \
    analysis/jaspdoublevalidator.cpp \
    widgets/listmodelfactorsform.cpp \
    gui/aboutmodel.cpp \
    modules/ribbonbutton.cpp \
    widgets/filemenu/currentdatafile.cpp \
    gui/jaspversionchecker.cpp \
    widgets/listmodeltableviewbase.cpp \
    widgets/sortmenumodel.cpp \
    widgets/sortable.cpp \
    widgets/listmodelfiltereddataentry.cpp \
    data/importers/readstat/readstat_custom_io.cpp \
    data/datasettablemodel.cpp \
    data/labelmodel.cpp \
    results/ploteditormodel.cpp \
    results/ploteditoraxismodel.cpp \
    results/ploteditorcoordinates.cpp \
    widgets/listmodelinputvalue.cpp \
    widgets/rowcontrols.cpp \
    widgets/listmodelmultitermsassigned.cpp \
    widgets/listmodelcustomcontrasts.cpp \
    widgets/listmodellabelvalueterms.cpp \
    analysis/jaspcontrol.cpp \
    widgets/checkboxbase.cpp \
    widgets/comboboxbase.cpp \
    widgets/textinputbase.cpp \
    widgets/componentslistbase.cpp \
    widgets/factorsformbase.cpp \
    widgets/inputlistbase.cpp \
    widgets/sliderbase.cpp \
    widgets/expanderbuttonbase.cpp \
    widgets/variableslistbase.cpp \
    widgets/boundcontrolmeasurescells.cpp \
    widgets/boundcontrollayers.cpp \
    widgets/boundcontrolterms.cpp \
    widgets/boundcontrolmultiterms.cpp \
    widgets/tableviewbase.cpp \
    widgets/textareabase.cpp \
    widgets/boundcontroltextarea.cpp \
    widgets/boundcontrolsourcetextarea.cpp \
    widgets/boundcontrollavaantextarea.cpp \
    widgets/boundcontroljagstextarea.cpp \
    widgets/radiobuttonsgroupbase.cpp \
    widgets/jasplistcontrol.cpp \
    widgets/radiobuttonbase.cpp \
    widgets/repeatedmeasuresfactorslistbase.cpp \
    widgets/sourceitem.cpp \
    analysis/boundcontrolbase.cpp \
    widgets/boundcontroltableview.cpp \
    widgets/boundcontrolcontraststableview.cpp \
    widgets/boundcontrolfilteredtableview.cpp

RESOURCES += \
    html/html.qrc \
    resources/icons.qrc \
    resources/resources.qrc \
    qml.qrc

   unix:OTHER_FILES += macOS/icon.icns
windows:OTHER_FILES += icon.rc

DISTFILES += \
    modules/upgrader/upgrades.json \
    modules/upgrader/upgrades_template.json \
    po/jasp.po \
    po/jasp_nl.po \
    resources/CC-Attributions.txt
