QT      += webengine webchannel svg network printsupport xml qml quick quickwidgets quickcontrols2
DEFINES += JASP_USES_QT_HERE

GENERATE_LANGUAGE_FILES = false

#AM_I_BUILDBOT is set as a "qmake internal var" in the command line
message("AM_I_BUILDBOT: '$$[AM_I_BUILDBOT]'")
COPY_BUILDBOTNESS = $$[AM_I_BUILDBOT] # We need to copy it to make sure the equals function below actually works...
!equals(COPY_BUILDBOTNESS, "") {
!equals(COPY_BUILDBOTNESS, "\"\"") { #this should be done less stupidly but I do not want to waste my time on that now
     GENERATE_LANGUAGE_FILES = true
}
}

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
INCLUDEPATH += ../JASP-Common/

#exists(/app/lib/*) should only be true when building flatpak
exists(/app/lib/*)	{ target.path += /app/bin }
else                { target.path += /usr/bin }

INSTALLS += target

LIBS += -L.. -lJASP-Common

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

INCLUDEPATH += $$PWD/../JASP-Common/

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

LANGUAGE_CODE= $$(TARGET_LANGUAGE_CODE)
isEmpty(LANGUAGE_CODE): LANGUAGE_CODE=nl

win32 {

  SOURCES_TRANSLATIONS ~= s,/,\\,g

  RESOURCES_PATH ~= s,/,\\,g
  RESOURCES_TRANSLATIONS ~= s,/,\\,g

  RESOURCES_DESTINATION ~= s,/,\\,g
  RESOURCES_DESTINATION_TRANSLATIONS ~= s,/,\\,g

  QTBIN=$$QMAKE_QMAKE
  QTBIN ~= s,qmake.exe,,g
  QTBIN ~= s,/,\\,g
  EXTENSIONS=cpp,qml
  WINPWD=$$PWD/..
  WINPWD ~= s,/,\\,g

  GETTEXT_LOCATION = $$(GETTEXT_PATH)
  isEmpty(GETTEXT_LOCATION): GETTEXT_LOCATION=$${_GIT_LOCATION}\usr\bin

  delres.commands  += $$quote(IF exist \"$$RESOURCES_DESTINATION\" (rd /s /q \"$$RESOURCES_DESTINATION\";) );
  copyres.commands +=  $$quote(cmd /c xcopy /S /I /Y $${RESOURCES_PATH} $${RESOURCES_DESTINATION})

  $$GENERATE_LANGUAGE_FILES {
    maketranslations.commands += $$quote(echo "Generating language Files") &&
	maketranslations.commands += $$quote($${QTBIN}lupdate.exe -locations none -extensions $${EXTENSIONS} -recursive $${WINPWD} -ts $${SOURCES_TRANSLATIONS}\jasp.po) &&
	maketranslations.commands += $$quote($${QTBIN}lupdate.exe -locations none -extensions $${EXTENSIONS} -recursive $${WINPWD} -ts $${SOURCES_TRANSLATIONS}\jasp_$${LANGUAGE_CODE}.po) &&

	#cleanup po files
	maketranslations.commands += $$quote(\"$${GETTEXT_LOCATION}\msgattrib.exe\" --no-obsolete --no-location $${SOURCES_TRANSLATIONS}\jasp.po -o $${SOURCES_TRANSLATIONS}\jasp.po) &&
	maketranslations.commands += $$quote(\"$${GETTEXT_LOCATION}\msgattrib.exe\" --no-obsolete --no-location $${SOURCES_TRANSLATIONS}\jasp_$${LANGUAGE_CODE}.po -o $${SOURCES_TRANSLATIONS}\jasp_$${LANGUAGE_CODE}.po)  &&

	#Create jasp_$${LANGUAGE_CODE}.qm
	maketranslations.commands += $$quote($${QTBIN}lrelease.exe $${SOURCES_TRANSLATIONS}\jasp_$${LANGUAGE_CODE}.po -qm $${RESOURCES_TRANSLATIONS}\jasp_$${LANGUAGE_CODE}.qm) &&
	maketranslations.commands += $$quote(copy $${RESOURCES_TRANSLATIONS}\*.qm $${RESOURCES_DESTINATION_TRANSLATIONS}\ ) &&

    #Create R-JASP.mo translation file. (Need to add GETTEXT location to PATH environment.)
    maketranslations.commands += $$quote($$PWD/../Tools/translate.cmd $$_R_HOME/bin \"$${GETTEXT_LOCATION}\" $$PWD/../Tools $$PWD/../JASP-Engine/JASP ) &&

    #Create R-JASPgraphs.mo translation file. (Need to add GETTEXT location to PATH environment.)
    maketranslations.commands += $$quote($$PWD/../Tools/translate.cmd $$_R_HOME/bin \"$${GETTEXT_LOCATION}\" $$PWD/../Tools $$PWD/../JASP-Engine/JASPgraphs )

    maketranslations.depends  = copyres
  }
}

unix {

  GETTEXT_LOCATION = $$(GETTEXT_PATH)
  isEmpty(GETTEXT_LOCATION): GETTEXT_LOCATION=/usr/local/bin

  EXTENDED_PATH = $$(PATH):$$GETTEXT_LOCATION:$$_R_HOME

  delres.commands += rm -rf $$RESOURCES_DESTINATION;
  copyres.commands += $(MKDIR) $$RESOURCES_DESTINATION ;
  copyres.commands += cp -R $$RESOURCES_PATH/* $$RESOURCES_DESTINATION ;

  $$GENERATE_LANGUAGE_FILES {
    maketranslations.commands += $$quote(echo "Generating language Files");
	maketranslations.commands += export PATH=$$EXTENDED_PATH;
    maketranslations.commands += lupdate -locations none -extensions cpp,qml -recursive $$PWD/.. -ts $$SOURCES_TRANSLATIONS/jasp.po ;
    maketranslations.commands += lupdate -locations none -extensions cpp,qml -recursive $$PWD/.. -ts $$SOURCES_TRANSLATIONS/jasp_$${LANGUAGE_CODE}.po ;

    #cleanup po files
	maketranslations.commands += msgattrib --no-obsolete --no-location $$SOURCES_TRANSLATIONS/jasp.po -o $$SOURCES_TRANSLATIONS/jasp.po ;
	maketranslations.commands += msgattrib --no-obsolete --no-location $$SOURCES_TRANSLATIONS/jasp_$${LANGUAGE_CODE}.po -o $$SOURCES_TRANSLATIONS/jasp_$${LANGUAGE_CODE}.po ;

    #Create jasp_$${LANGUAGE_CODE}.qm
    maketranslations.commands += lrelease $$SOURCES_TRANSLATIONS/jasp_$${LANGUAGE_CODE}.po -qm $$RESOURCES_TRANSLATIONS/jasp_$${LANGUAGE_CODE}.qm ;
    maketranslations.commands += cp $$RESOURCES_TRANSLATIONS/*.qm $$RESOURCES_DESTINATION_TRANSLATIONS/ ;

    #Create R-JASP.mo translation file. (Need to add GETTEXT location to PATH environment.)
	maketranslations.commands +=  Rscript $$PWD/../Tools/translate.R $$PWD/../JASP-Engine/JASP ;

    #Create R-JASPgraphs.mo translation file. (Need to add GETTEXT location to PATH environment.)
	maketranslations.commands +=  Rscript $$PWD/../Tools/translate.R $$PWD/../JASP-Engine/JASPgraphs ;


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
    analysis/boundqmlitem.h \
    analysis/options/bound.h \
    analysis/options/option.h \
    analysis/options/optionboolean.h \
    analysis/options/optioncomputedcolumn.h \
    analysis/options/optiondoublearray.h \
    analysis/options/optionencodablestring.h \
    analysis/options/optioni.h \
    analysis/options/optioninteger.h \
    analysis/options/optionintegerarray.h \
    analysis/options/optionlist.h \
    analysis/options/optionnumber.h \
    analysis/options/options.h \
    analysis/options/optionstable.h \
    analysis/options/optionstring.h \
    analysis/options/optionterm.h \
    analysis/options/optionterms.h \
    analysis/options/optionvariable.h \
    analysis/options/optionvariablei.h \
    analysis/options/optionvariables.h \
    analysis/options/optionvariablesgroups.h \
    analysis/options/term.h \
    analysis/options/terms.h \
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
    analysis/options/variableinfo.h \
    engine/enginerepresentation.h \
    engine/enginesync.h \
    engine/rscriptstore.h \
    gui/columntypesmodel.h \
    modules/description/description.h \
    modules/description/descriptionchildbase.h \
    modules/description/entrybase.h \
    modules/description/requiredpackage.h \
    modules/upgrader/upgradechange.h \
    modules/upgrader/upgrader.h \
    modules/upgrader/upgradestep.h \
    modules/upgrader/version.h \
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
    widgets/boundqmlcheckbox.h \
    widgets/boundqmlradiobuttons.h \
    widgets/boundqmltextinput.h \
    widgets/boundqmlcombobox.h \
    widgets/listmodelanovacustomcontrasts.h \
    widgets/listmodeljagsdatainput.h \
    widgets/listmodeltermsassigned.h \
    widgets/listmodeltermsavailable.h \
    mainwindow.h \
    utilities/extractarchive.h \
    widgets/listmodel.h \
    widgets/qmllistview.h \
    widgets/listmodelassignedinterface.h \
    widgets/listmodelavailableinterface.h \
    widgets/listmodeldraggable.h \
    widgets/qmllistviewdraggable.h \
    widgets/boundqmllistviewmeasurescells.h \
    widgets/boundqmllistviewdraggable.h \
    widgets/listmodelmeasurescellsassigned.h \
    widgets/qmllistviewtermsavailable.h \
    widgets/boundqmllistviewterms.h \
    widgets/boundqmlslider.h \
    widgets/boundqmltextarea.h \
    widgets/boundqmltableview.h \
    widgets/boundqmllistviewlayers.h \
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
    widgets/boundqmlrepeatedmeasuresfactors.h \
    widgets/listmodelrepeatedmeasuresfactors.h \
    widgets/interactionmodel.h \
    widgets/listmodelinteractionavailable.h \
    results/resultmenuentry.h \
    results/resultmenumodel.h \
    analysis/jaspdoublevalidator.h \
    widgets/boundqmlfactorsform.h \
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
    widgets/boundqmlinputlist.h \
    widgets/listmodelinputvalue.h \
    analysis/jaspcontrolbase.h \
    widgets/jaspcontrolwrapper.h \
    widgets/rowcontrols.h \
    widgets/qmlexpander.h \
    widgets/listmodelmultitermsassigned.h \
    widgets/boundqmllavaantextarea.h \
    widgets/boundqmljagstextarea.h \
    widgets/boundqmlcomponentslist.h \
    widgets/listmodelmarginalmeanscontrasts.h

SOURCES += \
    analysis/analysisform.cpp \
    analysis/boundqmlitem.cpp \
    analysis/options/option.cpp \
    analysis/options/optionboolean.cpp \
    analysis/options/optioncomputedcolumn.cpp \
    analysis/options/optiondoublearray.cpp \
    analysis/options/optionencodablestring.cpp \
    analysis/options/optioninteger.cpp \
    analysis/options/optionintegerarray.cpp \
    analysis/options/optionlist.cpp \
    analysis/options/optionnumber.cpp \
    analysis/options/options.cpp \
    analysis/options/optionstable.cpp \
    analysis/options/optionstring.cpp \
    analysis/options/optionterm.cpp \
    analysis/options/optionterms.cpp \
    analysis/options/optionvariable.cpp \
    analysis/options/optionvariables.cpp \
    analysis/options/optionvariablesgroups.cpp \
    analysis/options/term.cpp \
    analysis/options/terms.cpp \
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
    modules/description/requiredpackage.cpp \
    modules/upgrader/upgradechange.cpp \
    modules/upgrader/upgrader.cpp \
    modules/upgrader/upgradestep.cpp \
    modules/upgrader/version.cpp \
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
    widgets/boundqmlcheckbox.cpp \
    widgets/boundqmlradiobuttons.cpp \
    widgets/boundqmltextinput.cpp \
    widgets/boundqmlcombobox.cpp \
    widgets/listmodelanovacustomcontrasts.cpp \
    widgets/listmodeljagsdatainput.cpp \
    widgets/listmodeltermsassigned.cpp \
    widgets/listmodeltermsavailable.cpp \
    main.cpp \
    mainwindow.cpp \
    utilities/extractarchive.cpp \
    widgets/listmodel.cpp \
    widgets/qmllistview.cpp \
    widgets/listmodelavailableinterface.cpp \
    widgets/listmodelassignedinterface.cpp \
    widgets/listmodeldraggable.cpp \
    widgets/qmllistviewdraggable.cpp \
    widgets/boundqmllistviewmeasurescells.cpp \
    widgets/boundqmllistviewdraggable.cpp \
    widgets/listmodelmeasurescellsassigned.cpp \
    widgets/qmllistviewtermsavailable.cpp \
    widgets/boundqmllistviewterms.cpp \
    widgets/boundqmlslider.cpp \
    widgets/boundqmltextarea.cpp \
    widgets/boundqmltableview.cpp \
    widgets/boundqmllistviewlayers.cpp \
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
    widgets/boundqmlrepeatedmeasuresfactors.cpp \
    widgets/listmodelrepeatedmeasuresfactors.cpp \
    widgets/interactionmodel.cpp \
    widgets/listmodelinteractionavailable.cpp \
    results/resultmenumodel.cpp \
    results/resultmenuentry.cpp \
    analysis/jaspdoublevalidator.cpp \
    widgets/boundqmlfactorsform.cpp \
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
    widgets/boundqmlinputlist.cpp \
    widgets/listmodelinputvalue.cpp \
    analysis/jaspcontrolbase.cpp \
    widgets/jaspcontrolwrapper.cpp \
    widgets/rowcontrols.cpp \
    widgets/qmlexpander.cpp \
    widgets/listmodelmultitermsassigned.cpp \
    widgets/boundqmllavaantextarea.cpp \
    widgets/boundqmljagstextarea.cpp \
    widgets/boundqmlcomponentslist.cpp \
    widgets/listmodelmarginalmeanscontrasts.cpp

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
