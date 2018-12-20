QT += core gui webenginewidgets webchannel svg network printsupport xml qml quick quickwidgets quickcontrols2
DEFINES += JASP_USES_QT_HERE

include(../JASP.pri)

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

include(../R_HOME.pri)

CONFIG += c++11

DESTDIR = ..

windows:TARGET = JASP
   macx:TARGET = JASP
  linux:{ exists(/app/lib/*) {TARGET = org.jasp.JASP } else { TARGET = jasp }}


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
    LIBS += -larchive
    exists(/app/lib/*)	{ LIBS += -L/app/lib }
    LIBS += -lboost_filesystem -lboost_system -lrt
}

$$JASPTIMER_USED {
    windows:CONFIG(ReleaseBuild)    LIBS += -llibboost_timer-vc141-mt-1_64
    windows:CONFIG(DebugBuild)      LIBS += -llibboost_timer-vc141-mt-gd-1_64
    linux:                          LIBS += -lboost_timer
    macx:                           LIBS += -lboost_timer-clang-mt-1_64
}

macx:QMAKE_CXXFLAGS_WARN_ON += -Wno-unused-parameter -Wno-unused-local-typedef
macx:QMAKE_CXXFLAGS += -Wno-c++11-extensions
macx:QMAKE_CXXFLAGS += -Wno-c++11-long-long
macx:QMAKE_CXXFLAGS += -Wno-c++11-extra-semi
macx:QMAKE_CXXFLAGS += -stdlib=libc++

windows:QMAKE_CXXFLAGS += -DBOOST_USE_WINDOWS_H -DNOMINMAX -D__WIN32__ -DBOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED

INCLUDEPATH += $$PWD/../JASP-Common/

# Additional import path used to resolve QML modules in Qt Creator's code model
QML_IMPORT_PATH = $$PWD/imports


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

#And of course also a version description to include in the Windows installer
windows {
        WIXFILENAME=$${OUT_PWD}/../jasp.wxi
        createVersionWix.commands += $$quote(echo ^<?xml version=\"1.0\" encoding=\"utf-8\"?^>^<Include^> >  $${WIXFILENAME}) &&
        createVersionWix.commands += $$quote(echo ^<?define MajorVersion=\"$${JASP_VERSION_MAJOR}\" ?^> >>  $${WIXFILENAME}) &&
        createVersionWix.commands += $$quote(echo ^<?define MinorVersion=\"$${JASP_VERSION_MINOR}\" ?^> >>  $${WIXFILENAME}) &&
        createVersionWix.commands += $$quote(echo ^<?define BuildVersion=\"$${JASP_VERSION_BUILD}\" ?^> >>  $${WIXFILENAME}) &&
        createVersionWix.commands += $$quote(echo ^<?define Revision=\"$${JASP_VERSION_REVISION}\" ?^> >>  $${WIXFILENAME}) &&
        createVersionWix.commands += $$quote(echo ^<?define JaspType=\"$${JASP_VERSION_TYPE}\"?^>^</Include^> >>  $${WIXFILENAME})

        QMAKE_EXTRA_TARGETS += createVersionWix
        POST_TARGETDEPS     += createVersionWix
}
#ENVIRONMENT_CRYPTKEY="$(SIMPLECRYPTKEY)"
#message("ENVIRONMENT_CRYPTKEY: $$[ENVIRONMENT_CRYPTKEY]")
!isEmpty($$[ENVIRONMENT_CRYPTKEY]) {
    DEFINES+="ENVIRONMENT_CRYPTKEY=$$[ENVIRONMENT_CRYPTKEY]"
}


   macx:ICON = icon.icns
windows:RC_FILE = icon.rc

HELP_PATH = $${PWD}/../Docs/help
RESOURCES_PATH = $${PWD}/../Resources

win32 {
    RESOURCES_PATH_DEST = $${OUT_PWD}/../Resources/

    RESOURCES_PATH ~= s,/,\\,g
    RESOURCES_PATH_DEST ~= s,/,\\,g

    copyres.commands  += $$quote(cmd /c xcopy /S /I /Y $${RESOURCES_PATH} $${RESOURCES_PATH_DEST})
}

macx {
    RESOURCES_PATH_DEST = $${OUT_PWD}/../Resources/

    copyres.commands += $(MKDIR) $$RESOURCES_PATH_DEST ;
    copyres.commands += cp -R $$RESOURCES_PATH/* $$RESOURCES_PATH_DEST ;
}

linux {
    RESOURCES_PATH_DEST = $${OUT_PWD}/../Resources/

    copyres.commands += $(MKDIR) $$RESOURCES_PATH_DEST ;
    copyres.commands += cp -R $$RESOURCES_PATH/* $$RESOURCES_PATH_DEST ;
}

! equals(PWD, $${OUT_PWD}) {
    QMAKE_EXTRA_TARGETS += copyres
    POST_TARGETDEPS     += copyres
}

INCLUDEPATH += $$PWD/

HEADERS += \
    analysis/analysisform.h \
    analysis/analysisqmldefines.h \
    analysis/boundqmlitem.h \
    analysis/options/availablefields.h \
    analysis/options/bound.h \
    analysis/options/option.h \
    analysis/options/optionboolean.h \
    analysis/options/optioncomputedcolumn.h \
    analysis/options/optiondoublearray.h \
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
    analysis/analysisloader.h \
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
    data/importers/spss/characterencodingrecord.h \
    data/importers/spss/cpconverter.h \
    data/importers/spss/datainforecord.h \
    data/importers/spss/datarecords.h \
    data/importers/spss/dictionaryterminationrecord.h \
    data/importers/spss/documentrecord.h \
    data/importers/spss/extnumbercasesrecord.h \
    data/importers/spss/fileheaderrecord.h \
    data/importers/spss/floatinforecord.h \
    data/importers/spss/integerinforecord.h \
    data/importers/spss/longvarnamesrecord.h \
    data/importers/spss/measures.h \
    data/importers/spss/miscinforecord.h \
    data/importers/spss/missingvaluechecker.h \
    data/importers/spss/numericconverter.h \
    data/importers/spss/readablerecord.h \
    data/importers/spss/spssformattype.h \
    data/importers/spss/spssimportcolumn.h \
    data/importers/spss/spssimportdataset.h \
    data/importers/spss/spssstream.h \
    data/importers/spss/stringutils.h \
    data/importers/spss/systemfileformat.h \
    data/importers/spss/valuelabelvarsrecord.h \
    data/importers/spss/vardisplayparamrecord.h \
    data/importers/spss/variablerecord.h \
    data/importers/spss/verylongstringrecord.h \
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
    data/importers/spssimporter.h \
    data/asyncloader.h \
    data/asyncloaderthread.h \
    data/columnsmodel.h \
    data/computedcolumn.h \
    data/computedcolumns.h \
    data/computedcolumnsmodel.h \
    data/datasetloader.h \
    data/datasetpackage.h \
    data/datasettablemodel.h \
    data/fileevent.h \
    analysis/options/variableinfo.h \
    engine/enginerepresentation.h \
    engine/enginesync.h \
    engine/rscriptstore.h \
    gui/aboutdialog.h \
    gui/aboutdialogjsinterface.h \
    qquick/datasetview.h \
    modules/analysisentry.h \
    modules/dynamicmodule.h \
    modules/dynamicmodules.h \
    modules/ribbonentry.h \
    modules/ribbonmodel.h \
    modules/ribbonbuttonmodel.h \
    modules/analysismenumodel.h \
    osf/onlinedataconnection.h \
    osf/onlinedatamanager.h \
    osf/onlinedatanode.h \
    osf/onlinedatanodeosf.h \
    osf/onlinenode.h \
    osf/onlineusernode.h \
    osf/onlineusernodeosf.h \
    osf/osfnam.h \
    utilities/appdirs.h \
    utilities/application.h \
    utilities/jsonutilities.h \
    utilities/qutils.h \
    utilities/resultsjsinterface.h \
    utilities/settings.h \
    utilities/simplecrypt.h \
    utilities/simplecryptkey.h \
    variablespage/labelfiltergenerator.h \
    variablespage/levelstablemodel.h \
    widgets/filemenu/filemenuobject.h \
    widgets/filemenu/datalibrary.h \
    widgets/filemenu/recentfiles.h \
    widgets/filemenu/currentfile.h \
    widgets/filemenu/computer.h \
    widgets/filemenu/osf.h \
    widgets/filemenu/datalibrarybreadcrumbsmodel.h \
    widgets/filemenu/datalibrarylistmodel.h \
    widgets/filemenu/filesystemmodel.h \
    widgets/filemenu/osffilesystem.h \
    widgets/filemenu/computerfilesystem.h \
    widgets/filemenu/filesystementry.h \
    widgets/boundmodel.h \
    widgets/boundqmlcheckbox.h \
    widgets/boundqmlradiobuttons.h \
    widgets/boundqmltextinput.h \
    widgets/boundqmlcombobox.h \
    widgets/draganddrop.h \
    widgets/droptarget.h \
    widgets/enhanceddroptarget.h \
    widgets/listmodelanovaassigned.h \
    widgets/listmodelpairsassigned.h \
    widgets/listmodeltermsassigned.h \
    widgets/listmodeltermsavailable.h \
    gui/preferencesdialog.h \
    widgets/tablemodel.h \
    widgets/tablemodelanovadesign.h \
    widgets/tablemodelanovamodel.h \
    widgets/tablemodelanovamodelnuisancefactors.h \
    widgets/tablemodelanovawithinsubjectcells.h \
    widgets/tablemodelcontrasts.h \
    widgets/tablemodelpairsassigned.h \
    widgets/tablemodelvariables.h \
    widgets/tablemodelvariablesassigned.h \
    widgets/tablemodelvariablesavailable.h \
    widgets/tablemodelvariableslevels.h \
    widgets/tablemodelvariablesoptions.h \
    widgets/textmodellavaan.h \
    mainwindow.h \
    utilities/extractarchive.h \
    widgets/boundqmlfactorslist.h \
    widgets/listmodelfactors.h \
    widgets/listmodel.h \
    widgets/qmllistview.h \
    widgets/qmlitem.h \
    widgets/listmodelassignedinterface.h \
    widgets/listmodelavailableinterface.h \
    widgets/listmodeldraggable.h \
    widgets/qmllistviewdraggable.h \
    widgets/boundqmllistviewmeasurescells.h \
    widgets/boundqmllistviewpairs.h \
    widgets/boundqmllistviewdraggable.h \
    widgets/listmodelmeasurescellsassigned.h \
    widgets/qmllistviewtermsavailable.h \
    widgets/boundqmllistviewterms.h \
    widgets/boundqmllistviewanovamodels.h \
    widgets/boundqmlslider.h \
    widgets/boundqmltextarea.h \
    widgets/boundqmltableview.h \
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
    $$PWD/gui/messageforwarder.h \
    widgets/filemenu/filemenulistitem.h \
    widgets/filemenu/filemenubasiclistmodel.h

SOURCES += \
    analysis/analysisform.cpp \
    analysis/analysisqmldefines.cpp \
    analysis/boundqmlitem.cpp \
    analysis/options/availablefields.cpp \
    analysis/options/option.cpp \
    analysis/options/optionboolean.cpp \
    analysis/options/optioncomputedcolumn.cpp \
    analysis/options/optiondoublearray.cpp \
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
    analysis/analysisloader.cpp \
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
    data/importers/spss/characterencodingrecord.cpp \
    data/importers/spss/datainforecord.cpp \
    data/importers/spss/datarecords.cpp \
    data/importers/spss/dictionaryterminationrecord.cpp \
    data/importers/spss/documentrecord.cpp \
    data/importers/spss/extnumbercasesrecord.cpp \
    data/importers/spss/fileheaderrecord.cpp \
    data/importers/spss/floatinforecord.cpp \
    data/importers/spss/integerinforecord.cpp \
    data/importers/spss/longvarnamesrecord.cpp \
    data/importers/spss/miscinforecord.cpp \
    data/importers/spss/missingvaluechecker.cpp \
    data/importers/spss/numericconvertor.cpp \
    data/importers/spss/readablerecord.cpp \
    data/importers/spss/spssimportcolumn.cpp \
    data/importers/spss/spssimportdataset.cpp \
    data/importers/spss/stringutils.cpp \
    data/importers/spss/valuelabelvarsrecord.cpp \
    data/importers/spss/vardisplayparamrecord.cpp \
    data/importers/spss/variablerecord.cpp \
    data/importers/spss/verylongstringrecord.cpp \
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
    data/importers/spssimporter.cpp \
    data/asyncloader.cpp \
    data/asyncloaderthread.cpp \
    data/columnsmodel.cpp \
    data/computedcolumn.cpp \
    data/computedcolumns.cpp \
    data/computedcolumnsmodel.cpp \
    data/datasetloader.cpp \
    data/datasetpackage.cpp \
    data/datasettablemodel.cpp \
    data/fileevent.cpp \
    engine/enginerepresentation.cpp \
    engine/enginesync.cpp \
    gui/aboutdialog.cpp \
    gui/aboutdialogjsinterface.cpp \
    qquick/datasetview.cpp \
    modules/analysisentry.cpp \
    modules/dynamicmodule.cpp \
    modules/dynamicmodules.cpp \
    modules/ribbonentry.cpp \
    modules/ribbonmodel.cpp \
    modules/ribbonbuttonmodel.cpp \
    modules/analysismenumodel.cpp \
    osf/onlinedataconnection.cpp \
    osf/onlinedatamanager.cpp \
    osf/onlinedatanode.cpp \
    osf/onlinedatanodeosf.cpp \
    osf/onlinenode.cpp \
    osf/onlineusernode.cpp \
    osf/onlineusernodeosf.cpp \
    osf/osfnam.cpp \
    utilities/appdirs.cpp \
    utilities/application.cpp \
    utilities/jsonutilities.cpp \
    utilities/qutils.cpp \
    utilities/resultsjsinterface.cpp \
    utilities/settings.cpp \
    utilities/simplecrypt.cpp \
    variablespage/labelfiltergenerator.cpp \
    variablespage/levelstablemodel.cpp \
    widgets/filemenu/filemenuobject.cpp \
    widgets/filemenu/datalibrary.cpp \
    widgets/filemenu/recentfiles.cpp \
    widgets/filemenu/currentfile.cpp \
    widgets/filemenu/computer.cpp \
    widgets/filemenu/osf.cpp \
    widgets/filemenu/datalibrarybreadcrumbsmodel.cpp \
    widgets/filemenu/datalibrarylistmodel.cpp \
    widgets/filemenu/filesystemmodel.cpp \
    widgets/filemenu/osffilesystem.cpp \
    widgets/filemenu/computerfilesystem.cpp \
    widgets/boundqmlcheckbox.cpp \
    widgets/boundqmlradiobuttons.cpp \
    widgets/boundqmltextinput.cpp \
    widgets/boundqmlcombobox.cpp \
    widgets/draganddrop.cpp \
    widgets/listmodelanovaassigned.cpp \
    widgets/listmodelpairsassigned.cpp \
    widgets/listmodeltermsassigned.cpp \
    widgets/listmodeltermsavailable.cpp \
    gui/preferencesdialog.cpp \
    widgets/tablemodelanovadesign.cpp \
    widgets/tablemodelanovamodel.cpp \
    widgets/tablemodelanovamodelnuisancefactors.cpp \
    widgets/tablemodelanovawithinsubjectcells.cpp \
    widgets/tablemodelcontrasts.cpp \
    widgets/tablemodelpairsassigned.cpp \
    widgets/tablemodelvariables.cpp \
    widgets/tablemodelvariablesassigned.cpp \
    widgets/tablemodelvariablesavailable.cpp \
    widgets/tablemodelvariableslevels.cpp \
    widgets/tablemodelvariablesoptions.cpp \
    widgets/textmodellavaan.cpp \
    main.cpp \
    mainwindow.cpp \
    utilities/extractarchive.cpp \
    widgets/boundqmlfactorslist.cpp \
    widgets/listmodelfactors.cpp \
    widgets/listmodel.cpp \
    widgets/qmllistview.cpp \
    widgets/qmlitem.cpp \
    widgets/listmodelavailableinterface.cpp \
    widgets/listmodelassignedinterface.cpp \
    widgets/listmodeldraggable.cpp \
    widgets/qmllistviewdraggable.cpp \
    widgets/boundqmllistviewmeasurescells.cpp \
    widgets/boundqmllistviewpairs.cpp \
    widgets/boundqmllistviewdraggable.cpp \
    widgets/listmodelmeasurescellsassigned.cpp \
    widgets/qmllistviewtermsavailable.cpp \
    widgets/boundqmllistviewterms.cpp \
    widgets/boundqmllistviewanovamodels.cpp \
    widgets/boundqmlslider.cpp \
    widgets/boundqmltextarea.cpp \
    widgets/boundqmltableview.cpp \
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
    $$PWD/gui/messageforwarder.cpp \
    widgets/filemenu/filemenubasiclistmodel.cpp

RESOURCES += \
    html/html.qrc \
    resources/icons.qrc \
    resources/resources.qrc \
    qml.qrc

   unix:OTHER_FILES += icon.icns
windows:OTHER_FILES += icon.rc


