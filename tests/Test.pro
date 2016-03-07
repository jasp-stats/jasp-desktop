QT += core gui webkit webkitwidgets svg testlib

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

windows:CONFIG += c++11

TARGET = UnitTest
CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app

INCLUDEPATH += ../JASP-Desktop/ \
	../JASP-Common/ 
	../JASP-Engine/

macx:INCLUDEPATH += ../../boost_1_54_0
windows:INCLUDEPATH += ../../boost_1_54_0

PRE_TARGETDEPS += ../../jasp-build/libJASP-Common.a

LIBS += -L../../jasp-build -lJASP-Common

windows:LIBS += -lboost_filesystem-mt -lboost_system-mt -larchive.dll
macx:LIBS += -lboost_filesystem-mt -lboost_system-mt -larchive -lz
linux:LIBS += -lrt
linux:LIBS += -lboost_filesystem    -lboost_system    -larchive

macx:ICON = icon.icns
windows:RC_FILE = icon.rc

windows:LIBS += -lole32 -lole

QMAKE_CXXFLAGS += -Wno-c++11-extensions
QMAKE_CXXFLAGS += -Wno-unused-parameter
QMAKE_CXXFLAGS += -Wno-c++11-long-long
QMAKE_CXXFLAGS += -Wno-c++11-extra-semi

QMAKE_CXXFLAGS += -DBOOST_USE_WINDOWS_H

linux {
    _R_HOME = $$(R_HOME)
    isEmpty(_R_HOME):_R_HOME = /usr/lib/R
    QMAKE_CXXFLAGS += -D\'R_HOME=\"$$_R_HOME\"\'
}


SOURCES += \
	main.cpp \
    textfilereadtest.cpp \
    osftest.cpp \
    ../JASP-Desktop/aboutdialog.cpp \
    ../JASP-Desktop/analyses.cpp \
    ../JASP-Desktop/mainwindow.cpp \
    ../JASP-Desktop/datasettablemodel.cpp \
    ../JASP-Desktop/enginesync.cpp \
    ../JASP-Desktop/availablefields.cpp \
    ../JASP-Desktop/asyncloader.cpp \
    ../JASP-Desktop/maintableview.cpp \
    ../JASP-Desktop/maintablehorizontalheader.cpp \
    ../JASP-Desktop/widgets/assignbutton.cpp \
    ../JASP-Desktop/widgets/availablefieldslistview.cpp \
    ../JASP-Desktop/widgets/boundcheckbox.cpp \
    ../JASP-Desktop/widgets/boundlistview.cpp \
    ../JASP-Desktop/widgets/expanderbutton.cpp \
    ../JASP-Desktop/widgets/infopopup.cpp \
    ../JASP-Desktop/widgets/ribbonbutton.cpp \
    ../JASP-Desktop/widgets/toolbutton.cpp \
    ../JASP-Desktop/widgets/boundtextbox.cpp \
    ../JASP-Desktop/widgets/boundgroupbox.cpp \
    ../JASP-Desktop/widgets/progresswidget.cpp \
    ../JASP-Desktop/widgets/anovamodelwidget.cpp \
    ../JASP-Desktop/widgets/listview.cpp \
    ../JASP-Desktop/widgets/draganddrop.cpp \
    ../JASP-Desktop/widgets/assignbuttonmenu.cpp \
    ../JASP-Desktop/widgets/tableview.cpp \
    ../JASP-Desktop/widgets/boundpairstable.cpp \
    ../JASP-Desktop/widgets/boundcombobox.cpp \
    ../JASP-Desktop/widgets/boundtableview.cpp \
    ../JASP-Desktop/widgets/tableviewmenueditor.cpp \
    ../JASP-Desktop/widgets/tableviewmenueditordelegate.cpp \
    ../JASP-Desktop/analysisforms/analysisform.cpp \
    ../JASP-Desktop/analysisforms/anovabayesianform.cpp \
    ../JASP-Desktop/analysisforms/ttestpairedsamplesform.cpp \
    ../JASP-Desktop/analysisforms/anovamultivariateform.cpp \
    ../JASP-Desktop/analysisforms/ttestbayesianonesampleform.cpp \
    ../JASP-Desktop/analysisforms/ancovaform.cpp \
    ../JASP-Desktop/analysisforms/anovaform.cpp \
    ../JASP-Desktop/analysisforms/descriptivesform.cpp \
    ../JASP-Desktop/analysisforms/anovaonewayform.cpp \
    ../JASP-Desktop/analysisforms/ttestonesampleform.cpp \
    ../JASP-Desktop/analysisforms/ttestindependentsamplesform.cpp \
    ../JASP-Desktop/analysisforms/ancovamultivariateform.cpp \
    ../JASP-Desktop/analysisforms/regressionlinearform.cpp \
    ../JASP-Desktop/analysisforms/correlationform.cpp \
    ../JASP-Desktop/widgets/boundassignwidget.cpp \
    ../JASP-Desktop/analysisforms/anovarepeatedmeasuresform.cpp \
    ../JASP-Desktop/analysisforms/contingencytablesform.cpp \
    ../JASP-Desktop/analysisforms/correlationpartialform.cpp \
    ../JASP-Desktop/ribbons/ribbonwidget.cpp \
    ../JASP-Desktop/ribbons/ribbonsem.cpp \
    ../JASP-Desktop/ribbons/ribbonanalysis.cpp \
    ../JASP-Desktop/ribbons/ribbonhome.cpp \
    ../JASP-Desktop/analysisforms/semsimpleform.cpp \
    ../JASP-Desktop/widgets/boundtextedit.cpp \
    ../JASP-Desktop/widgets/stealthbutton.cpp \
    ../JASP-Desktop/analysisforms/ttestbayesianindependentsamplesform.cpp \
    ../JASP-Desktop/analysisforms/ttestbayesianpairedsamplesform.cpp \
    ../JASP-Desktop/widgets/itemmodelselectitem.cpp \
    ../JASP-Desktop/widgets/itemmodelselectvariable.cpp \
    ../JASP-Desktop/widgets/tabbar.cpp \
    ../JASP-Desktop/optionsform.cpp \
    ../JASP-Desktop/widgets/textmodellavaan.cpp \
    ../JASP-Desktop/terms.cpp \
    ../JASP-Desktop/term.cpp \
    ../JASP-Desktop/widgets/tablemodelanovamodelnuisancefactors.cpp \
    ../JASP-Desktop/widgets/tablemodelpairsassigned.cpp \
    ../JASP-Desktop/widgets/tablemodelvariables.cpp \
    ../JASP-Desktop/widgets/tablemodelvariablesassigned.cpp \
    ../JASP-Desktop/widgets/tablemodelvariablesavailable.cpp \
    ../JASP-Desktop/widgets/tablemodelvariableslevels.cpp \
    ../JASP-Desktop/widgets/tablemodelvariablesoptions.cpp \
    ../JASP-Desktop/widgets/tablemodelanovamodel.cpp \
    ../JASP-Desktop/widgets/tablemodelcontrasts.cpp \
    ../JASP-Desktop/widgets/tablemodelanovadesign.cpp \
    ../JASP-Desktop/appdirs.cpp \
    ../JASP-Desktop/widgets/tablemodelanovawithinsubjectcells.cpp \
    ../JASP-Desktop/analysisforms/ancovabayesianform.cpp \
    ../JASP-Desktop/analysisforms/anovarepeatedmeasuresbayesianform.cpp \
    ../JASP-Desktop/analysisforms/correlationbayesianform.cpp \
    ../JASP-Desktop/analysisforms/contingencytablesbayesianform.cpp \
    ../JASP-Desktop/analysisforms/correlationbayesianpairsform.cpp \
    ../JASP-Desktop/application.cpp \
    ../JASP-Desktop/analysisforms/regressionlinearbayesianform.cpp \
    ../JASP-Desktop/qutils.cpp \
    ../JASP-Desktop/activitylog.cpp \
    ../JASP-Desktop/lrnamreply.cpp \
    ../JASP-Desktop/lrnam.cpp \
    ../JASP-Desktop/widgets/webview.cpp \
    ../JASP-Desktop/widgets/button.cpp \
    ../JASP-Desktop/analysisforms/r11tlearnform.cpp \
    ../JASP-Desktop/ribbons/ribbonr11tlearn.cpp \
    ../JASP-Desktop/backstage/breadcrumbs.cpp \
    ../JASP-Desktop/backstage/verticaltabbar.cpp \
    ../JASP-Desktop/backstage/verticaltabwidget.cpp \
    ../JASP-Desktop/backstagewidget.cpp \
    ../JASP-Desktop/backstage/fsentrywidget.cpp \
    ../JASP-Desktop/backstage/fsbrowser.cpp \
    ../JASP-Desktop/backstage/verticalscrollarea.cpp \
    ../JASP-Desktop/backstage/elidelabel.cpp \
    ../JASP-Desktop/backstage/backstageosf.cpp \
    ../JASP-Desktop/backstage/backstagecomputer.cpp \
    ../JASP-Desktop/backstage/backstagepage.cpp \
    ../JASP-Desktop/backstage/opensavewidget.cpp \
    ../JASP-Desktop/analysisforms/regressionloglinearform.cpp \
    ../JASP-Desktop/analysisforms/regressionloglinearbayesianform.cpp \
    ../JASP-Desktop/backstage/fsbmexamples.cpp \
    ../JASP-Desktop/backstage/fsbmodel.cpp \
    ../JASP-Desktop/backstage/fsbmcomputer.cpp \
    ../JASP-Desktop/backstage/fsbmrecent.cpp \
    ../JASP-Desktop/backstage/fsbmrecentfolders.cpp \
    ../JASP-Desktop/fileevent.cpp \
    ../JASP-Desktop/widgets/boundsingleitemview.cpp \
    ../JASP-Desktop/analysisforms/binomialtestform.cpp \
	../JASP-Desktop/analysisforms/binomialtestbayesianform.cpp \
    ../JASP-Desktop/analysisforms/bffromtform.cpp \
    ../JASP-Desktop/variableswidget.cpp \
    ../JASP-Desktop/variablespage/levelstablemodel.cpp \
    ../JASP-Desktop/variablespage/variablestablemodel.cpp \
    ../JASP-Desktop/backstage/fsbmosf.cpp \
    ../JASP-Desktop/osfnam.cpp \
    ../JASP-Desktop/onlinedatamanager.cpp \
    ../JASP-Desktop/onlinedataconnection.cpp \
    ../JASP-Desktop/onlinedatanodeosf.cpp \
    ../JASP-Desktop/onlinedatanode.cpp \
    ../JASP-Desktop/onlineusernode.cpp \
    ../JASP-Desktop/onlinenode.cpp \
    ../JASP-Desktop/onlineusernodeosf.cpp \
    ../JASP-Desktop/backstage/authwidget.cpp

HEADERS += \
    AutomatedTests.h \
    textfilereadtest.h \
    osftest.h \
    ../JASP-Desktop/aboutdialog.h \
    ../JASP-Desktop/analyses.h \
    ../JASP-Desktop/datasettablemodel.h \
    ../JASP-Desktop/enginesync.h \
    ../JASP-Desktop/availablefields.h \
    ../JASP-Desktop/analysisforms/analysisform.h \
    ../JASP-Desktop/widgets/assignbutton.h \
    ../JASP-Desktop/widgets/availablefieldslistview.h \
    ../JASP-Desktop/widgets/boundcheckbox.h \
    ../JASP-Desktop/widgets/boundlistview.h \
    ../JASP-Desktop/widgets/expanderbutton.h \
    ../JASP-Desktop/widgets/infopopup.h \
    ../JASP-Desktop/widgets/ribbonbutton.h \
    ../JASP-Desktop/widgets/toolbutton.h \
    ../JASP-Desktop/widgets/boundtextbox.h \
    ../JASP-Desktop/widgets/boundgroupbox.h \
    ../JASP-Desktop/analysisforms/anovabayesianform.h \
    ../JASP-Desktop/analysisforms/ttestpairedsamplesform.h \
    ../JASP-Desktop/analysisforms/anovamultivariateform.h \
    ../JASP-Desktop/analysisforms/ttestbayesianonesampleform.h \
    ../JASP-Desktop/widgets/boundpairstable.h \
    ../JASP-Desktop/asyncloader.h \
    ../JASP-Desktop/widgets/progresswidget.h \
    ../JASP-Desktop/widgets/anovamodelwidget.h \
    ../JASP-Desktop/bound.h \
    ../JASP-Desktop/widgets/boundmodel.h \
    ../JASP-Desktop/widgets/listview.h \
    ../JASP-Desktop/widgets/draganddrop.h \
    ../JASP-Desktop/widgets/assignbuttonmenu.h \
    ../JASP-Desktop/widgets/enhanceddroptarget.h \
    ../JASP-Desktop/widgets/tableview.h \
    ../JASP-Desktop/maintableview.h \
    ../JASP-Desktop/widgets/droptarget.h \
    ../JASP-Desktop/widgets/tablemodel.h \
    ../JASP-Desktop/analysisforms/ancovaform.h \
    ../JASP-Desktop/analysisforms/anovaform.h \
    ../JASP-Desktop/analysisforms/descriptivesform.h \
    ../JASP-Desktop/analysisforms/anovaonewayform.h \
    ../JASP-Desktop/analysisforms/ttestindependentsamplesform.h \
    ../JASP-Desktop/analysisforms/ttestonesampleform.h \
    ../JASP-Desktop/analysisforms/ancovamultivariateform.h \
    ../JASP-Desktop/maintablehorizontalheader.h \
    ../JASP-Desktop/analysisforms/regressionlinearform.h \
    ../JASP-Desktop/mainwindow.h \
    ../JASP-Desktop/analysisforms/correlationform.h \
    ../JASP-Desktop/widgets/boundcombobox.h \
    ../JASP-Desktop/widgets/boundtableview.h \
    ../JASP-Desktop/widgets/tableviewmenueditor.h \
    ../JASP-Desktop/widgets/tableviewmenueditordelegate.h \
    ../JASP-Desktop/widgets/boundassignwidget.h \
    ../JASP-Desktop/analysisforms/anovarepeatedmeasuresform.h \
    ../JASP-Desktop/analysisforms/contingencytablesform.h \
    ../JASP-Desktop/analysisforms/correlationpartialform.h \
    ../JASP-Desktop/ribbons/ribbonwidget.h \
    ../JASP-Desktop/ribbons/ribbonsem.h \
    ../JASP-Desktop/ribbons/ribbonanalysis.h \
    ../JASP-Desktop/ribbons/ribbonhome.h \
    ../JASP-Desktop/analysisforms/semsimpleform.h \
    ../JASP-Desktop/widgets/boundtextedit.h \
    ../JASP-Desktop/widgets/stealthbutton.h \
    ../JASP-Desktop/analysisforms/ttestbayesianindependentsamplesform.h \
    ../JASP-Desktop/analysisforms/ttestbayesianpairedsamplesform.h \
    ../JASP-Desktop/widgets/itemmodelselectitem.h \
    ../JASP-Desktop/widgets/itemmodelselectvariable.h \
    ../JASP-Desktop/widgets/tabbar.h \
    ../JASP-Desktop/optionsform.h \
    ../JASP-Desktop/widgets/textmodellavaan.h \
    ../JASP-Desktop/term.h \
    ../JASP-Desktop/terms.h \
    ../JASP-Desktop/variableinfo.h \
    ../JASP-Desktop/widgets/tablemodelanovamodelnuisancefactors.h \
    ../JASP-Desktop/widgets/tablemodelpairsassigned.h \
    ../JASP-Desktop/widgets/tablemodelvariables.h \
    ../JASP-Desktop/widgets/tablemodelvariablesassigned.h \
    ../JASP-Desktop/widgets/tablemodelvariablesavailable.h \
    ../JASP-Desktop/widgets/tablemodelvariableslevels.h \
    ../JASP-Desktop/widgets/tablemodelvariablesoptions.h \
    ../JASP-Desktop/widgets/tablemodelanovamodel.h \
    ../JASP-Desktop/widgets/tablemodelcontrasts.h \
    ../JASP-Desktop/widgets/tablemodelanovadesign.h \
    ../JASP-Desktop/appdirs.h \
    ../JASP-Desktop/widgets/tablemodelanovawithinsubjectcells.h \
    ../JASP-Desktop/analysisforms/ancovabayesianform.h \
    ../JASP-Desktop/analysisforms/anovarepeatedmeasuresbayesianform.h \
    ../JASP-Desktop/analysisforms/correlationbayesianform.h \
    ../JASP-Desktop/analysisforms/contingencytablesbayesianform.h \
    ../JASP-Desktop/analysisforms/correlationbayesianpairsform.h \
    ../JASP-Desktop/application.h \
    ../JASP-Desktop/analysisforms/regressionlinearbayesianform.h \
    ../JASP-Desktop/qutils.h \
    ../JASP-Desktop/activitylog.h \
    ../JASP-Desktop/lrnamreply.h \
    ../JASP-Desktop/lrnam.h \
    ../JASP-Desktop/widgets/groupbox.h \
    ../JASP-Desktop/widgets/button.h \
    ../JASP-Desktop/widgets/webview.h \
    ../JASP-Desktop/analysisforms/r11tlearnform.h \
    ../JASP-Desktop/ribbons/ribbonr11tlearn.h \
    ../JASP-Desktop/backstage/breadcrumbs.h \
    ../JASP-Desktop/backstage/verticaltabbar.h \
    ../JASP-Desktop/backstage/verticaltabwidget.h \
    ../JASP-Desktop/backstagewidget.h \
    ../JASP-Desktop/backstage/fsentrywidget.h \
    ../JASP-Desktop/backstage/fsbrowser.h \
    ../JASP-Desktop/backstage/fsentry.h \
    ../JASP-Desktop/backstage/verticalscrollarea.h \
    ../JASP-Desktop/backstage/elidelabel.h \
    ../JASP-Desktop/backstage/backstageosf.h \
    ../JASP-Desktop/backstage/backstagecomputer.h \
    ../JASP-Desktop/backstage/backstagepage.h \
    ../JASP-Desktop/backstage/opensavewidget.h \
    ../JASP-Desktop/analysisforms/regressionloglinearform.h \
    ../JASP-Desktop/analysisforms/regressionloglinearbayesianform.h \
    ../JASP-Desktop/backstage/fsbmexamples.h \
    ../JASP-Desktop/backstage/fsbmodel.h \
    ../JASP-Desktop/backstage/fsbmcomputer.h \
    ../JASP-Desktop/backstage/fsbmrecent.h \
    ../JASP-Desktop/backstage/fsbmrecentfolders.h \
    ../JASP-Desktop/fileevent.h \
    ../JASP-Desktop/widgets/boundsingleitemview.h \
    ../JASP-Desktop/analysisforms/binomialtestform.h \
	../JASP-Desktop/analysisforms/binomialtestbayesianform.h \
    ../JASP-Desktop/analysisforms/bffromtform.h \
    ../JASP-Desktop/variableswidget.h \
    ../JASP-Desktop/variablespage/levelstablemodel.h \
    ../JASP-Desktop/variablespage/variablestablemodel.h \
    ../JASP-Desktop/backstage/fsbmosf.h \
    ../JASP-Desktop/osfnam.h \
    ../JASP-Desktop/onlinedatamanager.h \
    ../JASP-Desktop/onlinedataconnection.h \
    ../JASP-Desktop/onlinedatanodeosf.h \
    ../JASP-Desktop/onlinedatanode.h \
    ../JASP-Desktop/onlineusernode.h \
    ../JASP-Desktop/onlinenode.h \
    ../JASP-Desktop/onlineusernodeosf.h \
    ../JASP-Desktop/backstage/authwidget.h

FORMS += \
    ../JASP-Desktop/analysisforms/anovabayesianform.ui \
    ../JASP-Desktop/analysisforms/ttestpairedsamplesform.ui \
    ../JASP-Desktop/analysisforms/anovamultivariateform.ui \
    ../JASP-Desktop/analysisforms/ttestbayesianonesampleform.ui \
    ../JASP-Desktop/widgets/progresswidget.ui \
    ../JASP-Desktop/widgets/anovamodelwidget.ui \
    ../JASP-Desktop/analysisforms/ancovaform.ui \
    ../JASP-Desktop/analysisforms/anovaform.ui \
    ../JASP-Desktop/analysisforms/descriptivesform.ui \
    ../JASP-Desktop/analysisforms/anovaonewayform.ui \
    ../JASP-Desktop/analysisforms/ttestindependentsamplesform.ui \
    ../JASP-Desktop/analysisforms/ttestonesampleform.ui \
    ../JASP-Desktop/analysisforms/ancovamultivariateform.ui \
    ../JASP-Desktop/analysisforms/regressionlinearform.ui \
    ../JASP-Desktop/mainwindow.ui \
    ../JASP-Desktop/analysisforms/correlationform.ui \
    ../JASP-Desktop/widgets/boundassignwidget.ui \
    ../JASP-Desktop/analysisforms/anovarepeatedmeasuresform.ui \
    ../JASP-Desktop/analysisforms/contingencytablesform.ui \
    ../JASP-Desktop/analysisforms/correlationpartialform.ui \
    ../JASP-Desktop/ribbons/ribbonhome.ui \
    ../JASP-Desktop/ribbons/ribbonsem.ui \
    ../JASP-Desktop/ribbons/ribbonanalysis.ui \
    ../JASP-Desktop/analysisforms/semsimpleform.ui \
    ../JASP-Desktop/analysisforms/ttestbayesianindependentsamplesform.ui \
    ../JASP-Desktop/analysisforms/ttestbayesianpairedsamplesform.ui \
    ../JASP-Desktop/optionsform.ui \
    ../JASP-Desktop/analysisforms/ancovabayesianform.ui \
    ../JASP-Desktop/analysisforms/anovarepeatedmeasuresbayesianform.ui \
    ../JASP-Desktop/analysisforms/correlationbayesianform.ui \
    ../JASP-Desktop/analysisforms/correlationbayesianpairsform.ui \
    ../JASP-Desktop/analysisforms/contingencytablesbayesianform.ui \
    ../JASP-Desktop/analysisforms/regressionlinearbayesianform.ui \
    ../JASP-Desktop/analysisforms/r11tlearnform.ui \
    ../JASP-Desktop/ribbons/ribbonr11tlearn.ui \
    ../JASP-Desktop/backstage/backstagecomputer.ui \
    ../JASP-Desktop/analysisforms/regressionloglinearform.ui \
    ../JASP-Desktop/analysisforms/regressionloglinearbayesianform.ui \
    ../JASP-Desktop/analysisforms/binomialtestform.ui \
	../JASP-Desktop/analysisforms/binomialtestbayesianform.ui \
    ../JASP-Desktop/analysisforms/bffromtform.ui \
    ../JASP-Desktop/variableswidget.ui \
    ../JASP-Desktop/backstage/authwidget.ui\
    ../JASP-Desktop/aboutdialog.ui

RESOURCES += \
    ../JASP-Desktop/backstage/backstage.qrc \
    ../JASP-Desktop/html/html.qrc \
    ../JASP-Desktop/resources/icons.qrc \
    ../JASP-Desktop/resources/resources.qrc

   unix:OTHER_FILES += ../JASP-Desktop/icon.icns
windows:OTHER_FILES += ../JASP-Desktop/icon.rc

OTHER_FILES += \
    ../JASP-Desktop/html/index.html \
    ../JASP-Desktop/html/css/jquery-ui-1.10.1.custom.css \
    ../JASP-Desktop/html/css/jquery-ui-1.10.1.custom.min.css \
    ../JASP-Desktop/html/css/images/animated-overlay.gif \
    ../JASP-Desktop/html/css/images/ui-bg_flat_0_aaaaaa_40x100.png \
    ../JASP-Desktop/html/css/images/ui-bg_flat_75_ffffff_40x100.png \
    ../JASP-Desktop/html/css/images/ui-bg_glass_55_fbf9ee_1x400.png \
    ../JASP-Desktop/html/css/images/ui-bg_glass_65_ffffff_1x400.png \
    ../JASP-Desktop/html/css/images/ui-bg_glass_75_dadada_1x400.png \
    ../JASP-Desktop/html/css/images/ui-bg_glass_75_e6e6e6_1x400.png \
    ../JASP-Desktop/html/css/images/ui-bg_glass_95_fef1ec_1x400.png \
    ../JASP-Desktop/html/css/images/ui-bg_highlight-soft_75_cccccc_1x100.png \
    ../JASP-Desktop/html/css/images/ui-icons_2e83ff_256x240.png \
    ../JASP-Desktop/html/css/images/ui-icons_222222_256x240.png \
    ../JASP-Desktop/html/css/images/ui-icons_454545_256x240.png \
    ../JASP-Desktop/html/css/images/ui-icons_888888_256x240.png \
    ../JASP-Desktop/html/css/images/ui-icons_cd0a0a_256x240.png \
    ../JASP-Desktop/html/js/jquery-1.9.1.js \
    ../JASP-Desktop/html/js/jquery-ui-1.10.1.custom.js \
    ../JASP-Desktop/html/js/jquery-ui-1.10.1.custom.min.js \
    ../JASP-Desktop/html/js/main.js \
    ../JASP-Desktop/html/js/underscore-min.js \
    ../JASP-Desktop/html/js/underscore.js \
    ../JASP-Desktop/html/js/table.js \
    ../JASP-Desktop/html/js/tables.js \
    ../JASP-Desktop/resources/expanded.png \
    ../JASP-Desktop/resources/expander-arrow-down-disabled.png \
    ../JASP-Desktop/resources/expander-arrow-down-hover.png \
    ../JASP-Desktop/resources/expander-arrow-down.png \
    ../JASP-Desktop/resources/expander-arrow-up-disabled.png \
    ../JASP-Desktop/resources/expander-arrow-up-hover.png \
    ../JASP-Desktop/resources/expander-arrow-up.png \
    ../JASP-Desktop/resources/icons/accessories-calculator.png \
    ../JASP-Desktop/resources/icons/accessories-text-editor.png \
    ../JASP-Desktop/resources/icons/application-exit.png \
    ../JASP-Desktop/resources/icons/application-vnd.oasis.opendocument.spreadsheet.png \
    ../JASP-Desktop/resources/icons/application-x-kchart.png \
    ../JASP-Desktop/resources/icons/applications-development.png \
    ../JASP-Desktop/resources/icons/applications-education-mathematics.png \
    ../JASP-Desktop/resources/icons/applications-education.png \
    ../JASP-Desktop/resources/icons/applications-system.png \
    ../JASP-Desktop/resources/icons/applications-toys.png \
    ../JASP-Desktop/resources/icons/bookcase.png \
    ../JASP-Desktop/resources/icons/core.png \
    ../JASP-Desktop/resources/icons/dialog-close.png \
    ../JASP-Desktop/resources/icons/document-new.png \
    ../JASP-Desktop/resources/icons/document-open.png \
    ../JASP-Desktop/resources/icons/document-print.png \
    ../JASP-Desktop/resources/icons/document-save-as.png \
    ../JASP-Desktop/resources/icons/document-save.png \
    ../JASP-Desktop/resources/icons/edit-copy.png \
    ../JASP-Desktop/resources/icons/edit-delete-shred.png \
    ../JASP-Desktop/resources/icons/edit-find.png \
    ../JASP-Desktop/resources/icons/edit-paste.png \
    ../JASP-Desktop/resources/icons/edit-redo.png \
    ../JASP-Desktop/resources/icons/edit-undo.png \
    ../JASP-Desktop/resources/icons/emblem-favorite.png \
    ../JASP-Desktop/resources/icons/folder-blue.png \
    ../JASP-Desktop/resources/icons/folder-cyan.png \
    ../JASP-Desktop/resources/icons/folder-grey.png \
    ../JASP-Desktop/resources/icons/folder-orange.png \
    ../JASP-Desktop/resources/icons/folder-red.png \
    ../JASP-Desktop/resources/icons/folder-txt.png \
    ../JASP-Desktop/resources/icons/folder-violet.png \
    ../JASP-Desktop/resources/icons/folder-yellow.png \
    ../JASP-Desktop/resources/icons/help-about.png \
    ../JASP-Desktop/resources/icons/kbrunch.png \
    ../JASP-Desktop/resources/icons/kchart.png \
    ../JASP-Desktop/resources/icons/kcmpartitions.png \
    ../JASP-Desktop/resources/icons/kcoloredit.png \
    ../JASP-Desktop/resources/icons/preferences-system-session-services.png \
    ../JASP-Desktop/resources/icons/roll.png \
    ../JASP-Desktop/resources/icons/text-x-moc.png \
    ../JASP-Desktop/resources/icons/tools-wizard.png \
    ../JASP-Desktop/resources/icons/user-home.png \
    ../JASP-Desktop/resources/arrow-left.png \
    ../JASP-Desktop/resources/arrow-right.png \
    ../JASP-Desktop/resources/osx/ribbonbutton.css \
    ../JASP-Desktop/html/js/displaydefs.js \
    ../JASP-Desktop/html/css/theme-spss.css \
    ../JASP-Desktop/html/css/theme-jasp.css \
    ../JASP-Desktop/resources/icons/variable-nominal.png \
    ../JASP-Desktop/resources/icons/variable-ordinal.png \
    ../JASP-Desktop/resources/icons/variable-scale.png \
    ../JASP-Desktop/resources/icons/analysis-ttest.png \
    ../JASP-Desktop/html/css/images/copy.png \
    ../JASP-Desktop/html/css/images/running.gif \
    ../JASP-Desktop/html/css/images/waiting.gif \
    ../JASP-Desktop/resources/icons/toolbutton-descriptives.png \
    ../JASP-Desktop/resources/icons/analysis-bayesian-crosstabs.png \
    ../JASP-Desktop/resources/icons/analysis-bayesian-regression.png \
    ../JASP-Desktop/resources/icons/analysis-bayesian-anova.png \
    ../JASP-Desktop/resources/icons/analysis-bayesian-ttest.png \
    ../JASP-Desktop/resources/icons/analysis-classical-anova.png \
    ../JASP-Desktop/resources/icons/analysis-classical-crosstabs.png \
    ../JASP-Desktop/resources/icons/analysis-classical-regression.png \
    ../JASP-Desktop/resources/icons/analysis-classical-ttest.png \
    ../JASP-Desktop/resources/icons/analysis-descriptives.png \
    ../JASP-Desktop/resources/icons/analysis-background-mouseover.png \
    ../JASP-Desktop/resources/icons/analysis-background-clicked.png \
    ../JASP-Desktop/resources/icons/analysis-descriptives.svg \
    ../JASP-Desktop/resources/icons/toolbutton-menu-indicator.svg \
    ../JASP-Desktop/resources/icons/variable-scale.svg \
    ../JASP-Desktop/resources/icons/variable-ordinal.svg \
    ../JASP-Desktop/resources/icons/variable-nominal.svg \
    ../JASP-Desktop/resources/icons/analysis-classical-regression.svg \
    ../JASP-Desktop/resources/icons/analysis-classical-anova.svg \
    ../JASP-Desktop/resources/icons/analysis-classical-ttest.svg \
    ../JASP-Desktop/resources/icons/analysis-bayesian-ttest.svg \
    ../JASP-Desktop/resources/icons/analysis-bayesian-anova.svg \
    ../JASP-Desktop/resources/icons/analysis-bayesian-crosstabs.svg \
    ../JASP-Desktop/resources/icons/analysis-classical-crosstabs.svg \
    ../JASP-Desktop/resources/icons/analysis-bayesian-regression.svg \
    ../JASP-Desktop/resources/icons/toolbutton-menu-indicator.svg \
    ../JASP-Desktop/html/js/image.js \
    ../JASP-Desktop/resources/icons/variable-scale-inactive.svg \
    ../JASP-Desktop/resources/icons/variable-ordinal-inactive.svg \
    ../JASP-Desktop/resources/icons/variable-nominal-inactive.svg \
    ../JASP-Desktop/html/css/images/logo.svg \
    ../JASP-Desktop/html/js/images.js \
    ../JASP-Desktop/html/js/analysis.js \
    ../JASP-Desktop/resources/icons/variable-nominal-text.svg \
    ../JASP-Desktop/html/css/images/waiting.svg \
    ../JASP-Desktop/resources/icons/analysis-classical-sem.svg \
    ../JASP-Desktop/html/js/jaspwidget.js \
    ../JASP-Desktop/html/js/backbone-min-1.1.2.js \
    ../JASP-Desktop/html/css/images/resizer.png \
    ../JASP-Desktop/html/css/images/arrowsmalldownbtn.png

HELP_PATH = $${PWD}/../Docs/help
RESOURCES_PATH = $${PWD}/../Resources

win32 {

    RESOURCES_PATH_DEST = $${OUT_PWD}/../Resources/

    RESOURCES_PATH ~= s,/,\\,g
    RESOURCES_PATH_DEST ~= s,/,\\,g

    copyres.commands  += $$quote(cmd /c xcopy /S /I /Y $${RESOURCES_PATH} $${RESOURCES_PATH_DEST})
}

macx {

    RESOURCES_PATH_DEST = $${OUT_PWD}/../../Resources/

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

DISTFILES += \
    ../JASP-Desktop/backstage/firsttabsstylesheet.qss \
    ../JASP-Desktop/backstage/secondtabsstylesheet.qss \
    ../JASP-Desktop/resources/icons/file-jasp.svg \
    ../JASP-Desktop/html/css/images/tinylogo.svg


