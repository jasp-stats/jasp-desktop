   macx:ICON = $$PWD/icon.icns
windows:RC_FILE = $$PWD/icon.rc

SOURCES += \
    $$PWD/aboutdialog.cpp \
    $$PWD/analyses.cpp \
    $$PWD/analysisforms/analysisform.cpp \
    $$PWD/appdirs.cpp \
    $$PWD/application.cpp \
    $$PWD/asyncloader.cpp \
    $$PWD/availablefields.cpp \
    $$PWD/backstage/authwidget.cpp \
    $$PWD/backstage/backstagecomputer.cpp \
    $$PWD/backstage/backstageosf.cpp \
    $$PWD/backstage/backstagepage.cpp \
    $$PWD/backstage/breadcrumbs.cpp \
    $$PWD/backstage/elidelabel.cpp \
    $$PWD/backstage/fsbmcomputer.cpp \
    $$PWD/backstage/fsbmcurrent.cpp \
    $$PWD/backstage/fsbmexamples.cpp \
    $$PWD/backstage/fsbmodel.cpp \
    $$PWD/backstage/fsbmosf.cpp \
    $$PWD/backstage/fsbmrecent.cpp \
    $$PWD/backstage/fsbmrecentfolders.cpp \
    $$PWD/backstage/fsbrowser.cpp \
    $$PWD/backstage/fsentrywidget.cpp \
    $$PWD/backstage/opensavewidget.cpp \
    $$PWD/backstage/verticalscrollarea.cpp \
    $$PWD/backstage/verticaltabbar.cpp \
    $$PWD/backstage/verticaltabwidget.cpp \
    $$PWD/backstagewidget.cpp \
    $$PWD/datasetloader.cpp \
    $$PWD/datasettablemodel.cpp \
    $$PWD/enginesync.cpp \
    $$PWD/exporters/dataexporter.cpp \
    $$PWD/exporters/exporter.cpp \
    $$PWD/exporters/jaspexporter.cpp \
    $$PWD/exporters/resultexporter.cpp \
    $$PWD/fileevent.cpp \
    $$PWD/importers/codepageconvert.cpp \
    $$PWD/importers/convertedstringcontainer.cpp \
    $$PWD/importers/csv.cpp \
    $$PWD/importers/csvimportcolumn.cpp \
    $$PWD/importers/csvimporter.cpp \
    $$PWD/importers/importcolumn.cpp \
    $$PWD/importers/importdataset.cpp \
    $$PWD/importers/importer.cpp \
    $$PWD/importers/jaspimporter.cpp \
    $$PWD/importers/ods/odsimportcolumn.cpp \
    $$PWD/importers/ods/odsimportdataset.cpp \
    $$PWD/importers/ods/odssheetcell.cpp \
    $$PWD/importers/ods/odstypes.cpp \
    $$PWD/importers/ods/odsxmlcontentshandler.cpp \
    $$PWD/importers/ods/odsxmlhandler.cpp \
    $$PWD/importers/ods/odsxmlmanifesthandler.cpp \
    $$PWD/importers/odsimporter.cpp \
    $$PWD/importers/spss/characterencodingrecord.cpp \
    $$PWD/importers/spss/datainforecord.cpp \
    $$PWD/importers/spss/datarecords.cpp \
    $$PWD/importers/spss/dictionaryterminationrecord.cpp \
    $$PWD/importers/spss/documentrecord.cpp \
    $$PWD/importers/spss/extnumbercasesrecord.cpp \
    $$PWD/importers/spss/fileheaderrecord.cpp \
    $$PWD/importers/spss/floatinforecord.cpp \
    $$PWD/importers/spss/integerinforecord.cpp \
    $$PWD/importers/spss/longvarnamesrecord.cpp \
    $$PWD/importers/spss/miscinforecord.cpp \
    $$PWD/importers/spss/missingvaluechecker.cpp \
    $$PWD/importers/spss/numericconvertor.cpp \
    $$PWD/importers/spss/readablerecord.cpp \
    $$PWD/importers/spss/spssimportcolumn.cpp \
    $$PWD/importers/spss/spssimportdataset.cpp \
    $$PWD/importers/spss/stringutils.cpp \
    $$PWD/importers/spss/valuelabelvarsrecord.cpp \
    $$PWD/importers/spss/vardisplayparamrecord.cpp \
    $$PWD/importers/spss/variablerecord.cpp \
    $$PWD/importers/spss/verylongstringrecord.cpp \
    $$PWD/importers/spssimporter.cpp \
    $$PWD/main.cpp \
    $$PWD/mainwindow.cpp \
    $$PWD/module.cpp \
    $$PWD/onlinedataconnection.cpp \
    $$PWD/onlinedatamanager.cpp \
    $$PWD/onlinedatanode.cpp \
    $$PWD/onlinedatanodeosf.cpp \
    $$PWD/onlinenode.cpp \
    $$PWD/onlineusernode.cpp \
    $$PWD/onlineusernodeosf.cpp \
    $$PWD/osfnam.cpp \
    $$PWD/preferencesdialog.cpp \
    $$PWD/qutils.cpp \
    $$PWD/ribbons/ribbonhome.cpp \
    $$PWD/ribbons/ribbonwidget.cpp \
    $$PWD/simplecrypt.cpp \
    $$PWD/term.cpp \
    $$PWD/terms.cpp \
    $$PWD/variablespage/levelstablemodel.cpp \
    $$PWD/widgets/anovamodelwidget.cpp \
    $$PWD/widgets/assignbutton.cpp \
    $$PWD/widgets/assignbuttonmenu.cpp \
    $$PWD/widgets/availablefieldslistview.cpp \
    $$PWD/widgets/boundassignwidget.cpp \
    $$PWD/widgets/boundcheckbox.cpp \
    $$PWD/widgets/boundcombobox.cpp \
    $$PWD/widgets/boundgroupbox.cpp \
    $$PWD/widgets/boundlistview.cpp \
    $$PWD/widgets/boundpairstable.cpp \
    $$PWD/widgets/boundsingleitemview.cpp \
    $$PWD/widgets/boundtableview.cpp \
    $$PWD/widgets/boundtablewidget.cpp \
    $$PWD/widgets/boundtextbox.cpp \
    $$PWD/widgets/boundtextedit.cpp \
    $$PWD/widgets/button.cpp \
    $$PWD/widgets/draganddrop.cpp \
    $$PWD/widgets/expanderbutton.cpp \
    $$PWD/widgets/infopopup.cpp \
    $$PWD/widgets/itemmodelselectitem.cpp \
    $$PWD/widgets/itemmodelselectvariable.cpp \
    $$PWD/widgets/listview.cpp \
    $$PWD/widgets/ribbonbutton.cpp \
    $$PWD/widgets/stealthbutton.cpp \
    $$PWD/widgets/subjectivepriorswidget.cpp \
    $$PWD/widgets/tabbar.cpp \
    $$PWD/widgets/tablemodelanovadesign.cpp \
    $$PWD/widgets/tablemodelanovamodel.cpp \
    $$PWD/widgets/tablemodelanovamodelnuisancefactors.cpp \
    $$PWD/widgets/tablemodelanovawithinsubjectcells.cpp \
    $$PWD/widgets/tablemodelcontrasts.cpp \
    $$PWD/widgets/tablemodelpairsassigned.cpp \
    $$PWD/widgets/tablemodelvariables.cpp \
    $$PWD/widgets/tablemodelvariablesassigned.cpp \
    $$PWD/widgets/tablemodelvariablesavailable.cpp \
    $$PWD/widgets/tablemodelvariableslevels.cpp \
    $$PWD/widgets/tablemodelvariablesoptions.cpp \
    $$PWD/widgets/tableview.cpp \
    $$PWD/widgets/tableviewmenueditor.cpp \
    $$PWD/widgets/tableviewmenueditordelegate.cpp \
    $$PWD/widgets/textmodellavaan.cpp \
    $$PWD/widgets/toolbutton.cpp \
    $$PWD/widgets/customwebengineview.cpp \
    $$PWD/resultsjsinterface.cpp \
    $$PWD/customwebenginepage.cpp \
    $$PWD/asyncloaderthread.cpp \
    $$PWD/aboutdialogjsinterface.cpp \
    $$PWD/variablespage/labelfiltergenerator.cpp \
    $$PWD/columnsmodel.cpp \
    $$PWD/datasetview.cpp \
    $$PWD/jsonutilities.cpp \
    $$PWD/backstage/backstagedatalibrary.cpp \
    $$PWD/backstage/datalibrarylistmodel.cpp \
    $$PWD/backstage/datalibrarybreadcrumbsmodel.cpp \
    $$PWD/settings.cpp \
    $$PWD/enginerepresentation.cpp \
    $$PWD/computedcolumnsmodel.cpp \
    $$PWD/modules/dynamicmodule.cpp \
    $$PWD/modules/dynamicmodules.cpp \
    $$PWD/modules/analysisentry.cpp \
    $$PWD/modules/ribbonentry.cpp



HEADERS  += \
    $$PWD/aboutdialog.h \
    $$PWD/analyses.h \
    $$PWD/analysisforms/analysisform.h \
    $$PWD/appdirs.h \
    $$PWD/application.h \
    $$PWD/asyncloader.h \
    $$PWD/availablefields.h \
    $$PWD/backstage/authwidget.h \
    $$PWD/backstage/backstagecomputer.h \
    $$PWD/backstage/backstageosf.h \
    $$PWD/backstage/backstagepage.h \
    $$PWD/backstage/breadcrumbs.h \
    $$PWD/backstage/elidelabel.h \
    $$PWD/backstage/fsbmcomputer.h \
    $$PWD/backstage/fsbmcurrent.h \
    $$PWD/backstage/fsbmexamples.h \
    $$PWD/backstage/fsbmodel.h \
    $$PWD/backstage/fsbmosf.h \
    $$PWD/backstage/fsbmrecent.h \
    $$PWD/backstage/fsbmrecentfolders.h \
    $$PWD/backstage/fsbrowser.h \
    $$PWD/backstage/fsentry.h \
    $$PWD/backstage/fsentrywidget.h \
    $$PWD/backstage/opensavewidget.h \
    $$PWD/backstage/verticalscrollarea.h \
    $$PWD/backstage/verticaltabbar.h \
    $$PWD/backstage/verticaltabwidget.h \
    $$PWD/backstagewidget.h \
    $$PWD/bound.h \
    $$PWD/customhoverdelegate.h \
    $$PWD/datasetloader.h \
    $$PWD/datasettablemodel.h \
    $$PWD/enginesync.h \
    $$PWD/exporters/dataexporter.h \
    $$PWD/exporters/exporter.h \
    $$PWD/exporters/jaspexporter.h \
    $$PWD/exporters/resultexporter.h \
    $$PWD/fileevent.h \
    $$PWD/importers/codepageconvert.h \
    $$PWD/importers/convertedstringcontainer.h \
    $$PWD/importers/csv.h \
    $$PWD/importers/csvimportcolumn.h \
    $$PWD/importers/csvimporter.h \
    $$PWD/importers/importcolumn.h \
    $$PWD/importers/importdataset.h \
    $$PWD/importers/importer.h \
    $$PWD/importers/importerutils.h \
    $$PWD/importers/jaspimporter.h \
    $$PWD/importers/ods/odsimportcolumn.h \
    $$PWD/importers/ods/odsimportdataset.h \
    $$PWD/importers/ods/odssheetcell.h \
    $$PWD/importers/ods/odstypes.h \
    $$PWD/importers/ods/odsxmlcontentshandler.h \
    $$PWD/importers/ods/odsxmlhandler.h \
    $$PWD/importers/ods/odsxmlmanifesthandler.h \
    $$PWD/importers/odsimporter.h \
    $$PWD/importers/spss/characterencodingrecord.h \
    $$PWD/importers/spss/cpconverter.h \
    $$PWD/importers/spss/datainforecord.h \
    $$PWD/importers/spss/datarecords.h \
    $$PWD/importers/spss/dictionaryterminationrecord.h \
    $$PWD/importers/spss/documentrecord.h \
    $$PWD/importers/spss/extnumbercasesrecord.h \
    $$PWD/importers/spss/fileheaderrecord.h \
    $$PWD/importers/spss/floatinforecord.h \
    $$PWD/importers/spss/integerinforecord.h \
    $$PWD/importers/spss/longvarnamesrecord.h \
    $$PWD/importers/spss/measures.h \
    $$PWD/importers/spss/miscinforecord.h \
    $$PWD/importers/spss/missingvaluechecker.h \
    $$PWD/importers/spss/numericconverter.h \
    $$PWD/importers/spss/readablerecord.h \
    $$PWD/importers/spss/spssformattype.h \
    $$PWD/importers/spss/spssimportcolumn.h \
    $$PWD/importers/spss/spssimportdataset.h \
    $$PWD/importers/spss/spssstream.h \
    $$PWD/importers/spss/stringutils.h \
    $$PWD/importers/spss/systemfileformat.h \
    $$PWD/importers/spss/valuelabelvarsrecord.h \
    $$PWD/importers/spss/vardisplayparamrecord.h \
    $$PWD/importers/spss/variablerecord.h \
    $$PWD/importers/spss/verylongstringrecord.h \
    $$PWD/importers/spssimporter.h \
    $$PWD/mainwindow.h \
    $$PWD/module.h \
    $$PWD/onlinedataconnection.h \
    $$PWD/onlinedatamanager.h \
    $$PWD/onlinedatanode.h \
    $$PWD/onlinedatanodeosf.h \
    $$PWD/onlinenode.h \
    $$PWD/onlineusernode.h \
    $$PWD/onlineusernodeosf.h \
    $$PWD/osfnam.h \
    $$PWD/preferencesdialog.h \
    $$PWD/qutils.h \
    $$PWD/ribbons/ribbonhome.h \
    $$PWD/ribbons/ribbonwidget.h \
    $$PWD/simplecrypt.h \
    $$PWD/simplecryptkey.h \
    $$PWD/term.h \
    $$PWD/terms.h \
    $$PWD/variableinfo.h \
    $$PWD/variablespage/levelstablemodel.h \
    $$PWD/widgets/anovamodelwidget.h \
    $$PWD/widgets/assignbutton.h \
    $$PWD/widgets/assignbuttonmenu.h \
    $$PWD/widgets/availablefieldslistview.h \
    $$PWD/widgets/boundassignwidget.h \
    $$PWD/widgets/boundcheckbox.h \
    $$PWD/widgets/boundcombobox.h \
    $$PWD/widgets/boundgroupbox.h \
    $$PWD/widgets/boundlistview.h \
    $$PWD/widgets/boundmodel.h \
    $$PWD/widgets/boundpairstable.h \
    $$PWD/widgets/boundsingleitemview.h \
    $$PWD/widgets/boundtableview.h \
    $$PWD/widgets/boundtablewidget.h \
    $$PWD/widgets/boundtextbox.h \
    $$PWD/widgets/boundtextedit.h \
    $$PWD/widgets/button.h \
    $$PWD/widgets/draganddrop.h \
    $$PWD/widgets/droptarget.h \
    $$PWD/widgets/enhanceddroptarget.h \
    $$PWD/widgets/expanderbutton.h \
    $$PWD/widgets/groupbox.h \
    $$PWD/widgets/infopopup.h \
    $$PWD/widgets/itemmodelselectitem.h \
    $$PWD/widgets/itemmodelselectvariable.h \
    $$PWD/widgets/listview.h \
    $$PWD/widgets/ribbonbutton.h \
    $$PWD/widgets/stealthbutton.h \
    $$PWD/widgets/subjectivepriorswidget.h \
    $$PWD/widgets/tabbar.h \
    $$PWD/widgets/tablemodel.h \
    $$PWD/widgets/tablemodelanovadesign.h \
    $$PWD/widgets/tablemodelanovamodel.h \
    $$PWD/widgets/tablemodelanovamodelnuisancefactors.h \
    $$PWD/widgets/tablemodelanovawithinsubjectcells.h \
    $$PWD/widgets/tablemodelcontrasts.h \
    $$PWD/widgets/tablemodelpairsassigned.h \
    $$PWD/widgets/tablemodelvariables.h \
    $$PWD/widgets/tablemodelvariablesassigned.h \
    $$PWD/widgets/tablemodelvariablesavailable.h \
    $$PWD/widgets/tablemodelvariableslevels.h \
    $$PWD/widgets/tablemodelvariablesoptions.h \
    $$PWD/widgets/tableview.h \
    $$PWD/widgets/tableviewmenueditor.h \
    $$PWD/widgets/tableviewmenueditordelegate.h \
    $$PWD/widgets/textmodellavaan.h \
    $$PWD/widgets/toolbutton.h \
    $$PWD/widgets/customwebengineview.h \
    $$PWD/resultsjsinterface.h \
    $$PWD/customwebenginepage.h \
    $$PWD/asyncloaderthread.h \
    $$PWD/aboutdialogjsinterface.h \
    $$PWD/variablespage/labelfiltergenerator.h \
    $$PWD/columnsmodel.h \
    $$PWD/datasetview.h \
    $$PWD/jsonutilities.h \
    $$PWD/backstage/backstagedatalibrary.h \
    $$PWD/backstage/datalibrarylistmodel.h \
    $$PWD/backstage/datalibrarybreadcrumbsmodel.h \
    $$PWD/settings.h \
    $$PWD/enginerepresentation.h \
    $$PWD/rscriptstore.h \
    $$PWD/computedcolumnsmodel.h \
    $$PWD/modules/dynamicmodule.h \
    $$PWD/modules/dynamicmodules.h \
    $$PWD/modules/analysisentry.h \
    $$PWD/modules/ribbonentry.h

FORMS += \
    $$PWD/aboutdialog.ui \
    $$PWD/backstage/authwidget.ui\
    $$PWD/backstage/backstagecomputer.ui \
    $$PWD/mainwindow.ui \
    $$PWD/preferencesdialog.ui \
    $$PWD/ribbons/ribbonhome.ui \
    $$PWD/variableswidget.ui \
    $$PWD/widgets/anovamodelwidget.ui \
    $$PWD/widgets/boundassignwidget.ui \
    $$PWD/widgets/subjectivepriorswidget.ui \
    $$PWD/backstage/backstagedatalibrary.ui

RESOURCES += \
    $$PWD/backstage/backstage.qrc \
    $$PWD/html/html.qrc \
    $$PWD/resources/icons.qrc \
    $$PWD/resources/resources.qrc

   unix:OTHER_FILES += $$PWD/icon.icns
windows:OTHER_FILES += $$PWD/icon.rc

OTHER_FILES += \
    $$PWD/html/css/images/animated-overlay.gif \
    $$PWD/html/css/images/arrowsmalldownbtn.png \
    $$PWD/html/css/images/copy.png \
    $$PWD/html/css/images/logo.svg \
    $$PWD/html/css/images/resizer.png \
    $$PWD/html/css/images/running.gif \
    $$PWD/html/css/images/ui-bg_flat_0_aaaaaa_40x100.png \
    $$PWD/html/css/images/ui-bg_flat_75_ffffff_40x100.png \
    $$PWD/html/css/images/ui-bg_glass_55_fbf9ee_1x400.png \
    $$PWD/html/css/images/ui-bg_glass_65_ffffff_1x400.png \
    $$PWD/html/css/images/ui-bg_glass_75_dadada_1x400.png \
    $$PWD/html/css/images/ui-bg_glass_75_e6e6e6_1x400.png \
    $$PWD/html/css/images/ui-bg_glass_95_fef1ec_1x400.png \
    $$PWD/html/css/images/ui-bg_highlight-soft_75_cccccc_1x100.png \
    $$PWD/html/css/images/ui-icons_222222_256x240.png \
    $$PWD/html/css/images/ui-icons_2e83ff_256x240.png \
    $$PWD/html/css/images/ui-icons_454545_256x240.png \
    $$PWD/html/css/images/ui-icons_888888_256x240.png \
    $$PWD/html/css/images/ui-icons_cd0a0a_256x240.png \
    $$PWD/html/css/images/waiting.gif \
    $$PWD/html/css/images/waiting.svg \
    $$PWD/html/css/jquery-ui-1.10.1.custom.css \
    $$PWD/html/css/theme-jasp.css \
    $$PWD/html/css/theme-spss.css \
    $$PWD/html/index.html \
    $$PWD/html/js/analysis.js \
    $$PWD/html/js/displaydefs.js \
    $$PWD/html/js/image.js \
    $$PWD/html/js/images.js \
    $$PWD/html/js/jaspwidget.js \
    $$PWD/html/js/jquery-1.9.1.js \
    $$PWD/html/js/jquery-ui-1.10.1.custom.js \
    $$PWD/html/js/main.js \
    $$PWD/html/js/table.js \
    $$PWD/html/js/utils.js \
    $$PWD/html/js/tables.js \
    $$PWD/html/js/underscore-min.js \
    $$PWD/html/js/underscore.js \
    $$PWD/resources/arrow-left.png \
    $$PWD/resources/arrow-right.png \
    $$PWD/resources/expanded.png \
    $$PWD/resources/expander-arrow-down-disabled.png \
    $$PWD/resources/expander-arrow-down-hover.png \
    $$PWD/resources/expander-arrow-down.png \
    $$PWD/resources/expander-arrow-up-disabled.png \
    $$PWD/resources/expander-arrow-up-hover.png \
    $$PWD/resources/expander-arrow-up.png \
    $$PWD/resources/icons/accessories-calculator.png \
    $$PWD/resources/icons/accessories-text-editor.png \
    $$PWD/resources/icons/analysis-background-clicked.png \
    $$PWD/resources/icons/analysis-background-mouseover.png \
    $$PWD/resources/icons/analysis-bayesian-anova.png \
    $$PWD/resources/icons/analysis-bayesian-anova.svg \
    $$PWD/resources/icons/analysis-bayesian-crosstabs.png \
    $$PWD/resources/icons/analysis-bayesian-crosstabs.svg \
    $$PWD/resources/icons/analysis-bayesian-regression.png \
    $$PWD/resources/icons/analysis-bayesian-regression.svg \
    $$PWD/resources/icons/analysis-bayesian-ttest.png \
    $$PWD/resources/icons/analysis-bayesian-ttest.svg \
    $$PWD/resources/icons/analysis-classical-anova.png \
    $$PWD/resources/icons/analysis-classical-anova.svg \
    $$PWD/resources/icons/analysis-classical-crosstabs.png \
    $$PWD/resources/icons/analysis-classical-crosstabs.svg \
    $$PWD/resources/icons/analysis-classical-regression.png \
    $$PWD/resources/icons/analysis-classical-regression.svg \
    $$PWD/resources/icons/analysis-classical-sem.svg \
    $$PWD/resources/icons/analysis-classical-ttest.png \
    $$PWD/resources/icons/analysis-classical-ttest.svg \
    $$PWD/resources/icons/analysis-descriptives.png \
    $$PWD/resources/icons/analysis-descriptives.svg \
    $$PWD/resources/icons/analysis-ttest.png \
    $$PWD/resources/icons/application-exit.png \
    $$PWD/resources/icons/application-vnd.oasis.opendocument.spreadsheet.png \
    $$PWD/resources/icons/application-x-kchart.png \
    $$PWD/resources/icons/applications-development.png \
    $$PWD/resources/icons/applications-education-mathematics.png \
    $$PWD/resources/icons/applications-education.png \
    $$PWD/resources/icons/applications-system.png \
    $$PWD/resources/icons/applications-toys.png \
    $$PWD/resources/icons/bookcase.png \
    $$PWD/resources/icons/core.png \
    $$PWD/resources/icons/dialog-close.png \
    $$PWD/resources/icons/document-new.png \
    $$PWD/resources/icons/document-open.png \
    $$PWD/resources/icons/document-print.png \
    $$PWD/resources/icons/document-save-as.png \
    $$PWD/resources/icons/document-save.png \
    $$PWD/resources/icons/edit-copy.png \
    $$PWD/resources/icons/edit-delete-shred.png \
    $$PWD/resources/icons/edit-find.png \
    $$PWD/resources/icons/editImage.png \
    $$PWD/resources/icons/edit-paste.png \
    $$PWD/resources/icons/edit-redo.png \
    $$PWD/resources/icons/edit-undo.png \
    $$PWD/resources/icons/emblem-favorite.png \
    $$PWD/resources/icons/folder-blue.png \
    $$PWD/resources/icons/folder-cyan.png \
    $$PWD/resources/icons/folder-grey.png \
    $$PWD/resources/icons/folder-orange.png \
    $$PWD/resources/icons/folder-red.png \
    $$PWD/resources/icons/folder-txt.png \
    $$PWD/resources/icons/folder-violet.png \
    $$PWD/resources/icons/folder-yellow.png \
    $$PWD/resources/icons/help-about.png \
    $$PWD/resources/icons/kbrunch.png \
    $$PWD/resources/icons/kchart.png \
    $$PWD/resources/icons/kcmpartitions.png \
    $$PWD/resources/icons/kcoloredit.png \
    $$PWD/resources/icons/preferences-system-session-services.png \
    $$PWD/resources/icons/roll.png \
    $$PWD/resources/icons/text-x-moc.png \
    $$PWD/resources/icons/toolbutton-descriptives.png \
    $$PWD/resources/icons/toolbutton-menu-indicator.svg \
    $$PWD/resources/icons/toolbutton-menu-indicator.svg \
    $$PWD/resources/icons/tools-wizard.png \
    $$PWD/resources/icons/user-home.png \
    $$PWD/resources/icons/variable-nominal-inactive.svg \
    $$PWD/resources/icons/variable-nominal-text.svg \
    $$PWD/resources/icons/variable-nominal.png \
    $$PWD/resources/icons/variable-nominal.svg \
    $$PWD/resources/icons/variable-ordinal-inactive.svg \
    $$PWD/resources/icons/variable-ordinal.png \
    $$PWD/resources/icons/variable-ordinal.svg \
    $$PWD/resources/icons/variable-scale-inactive.svg \
    $$PWD/resources/icons/variable-scale.png \
    $$PWD/resources/icons/variable-scale.svg \
    $$PWD/resources/osx/ribbonbutton.css

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
    $$PWD/backstage/firsttabsstylesheet.qss \
    $$PWD/backstage/secondtabsstylesheet.qss \
    $$PWD/html/css/images/tinylogo.svg \
    $$PWD/resources/icons/file-jasp.svg \
    $$PWD/html/js/analyses.js \
    $$PWD/html/js/analysis.js \
    $$PWD/html/js/backbone-1.1.2.js \
    $$PWD/html/js/collection.js \
    $$PWD/html/js/etch.js \
    $$PWD/html/js/image.js \
    $$PWD/html/js/jaspwidgets.js \
    $$PWD/html/js/jquery-1.9.1.js \
    $$PWD/html/js/jquery-ui-1.10.1.custom.js \
    $$PWD/html/js/jquery-ui-1.10.1.custom.min.js \
    $$PWD/html/js/main.js \
    $$PWD/html/js/mrkdwn.js \
    $$PWD/html/js/object.js \
    $$PWD/html/js/qwebchannel.js \
    $$PWD/html/js/table.js \
    $$PWD/html/js/underscore-min.js \
    $$PWD/html/js/underscore.js \
    $$PWD/html/js/utils.js
