
QT       += core gui webkit webkitwidgets svg

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

windows:CONFIG += c++11

DESTDIR = ..
TARGET = JASP
TEMPLATE = app

DEPENDPATH = ..

CONFIG -= app_bundle

INCLUDEPATH += ../JASP-Common/

   unix:INCLUDEPATH += /opt/local/include
windows:INCLUDEPATH += C:/progra~1/boost/boost_1_53_0

PRE_TARGETDEPS += ../libJASP-Common.a

LIBS += -L.. -lJASP-Common

unix:ICON = icon.icns
windows:RC_FILE = icon.rc

windows:LIBS += -lole32 -loleaut32

QMAKE_CXXFLAGS += -Wno-c++11-extensions
QMAKE_CXXFLAGS += -Wno-unused-parameter
QMAKE_CXXFLAGS += -Wno-c++11-long-long
QMAKE_CXXFLAGS += -Wno-c++11-extra-semi

SOURCES += main.cpp\
	mainwindow.cpp \
	datasettablemodel.cpp \
    backstageform.cpp \
    enginesync.cpp \
    availablefields.cpp \
	asyncloader.cpp \
	maintableview.cpp \
	maintablehorizontalheader.cpp \
    widgets/assignbutton.cpp \
    widgets/availablefieldslistview.cpp \
    widgets/boundcheckbox.cpp \
    widgets/boundlistview.cpp \
    widgets/expanderbutton.cpp \
    widgets/infopopup.cpp \
    widgets/ribbonbutton.cpp \
    widgets/toolbutton.cpp \
    widgets/boundtextbox.cpp \
    widgets/boundgroupbox.cpp \
	widgets/progresswidget.cpp \
	widgets/anovamodelwidget.cpp \
	widgets/listview.cpp \
	widgets/draganddrop.cpp \
	widgets/assignbuttonmenu.cpp \
	widgets/tableview.cpp \
	widgets/boundpairstable.cpp \
	widgets/boundcombobox.cpp \
	widgets/boundtableview.cpp \
	widgets/tableviewmenueditor.cpp \
	widgets/tableviewmenueditordelegate.cpp \
	analysisforms/analysisform.cpp \
    analysisforms/anovabayesianform.cpp \
    analysisforms/ttestpairedsamplesform.cpp \
    analysisforms/anovamultivariateform.cpp \
    analysisforms/ttestbayesianonesampleform.cpp \
    analysisforms/ancovaform.cpp \
    analysisforms/anovaform.cpp \
    analysisforms/descriptivesform.cpp \
    analysisforms/anovaonewayform.cpp \
    analysisforms/ttestonesampleform.cpp \
    analysisforms/ttestindependentsamplesform.cpp \
    analysisforms/ancovamultivariateform.cpp \
	analysisforms/regressionlinearform.cpp \
	analysisforms/correlationform.cpp \
    widgets/boundassignwidget.cpp \
    analysisforms/anovarepeatedmeasuresform.cpp \
    analysisforms/crosstabsform.cpp \
    utils.cpp \
    analysisforms/correlationpartialform.cpp \
    ribbons/ribbonwidget.cpp \
    ribbons/ribbonsem.cpp \
    ribbons/ribbonanalysis.cpp \
    ribbons/ribbonhome.cpp \
    analysisforms/semsimpleform.cpp \
	widgets/boundtextedit.cpp \
    widgets/stealthbutton.cpp \
    analysisforms/ttestbayesianindependentsamplesform.cpp \
    analysisforms/ttestbayesianpairedsamplesform.cpp \
    widgets/itemmodelselectitem.cpp \
    widgets/itemmodelselectvariable.cpp \
    widgets/tabbar.cpp \
    optionsform.cpp \
    widgets/textmodellavaan.cpp \
    terms.cpp \
    term.cpp \
    widgets/tablemodelanovamodelnuisancefactors.cpp \
    widgets/tablemodelpairsassigned.cpp \
    widgets/tablemodelvariables.cpp \
    widgets/tablemodelvariablesassigned.cpp \
    widgets/tablemodelvariablesavailable.cpp \
    widgets/tablemodelvariableslevels.cpp \
    widgets/tablemodelvariablesoptions.cpp \
    widgets/tablemodelanovamodel.cpp \
    widgets/tablemodelcontrasts.cpp

HEADERS  += \
    datasettablemodel.h \
    backstageform.h \
    enginesync.h \
    availablefields.h \
    analysisforms/analysisform.h \
    widgets/assignbutton.h \
    widgets/availablefieldslistview.h \
    widgets/boundcheckbox.h \
    widgets/boundlistview.h \
    widgets/expanderbutton.h \
    widgets/infopopup.h \
    widgets/ribbonbutton.h \
    widgets/toolbutton.h \
    widgets/boundtextbox.h \
    widgets/boundgroupbox.h \
    analysisforms/anovabayesianform.h \
    analysisforms/ttestpairedsamplesform.h \
    analysisforms/anovamultivariateform.h \
    analysisforms/ttestbayesianonesampleform.h \
    widgets/boundpairstable.h \
    asyncloader.h \
    widgets/progresswidget.h \
    widgets/anovamodelwidget.h \
    bound.h \
    widgets/boundmodel.h \
    widgets/listview.h \
    widgets/draganddrop.h \
    widgets/assignbuttonmenu.h \
    widgets/enhanceddroptarget.h \
    widgets/tableview.h \
    maintableview.h \
    widgets/droptarget.h \
    widgets/tablemodel.h \
    analysisforms/ancovaform.h \
    analysisforms/anovaform.h \
    analysisforms/descriptivesform.h \
    analysisforms/anovaonewayform.h \
    analysisforms/ttestindependentsamplesform.h \
    analysisforms/ttestonesampleform.h \
    analysisforms/ancovamultivariateform.h \
    maintablehorizontalheader.h \
    analysisforms/regressionlinearform.h \
    mainwindow.h \
    analysisforms/correlationform.h \
    widgets/boundcombobox.h \
    widgets/boundtableview.h \
    widgets/tableviewmenueditor.h \
    widgets/tableviewmenueditordelegate.h \
    widgets/boundassignwidget.h \
    analysisforms/anovarepeatedmeasuresform.h \
    utils.h \
    analysisforms/crosstabsform.h \
    analysisforms/correlationpartialform.h \
    ribbons/ribbonwidget.h \
    ribbons/ribbonsem.h \
    ribbons/ribbonanalysis.h \
    ribbons/ribbonhome.h \
    analysisforms/semsimpleform.h \
	widgets/boundtextedit.h \
    widgets/stealthbutton.h \
    analysisforms/ttestbayesianindependentsamplesform.h \
    analysisforms/ttestbayesianpairedsamplesform.h \
    widgets/itemmodelselectitem.h \
    widgets/itemmodelselectvariable.h \
    widgets/tabbar.h \
    optionsform.h \
    widgets/textmodellavaan.h \
    term.h \
    terms.h \
    variableinfo.h \
    widgets/tablemodelanovamodelnuisancefactors.h \
    widgets/tablemodelpairsassigned.h \
    widgets/tablemodelvariables.h \
    widgets/tablemodelvariablesassigned.h \
    widgets/tablemodelvariablesavailable.h \
    widgets/tablemodelvariableslevels.h \
    widgets/tablemodelvariablesoptions.h \
    widgets/tablemodelanovamodel.h \
    widgets/tablemodelcontrasts.h

FORMS    += \
    backstageform.ui \
    analysisforms/anovabayesianform.ui \
    analysisforms/ttestpairedsamplesform.ui \
    analysisforms/anovamultivariateform.ui \
    analysisforms/ttestbayesianonesampleform.ui \
    widgets/progresswidget.ui \
    widgets/anovamodelwidget.ui \
    analysisforms/ancovaform.ui \
    analysisforms/anovaform.ui \
    analysisforms/descriptivesform.ui \
    analysisforms/anovaonewayform.ui \
    analysisforms/ttestindependentsamplesform.ui \
    analysisforms/ttestonesampleform.ui \
    analysisforms/ancovamultivariateform.ui \
    analysisforms/regressionlinearform.ui \
    mainwindow.ui \
    analysisforms/correlationform.ui \
    widgets/boundassignwidget.ui \
    analysisforms/anovarepeatedmeasuresform.ui \
    analysisforms/crosstabsform.ui \
    analysisforms/correlationpartialform.ui \
    ribbons/ribbonhome.ui \
    ribbons/ribbonsem.ui \
    ribbons/ribbonanalysis.ui \
    analysisforms/semsimpleform.ui \
    analysisforms/ttestbayesianindependentsamplesform.ui \
    analysisforms/ttestbayesianpairedsamplesform.ui \
    optionsform.ui


RESOURCES += \
	html/html.qrc \
    resources/icons.qrc \
    resources/resources.qrc \
    resources/win/stylesheets.qrc

unix:OTHER_FILES += icon.icns
windows:OTHER_FILES += icon.rc

OTHER_FILES += \
    html/index.html \
    html/css/jquery-ui-1.10.1.custom.css \
    html/css/jquery-ui-1.10.1.custom.min.css \
    html/css/images/animated-overlay.gif \
    html/css/images/ui-bg_flat_0_aaaaaa_40x100.png \
    html/css/images/ui-bg_flat_75_ffffff_40x100.png \
    html/css/images/ui-bg_glass_55_fbf9ee_1x400.png \
    html/css/images/ui-bg_glass_65_ffffff_1x400.png \
    html/css/images/ui-bg_glass_75_dadada_1x400.png \
    html/css/images/ui-bg_glass_75_e6e6e6_1x400.png \
    html/css/images/ui-bg_glass_95_fef1ec_1x400.png \
    html/css/images/ui-bg_highlight-soft_75_cccccc_1x100.png \
    html/css/images/ui-icons_2e83ff_256x240.png \
    html/css/images/ui-icons_222222_256x240.png \
    html/css/images/ui-icons_454545_256x240.png \
    html/css/images/ui-icons_888888_256x240.png \
    html/css/images/ui-icons_cd0a0a_256x240.png \
    html/js/jquery-1.9.1.js \
    html/js/jquery-ui-1.10.1.custom.js \
    html/js/jquery-ui-1.10.1.custom.min.js \
    html/js/main.js \
    html/js/underscore-min.js \
    html/js/underscore.js \
    html/js/table.js \
    html/js/tables.js \
    resources/expanded.png \
    resources/expander-arrow-down-disabled.png \
    resources/expander-arrow-down-hover.png \
    resources/expander-arrow-down.png \
    resources/expander-arrow-up-disabled.png \
    resources/expander-arrow-up-hover.png \
    resources/expander-arrow-up.png \
    resources/icons/accessories-calculator.png \
    resources/icons/accessories-text-editor.png \
    resources/icons/application-exit.png \
    resources/icons/application-vnd.oasis.opendocument.spreadsheet.png \
    resources/icons/application-x-kchart.png \
    resources/icons/applications-development.png \
    resources/icons/applications-education-mathematics.png \
    resources/icons/applications-education.png \
    resources/icons/applications-system.png \
    resources/icons/applications-toys.png \
    resources/icons/bookcase.png \
    resources/icons/core.png \
    resources/icons/dialog-close.png \
    resources/icons/document-new.png \
    resources/icons/document-open.png \
    resources/icons/document-print.png \
    resources/icons/document-save-as.png \
    resources/icons/document-save.png \
    resources/icons/edit-copy.png \
    resources/icons/edit-delete-shred.png \
    resources/icons/edit-find.png \
    resources/icons/edit-paste.png \
    resources/icons/edit-redo.png \
    resources/icons/edit-undo.png \
    resources/icons/emblem-favorite.png \
    resources/icons/folder-blue.png \
    resources/icons/folder-cyan.png \
    resources/icons/folder-grey.png \
    resources/icons/folder-orange.png \
    resources/icons/folder-red.png \
    resources/icons/folder-txt.png \
    resources/icons/folder-violet.png \
    resources/icons/folder-yellow.png \
    resources/icons/help-about.png \
    resources/icons/kbrunch.png \
    resources/icons/kchart.png \
    resources/icons/kcmpartitions.png \
    resources/icons/kcoloredit.png \
    resources/icons/preferences-system-session-services.png \
    resources/icons/roll.png \
    resources/icons/text-x-moc.png \
    resources/icons/tools-wizard.png \
    resources/icons/user-home.png \
    resources/arrow-left.png \
    resources/arrow-right.png \
    resources/osx/ribbonbutton.css \
    html/js/displaydefs.js \
    html/css/theme-spss.css \
    html/css/theme-jasp.css \
    resources/icons/variable-nominal.png \
    resources/icons/variable-ordinal.png \
    resources/icons/variable-scale.png \
    resources/icons/analysis-ttest.png \
    html/css/images/copy.png \
    html/css/images/running.gif \
    html/css/images/waiting.gif \
    resources/icons/toolbutton-descriptives.png \
    resources/icons/analysis-bayesian-crosstabs.png \
    resources/icons/analysis-bayesian-regression.png \
    resources/icons/analysis-bayesian-anova.png \
    resources/icons/analysis-bayesian-ttest.png \
    resources/icons/analysis-classical-anova.png \
    resources/icons/analysis-classical-crosstabs.png \
    resources/icons/analysis-classical-regression.png \
    resources/icons/analysis-classical-ttest.png \
    resources/icons/analysis-descriptives.png \
    resources/icons/analysis-background-mouseover.png \
    resources/icons/analysis-background-clicked.png \
    resources/icons/analysis-descriptives.svg \
    resources/icons/toolbutton-menu-indicator.svg \
    resources/icons/variable-scale.svg \
    resources/icons/variable-ordinal.svg \
    resources/icons/variable-nominal.svg \
    resources/icons/analysis-classical-regression.svg \
    resources/icons/analysis-classical-anova.svg \
    resources/icons/analysis-classical-ttest.svg \
    resources/icons/analysis-bayesian-ttest.svg \
    resources/icons/analysis-bayesian-anova.svg \
    resources/icons/analysis-bayesian-crosstabs.svg \
    resources/icons/analysis-classical-crosstabs.svg \
    resources/icons/analysis-bayesian-regression.svg \
    resources/icons/toolbutton-menu-indicator.svg \
	html/js/image.js \
    resources/icons/variable-scale-inactive.svg \
    resources/icons/variable-ordinal-inactive.svg \
    resources/icons/variable-nominal-inactive.svg \
	html/css/images/logo.svg \
	html/js/images.js \
	html/js/analysis.js \
    resources/icons/variable-nominal-text.svg

