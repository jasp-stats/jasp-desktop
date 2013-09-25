
QT       += core gui webkit webkitwidgets

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

windows:LIBS += -lole32 -loleaut32

SOURCES += main.cpp\
	mainwindow.cpp \
	datasettablemodel.cpp \
    backstageform.cpp \
    enginesync.cpp \
    mainwidget.cpp \
    availablefields.cpp \
    ribbonanalysis.cpp \
    ribbonhome.cpp \
    analysisforms/analysisform.cpp \
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
    analysisforms/anovabayesianform.cpp \
    analysisforms/ttestpairedsamplesform.cpp \
    analysisforms/anovamultivariateform.cpp \
    analysisforms/ttestbayesianonesampleform.cpp \
    widgets/boundpairstable.cpp \
    asyncloader.cpp \
    widgets/progresswidget.cpp \
    widgets/anovamodelwidget.cpp \
    widgets/listmodelvariablesavailable.cpp \
    widgets/listmodelvariables.cpp \
    widgets/listmodelvariablesassigned.cpp \
    widgets/listview.cpp \
    widgets/tablemodelvariablesassigned.cpp \
    widgets/draganddrop.cpp \
    widgets/listmodelanovamodel.cpp \
    widgets/assignbuttonmenu.cpp \
    widgets/tableview.cpp \
    maintableview.cpp \
    widgets/listmodelanovamodelnuisancefactors.cpp \
    widgets/tablemodel.cpp \
    analysisforms/ancovaform.cpp \
    analysisforms/anovaform.cpp \
    analysisforms/descriptivesform.cpp \
    analysisforms/anovaonewayform.cpp \
    analysisforms/ttestonesampleform.cpp \
    analysisforms/ttestindependentsamplesform.cpp \
    analysisforms/ancovamultivariateform.cpp

HEADERS  += mainwindow.h \
    datasettablemodel.h \
    backstageform.h \
    enginesync.h \
    mainwidget.h \
    availablefields.h \
    ribbonanalysis.h \
    ribbonhome.h \
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
    widgets/listmodelvariablesavailable.h \
    widgets/listmodelvariables.h \
    widgets/listmodelvariablesassigned.h \
    widgets/listview.h \
    widgets/tablemodelvariablesassigned.h \
    widgets/draganddrop.h \
    widgets/listmodelanovamodel.h \
    widgets/assignbuttonmenu.h \
    widgets/enhanceddroptarget.h \
    widgets/tableview.h \
    maintableview.h \
    widgets/droptarget.h \
    widgets/listmodelanovamodelnuisancefactors.h \
    widgets/tablemodel.h \
    analysisforms/ancovaform.h \
    analysisforms/anovaform.h \
    analysisforms/descriptivesform.h \
    analysisforms/anovaonewayform.h \
    analysisforms/ttestindependentsamplesform.h \
    analysisforms/ttestonesampleform.h \
    analysisforms/ancovamultivariateform.h

FORMS    += mainwindow.ui \
    backstageform.ui \
    mainwidget.ui \
    ribbonanalysis.ui \
	ribbonhome.ui \
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
    analysisforms/ancovamultivariateform.ui


RESOURCES += \
	html/html.qrc \
    resources/icons.qrc \
    resources/resources.qrc \
    resources/win/stylesheets.qrc

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
    html/js/jquery.websocket-0.0.1.js \
    html/js/main.js \
    html/js/underscore-min.js \
    html/js/underscore.js \
    html/js/jquery.svg.js \
    html/js/jquery.svggraph.js \
    html/js/jquery.svg.pack.js \
    html/js/table.js \
    html/js/bargraph.js \
    html/js/frequencies.js \
    html/js/d3.v2.js \
    html/js/nv.d3.js \
    html/js/nv.d3.min.js \
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
    resources/icons/variable-scale-trans.png \
    resources/icons/variable-nominal-trans.png \
    resources/icons/variable-ordinal-trans.png
