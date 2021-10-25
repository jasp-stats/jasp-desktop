# Generated from Desktop.pro.
cmake_minimum_required(VERSION 3.2)

find_package(PkgConfig REQUIRED)
pkg_check_modules(LIBARCHIVE REQUIRED IMPORTED_TARGET libarchive)
pkg_check_modules(LIBJSON REQUIRED IMPORTED_TARGET jsoncpp)
pkg_check_modules(LIBSSL REQUIRED IMPORTED_TARGET libssl)
pkg_check_modules(LIBSSL REQUIRED IMPORTED_TARGET libcrypto)
pkg_check_modules(LIBZ REQUIRED IMPORTED_TARGET zlib)
pkg_check_modules(LIBREADSTAT REQUIRED IMPORTED_TARGET readstat)


find_package(Boost 
                REQUIRED
                COMPONENTS nowide filesystem system date_time timer chrono
            )
find_package(Qt6 
                REQUIRED
                COMPONENTS Core Gui OpenGL Widgets Qml WebEngineWidgets WebEngineQuick WebChannel Svg Network PrintSupport Xml Quick QuickControls2 QuickWidgets Core5Compat
            )

include_directories(${Boost_INCLUDE_DIRS})
include_directories(${LIBJSON_INCLUDE_DIRS})
include_directories(${LIBARCHIVE_INCLUDE_DIRS})
include_directories(${LIBREADSTAT_INCLUDE_DIRS})

include_directories(${PROJECT_SOURCE_DIR}/Common)
include_directories(${PROJECT_SOURCE_DIR}/Common/jaspColumnEncoder)

add_definitions(-DBUILDING_JASP)
add_definitions(-DJASP_USES_QT_HERE)
add_definitions(-DPRINT_ENGINE_MESSAGES)


# I think we can put some of these files into the JASP binary, e.g., translations, and helps
file(COPY ${CMAKE_SOURCE_DIR}/Resources DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

# TODO: This can all be done using the include_directories I think
set(HEADER_FILES
    analysis/analysisform.h
    analysis/knownissues.h
    analysis/term.h
    analysis/terms.h
    analysis/analyses.h
    analysis/analysis.h
    data/datasettableproxy.h
    data/exporters/dataexporter.h
    data/exporters/exporter.h
    data/exporters/jaspexporter.h
    data/exporters/resultexporter.h
    data/importers/ods/odsimportcolumn.h
    data/importers/ods/odsimportdataset.h
    data/importers/ods/odssheetcell.h
    data/importers/ods/odstypes.h
    data/importers/ods/odsxmlcontentshandler.h
    data/importers/ods/odsxmlhandler.h
    data/importers/ods/odsxmlmanifesthandler.h
    data/importers/readstat/readstat.h
    data/importers/readstat/readstatimportcolumn.h
    data/importers/readstat/readstatimportdataset.h
    data/importers/readstatimporter.h
    data/importers/codepageconvert.h
    data/importers/convertedstringcontainer.h
    data/importers/csv.h
    data/importers/csvimportcolumn.h
    data/importers/csvimporter.h
    data/importers/importcolumn.h
    data/importers/importdataset.h
    data/importers/importer.h
    data/importers/importerutils.h
    data/importers/jaspimporter.h
    data/importers/odsimporter.h
    data/asyncloader.h
    data/asyncloaderthread.h
    data/columnsmodel.h
    data/computedcolumn.h
    data/computedcolumns.h
    data/computedcolumnsmodel.h
    data/datasetloader.h
    data/datasetpackage.h
    data/fileevent.h
    analysis/variableinfo.h
    engine/enginerepresentation.h
    engine/enginesync.h
    engine/rscriptstore.h
    gui/columntypesmodel.h
    modules/description/description.h
    modules/description/descriptionchildbase.h
    modules/description/entrybase.h
    modules/upgrader/changebase.h
    modules/upgrader/changecopy.h
    modules/upgrader/changejs.h
    modules/upgrader/changeremove.h
    modules/upgrader/changerename.h
    modules/upgrader/changesetvalue.h
    modules/upgrader/upgrade.h
    modules/upgrader/upgradeDefinitions.h
    modules/upgrader/upgradechange.h
    modules/upgrader/upgrader.h
    modules/upgrader/upgrades.h
    modules/upgrader/upgradestep.h
    modules/ribbonmodeluncommon.h
    qquick/datasetview.h
    modules/analysisentry.h
    modules/dynamicmodule.h
    modules/dynamicmodules.h
    modules/ribbonmodel.h
    modules/analysismenumodel.h
    osf/onlinedataconnection.h
    osf/onlinedatamanager.h
    osf/onlinedatanode.h
    osf/onlinedatanodeosf.h
    osf/onlinenode.h
    osf/onlineusernode.h
    osf/onlineusernodeosf.h
    osf/osfnam.h
    qquick/jasptheme.h
    qquick/rcommander.h
    utilities/appdirs.h
    utilities/application.h
    utilities/jsonutilities.h
    utilities/processhelper.h
    utilities/qmlutils.h
    utilities/qutils.h
    results/resultsjsinterface.h
    utilities/settings.h
    utilities/simplecrypt.h
    utilities/languagemodel.h
    utilities/simplecryptkey.h
    data/labelfiltergenerator.h
    widgets/filemenu/filemenuobject.h
    widgets/filemenu/datalibrary.h
    widgets/filemenu/filesystem.h
    widgets/filemenu/osffilesystem.h
    widgets/filemenu/recentfiles.h
    widgets/filemenu/computer.h
    widgets/filemenu/osf.h
    widgets/filemenu/datalibrarybreadcrumbsmodel.h
    widgets/filemenu/datalibrarylistmodel.h
    widgets/filemenu/computerfilesystem.h
    widgets/filemenu/filesystementry.h
    widgets/listmodeljagsdatainput.h
    widgets/listmodeltermsassigned.h
    widgets/listmodeltermsavailable.h
    mainwindow.h
    utilities/extractarchive.h
    widgets/listmodel.h
    widgets/listmodelassignedinterface.h
    widgets/listmodelavailableinterface.h
    widgets/listmodeldraggable.h
    widgets/listmodelmeasurescellsassigned.h
    widgets/listmodellayersassigned.h
    widgets/listmodelmultinomialchi2test.h 
    data/filtermodel.h
    widgets/filemenu/recentfileslistmodel.h
    widgets/filemenu/datalibraryfilesystem.h
    widgets/filemenu/recentfilesfilesystem.h
    widgets/filemenu/currentfilelistmodel.h
    widgets/filemenu/currentfilefilesystem.h
    widgets/filemenu/computerlistmodel.h
    widgets/filemenu/osflistmodel.h
    widgets/filemenu/osfbreadcrumbslistmodel.h
    resultstesting/compareresults.h
    resultstesting/resultscomparetable.h
    widgets/filemenu/filemenu.h
    gui/messageforwarder.h
    widgets/filemenu/filemenulistitem.h
    widgets/filemenu/filemenubasiclistmodel.h
    modules/ribbonmodelfiltered.h
    utilities/helpmodel.h
    widgets/lavaansyntaxhighlighter.h
    widgets/listmodelinteractionassigned.h
    gui/preferencesmodel.h
    widgets/filemenu/actionbuttons.h
    widgets/filemenu/resourcebuttons.h
    widgets/filemenu/resourcebuttonsvisible.h
    widgets/interactionmodel.h
    widgets/listmodelinteractionavailable.h
    results/resultmenuentry.h
    results/resultmenumodel.h
    analysis/jaspdoublevalidator.h
    widgets/listmodelfactorsform.h
    gui/aboutmodel.h
    modules/ribbonbutton.h
    widgets/filemenu/currentdatafile.h
    gui/jaspversionchecker.h
    widgets/listmodeltableviewbase.h
    widgets/sortmenumodel.h
    widgets/sortable.h
    widgets/listmodelfiltereddataentry.h
    data/importers/readstat/readstat_custom_io.h
    data/importers/readstat/readstat_windows_helper.h
    data/datasettablemodel.h
    data/labelmodel.h
    results/ploteditormodel.h
    results/ploteditoraxismodel.h
    results/ploteditorcoordinates.h
    widgets/listmodelinputvalue.h
    widgets/rowcontrols.h
    widgets/listmodelmultitermsassigned.h
    widgets/listmodelcustomcontrasts.h
    widgets/listmodellabelvalueterms.h
    analysis/jaspcontrol.h
    widgets/checkboxbase.h
    widgets/comboboxbase.h
    widgets/textinputbase.h
    widgets/componentslistbase.h
    widgets/factorsformbase.h
    widgets/inputlistbase.h
    widgets/sliderbase.h
    widgets/expanderbuttonbase.h
    widgets/variableslistbase.h
    widgets/boundcontrolmeasurescells.h
    widgets/boundcontrollayers.h
    widgets/boundcontrolterms.h
    widgets/boundcontrolmultiterms.h
    widgets/tableviewbase.h
    widgets/textareabase.h
    widgets/boundcontroltextarea.h
    widgets/boundcontrolsourcetextarea.h
    widgets/boundcontrollavaantextarea.h
    widgets/boundcontroljagstextarea.h
    widgets/radiobuttonsgroupbase.h
    widgets/jasplistcontrol.h
    widgets/radiobuttonbase.h
    widgets/sourceitem.h
    analysis/boundcontrol.h
    analysis/boundcontrolbase.h
    widgets/boundcontroltableview.h
    widgets/boundcontrolcontraststableview.h
    widgets/boundcontrolfilteredtableview.h
    widgets/variablesformbase.h
    widgets/factorlevellistbase.h
    widgets/listmodelfactorlevels.h
    widgets/listmodelgridinput.h
    widgets/boundcontrolgridtableview.h
)

set(SOURCE_FILES
    analysis/analysisform.cpp
    analysis/knownissues.cpp
    analysis/term.cpp
    analysis/terms.cpp
    analysis/analyses.cpp
    analysis/analysis.cpp
    data/datasettableproxy.cpp
    data/exporters/dataexporter.cpp
    data/exporters/exporter.cpp
    data/exporters/jaspexporter.cpp
    data/exporters/resultexporter.cpp
    data/importers/ods/odsimportcolumn.cpp
    data/importers/ods/odsimportdataset.cpp
    data/importers/ods/odssheetcell.cpp
    data/importers/ods/odstypes.cpp
    data/importers/ods/odsxmlcontentshandler.cpp
    data/importers/ods/odsxmlhandler.cpp
    data/importers/ods/odsxmlmanifesthandler.cpp
    data/importers/readstat/readstatimportcolumn.cpp
    data/importers/readstat/readstatimportdataset.cpp
    data/importers/readstatimporter.cpp
    data/importers/codepageconvert.cpp
    data/importers/convertedstringcontainer.cpp
    data/importers/csv.cpp
    data/importers/csvimportcolumn.cpp
    data/importers/csvimporter.cpp
    data/importers/importcolumn.cpp
    data/importers/importdataset.cpp
    data/importers/importer.cpp
    data/importers/jaspimporter.cpp
    data/importers/odsimporter.cpp
    data/asyncloader.cpp
    data/asyncloaderthread.cpp
    data/columnsmodel.cpp
    data/computedcolumn.cpp
    data/computedcolumns.cpp
    data/computedcolumnsmodel.cpp
    data/datasetloader.cpp
    data/datasetpackage.cpp
    data/fileevent.cpp
    engine/enginerepresentation.cpp
    engine/enginesync.cpp
    gui/columntypesmodel.cpp
    modules/description/description.cpp
    modules/description/descriptionchildbase.cpp
    modules/description/entrybase.cpp
    modules/upgrader/changebase.cpp
    modules/upgrader/changecopy.cpp
    modules/upgrader/changejs.cpp
    modules/upgrader/changeremove.cpp
    modules/upgrader/changerename.cpp
    modules/upgrader/changesetvalue.cpp
    modules/upgrader/upgrade.cpp
    modules/upgrader/upgradeDefinitions.cpp
    modules/upgrader/upgradechange.cpp
    modules/upgrader/upgrader.cpp
    modules/upgrader/upgrades.cpp
    modules/upgrader/upgradestep.cpp
    modules/ribbonmodeluncommon.cpp
    qquick/datasetview.cpp
    modules/analysisentry.cpp
    modules/dynamicmodule.cpp
    modules/dynamicmodules.cpp
    modules/ribbonmodel.cpp
    modules/analysismenumodel.cpp
    osf/onlinedataconnection.cpp
    osf/onlinedatamanager.cpp
    osf/onlinedatanode.cpp
    osf/onlinedatanodeosf.cpp
    osf/onlinenode.cpp
    osf/onlineusernode.cpp
    osf/onlineusernodeosf.cpp
    osf/osfnam.cpp
    qquick/jasptheme.cpp
    qquick/rcommander.cpp
    utilities/appdirs.cpp
    utilities/application.cpp
    utilities/jsonutilities.cpp
    utilities/processhelper.cpp
    utilities/qmlutils.cpp
    utilities/qutils.cpp
    results/resultsjsinterface.cpp
    utilities/settings.cpp
    utilities/simplecrypt.cpp
    utilities/languagemodel.cpp
    data/labelfiltergenerator.cpp
    widgets/filemenu/filemenuobject.cpp
    widgets/filemenu/datalibrary.cpp
    widgets/filemenu/filesystem.cpp
    widgets/filemenu/osffilesystem.cpp
    widgets/filemenu/recentfiles.cpp
    widgets/filemenu/computer.cpp
    widgets/filemenu/osf.cpp
    widgets/filemenu/datalibrarybreadcrumbsmodel.cpp
    widgets/filemenu/datalibrarylistmodel.cpp
    widgets/filemenu/computerfilesystem.cpp
    widgets/listmodeljagsdatainput.cpp
    widgets/listmodeltermsassigned.cpp
    widgets/listmodeltermsavailable.cpp
    main.cpp
    mainwindow.cpp
    utilities/extractarchive.cpp
    widgets/listmodel.cpp
    widgets/listmodelavailableinterface.cpp
    widgets/listmodelassignedinterface.cpp
    widgets/listmodeldraggable.cpp
    widgets/listmodelmeasurescellsassigned.cpp
    widgets/listmodellayersassigned.cpp
    widgets/listmodelmultinomialchi2test.cpp
    data/filtermodel.cpp
    widgets/filemenu/recentfileslistmodel.cpp
    widgets/filemenu/datalibraryfilesystem.cpp
    widgets/filemenu/recentfilesfilesystem.cpp
    widgets/filemenu/currentfilelistmodel.cpp
    widgets/filemenu/currentfilefilesystem.cpp
    widgets/filemenu/computerlistmodel.cpp
    widgets/filemenu/osflistmodel.cpp
    widgets/filemenu/osfbreadcrumbslistmodel.cpp
    resultstesting/compareresults.cpp
    resultstesting/resultscomparetable.cpp
    widgets/filemenu/filemenu.cpp
    gui/messageforwarder.cpp
    widgets/filemenu/filemenubasiclistmodel.cpp
    modules/ribbonmodelfiltered.cpp
    utilities/helpmodel.cpp
    widgets/lavaansyntaxhighlighter.cpp
    widgets/listmodelinteractionassigned.cpp
    gui/preferencesmodel.cpp
    widgets/filemenu/actionbuttons.cpp
    widgets/filemenu/resourcebuttons.cpp
    widgets/filemenu/resourcebuttonsvisible.cpp
    widgets/interactionmodel.cpp
    widgets/listmodelinteractionavailable.cpp
    results/resultmenumodel.cpp
    results/resultmenuentry.cpp
    analysis/jaspdoublevalidator.cpp
    widgets/listmodelfactorsform.cpp
    gui/aboutmodel.cpp
    modules/ribbonbutton.cpp
    widgets/filemenu/currentdatafile.cpp
    gui/jaspversionchecker.cpp
    widgets/listmodeltableviewbase.cpp
    widgets/sortmenumodel.cpp
    widgets/sortable.cpp
    widgets/listmodelfiltereddataentry.cpp
    data/importers/readstat/readstat_custom_io.cpp
    data/datasettablemodel.cpp
    data/labelmodel.cpp
    results/ploteditormodel.cpp
    results/ploteditoraxismodel.cpp
    results/ploteditorcoordinates.cpp
    widgets/listmodelinputvalue.cpp
    widgets/rowcontrols.cpp
    widgets/listmodelmultitermsassigned.cpp
    widgets/listmodelcustomcontrasts.cpp
    widgets/listmodellabelvalueterms.cpp
    analysis/jaspcontrol.cpp
    widgets/checkboxbase.cpp
    widgets/comboboxbase.cpp
    widgets/textinputbase.cpp
    widgets/componentslistbase.cpp
    widgets/factorsformbase.cpp
    widgets/inputlistbase.cpp
    widgets/sliderbase.cpp
    widgets/expanderbuttonbase.cpp
    widgets/variableslistbase.cpp
    widgets/boundcontrolmeasurescells.cpp
    widgets/boundcontrollayers.cpp
    widgets/boundcontrolterms.cpp
    widgets/boundcontrolmultiterms.cpp
    widgets/tableviewbase.cpp
    widgets/textareabase.cpp
    widgets/boundcontroltextarea.cpp
    widgets/boundcontrolsourcetextarea.cpp
    widgets/boundcontrollavaantextarea.cpp
    widgets/boundcontroljagstextarea.cpp
    widgets/radiobuttonsgroupbase.cpp
    widgets/jasplistcontrol.cpp
    widgets/radiobuttonbase.cpp
    widgets/sourceitem.cpp
    analysis/boundcontrolbase.cpp
    widgets/boundcontroltableview.cpp
    widgets/boundcontrolcontraststableview.cpp
    widgets/boundcontrolfilteredtableview.cpp
    widgets/variablesformbase.cpp
    widgets/factorlevellistbase.cpp
    widgets/listmodelfactorlevels.cpp
    widgets/listmodelgridinput.cpp
    widgets/boundcontrolgridtableview.cpp
)

qt_add_executable(JASP
    ${HEADER_FILES}
    ${SOURCE_FILES}
)

target_link_libraries(
    JASP PUBLIC 
    
    Common

    Qt::Core 
    Qt::Gui 
    Qt::OpenGL 
    Qt::Widgets 
    Qt::Qml 
    Qt::WebEngineWidgets
    Qt::WebEngineQuick 
    Qt::WebChannel 
    Qt::Svg
    Qt::Network 
    Qt::PrintSupport 
    Qt::Xml
    Qt::Quick 
    Qt::QuickControls2 
    Qt::QuickWidgets
    Qt::Core5Compat

    ${Boost_LIBRARIES}
    PkgConfig::LIBJSON
    PkgConfig::LIBARCHIVE
    PkgConfig::LIBREADSTAT
)

# Resources:
# set_source_files_properties("html/css/bootstrap.min.css"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/darkTheme-jasp.css"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/etch.css"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/animated-overlay.gif"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/darkTheme/arrowsmalldownbtn.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/darkTheme/closer.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/darkTheme/collapse.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/darkTheme/empty-plot.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/darkTheme/expand.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/darkTheme/menu-indicator.svg"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/darkTheme/resizer.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/darkTheme/running-large.gif"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/darkTheme/running.gif"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/darkTheme/waiting-large.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/darkTheme/waiting.gif"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/darkTheme/waiting.svg"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/editor_icons.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/lightTheme/arrowsmalldownbtn.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/lightTheme/closer.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/lightTheme/collapse.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/lightTheme/empty-plot.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/lightTheme/expand.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/lightTheme/menu-indicator.svg"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/lightTheme/resizer.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/lightTheme/running-large.gif"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/lightTheme/running.gif"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/lightTheme/waiting-large.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/lightTheme/waiting.gif"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/lightTheme/waiting.svg"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/ui-bg_flat_0_aaaaaa_40x100.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/ui-bg_flat_75_ffffff_40x100.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/ui-bg_glass_55_fbf9ee_1x400.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/ui-bg_glass_65_ffffff_1x400.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/ui-bg_glass_75_dadada_1x400.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/ui-bg_glass_75_e6e6e6_1x400.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/ui-bg_glass_95_fef1ec_1x400.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/ui-bg_highlight-soft_75_cccccc_1x100.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/ui-icons_222222_256x240.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/ui-icons_2e83ff_256x240.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/ui-icons_454545_256x240.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/ui-icons_888888_256x240.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/images/ui-icons_cd0a0a_256x240.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/jasp.css"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/jquery-ui-1.10.1.custom.css"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/lightTheme-jasp.css"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/printing.css"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/css/quill.snow.css"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/font/Lato-Light.ttf"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/font/Lato-Regular.ttf"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/img/jasp-logo-black.svg"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/img/jasp-wave-down-blue-120-squat.svg"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/img/snow.gif"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/img/ul-orange-dot.png"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/index.html"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/analyses.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/analysis.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/backbone-1.1.2.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/collection.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/column.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/etch.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/htmlNode.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/image.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/jaspwidgets.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/jquery-1.9.1.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/jquery-ui-1.10.1.custom.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/jquery-ui-1.10.1.custom.min.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/main.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/mrkdwn.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/object.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/quill.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/qwebchannel.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/table.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/underscore-min.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/underscore.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )
# set_source_files_properties("html/js/utils.js"
#     PROPERTIES QT_SKIP_QUICKCOMPILER 1
# )


# Amir:
# We can either add the resources using the `.qrc` files, or ask CMake to
# build them for us based on the directory that they are in. This can be 
# simplified with some GLOBing, then we can just create a new files, and 
# don't worry about keeping the `.qrc` files up-to-date.

# Either this or the qt_add_resources.
# set(RESOURCE_FILES
#     html/html.qrc
#     resources/icons.qrc
#     resources/resources.qrc
#     qml.qrc
# )


set(HTML_SOURCE_FILES
    "html/css/bootstrap.min.css"
    "html/css/darkTheme-jasp.css"
    "html/css/etch.css"
    "html/css/images/animated-overlay.gif"
    "html/css/images/darkTheme/arrowsmalldownbtn.png"
    "html/css/images/darkTheme/closer.png"
    "html/css/images/darkTheme/collapse.png"
    "html/css/images/darkTheme/empty-plot.png"
    "html/css/images/darkTheme/expand.png"
    "html/css/images/darkTheme/menu-indicator.svg"
    "html/css/images/darkTheme/resizer.png"
    "html/css/images/darkTheme/running-large.gif"
    "html/css/images/darkTheme/running.gif"
    "html/css/images/darkTheme/waiting-large.png"
    "html/css/images/darkTheme/waiting.gif"
    "html/css/images/darkTheme/waiting.svg"
    "html/css/images/editor_icons.png"
    "html/css/images/lightTheme/arrowsmalldownbtn.png"
    "html/css/images/lightTheme/closer.png"
    "html/css/images/lightTheme/collapse.png"
    "html/css/images/lightTheme/empty-plot.png"
    "html/css/images/lightTheme/expand.png"
    "html/css/images/lightTheme/menu-indicator.svg"
    "html/css/images/lightTheme/resizer.png"
    "html/css/images/lightTheme/running-large.gif"
    "html/css/images/lightTheme/running.gif"
    "html/css/images/lightTheme/waiting-large.png"
    "html/css/images/lightTheme/waiting.gif"
    "html/css/images/lightTheme/waiting.svg"
    "html/css/images/ui-bg_flat_0_aaaaaa_40x100.png"
    "html/css/images/ui-bg_flat_75_ffffff_40x100.png"
    "html/css/images/ui-bg_glass_55_fbf9ee_1x400.png"
    "html/css/images/ui-bg_glass_65_ffffff_1x400.png"
    "html/css/images/ui-bg_glass_75_dadada_1x400.png"
    "html/css/images/ui-bg_glass_75_e6e6e6_1x400.png"
    "html/css/images/ui-bg_glass_95_fef1ec_1x400.png"
    "html/css/images/ui-bg_highlight-soft_75_cccccc_1x100.png"
    "html/css/images/ui-icons_222222_256x240.png"
    "html/css/images/ui-icons_2e83ff_256x240.png"
    "html/css/images/ui-icons_454545_256x240.png"
    "html/css/images/ui-icons_888888_256x240.png"
    "html/css/images/ui-icons_cd0a0a_256x240.png"
    "html/css/jasp.css"
    "html/css/jquery-ui-1.10.1.custom.css"
    "html/css/lightTheme-jasp.css"
    "html/css/printing.css"
    "html/css/quill.snow.css"
    "html/font/Lato-Light.ttf"
    "html/font/Lato-Regular.ttf"
    "html/img/jasp-logo-black.svg"
    "html/img/jasp-wave-down-blue-120-squat.svg"
    "html/img/snow.gif"
    "html/img/ul-orange-dot.png"
    "html/index.html"
    "html/js/analyses.js"
    "html/js/analysis.js"
    "html/js/backbone-1.1.2.js"
    "html/js/collection.js"
    "html/js/column.js"
    "html/js/etch.js"
    "html/js/htmlNode.js"
    "html/js/image.js"
    "html/js/jaspwidgets.js"
    "html/js/jquery-1.9.1.js"
    "html/js/jquery-ui-1.10.1.custom.js"
    "html/js/jquery-ui-1.10.1.custom.min.js"
    "html/js/main.js"
    "html/js/mrkdwn.js"
    "html/js/object.js"
    "html/js/quill.js"
    "html/js/qwebchannel.js"
    "html/js/table.js"
    "html/js/underscore-min.js"
    "html/js/underscore.js"
    "html/js/utils.js"
)

qt_add_resources(JASP "html"
    PREFIX
        "/html"
    BASE
        "html"
    FILES
        ${HTML_RESOURCE_FILES}
)

set(TRANSLATION_RESOURCE_FILES
    "translations/images/flag_en.png"
    "translations/images/flag_fr.png"
    "translations/images/flag_ge.png"
    "translations/images/flag_nl.png"
)

qt_add_resources(JASP "translations"
    PREFIX
        "/translations"
    BASE
        "translations"
    FILES
        ${TRANSLATION_RESOURCE_FILES}
)


set(ICONS_RESOURCE_FILES
    "resources/icons/Rlogo.svg"
    "resources/icons/darkTheme/ConditionBy.png"
    "resources/icons/darkTheme/JASP-logo_blue.svg"
    "resources/icons/darkTheme/JASP_logo_gray.svg"
    "resources/icons/darkTheme/JASP_logo_green.svg"
    "resources/icons/darkTheme/NotR.png"
    "resources/icons/darkTheme/QuestionMark.png"
    "resources/icons/darkTheme/R.png"
    "resources/icons/darkTheme/addition-sign-small-green.svg"
    "resources/icons/darkTheme/addition-sign-small.svg"
    "resources/icons/darkTheme/addition-sign.svg"
    "resources/icons/darkTheme/and.png"
    "resources/icons/darkTheme/arrow-down.png"
    "resources/icons/darkTheme/arrow-left.png"
    "resources/icons/darkTheme/arrow-reverse.png"
    "resources/icons/darkTheme/arrow-right.png"
    "resources/icons/darkTheme/arrow-up.png"
    "resources/icons/darkTheme/check-mark.png"
    "resources/icons/darkTheme/cite.png"
    "resources/icons/darkTheme/close-button-black.png"
    "resources/icons/darkTheme/close-button.png"
    "resources/icons/darkTheme/code-icon.png"
    "resources/icons/darkTheme/collapse.png"
    "resources/icons/darkTheme/columnConstructorBackground.png"
    "resources/icons/darkTheme/computed.png"
    "resources/icons/darkTheme/copy.png"
    "resources/icons/darkTheme/create-folder.png"
    "resources/icons/darkTheme/cross-sign-small-red.svg"
    "resources/icons/darkTheme/cross.png"
    "resources/icons/darkTheme/delete_icon.png"
    "resources/icons/darkTheme/delete_icon_gray.png"
    "resources/icons/darkTheme/divide.png"
    "resources/icons/darkTheme/document-save-as.png"
    "resources/icons/darkTheme/duplicate.png"
    "resources/icons/darkTheme/duplicate_disabled.png"
    "resources/icons/darkTheme/edit-pencil.png"
    "resources/icons/darkTheme/editImage.png"
    "resources/icons/darkTheme/equal.png"
    "resources/icons/darkTheme/eraser.png"
    "resources/icons/darkTheme/eraser_all.png"
    "resources/icons/darkTheme/error.png"
    "resources/icons/darkTheme/exclamation.svg"
    "resources/icons/darkTheme/expand.png"
    "resources/icons/darkTheme/expanded.png"
    "resources/icons/darkTheme/expander-arrow-down-disabled.png"
    "resources/icons/darkTheme/expander-arrow-down-hover.png"
    "resources/icons/darkTheme/expander-arrow-down.png"
    "resources/icons/darkTheme/expander-arrow-up-disabled.png"
    "resources/icons/darkTheme/expander-arrow-up-hover.png"
    "resources/icons/darkTheme/expander-arrow-up.png"
    "resources/icons/darkTheme/eyeClosed.png"
    "resources/icons/darkTheme/eyeOpen.png"
    "resources/icons/darkTheme/file-jasp.svg"
    "resources/icons/darkTheme/filter.png"
    "resources/icons/darkTheme/filterConstructorBackground.png"
    "resources/icons/darkTheme/folder.svg"
    "resources/icons/darkTheme/gear.svg"
    "resources/icons/darkTheme/greaterThan.png"
    "resources/icons/darkTheme/greaterThanEqual.png"
    "resources/icons/darkTheme/info-button-black.png"
    "resources/icons/darkTheme/info-button.png"
    "resources/icons/darkTheme/install_icon.png"
    "resources/icons/darkTheme/jasp-logo.svg"
    "resources/icons/darkTheme/jasp-wave-down-blue-120.svg"
    "resources/icons/darkTheme/jasp-wave-up-green-120.svg"
    "resources/icons/darkTheme/large-arrow-right.png"
    "resources/icons/darkTheme/lessThan.png"
    "resources/icons/darkTheme/lessThanEqual.png"
    "resources/icons/darkTheme/loading.svg"
    "resources/icons/darkTheme/modulo.png"
    "resources/icons/darkTheme/negative.png"
    "resources/icons/darkTheme/notEqual.png"
    "resources/icons/darkTheme/or.png"
    "resources/icons/darkTheme/osf-logo.png"
    "resources/icons/darkTheme/product.png"
    "resources/icons/darkTheme/redo.svg"
    "resources/icons/darkTheme/right-arrow.png"
    "resources/icons/darkTheme/rootHead.png"
    "resources/icons/darkTheme/rootTail.png"
    "resources/icons/darkTheme/sigma.png"
    "resources/icons/darkTheme/sort-az.png"
    "resources/icons/darkTheme/sort-za.png"
    "resources/icons/darkTheme/spreadsheet.svg"
    "resources/icons/darkTheme/square_trashcan.png"
    "resources/icons/darkTheme/subtraction-sign-small.svg"
    "resources/icons/darkTheme/subtraction-sign.svg"
    "resources/icons/darkTheme/sum.png"
    "resources/icons/darkTheme/summarize.svg"
    "resources/icons/darkTheme/toolbutton-menu-indicator.svg"
    "resources/icons/darkTheme/trashcan.png"
    "resources/icons/darkTheme/trashcan.svg"
    "resources/icons/darkTheme/trashcan_open.png"
    "resources/icons/darkTheme/trashcan_open.svg"
    "resources/icons/darkTheme/ul-orange-dot.png"
    "resources/icons/darkTheme/undo.svg"
    "resources/icons/darkTheme/variable-nominal-disabled.png"
    "resources/icons/darkTheme/variable-nominal-inactive.png"
    "resources/icons/darkTheme/variable-nominal-text-inactive.svg"
    "resources/icons/darkTheme/variable-nominal-text.png"
    "resources/icons/darkTheme/variable-nominal.png"
    "resources/icons/darkTheme/variable-ordinal-disabled.png"
    "resources/icons/darkTheme/variable-ordinal-inactive.png"
    "resources/icons/darkTheme/variable-ordinal.png"
    "resources/icons/darkTheme/variable-scale-disabled.png"
    "resources/icons/darkTheme/variable-scale-inactive.png"
    "resources/icons/darkTheme/variable-scale.png"
    "resources/icons/darkTheme/variance.png"
    "resources/icons/lightTheme/ConditionBy.png"
    "resources/icons/lightTheme/JASP-logo_blue.svg"
    "resources/icons/lightTheme/JASP_logo_gray.svg"
    "resources/icons/lightTheme/JASP_logo_green.svg"
    "resources/icons/lightTheme/NotR.png"
    "resources/icons/lightTheme/QuestionMark.png"
    "resources/icons/lightTheme/R.png"
    "resources/icons/lightTheme/addition-sign-small-green.svg"
    "resources/icons/lightTheme/addition-sign-small.svg"
    "resources/icons/lightTheme/addition-sign.svg"
    "resources/icons/lightTheme/and.png"
    "resources/icons/lightTheme/arrow-down.png"
    "resources/icons/lightTheme/arrow-left.png"
    "resources/icons/lightTheme/arrow-reverse.png"
    "resources/icons/lightTheme/arrow-right.png"
    "resources/icons/lightTheme/arrow-up.png"
    "resources/icons/lightTheme/check-mark.png"
    "resources/icons/lightTheme/cite.png"
    "resources/icons/lightTheme/close-button-black.png"
    "resources/icons/lightTheme/close-button.png"
    "resources/icons/lightTheme/code-icon.png"
    "resources/icons/lightTheme/collapse.png"
    "resources/icons/lightTheme/columnConstructorBackground.png"
    "resources/icons/lightTheme/computed.png"
    "resources/icons/lightTheme/copy.png"
    "resources/icons/lightTheme/create-folder.png"
    "resources/icons/lightTheme/cross-sign-small-red.svg"
    "resources/icons/lightTheme/cross.png"
    "resources/icons/lightTheme/delete_icon.png"
    "resources/icons/lightTheme/delete_icon_gray.png"
    "resources/icons/lightTheme/divide.png"
    "resources/icons/lightTheme/document-save-as.png"
    "resources/icons/lightTheme/duplicate.png"
    "resources/icons/lightTheme/duplicate_disabled.png"
    "resources/icons/lightTheme/edit-pencil.png"
    "resources/icons/lightTheme/editImage.png"
    "resources/icons/lightTheme/equal.png"
    "resources/icons/lightTheme/eraser.png"
    "resources/icons/lightTheme/eraser_all.png"
    "resources/icons/lightTheme/error.png"
    "resources/icons/lightTheme/exclamation.svg"
    "resources/icons/lightTheme/expand.png"
    "resources/icons/lightTheme/expanded.png"
    "resources/icons/lightTheme/expander-arrow-down-disabled.png"
    "resources/icons/lightTheme/expander-arrow-down-hover.png"
    "resources/icons/lightTheme/expander-arrow-down.png"
    "resources/icons/lightTheme/expander-arrow-up-disabled.png"
    "resources/icons/lightTheme/expander-arrow-up-hover.png"
    "resources/icons/lightTheme/expander-arrow-up.png"
    "resources/icons/lightTheme/eyeClosed.png"
    "resources/icons/lightTheme/eyeOpen.png"
    "resources/icons/lightTheme/file-jasp.svg"
    "resources/icons/lightTheme/filter.png"
    "resources/icons/lightTheme/filterConstructorBackground.png"
    "resources/icons/lightTheme/folder.svg"
    "resources/icons/lightTheme/gear.svg"
    "resources/icons/lightTheme/greaterThan.png"
    "resources/icons/lightTheme/greaterThanEqual.png"
    "resources/icons/lightTheme/info-button-black.png"
    "resources/icons/lightTheme/info-button.png"
    "resources/icons/lightTheme/install_icon.png"
    "resources/icons/lightTheme/jasp-logo-black.svg"
    "resources/icons/lightTheme/jasp-logo.svg"
    "resources/icons/lightTheme/jasp-wave-down-blue-120.svg"
    "resources/icons/lightTheme/jasp-wave-up-green-120.svg"
    "resources/icons/lightTheme/large-arrow-right.png"
    "resources/icons/lightTheme/lessThan.png"
    "resources/icons/lightTheme/lessThanEqual.png"
    "resources/icons/lightTheme/loading.svg"
    "resources/icons/lightTheme/modulo.png"
    "resources/icons/lightTheme/negative.png"
    "resources/icons/lightTheme/notEqual.png"
    "resources/icons/lightTheme/or.png"
    "resources/icons/lightTheme/osf-logo.png"
    "resources/icons/lightTheme/product.png"
    "resources/icons/lightTheme/redo.svg"
    "resources/icons/lightTheme/right-arrow.png"
    "resources/icons/lightTheme/rootHead.png"
    "resources/icons/lightTheme/rootTail.png"
    "resources/icons/lightTheme/sigma.png"
    "resources/icons/lightTheme/sort-az.png"
    "resources/icons/lightTheme/sort-za.png"
    "resources/icons/lightTheme/spreadsheet.svg"
    "resources/icons/lightTheme/square_trashcan.png"
    "resources/icons/lightTheme/subtraction-sign-small.svg"
    "resources/icons/lightTheme/subtraction-sign.svg"
    "resources/icons/lightTheme/sum.png"
    "resources/icons/lightTheme/summarize.svg"
    "resources/icons/lightTheme/toolbutton-menu-indicator.svg"
    "resources/icons/lightTheme/trashcan.png"
    "resources/icons/lightTheme/trashcan.svg"
    "resources/icons/lightTheme/trashcan_open.png"
    "resources/icons/lightTheme/trashcan_open.svg"
    "resources/icons/lightTheme/ul-orange-dot.png"
    "resources/icons/lightTheme/undo.svg"
    "resources/icons/lightTheme/variable-nominal-disabled.png"
    "resources/icons/lightTheme/variable-nominal-inactive.png"
    "resources/icons/lightTheme/variable-nominal-text-inactive.svg"
    "resources/icons/lightTheme/variable-nominal-text.png"
    "resources/icons/lightTheme/variable-nominal.png"
    "resources/icons/lightTheme/variable-ordinal-disabled.png"
    "resources/icons/lightTheme/variable-ordinal-inactive.png"
    "resources/icons/lightTheme/variable-ordinal.png"
    "resources/icons/lightTheme/variable-scale-disabled.png"
    "resources/icons/lightTheme/variable-scale-inactive.png"
    "resources/icons/lightTheme/variable-scale.png"
    "resources/icons/lightTheme/variance.png"
)

qt_add_resources(JASP "icons"
    PREFIX
        "/"
    BASE
        "resources"
    FILES
        ${ICONS_RESOURCE_FILES}
)
set(resources_resource_files
    "resources/../modules/upgrader/upgrades.json"
    "resources/fonts/FiraCode-Retina.ttf"
    "resources/fonts/FreeSans.ttf"
    "resources/icons/darkTheme/jasp-logo-black.svg"
)

qt_add_resources(JASP "resources"
    PREFIX
        "/"
    BASE
        "resources"
    FILES
        ${RESOURCES_RESOURCE_FILES}
)
set(QML_RESOURCE_FILES
    "components/JASP/Controls/AddColumnField.qml"
    "components/JASP/Controls/AssignButton.qml"
    "components/JASP/Controls/AssignedPairsVariablesList.qml"
    "components/JASP/Controls/AssignedRepeatedMeasuresCells.qml"
    "components/JASP/Controls/AssignedVariablesList.qml"
    "components/JASP/Controls/AvailableVariablesList.qml"
    "components/JASP/Controls/Button.qml"
    "components/JASP/Controls/CIField.qml"
    "components/JASP/Controls/CheckBox.qml"
    "components/JASP/Controls/ColumnLayout.qml"
    "components/JASP/Controls/ComboBox.qml"
    "components/JASP/Controls/ComponentsList.qml"
    "components/JASP/Controls/ComputedColumnField.qml"
    "components/JASP/Controls/DoubleField.qml"
    "components/JASP/Controls/ExpanderButton.qml"
    "components/JASP/Controls/FactorLevelList.qml"
    "components/JASP/Controls/FileSelector.qml"
    "components/JASP/Controls/Form.qml"
    "components/JASP/Controls/FormulaField.qml"
    "components/JASP/Controls/GridLayout.qml"
    "components/JASP/Controls/GroupBox.qml"
    "components/JASP/Controls/HelpButton.qml"
    "components/JASP/Controls/InputListView.qml"
    "components/JASP/Controls/IntegerField.qml"
    "components/JASP/Controls/JAGSTextArea.qml"
    "components/JASP/Controls/JASPScrollBar.qml"
    "components/JASP/Controls/Label.qml"
    "components/JASP/Controls/ModelTermsList.qml"
    "components/JASP/Controls/PercentField.qml"
    "components/JASP/Controls/RadioButton.qml"
    "components/JASP/Controls/RadioButtonGroup.qml"
    "components/JASP/Controls/RowLayout.qml"
    "components/JASP/Controls/Slider.qml"
    "components/JASP/Controls/Switch.qml"
    "components/JASP/Controls/TabView.qml"
    "components/JASP/Controls/TableView.qml"
    "components/JASP/Controls/Text.qml"
    "components/JASP/Controls/TextArea.qml"
    "components/JASP/Controls/TextField.qml"
    "components/JASP/Controls/VariablesForm.qml"
    "components/JASP/Controls/VariablesList.qml"
    "components/JASP/Controls/qmldir"
    "components/JASP/Theme/DarkTheme.qml"
    "components/JASP/Theme/Theme.qml"
    "components/JASP/Theme/qmldir"
    "components/JASP/Widgets/AboutWindow.qml"
    "components/JASP/Widgets/AnalysisFormExpander.qml"
    "components/JASP/Widgets/AnalysisForms.qml"
    "components/JASP/Widgets/BasicTableViewJasp.qml"
    "components/JASP/Widgets/BasicThreeButtonTableView.qml"
    "components/JASP/Widgets/BayesFactorType.qml"
    "components/JASP/Widgets/Chi2TestTableView.qml"
    "components/JASP/Widgets/ComputeColumnWindow.qml"
    "components/JASP/Widgets/ContrastsList.qml"
    "components/JASP/Widgets/ControlErrorMessage.qml"
    "components/JASP/Widgets/CreateComputeColumnDialog.qml"
    "components/JASP/Widgets/CrossButton.qml"
    "components/JASP/Widgets/CustomContrastsTableView.qml"
    "components/JASP/Widgets/CustomMenu.qml"
    "components/JASP/Widgets/DataPanel.qml"
    "components/JASP/Widgets/DataTableView.qml"
    "components/JASP/Widgets/Divider.qml"
    "components/JASP/Widgets/FactorsForm.qml"
    "components/JASP/Widgets/FactorsList.qml"
    "components/JASP/Widgets/FileMenu/BreadCrumbs.qml"
    "components/JASP/Widgets/FileMenu/Computer.qml"
    "components/JASP/Widgets/FileMenu/CurrentFile.qml"
    "components/JASP/Widgets/FileMenu/DataLibrary.qml"
    "components/JASP/Widgets/FileMenu/FileList.qml"
    "components/JASP/Widgets/FileMenu/FileMenu.qml"
    "components/JASP/Widgets/FileMenu/ListItem.qml"
    "components/JASP/Widgets/FileMenu/MenuHeader.qml"
    "components/JASP/Widgets/FileMenu/OSF.qml"
    "components/JASP/Widgets/FileMenu/OSFLogin.qml"
    "components/JASP/Widgets/FileMenu/PrefsAdvanced.qml"
    "components/JASP/Widgets/FileMenu/PrefsData.qml"
    "components/JASP/Widgets/FileMenu/PrefsGroupRect.qml"
    "components/JASP/Widgets/FileMenu/PrefsMissingValues.qml"
    "components/JASP/Widgets/FileMenu/PrefsResults.qml"
    "components/JASP/Widgets/FileMenu/PrefsTextInput.qml"
    "components/JASP/Widgets/FileMenu/PrefsUI.qml"
    "components/JASP/Widgets/FileMenu/RecentFiles.qml"
    "components/JASP/Widgets/FilterConstructor/ColumnDrag.qml"
    "components/JASP/Widgets/FilterConstructor/ColumnsSelector.qml"
    "components/JASP/Widgets/FilterConstructor/ComputedColumnsConstructor.qml"
    "components/JASP/Widgets/FilterConstructor/DragGeneric.qml"
    "components/JASP/Widgets/FilterConstructor/DropSpot.qml"
    "components/JASP/Widgets/FilterConstructor/DropTrash.qml"
    "components/JASP/Widgets/FilterConstructor/ElementView.qml"
    "components/JASP/Widgets/FilterConstructor/FilterConstructor.qml"
    "components/JASP/Widgets/FilterConstructor/FilterConstructor.qmlc"
    "components/JASP/Widgets/FilterConstructor/Function.qml"
    "components/JASP/Widgets/FilterConstructor/FunctionDrag.qml"
    "components/JASP/Widgets/FilterConstructor/JASPColumn.qml"
    "components/JASP/Widgets/FilterConstructor/JSONtoFormulas.qml"
    "components/JASP/Widgets/FilterConstructor/Number.qml"
    "components/JASP/Widgets/FilterConstructor/NumberDrag.qml"
    "components/JASP/Widgets/FilterConstructor/Operator.qml"
    "components/JASP/Widgets/FilterConstructor/OperatorDrag.qml"
    "components/JASP/Widgets/FilterConstructor/OperatorSelector.qml"
    "components/JASP/Widgets/FilterConstructor/OperatorSelectorComputedColumns.qml"
    "components/JASP/Widgets/FilterConstructor/OperatorVertical.qml"
    "components/JASP/Widgets/FilterConstructor/OperatorVerticalDrag.qml"
    "components/JASP/Widgets/FilterConstructor/String.qml"
    "components/JASP/Widgets/FilterConstructor/StringDrag.qml"
    "components/JASP/Widgets/FilterWindow.qml"
    "components/JASP/Widgets/HelpWindow.qml"
    "components/JASP/Widgets/ImageInverter.qml"
    "components/JASP/Widgets/JASPDataView.qml"
    "components/JASP/Widgets/JASPMouseAreaToolTipped.qml"
    "components/JASP/Widgets/JagsTableView.qml"
    "components/JASP/Widgets/LoadingIndicator.qml"
    "components/JASP/Widgets/MainPage.qml"
    "components/JASP/Widgets/MainWindow.qml"
    "components/JASP/Widgets/MenuButton.qml"
    "components/JASP/Widgets/MessageBox.qml"
    "components/JASP/Widgets/ModuleInstaller.qml"
    "components/JASP/Widgets/ModulesMenu.qml"
    "components/JASP/Widgets/PlotEditor/PlotEditingAxis.qml"
    "components/JASP/Widgets/PlotEditor/PlotEditor.qml"
    "components/JASP/Widgets/ProgressBarHolder.qml"
    "components/JASP/Widgets/RCommanderWindow.qml"
    "components/JASP/Widgets/RectangularButton.qml"
    "components/JASP/Widgets/Ribbon/MenuArrowButton.qml"
    "components/JASP/Widgets/Ribbon/RibbonBar.qml"
    "components/JASP/Widgets/Ribbon/RibbonButton.qml"
    "components/JASP/Widgets/Ribbon/Ribbons.qml"
    "components/JASP/Widgets/RoundedButton.qml"
    "components/JASP/Widgets/SaveDiscardCancelDialog.qml"
    "components/JASP/Widgets/SetSeed.qml"
    "components/JASP/Widgets/SimpleTableView.qml"
    "components/JASP/Widgets/SortMenuButton.qml"
    "components/JASP/Widgets/SpinBox.qml"
    "components/JASP/Widgets/SplitHandle.qml"
    "components/JASP/Widgets/SubjectivePriors.qml"
    "components/JASP/Widgets/TableViewJasp.qml"
    "components/JASP/Widgets/TableViewStyleJasp.qml"
    "components/JASP/Widgets/UIScaleNotifier.qml"
    "components/JASP/Widgets/VariablesWindow.qml"
    "components/JASP/Widgets/WelcomePage.qml"
    "components/JASP/Widgets/qmldir"
)

qt_add_resources(JASP "qml"
    PREFIX
        "/"
    FILES
        ${QML_RESOURCE_FILES}
)


# #### Keys ignored in scope 1:.:.:Desktop.pro:<TRUE>:
# # COPY_CRYPT = "$$[ENVIRONMENT_CRYPTKEY]"
# # DEPENDPATH = ".."
# # DISTFILES = "modules/upgrader/upgrades.json" "modules/upgrader/upgrades_template.json" "po/jasp.po" "po/jasp_nl.po" "resources/CC-Attributions.txt"
# # HELP_PATH = "$${PWD}/../Docs/help"
# # QML_IMPORT_PATH = "$$PWD/imports" "$$PWD/components"
# # RESOURCES_DESTINATION = "$${OUT_PWD}/../Resources"
# # RESOURCES_DESTINATION_TRANSLATIONS = "$$RESOURCES_DESTINATION/Translations"
# # RESOURCES_PATH = "$${PWD}/../Resources"
# # RESOURCES_TRANSLATIONS = "$${RESOURCES_PATH}/Translations"
# # Resources.files = "../Resources"
# # Resources.path = "$$INSTALLPATH"
# # SOURCES_TRANSLATIONS = "$${PWD}/po"
# # TEMPLATE = "app"
# # copyres.depends = "delres"

# ## Scopes:
# #####################################################################

# extend_target(Desktop CONDITION (QT_MAJOR_VERSION GREATER 4)
#     PUBLIC_LIBRARIES
#         Qt::Widgets
# )

# extend_target(Desktop CONDITION LINUX
#     DEFINES
#         'R_HOME=\\\"$ENV{R_HOME}\\\"'
#     INCLUDE_DIRECTORIES
#         $ENV{R_HOME}/library/include
#         $ENV{R_HOME}/site-library/Rcpp/include
#         /usr/include/R
#         /usr/share/R/include
#     PUBLIC_LIBRARIES
#         archive
#         boost_filesystem
#         boost_system
#         m
#         readstat
#         rt
#         z
#     LINK_OPTIONS
#         "-fuse-ld=gold"
# )

# extend_target(Desktop CONDITION (LINUX) AND (EXISTS /app/lib/_x_)
#     PUBLIC_LIBRARIES
#         # Remove: L/app/lib
# )

# #### Keys ignored in scope 7:.:.:Desktop.pro:EXISTS /app/lib/_x_:
# # target.path = "/app/bin"

# #### Keys ignored in scope 8:.:.:Desktop.pro:else:
# # target.path = "/usr/bin"

# extend_target(Desktop CONDITION windows
#     DEFINES
#         BOOST_INTERPROCESS_BOOTSTAMP_IS_SESSION_MANAGER_BASED
#         BOOST_USE_WINDOWS_H
#         NOMINMAX
#         WIN32_LEAN_AND_MEAN
#     PUBLIC_LIBRARIES
#         archive.dll
#         libboost_date_time
#         libboost_filesystem
#         libboost_system
#         libreadstat
#         ole32
#         oleaut32
# )

# extend_target(Desktop CONDITION MACOS
#     PUBLIC_LIBRARIES
#         WrapIconv::WrapIconv
#         archive
#         boost_filesystem-mt
#         boost_system-mt
#         readstat
#         z
#     COMPILE_OPTIONS
#         -Wno-c++11-extensions
#         -Wno-c++11-extra-semi
#         -Wno-c++11-long-long
#         -Wno-deprecated-declarations
#         -stdlib=libc++
# )

# extend_target(Desktop CONDITION _ss_JASPTIMER_USED
#     DEFINES
#         PROFILE_JASP
# )

# extend_target(Desktop CONDITION _ss_JASPTIMER_USED AND windows
#     PUBLIC_LIBRARIES
#         libboost_chrono
#         libboost_timer
# )

# extend_target(Desktop CONDITION LINUX AND _ss_JASPTIMER_USED
#     PUBLIC_LIBRARIES
#         boost_chrono
#         boost_timer
# )

# extend_target(Desktop CONDITION MACOS AND _ss_JASPTIMER_USED
#     PUBLIC_LIBRARIES
#         boost_chrono-mt
#         boost_timer-mt
# )

# extend_target(Desktop CONDITION UNIX
#     COMPILE_OPTIONS
#         -Werror=return-type
# )

# #### Keys ignored in scope 18:.:.:Desktop.pro:UNIX:
# # POST_TARGETDEPS = "createVersionScript"
# # QMAKE_EXTRA_TARGETS = "createVersionScript"
# # SCRIPTFILENAME = "$${OUT_PWD}/../versionScript.sh"
# # createVersionScript.commands = "echo" "\"$${LITERAL_HASH}!/bin/sh\"" ">" "$$SCRIPTFILENAME" ";" "echo" "\"JASP_VERSION_MAJOR=$$JASP_VERSION_MAJOR\"" ">>" "$$SCRIPTFILENAME" ";" "echo" "\"JASP_VERSION_MINOR=$$JASP_VERSION_MINOR\"" ">>" "$$SCRIPTFILENAME" ";" "echo" "\"JASP_VERSION_REVISION=$$JASP_VERSION_REVISION\"" ">>" "$$SCRIPTFILENAME" ";" "echo" "\"JASP_VERSION_BUILD=$$JASP_VERSION_BUILD\n\"" ">>" "$$SCRIPTFILENAME" ";" "echo" "\"JASP_VERSION=$${JASP_VERSION_MAJOR}.$${JASP_VERSION_MINOR}.$${JASP_VERSION_REVISION}.$${JASP_VERSION_BUILD}\n\"" ">>" "$$SCRIPTFILENAME" ";" "echo" "\"CURRENT_R_VERSION=$$CURRENT_R_VERSION\"" ">>" "$$SCRIPTFILENAME" ";"

# extend_target(Desktop CONDITION (NOT COPY_CRYPT___equals___) AND (NOT COPY_CRYPT___equals___\"\")
#     DEFINES
#         ENVIRONMENT_CRYPTKEY=$$[ENVIRONMENT_CRYPTKEY]
# )

# #### Keys ignored in scope 24:.:.:Desktop.pro:WIN32:
# # EXTENSIONS = "cpp,qml"
# # RESOURCES_DESTINATION = <EMPTY>
# # RESOURCES_DESTINATION_TRANSLATIONS = <EMPTY>
# # RESOURCES_PATH = <EMPTY>
# # RESOURCES_TRANSLATIONS = <EMPTY>
# # SOURCES_TRANSLATIONS = <EMPTY>
# # WINPWD = "$$PWD"
# # copyres.commands = "['quote', ['cmd', '/c', 'xcopy', '/S', '/I', '/Y', '\\"$${RESOURCES_PATH}\\"', '\\"$${RESOURCES_DESTINATION}\\"']]"
# # delres.commands = "['quote', ['IF', 'exist', '\\"$$RESOURCES_DESTINATION\\"', ['rd', '/s', '/q', '\\"$$RESOURCES_DESTINATION\\";']]]" ";"

# #### Keys ignored in scope 26:.:.:Desktop.pro:NOT PWD___equals____ss_{OUT_PWD}:
# # POST_TARGETDEPS = "delres" "copyres" "maketranslations"
# # QMAKE_EXTRA_TARGETS = "delres" "copyres" "maketranslations"

# #### Keys ignored in scope 30:.:..:../JASP.pri:GITHUB_PAT_DEF_ISEMPTY:
# # GITHUB_PAT_DEF = "$$(GITHUB_PAT)"

# #### Keys ignored in scope 34:.:..:../JASP.pri:_GIT_LOCATION_ISEMPTY:
# # _GIT_LOCATION = "C:Program FilesGit"

# extend_target(Desktop CONDITION (EXISTS /app/lib/_x_) AND (LINUX)
#     DEFINES
#         FLATPAK_USED
# )

# extend_target(Desktop CONDITION (NOT (EXISTS /app/lib/_x_)) AND (LINUX AND CONFIG(debug,debug OR release))
#     DEFINES
#         JASP_DEBUG
#         LINUX_NOT_FLATPAK
# )

# extend_target(Desktop CONDITION CMAKE_BUILD_TYPE STREQUAL Debug AND (MACOS OR windows)
#     DEFINES
#         JASP_DEBUG
# )

# #### Keys ignored in scope 42:.:..:../JASP.pri:QT_ARCH___contains___i386:
# # ARCH = "i386"
# # BOOST_ARCH = "x32"

# #### Keys ignored in scope 43:.:..:../JASP.pri:else:
# # ARCH = "x64"
# # BOOST_ARCH = "x64"

# #### Keys ignored in scope 44:.:..:../JASP.pri:CONFIG(ReleaseBuild):
# # BOOST_POSTFIX = "-vc142-mt-$${BOOST_ARCH}-1_71"

# #### Keys ignored in scope 45:.:..:../JASP.pri:CONFIG(DebugBuild):
# # BOOST_POSTFIX = "-vc142-mt-gd-$${BOOST_ARCH}-1_71"

# extend_target(Desktop CONDITION (MACOS) AND (CONFIG(debug))
#     COMPILE_OPTIONS
#         -fstandalone-debug
# )

#### Keys ignored in scope 56:.:..:../R_HOME.pri:QMAKE_HOST.arch___contains___x86_64:
# _R_HOME = "/app/lib64/R"

#### Keys ignored in scope 57:.:..:../R_HOME.pri:else:
# _R_HOME = "/app/lib/R"

#### Keys ignored in scope 60:.:..:../R_HOME.pri:_R_HOME_ISEMPTY:
# _R_HOME = "/usr/lib64/R"

#### Keys ignored in scope 62:.:..:../R_HOME.pri:_R_HOME_ISEMPTY:
# _R_HOME = "/usr/lib/R"

#### Keys ignored in scope 64:.:..:../R_HOME.pri:_R_HOME_ISEMPTY:
# _R_HOME = "$$JASP_REQUIRED_FILES/Frameworks/R.framework/Versions/$$CURRENT_R_VERSION/Resources"

#### Keys ignored in scope 67:.:..:../R_HOME.pri:JASP_BUILDROOT_DIR_ISEMPTY:
# _R_HOME = "$$OUT_PWD/../R"

#### Keys ignored in scope 68:.:..:../R_HOME.pri:else:
# _R_HOME = "$${JASP_BUILDROOT_DIR}/R"

#### Keys ignored in scope 69:.:..:../R_HOME.pri:_RLibrary_ISEMPTY:
# _RLibrary = "$$_R_HOME/library"

#### Keys ignored in scope 72:.:..:../R_HOME.pri:defineReplace(winPathFix):
# THE_PATH = "$$1"

#### Keys ignored in scope 76:.:..:../R_HOME.pri:GETTEXT_LOCATION_ISEMPTY:
# GETTEXT_LOCATION = "/usr/local/bin"

#### Keys ignored in scope 78:.:..:../R_HOME.pri:GETTEXT_LOCATION_ISEMPTY:
# GETTEXT_LOCATION = "$${_GIT_LOCATION}\usr\bin"

#### Keys ignored in scope 80:.:..:../R_INSTALL_CMDS.pri:JASP_BUILDROOT_DIR_ISEMPTY:
# JASP_BUILDROOT_DIR = "$$OUT_PWD/.."

#### Keys ignored in scope 87:.:..:../R_INSTALL_CMDS.pri:JASP_LIBRARY_DIR_ISEMPTY:
# JASP_LIBRARY_DIR = "$$ROOT_LIBRARY_DIR"

#### Keys ignored in scope 90:.:..:../R_INSTALL_CMDS.pri:LOAD_WORKAROUND_ISEMPTY:
# LOAD_WORKAROUND = "false"

#### Keys ignored in scope 91:.:..:../R_INSTALL_CMDS.pri:_ss_LOAD_WORKAROUND:
# WORKAROUND_LOADER = "source" "(" "\'../workarounds.R\'" ")" ";"

#### Keys ignored in scope 92:.:..:../R_INSTALL_CMDS.pri:defineReplace(runRCommandForInstall):
# CMD = "$$1"
