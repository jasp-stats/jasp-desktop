//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#ifndef MAINWIDGET_H
#define MAINWIDGET_H

#include <QMainWindow>
#include <QSettings>

#include "dataset.h"

#include "data/datasettablemodel.h"
#include "variablespage/levelstablemodel.h"
#include "variablespage/labelfiltergenerator.h"
#include "engine/enginesync.h"
#include "analysis/analyses.h"

#include "analysis/analysisform.h"
#include "data/asyncloader.h"
#include "data/asyncloaderthread.h"
#include "data/fileevent.h"
#include "utilities/resultsjsinterface.h"
#include "widgets/customwebenginepage.h"
#include "data/columnsmodel.h"
#include "utilities/jsonutilities.h"
#include "data/computedcolumnsmodel.h"

#include "modules/dynamicmodule.h"
#include "modules/ribbonmodel.h"
#include "modules/ribbonbuttonmodel.h"
#include "modules/ribbonentry.h"
#include "data/filtermodel.h"

class ResultsJsInterface;

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
	Q_OBJECT

	friend class ResultsJsInterface;
public:
	explicit MainWindow(QApplication *application);
	void open(QString filepath);
	void testLoadedJaspFile(int timeOut, bool save);

	~MainWindow() override;

	EngineSync* _engineSync;

	Q_INVOKABLE void showHelpFromQML(QString pageName);

public slots:
	void setPPIHandler(int ppi, bool refreshAllAnalyses = true);
	void setImageBackgroundHandler(QString value);
	void setUIScaleHandler(float scale);

protected:
	void resizeEvent(QResizeEvent *event)		override;
	void dragEnterEvent(QDragEnterEvent *event) override;
	void dropEvent(QDropEvent *event)			override;
	void closeEvent(QCloseEvent *event)			override;

private:
	void makeConnections();
	void initQWidgetGUIParts();
	void StartOnlineDataManager();


	void packageChanged(DataSetPackage *package);
	void packageDataChanged(DataSetPackage *package, std::vector<std::string> &changedColumns, std::vector<std::string> &missingColumns, std::map<std::string, std::string> &changeNameColumns,	bool rowCountChanged);
	void refreshAnalysesUsingColumns(std::vector<std::string> &changedColumns, std::vector<std::string> &missingColumns, std::map<std::string, std::string> &changeNameColumns, bool rowCountChanged);

	void setDataSetAndPackageInModels(DataSetPackage *package);
	bool closeRequestCheck(bool &isSaving);

	AnalysisForm* loadForm(Analysis *analysis);
	AnalysisForm* createAnalysisForm(Analysis *analysis);

	void closeCurrentOptionsWidget();
	void removeAnalysis(Analysis *analysis);
	void addAnalysisFromDynamicModule(Modules::AnalysisEntry * entry);

	QString escapeJavascriptString(const QString &str);
	void getAnalysesUserData();
	Json::Value getResultsMeta();

	void startDataEditor(QString path);
	void checkUsedModules();
	void resultsPageLoaded(bool success, int ppi);
	void analysisUnselectedHandler();
	void setPackageModified();
	void analysisSelectedHandler(int id);
	void saveTextToFileHandler(const QString &filename, const QString &data);
	void analysisChangedDownstreamHandler(int id, QString options);
	void analysisSaveImageHandler(int id, QString options);
	void analysisEditImageHandler(int id, QString options);
	void removeAnalysisRequestHandler(int id);
	void matchComputedColumnsToAnalyses();

	void startComparingResults();
	void analysesForComparingDoneAlready();
	void finishComparingResults();
	void finishSavingComparedResults();

	bool filterShortCut();
	void setupRibbonModels(QFileInfo modulePath) { _ribbonModel->addRibbonButtonModelFromModulePath(modulePath); }
	void loadRibbonQML();
	void loadQML();

	QWebEngineView* getWebViewResults();
	void			setCurrentTab(QString tabName);


	void pauseEngines();
	void resumeEngines();

signals:
	void updateAnalysesUserData(QString userData);
	void ppiChanged(int newPPI);
	void imageBackgroundChanged(QString value);
	void saveJaspFile();

private slots:
	void showForm(Analysis *analysis);
	void showQMLWindow(QString urlQml);

	void showBackstage();
	void showMainPage();

	void analysisResultsChangedHandler(Analysis* analysis);
	void analysisImageSavedHandler(Analysis* analysis);

	void removeAllAnalyses();
	void refreshAllAnalyses();
	void refreshAnalysesUsingColumn(QString col);
	void updateShownVariablesModel();

	void tabChanged(int index);
	void helpToggled(bool on);

	void dataSetIORequest(FileEvent *event);
	void dataSetIOCompleted(FileEvent *event);
	void populateUIfromDataSet();
	void ribbonEntrySelected(const QString &item);
	void onMenuClicked(QAction *);

	void adjustOptionsPanelWidth();
	void splitterMovedHandler(int, int);

	void hideOptionsPanel();
	void showOptionsPanel();
	void showDataPanel();
	void hideDataPanel();
	void startDataEditorHandler();
	void startDataEditorEventCompleted(FileEvent *event);

	void analysisOKed();
	void analysisRunned();

	void updateMenuEnabledDisabledStatus();

	void saveKeysSelected();
	void openKeysSelected();
	void syncKeysSelected();
	void refreshKeysSelected();
	void zoomInKeysSelected();
	void zoomOutKeysSelected();
	void zoomEqualKeysSelected();

	void illegalOptionStateChanged(AnalysisForm * form);
	void fatalError();

	void helpFirstLoaded(bool ok);
	void requestHelpPage(const QString &pageName);

	void emptyValuesChangedHandler();

	void resizeVariablesWindowLabelColumn();
	void closeVariablesPage();

	void showProgress();
	void hideProgress();
	void setProgressStatus(QString status, int progress);

	void updateExcludeKey();
	void analysisFormChangedHandler(Analysis *analysis);
	void dataSetChanged(DataSet * dataSet);
	void unitTestTimeOut();

	void saveJaspFileHandler();
	void handleRibbonButtonClicked(QVariant);

private:
	void _analysisSaveImageHandler(Analysis* analysis, QString options);

private:
	typedef std::map<Analysis*, AnalysisForm*> analysisFormMap;

	Ui::MainWindow					*ui						= NULL;
	Analyses						*_analyses				= NULL;
	ResultsJsInterface				*_resultsJsInterface	= NULL;
	AnalysisForm					*_currentOptionsWidget	= NULL;
	DataSetPackage					*_package				= NULL;
	DataSetTableModel				*_tableModel			= NULL;
	LevelsTableModel				*_levelsTableModel		= NULL;
	Analysis						*_currentAnalysis		= NULL;
	labelFilterGenerator			*_labelFilterGenerator	= NULL;
	ColumnsModel					*_columnsModel			= NULL;
	ComputedColumnsModel			*_computedColumnsModel	= NULL;
	FilterModel						*_filterModel			= NULL;	
	OnlineDataManager				*_odm					= NULL;
	DynamicModules					*_dynamicModules		= NULL;
	QWidget							*_buttonPanel			= NULL;
	QVBoxLayout						*_buttonPanelLayout		= NULL;
	QPushButton						*_okButton				= NULL,
									*_runButton				= NULL;
	CustomWebEnginePage				*_customPage			= NULL;
	RibbonModel						*_ribbonModel			= NULL;
	RibbonButtonModel				*_ribbonButtonModel		= NULL;
	QObject							*qmlProgressBar			= NULL;
	QApplication					*_application 			= nullptr;

	QSettings						_settings;
	analysisFormMap					_analysisFormsMap;
	TableModelVariablesAvailable	_availableVariablesModel;

	int								_scrollbarWidth = 0,
									_tableViewWidthBeforeOptionsMadeVisible;


	QString							_lastRequestedHelpPage,
									_openOnLoadFilename,
									_fatalError,
									_currentFilePath;

	AsyncLoader						_loader;
	AsyncLoaderThread				_loaderThread;
	

	bool							_inited,
									_applicationExiting		= false,
									_resultsViewLoaded		= false,
									_openedUsingArgs		= false,
									_excludeKey				= false;

};

#endif // MAINWIDGET_H
