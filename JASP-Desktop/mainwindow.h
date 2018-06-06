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

#include "dataset.h"

#include "datasettablemodel.h"
#include "variablespage/levelstablemodel.h"
#include "variablespage/labelfiltergenerator.h"
#include "enginesync.h"
#include "analyses.h"

#include "analysisforms/analysisform.h"
#include "asyncloader.h"
#include "asyncloaderthread.h"
#include "activitylog.h"
#include "fileevent.h"
#include "resultsjsinterface.h"
#include "customwebenginepage.h"
#include "columnsmodel.h"
#include "jsonutilities.h"

class ResultsJsInterface;

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
	Q_OBJECT

	friend class ResultsJsInterface;
public:
	explicit MainWindow(QWidget *parent = 0);
	void open(QString filepath);
	virtual ~MainWindow();

	EngineSync* _engineSync;

	Q_INVOKABLE void setFilterConstructorJSON(QString jsonString = DEFAULT_FILTER_JSON);
	Q_INVOKABLE void showHelpFromQML(QString pageName);


protected:
	virtual void resizeEvent(QResizeEvent *event) OVERRIDE;
	virtual void dragEnterEvent(QDragEnterEvent *event) OVERRIDE;
	virtual void dropEvent(QDropEvent *event) OVERRIDE;
	virtual void closeEvent(QCloseEvent *event) OVERRIDE;

private:
	Ui::MainWindow *ui;

	ResultsJsInterface		*_resultsJsInterface;
	AnalysisForm			*_currentOptionsWidget;
	DataSetPackage			*_package;
	DataSetTableModel		*_tableModel;
	LevelsTableModel		*_levelsTableModel;
	Analysis				*_currentAnalysis;
	labelFilterGenerator	*_labelFilterGenerator;
	ColumnsModel			*_columnsModel = NULL;
	
	std::map<Analysis*, AnalysisForm*> _analysisFormsMap;

	TableModelVariablesAvailable _availableVariablesModel;

	int _scrollbarWidth = 0;

	OnlineDataManager *_odm;

	QString _lastRequestedHelpPage;

	Analyses *_analyses;


	void refreshAnalysesUsingColumns(std::vector<std::string> &changedColumns
									, std::vector<std::string> &missingColumns
									, std::map<std::string, std::string> &changeNameColumns);


	void packageChanged(DataSetPackage *package);
	void packageDataChanged(DataSetPackage *package
							, std::vector<std::string> &changedColumns
							, std::vector<std::string> &missingColumns
							, std::map<std::string, std::string> &changeNameColumns
							);


	bool closeRequestCheck(bool &isSaving);

	AsyncLoader _loader;
	AsyncLoaderThread _loaderThread;
	QObject *qmlProgressBar = NULL, *qmlFilterWindow = NULL, *qmlStatusBar = NULL;

	bool _inited;
	bool _applicationExiting = false;

	AnalysisForm* loadForm(Analysis *analysis);
	AnalysisForm* loadForm(const std::string name);
	void showForm(Analysis *analysis);
	void closeCurrentOptionsWidget();
	void removeAnalysis(Analysis *analysis);

	QWidget *_buttonPanel;
	QVBoxLayout *_buttonPanelLayout;
	QPushButton *_okButton;
	QPushButton *_runButton;

	int _tableViewWidthBeforeOptionsMadeVisible;

	bool _resultsViewLoaded = false;
	bool _openedUsingArgs = false;
	QString _openOnLoadFilename;
	QSettings _settings;
	ActivityLog *_log;
	QString _fatalError;
	QString _currentFilePath;

	CustomWebEnginePage* _customPage;
	
	bool _excludeKey = false;

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
	
	bool filterShortCut();
	void loadQML();

signals:
	void updateAnalysesUserData(QString userData);

private slots:
	void analysisResultsChangedHandler(Analysis* analysis);
	void analysisImageSavedHandler(Analysis* analysis);

	void removeAllAnalyses();
	void refreshAllAnalyses();
	void refreshAnalysesUsingColumn(QString col);

	void tabChanged(int index);
	void helpToggled(bool on);

	void dataSetIORequest(FileEvent *event);
	void dataSetIOCompleted(FileEvent *event);
	void populateUIfromDataSet();
	void itemSelected(const QString &item);

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

	void illegalOptionStateChanged();
	void fatalError();

	void helpFirstLoaded(bool ok);
	void requestHelpPage(const QString &pageName);

    void emptyValuesChangedHandler();

	void resizeVariablesWindowLabelColumn();
	void closeVariablesPage();

	void showProgress();
	void hideProgress();
	void setProgressStatus(QString status, int progress);

	void setGeneratedFilter(QString generatedFilter);
	void setGeneratedFilterAndSend(QString generatedFilter);
	void setFilterErrorText(QString error);
	void applyAndSendFilter(QString filter);
	void setStatusBarText(QString text);
	void onFilterUpdated();

	void updateExcludeKey();

};

#endif // MAINWIDGET_H
