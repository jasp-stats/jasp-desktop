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

#include <QSettings>
#include <QGuiApplication>
#include <QQmlApplicationEngine>

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
#include "data/columnsmodel.h"
#include "utilities/jsonutilities.h"
#include "data/computedcolumnsmodel.h"

#include "modules/dynamicmodule.h"
#include "modules/ribbonmodel.h"
#include "modules/ribbonbuttonmodel.h"
#include "modules/ribbonentry.h"
#include "data/filtermodel.h"
#include "widgets/backstage/filemenu.h"

class ResultsJsInterface;
class FileMenu;

namespace Ui {
class MainWindow;
}

class MainWindow : public QObject
{
	Q_OBJECT
	Q_PROPERTY(QString	runButtonText		READ runButtonText			WRITE setRunButtonText			NOTIFY runButtonTextChanged			)
	Q_PROPERTY(bool		runButtonEnabled	READ runButtonEnabled		WRITE setRunButtonEnabled		NOTIFY runButtonEnabledChanged		)
	Q_PROPERTY(bool		progressBarVisible	READ progressBarVisible		WRITE setProgressBarVisible		NOTIFY progressBarVisibleChanged	)
	Q_PROPERTY(int		progressBarProgress	READ progressBarProgress	WRITE setProgressBarProgress	NOTIFY progressBarProgressChanged	)
	Q_PROPERTY(QString	progressBarStatus	READ progressBarStatus		WRITE setProgressBarStatus		NOTIFY progressBarStatusChanged		)

	friend class ResultsJsInterface;
	friend class FileMenu;

public:
	explicit MainWindow(QGuiApplication *application);
	void open(QString filepath);
	void testLoadedJaspFile(int timeOut, bool save);

	~MainWindow() override;

	EngineSync* _engineSync;

	Q_INVOKABLE void showHelpFromQML(QString pageName);

	QString runButtonText() const
	{
		return m_runButtonText;
	}

	bool runButtonEnabled() const
	{
		return m_runButtonEnabled;
	}

	bool progressBarVisible() const
	{
		return m_progressBarVisible;
	}

	int progressBarProgress() const
	{
		return m_progressBarProgress;
	}

	QString progressBarStatus() const
	{
		return m_progressBarStatus;
	}

public slots:
	void setPPIHandler(int ppi, bool refreshAllAnalyses = true);
	void setImageBackgroundHandler(QString value);
	void setUIScaleHandler(float scale);

/*protected:
	void resizeEvent(QResizeEvent *event)		override;
	void dragEnterEvent(QDragEnterEvent *event) override;
	void dropEvent(QDropEvent *event)			override;
	void closeEvent(QCloseEvent *event)			override;*/

	void setRunButtonText(QString runButtonText)
	{
		if (m_runButtonText == runButtonText)
			return;

		m_runButtonText = runButtonText;
		emit runButtonTextChanged(m_runButtonText);
	}

	void setRunButtonEnabled(bool runButtonEnabled)
	{
		if (m_runButtonEnabled == runButtonEnabled)
			return;

		m_runButtonEnabled = runButtonEnabled;
		emit runButtonEnabledChanged(m_runButtonEnabled);
	}

	void setProgressBarVisible(bool progressBarVisible)
	{
		if (m_progressBarVisible == progressBarVisible)
			return;

		m_progressBarVisible = progressBarVisible;
		emit progressBarVisibleChanged(m_progressBarVisible);
	}

	void setProgressBarProgress(int progressBarProgress)
	{
		if (m_progressBarProgress == progressBarProgress)
			return;

		m_progressBarProgress = progressBarProgress;
		emit progressBarProgressChanged(m_progressBarProgress);
	}

	void setProgressBarStatus(QString progressBarStatus)
	{
		if (m_progressBarStatus == progressBarStatus)
			return;

		m_progressBarStatus = progressBarStatus;
		emit progressBarStatusChanged(m_progressBarStatus);
	}

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
	void openFile(QString filepath); //I am not yet connected to anything!
	void saveFile();//I am not yet connected to anything!
	void titleChanged(QString newTitle);//I am not yet connected to anything!

	void runButtonTextChanged(QString runButtonText);

	void runButtonEnabledChanged(bool runButtonEnabled);

	void progressBarVisibleChanged(bool progressBarVisible);

	void progressBarProgressChanged(int progressBarProgress);

	void progressBarStatusChanged(QString progressBarStatus);

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

	void dataSetIORequestHandler(FileEvent *event);
	void dataSetIOCompleted(FileEvent *event);
	void populateUIfromDataSet();
	void ribbonEntrySelected(const QString &item);

	void startDataEditorHandler();
	void startDataEditorEventCompleted(FileEvent *event);

	//void analysisOKed();
	void analysisRunned();

	void updateMenuEnabledDisabledStatus();

	void saveKeysSelected();
	void openKeysSelected();
	void syncKeysSelected();
	void refreshKeysSelected();
	void zoomInKeysSelected();
	void zoomOutKeysSelected();
	void zoomEqualKeysSelected();

	//void illegalOptionStateChanged(AnalysisForm * form);
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
	//void handleRibbonButtonClicked(QVariant);

private:
	void _analysisSaveImageHandler(Analysis* analysis, QString options);

private:
	typedef std::map<Analysis*, AnalysisForm*> analysisFormMap;

	QQmlApplicationEngine			*_qml					= nullptr;
	Analyses						*_analyses				= nullptr;
	ResultsJsInterface				*_resultsJsInterface	= nullptr;
	DataSetPackage					*_package				= nullptr;
	DataSetTableModel				*_tableModel			= nullptr;
	LevelsTableModel				*_levelsTableModel		= nullptr;
	Analysis						*_currentAnalysis		= nullptr;
	labelFilterGenerator			*_labelFilterGenerator	= nullptr;
	ColumnsModel					*_columnsModel			= nullptr;
	ComputedColumnsModel			*_computedColumnsModel	= nullptr;
	FilterModel						*_filterModel			= nullptr;	
	OnlineDataManager				*_odm					= nullptr;
	DynamicModules					*_dynamicModules		= nullptr;
//	CustomWebEnginePage				*_customPage			= nullptr;
	RibbonModel						*_ribbonModel			= nullptr;
	RibbonButtonModel				*_ribbonButtonModel		= nullptr;
	QObject							*qmlProgressBar			= nullptr;
	QGuiApplication					*_application 			= nullptr;

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
	FileMenu						*_fileMenu;


	QString m_runButtonText;
	bool m_runButtonEnabled;
	bool m_progressBarVisible;
	int m_progressBarProgress;
	QString m_progressBarStatus;
};

#endif // MAINWIDGET_H
