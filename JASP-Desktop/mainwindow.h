//
// Copyright (C) 2013-2017 University of Amsterdam
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
#include "enginesync.h"
#include "analyses.h"
#include "widgets/progresswidget.h"

#include "analysisforms/analysisform.h"
#include "asyncloader.h"
#include "activitylog.h"
#include "fileevent.h"

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
	Q_OBJECT

public:
	explicit MainWindow(QWidget *parent = 0);
	void open(QString filepath);
	~MainWindow();

protected:
	virtual void resizeEvent(QResizeEvent *event) OVERRIDE;
	virtual void dragEnterEvent(QDragEnterEvent *event) OVERRIDE;
	virtual void dropEvent(QDropEvent *event) OVERRIDE;
	virtual void closeEvent(QCloseEvent *event) OVERRIDE;

private:
	Ui::MainWindow *ui;

	AnalysisForm *_currentOptionsWidget;
	QMenu* _analysisMenu;
	DataSetPackage *_package;
	DataSetTableModel *_tableModel;
	Analysis *_currentAnalysis;

	int _scrollbarWidth = 0;

	double _webViewZoom;

	OnlineDataManager *_odm;

	Analyses *_analyses;
	EngineSync* _engineSync;

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
	ProgressWidget *_progressIndicator;

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

	std::map<std::string, AnalysisForm *> _analysisForms;

	int _tableViewWidthBeforeOptionsMadeVisible;

	bool _resultsViewLoaded = false;
	bool _openedUsingArgs = false;
	QString _openOnLoadFilename;
	QSettings _settings;
	ActivityLog *_log;
	QString _fatalError;
	QString _currentFilePath;

	QString escapeJavascriptString(const QString &str);
	void getAnalysesUserData();
	Json::Value getResultsMeta();

	void startDataEditor(QString path);

signals:
	void analysisSelected(int id);
	void analysisUnselected();
	void analysisSaveImage(int id, QString options);
	void analysisChangedDownstream(int id, QString options);
	void saveTextToFile(QString filename, QString text);
	void pushToClipboard(QString mimeType, QString data, QString html);
	void pushImageToClipboard(QByteArray base64, QString html);
	void saveTempImage(int id, QString path, QByteArray data);
	void displayMessageFromResults(QString path);
	void showAnalysesMenu(QString options);
	void removeAnalysisRequest(int id);
	void updateUserData(int id, QString key);
	void updateAnalysesUserData(QString userData);
	void simulatedMouseClick(int x, int y, int count);
	void resultsDocumentChanged();

private slots:
	void analysisResultsChangedHandler(Analysis* analysis);
	void analysisImageSavedHandler(Analysis* analysis);
	void analysisUserDataLoadedHandler(Analysis *analysis);
	void analysisSelectedHandler(int id);
	void analysisUnselectedHandler();
	void pushImageToClipboardHandler(const QByteArray &base64, const QString &html);
	void saveTextToFileHandler(const QString &filename, const QString &data);
	void pushToClipboardHandler(const QString &mimeType, const QString &data, const QString &html);
	void saveTempImageHandler(int id, QString path, QByteArray data);
	void displayMessageFromResultsHandler(QString msg);
	void analysisChangedDownstreamHandler(int id, QString options);
	void analysisSaveImageHandler(int id, QString options);


	void resultsDocumentChangedHandler();
	void simulatedMouseClickHandler(int x, int y, int count);
	void updateUserDataHandler(int id, QString key);
	void removeAnalysisRequestHandler(int id);
	void removeAllAnalyses();
	void refreshAllAnalyses();
	void refreshAnalysesUsingColumn(QString col);
	void resetTableView();
	void showAnalysesMenuHandler(QString options);
	void removeSelected();
	void collapseSelected();
	void editTitleSelected();
	void copySelected();
	void citeSelected();
	void saveImage();
	void noteSelected();
	void menuHidding();

	void tabChanged(int index);
	void helpToggled(bool on);
	void dataSetIORequest(FileEvent *event);
	void dataSetIOCompleted(FileEvent *event);
	void populateUIfromDataSet();
	void itemSelected(const QString &item);
	void exportSelected(const QString &filename);

	void adjustOptionsPanelWidth();
	void splitterMovedHandler(int, int);

	void hideOptionsPanel();
	void showOptionsPanel();
	void showDataPanel();
	void hideDataPanel();
	void showVariablesPage();
	void startDataEditorHandler();
	void startDataEditorEventCompleted(FileEvent *event);

	void analysisOKed();
	void analysisRunned();

	void updateMenuEnabledDisabledStatus();
	void updateUIFromOptions();

	void resultsPageLoaded(bool success);
	void scrollValueChangedHandle();

	void saveKeysSelected();
	void openKeysSelected();
	void syncKeysSelected();
	void refreshKeysSelected();

	void illegalOptionStateChanged();
	void fatalError();

	void helpFirstLoaded(bool ok);
	void requestHelpPage(const QString &pageName);

};

#endif // MAINWIDGET_H
