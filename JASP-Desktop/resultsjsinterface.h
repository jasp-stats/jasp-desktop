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

#ifndef RESULTSJSINTERFACE_H
#define RESULTSJSINTERFACE_H

#include <QObject>
#include <QString>
#include <QWebEngineView>
#include <QWebChannel>
#include <QMenu>
#include <QAuthenticator>
#include <QNetworkReply>

#include "mainwindow.h"

class MainWindow;

class ResultsJsInterface : public QObject
{
	Q_OBJECT


public:
	explicit ResultsJsInterface(QWidget *parent = 0);

	void setZoom(double zoom);
	void zoomIn();
	void zoomOut();
	void zoomReset();

	void showAnalysis(int id);
	void analysisChanged(Analysis *analysis);
	void setResultsMeta(QString str);
	void unselect();
	void removeAnalysis(Analysis *analysis);
	Json::Value &getResultsMeta();
	QVariant &getAllUserData();
	void showInstruction();
	void exportPreviewHTML();
	void exportHTML();

signals:
	void getResultsMetaCompleted();
	void getAllUserDataCompleted();

public slots:
	void setExactPValuesHandler(bool exact);
	void setFixDecimalsHandler(QString numDecimals);
	void analysisImageEditedHandler(Analysis *analysis);
	void showAnalysesMenu(QString options);
	void simulatedMouseClick(int x, int y, int count);
	void analysisUnselected();
	void analysisSelected(int id);
	void saveTempImage(int id, QString path, QByteArray data);
	void analysisChangedDownstream(int id, QString options);
	void resultsDocumentChanged();
	void updateUserData(int id, QString key);
	void saveTextToFile(const QString &filename, const QString &data);
	void analysisSaveImage(int id, QString options);
	void analysisEditImage(int id, QString options);
	void removeAnalysisRequest(int id);
	void pushImageToClipboard(const QByteArray &base64, const QString &html);
	void pushToClipboard(const QString &mimeType, const QString &data, const QString &html);
	void displayMessageFromResults(QString path);

	void exportSelected(const QString &filename);
	void getImageInBase64(int id, const QString &path);
	void openFileTab();
	

private:
	MainWindow *_mainWindow;
	QWebEngineView *_webViewResults;
	QWebChannel *_channel;

	QMenu *_analysisMenu;
	QMenu *_copySpecialMenu;
	double _webViewZoom;

	Json::Value _resultsMeta;
	QVariant _allUserData;

	void setGlobalJsValues();
	QString escapeJavascriptString(const QString &str);
	void runJavaScript(const QString &str);
	void runJavaScript(const QString &str, std::function<void(const QVariant&)> cb);

	void cbSetPPI(const QVariant &vppi);
	void cbSetResultstMeta(const QVariant &vMetaData);
	void cbSetAllUserData(const QVariant &vAllUserData);

private slots:
	void resultsPageLoaded(bool success);
	void scrollValueChangedHandle();
	void menuHidding();
	void removeSelected();
	void collapseSelected();
	void editTitleSelected();
	void copySelected();
	void citeSelected();
	void latexCodeSelected();
	void saveImage();
	void editImage();
	void noteSelected();
};


#endif // RESULTSJSINTERFACE_H
