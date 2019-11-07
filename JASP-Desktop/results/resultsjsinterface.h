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
#include <QQmlWebChannel>
#include <QAuthenticator>
#include <QNetworkReply>

#include "utilities/jsonutilities.h"
#include "analysis/analysis.h"


class ResultsJsInterface : public QObject
{
	Q_OBJECT
	Q_PROPERTY(QString			resultsPageUrl	READ resultsPageUrl	WRITE setResultsPageUrl	NOTIFY resultsPageUrlChanged	)
	Q_PROPERTY(double			zoom			READ zoom			WRITE setZoom			NOTIFY zoomChanged				)

public:
	explicit ResultsJsInterface(QObject *parent = 0);

	void changeTitle(Analysis *analyses);
	void showAnalysis(int id);
	void analysisChanged(Analysis *analysis);
	void setResultsMeta(QString str);
	void unselect();
	void showInstruction();
	void exportPreviewHTML();
	void exportHTML();
	void resetResults();

	Json::Value &getResultsMeta();
	QVariant	&getAllUserData();

	QString			resultsPageUrl()	const { return _resultsPageUrl;	}
	double			zoom()				const { return _webEngineZoom;	}

	Q_INVOKABLE void purgeClipboard();
	Q_INVOKABLE void analysisEditImage(int id, QString options);

	//Callable from javascript through resultsJsInterfaceInterface...

signals:
	Q_INVOKABLE void openFileTab();
	Q_INVOKABLE void saveTextToFile(const QString &filename, const QString &data);
	Q_INVOKABLE void analysisUnselected();
	Q_INVOKABLE void analysisChangedDownstream(int id, QString options);
	Q_INVOKABLE void analysisSaveImage(int id, QString options);
				void analysisResizeImage(int id, QString options);
				void showPlotEditor(int id, QString options);
	Q_INVOKABLE void analysisSelected(int id);
	Q_INVOKABLE void analysisTitleChangedInResults(int id, QString title);
	Q_INVOKABLE void removeAnalysisRequest(int id);
	Q_INVOKABLE void packageModified();
	Q_INVOKABLE void refreshAllAnalyses();
	Q_INVOKABLE void removeAllAnalyses();
	Q_INVOKABLE void duplicateAnalysis(int id);
	Q_INVOKABLE void showDependenciesInAnalysis(int analysis_id, QString optionName);

public slots:
	void setZoom(double zoom);
	void resultsDocumentChanged()				{ emit packageModified(); }
	void updateUserData()						{ emit packageModified(); }
	void saveTempImage(int id, QString path, QByteArray data);
	void pushImageToClipboard(const QByteArray &base64, const QString &html);
	void pushToClipboard(const QString &mimeType, const QString &data, const QString &html);
	void displayMessageFromResults(QString path);
	void getImageInBase64(int id, const QString &path);
	void setAllUserDataFromJavascript(QString json);
	void setResultsMetaFromJavascript(QString json);
	void removeAnalysis(Analysis *analysis);
	void removeAnalyses();
	void moveAnalyses(size_t fromId, size_t toId);
	void setThemeCss(QString themeName);

//end callables


signals:
	void getResultsMetaCompleted();
	void getAllUserDataCompleted();
	void resultsPageUrlChanged(QUrl resultsPageUrl);
	void runJavaScript(QString js);
	void zoomChanged();
	void resultsPageLoadedSignal();

public slots:
	void setExactPValuesHandler(bool exact);
	void setFixDecimalsHandler(QString numDecimals);
	void analysisImageEditedHandler(Analysis *analysis);
	void cancelImageEdit(int id);
	void exportSelected(const QString &filename);
	void setResultsPageUrl(QString resultsPageUrl);
	void resultsPageLoaded(bool success);
	void setZoomInWebEngine();

private:
	void	setGlobalJsValues();
	QString escapeJavascriptString(const QString &str);

private slots:
	void menuHidding();

private:
	double			_webEngineZoom = 1.0;
	Json::Value		_resultsMeta;
	QVariant		_allUserData;
	QString			_resultsPageUrl = "qrc:///core/index.html";
};


#endif // RESULTSJSINTERFACE_H
