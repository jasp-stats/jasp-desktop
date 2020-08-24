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
	Q_PROPERTY(bool				resultsLoaded	READ resultsLoaded	WRITE setResultsLoaded	NOTIFY resultsLoadedChanged		)
	Q_PROPERTY(bool				scrollAtAll		READ scrollAtAll	WRITE setScrollAtAll	NOTIFY scrollAtAllChanged		)
public:
	explicit ResultsJsInterface(QObject *parent = 0);

	void setStatus(			Analysis *	analysis);
	void changeTitle(		Analysis *	analysis);
	void analysisChanged(	Analysis *	analysis);
	void overwriteUserdata(	Analysis *	analysis);
	void showAnalysis(		int			id);
	void setResultsMeta(	QString		str);
	void unselect();
	void showInstruction();
	void exportPreviewHTML();
	void exportHTML();
	void resetResults();

	QString			resultsPageUrl()	const { return _resultsPageUrl;	}
	double			zoom()				const { return _webEngineZoom;	}
	bool			resultsLoaded()		const { return _resultsLoaded;	}
	bool			scrollAtAll()		const {	return _scrollAtAll;	}

	Q_INVOKABLE void purgeClipboard();
	Q_INVOKABLE void analysisEditImage(int id, QString options);

	//Callable from javascript through resultsJsInterfaceInterface...
signals:
	Q_INVOKABLE void openFileTab();
	Q_INVOKABLE void saveTextToFile(const QString &filename, const QString &data);
	Q_INVOKABLE void analysisUnselected();
	Q_INVOKABLE void analysisChangedDownstream(		int id, QString options);
	Q_INVOKABLE void analysisSaveImage(				int id, QString options);
				void analysisResizeImage(			int id, QString options);
				void showPlotEditor(				int id, QString options);
	Q_INVOKABLE void analysisSelected(				int id);
	Q_INVOKABLE void analysisTitleChangedInResults(	int id, QString title);
	Q_INVOKABLE void removeAnalysisRequest(			int id);
	Q_INVOKABLE void duplicateAnalysis(				int id);
	Q_INVOKABLE void showDependenciesInAnalysis(	int id, QString optionName);
	Q_INVOKABLE void packageModified();
	Q_INVOKABLE void refreshAllAnalyses();
	Q_INVOKABLE void removeAllAnalyses();

public slots:
	void resultsDocumentChanged()		{ emit packageModified(); }
	void setZoom(						double			zoom);
	void saveTempImage(					int				id,			QString path,			QByteArray data);
	void getImageInBase64(				int				id,			const QString & path);
	void pushImageToClipboard(	const	QByteArray	&	base64,		const QString & html);
	void pushToClipboard(		const	QString		&	mimeType,	const QString & data,	const QString &html);
	void displayMessageFromResults(		QString			path);
	void setAllUserDataFromJavascript(	QString			json);
	void setResultsMetaFromJavascript(	QString			json);
	void removeAnalysis(				Analysis	*	analysis);
	void removeAnalyses();
	void moveAnalyses(					quint64 fromId,				quint64 toId);
	void setThemeCss(					QString themeName);
	void setFontFamily();

//end callables


signals:
	void resultsMetaChanged(	QString resultsMeta);
	void allUserDataChanged(	QString userData);
	void resultsPageUrlChanged(	QUrl	resultsPageUrl);
	void runJavaScript(			QString js);
	void zoomChanged();
	void resultsPageLoadedSignal();

	void resultsLoadedChanged(bool resultsLoaded);

	void scrollAtAllChanged(bool scrollAtAll);

public slots:
	void setExactPValuesHandler(	bool			exact);
	void setFixDecimalsHandler(		QString			numDecimals);
	void analysisImageEditedHandler(Analysis	*	analysis);
	void cancelImageEdit(			int				id);
	void exportSelected(	const	QString		&	filename);
	void setResultsPageUrl(			QString			resultsPageUrl);
	void setZoomInWebEngine();
	void setResultsLoaded(			bool			resultsLoaded);
	void setScrollAtAll(			bool			scrollAtAll);

private:
	void	setGlobalJsValues();
	QString escapeJavascriptString(const QString &str);

private slots:
	void menuHidding();

private:
	double			_webEngineZoom	= 1.0;
	QString			_resultsPageUrl = "qrc:///html/index.html";
	bool			_resultsLoaded	= false,
					_scrollAtAll	= true;
};


#endif // RESULTSJSINTERFACE_H
