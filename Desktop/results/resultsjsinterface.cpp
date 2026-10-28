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

#include "resultsjsinterface.h"

#include <QClipboard>
#include <QDir>
#include <QFileInfo>

#ifdef _WIN32
#include <QPainter>
#endif

#include "qutils.h"
#include "gui/aboutmodel.h"
#include "tempfiles.h"
#include "utilities/appdirs.h"
#include "data/datasetpackage.h"
#include "utilities/settings.h"
#include <QMimeData>
#include <QAction>
#include "utilities/messageforwarder.h"
#include <QApplication>
#include "gui/preferencesmodel.h"
#include <QThread>
#include <QFileDialog>
#include "log.h"

ResultsJsInterface * ResultsJsInterface::_singleton = nullptr;

ResultsJsInterface::ResultsJsInterface(QObject *parent) : QObject(parent)
{
	_singleton = this;

	// connect(this, &ResultsJsInterface::zoomChanged,					this, &ResultsJsInterface::setZoomInWebEngine);
	connect(this, &ResultsJsInterface::runJavaScriptSignalQueued,	this, &ResultsJsInterface::runJavaScriptSignal, Qt::QueuedConnection);
	

	setZoom(Settings::value(Settings::UI_SCALE).toDouble());
}

void ResultsJsInterface::uiScaleChangedHandler()
{
	setZoom(PreferencesModel::prefs()->uiScale());
}

void ResultsJsInterface::setZoom(double zoom)
{
	if(zoom == _webEngineZoom)
		return;

	_webEngineZoom = zoom;
	emit zoomChanged();
}

// void ResultsJsInterface::setZoomInWebEngine()
// {
// 	runJavaScript("window.setZoom(" + QString::number(_webEngineZoom) + ")");
// }

void ResultsJsInterface::setResultsLoaded(bool resultsLoaded)
{
	if (_resultsLoaded == resultsLoaded)
		return;

	_resultsLoaded = resultsLoaded;
	emit resultsLoadedChanged(_resultsLoaded);

	if (resultsLoaded)
	{
		QString version = AboutModel::version();

		runJavaScript("window.setAppVersion('" + version + "')");
#ifdef INTERACTIVE_PLOTS
		runJavaScript("window.setUseInteractivePlots(true)");
#endif

		setGlobalJsValues();
		setFontFamily();

		emit resultsPageLoadedSignal();
		emit zoomChanged();

		setThemeCss(Settings::value(Settings::THEME_NAME).toString());
		
		dequeueJsQueue();
	}
}

void ResultsJsInterface::setScrollAtAll(bool scrollAtAll)
{
	if (_scrollAtAll == scrollAtAll)
		return;

	_scrollAtAll = scrollAtAll;
	emit scrollAtAllChanged(_scrollAtAll);
}

void ResultsJsInterface::purgeClipboard()
{
	AppDirs::purgeClipboard();
}

void ResultsJsInterface::setExactPValuesHandler(bool exact)
{
	runJavaScript("window.globSet.pExact = " + QString(exact ? "true" : "false") + "; window.reRenderAnalyses();");
}

void ResultsJsInterface::setNormalizedNotationHandler(bool normalized)
{
    runJavaScript("window.globSet.normalizedNotation = " + QString(normalized ? "true" : "false") + "; window.reRenderAnalyses();");
}

void ResultsJsInterface::setFixDecimalsHandler(QString numDecimals)
{
	runJavaScript("window.globSet.decimals = " + numDecimals + "; window.reRenderAnalyses();");
}

void ResultsJsInterface::setShowInteractiveDefaultHandler(bool showIt)
{
	runJavaScript("window.globSet.showInteractiveDefault = " +  QString(showIt ? "true" : "false")  + "; window.reRenderAnalyses();");
}

void ResultsJsInterface::setGlobalJsValues()
{
	QString exactPValueString				= PreferencesModel::prefs()->exactPValues()				? "true" : "false",
			normalizedNotationString		= PreferencesModel::prefs()->normalizedNotation()		? "true" : "false",
			showInteractiveDefaultString	= PreferencesModel::prefs()->showInteractiveDefault()	? "true" : "false",
			tempFolder						= "file://" + tq(TempFiles::sessionDirName());

	QString js =	"  window.globSet.pExact = "					+ exactPValueString;
	js +=			"; window.globSet.normalizedNotation = "		+ normalizedNotationString;
	js +=			"; window.globSet.decimals = "					+ PreferencesModel::prefs()->fixedDecimalsForJS();
	js +=			"; window.globSet.tempFolder = '"				+ tempFolder + "/'";
	js +=			"; window.globSet.showInteractiveDefault = "	+ showInteractiveDefaultString;
	runJavaScript(js);
}

void ResultsJsInterface::saveTempImage(int id, QString path, QByteArray data)
{
	QByteArray byteArray = QByteArray::fromBase64(data);

	QString fullpath = AppDirs::clipboardDir() + path;
	QDir().mkpath(QFileInfo(fullpath).absolutePath());

	QFile file(fullpath);
	if(!file.open(QIODevice::WriteOnly))
		Log::log() << "Cannot open file in saveTempImage: " << file.fileName() << " with error: " << file.errorString() << std::endl;

	file.write(byteArray);
	file.close();

	QString eval = QString("window.imageSaved({ id: %1, fullPath: '%2'});").arg(id).arg(fullpath);
	runJavaScript(eval);
}

void ResultsJsInterface::analysisImageEditedHandler(Analysis *analysis)
{
	Json::Value imgJson = analysis->imgResults();

	QString	results = tq(imgJson.toStyledString());
	results = escapeJavascriptString(results);
	results = "window.refreshEditedImage(" + QString::number(analysis->id()) + ", JSON.parse('" + results + "'));";
	runJavaScript(results);

	return;
}

void ResultsJsInterface::cancelImageEdit(int id, const QString &name)
{
	runJavaScript("window.cancelImageEdit(" + QString::number(id) + ", '" + escapeJavascriptString(name) + "');");
}

void ResultsJsInterface::menuHiding()
{
	runJavaScript("window.analysisMenuHidden();");
}

void ResultsJsInterface::getImageInBase64(int id, const QString &path)
{
	QString fullPath = tq(TempFiles::sessionDirName()) + "/" + path;
	QFile *file = new QFile(fullPath);
	file->open(QIODevice::ReadOnly);
	QByteArray image = file->readAll();
	QString result = QString(image.toBase64());

	QString eval = QString("window.convertToBase64Done({ id: %1, result: '%2'});").arg(id).arg(result);
	runJavaScript(eval);

}

void ResultsJsInterface::pushToClipboard(const QString &mimeType, const QString &data, const QString &html)
{
	QMimeData *mimeData = new QMimeData();

	if (mimeType == "text/plain")
		mimeData->setText(data);

	if ( ! html.isEmpty())
	{
		mimeData->setHtml(html);

		if (mimeType != "text/plain")
		{
			mimeData->setText(html);
		}
	}

	QClipboard *clipboard = QApplication::clipboard();
	clipboard->setMimeData(mimeData, QClipboard::Clipboard);

}

void ResultsJsInterface::pushImageToClipboard(const QByteArray &base64, const QString &html)
{
	QMimeData *mimeData = new QMimeData();

	QByteArray byteArray = QByteArray::fromBase64(base64);

	QImage pm;
	if(pm.loadFromData(byteArray))
	{
#ifdef _WIN32 //needed because jpegs/clipboard doesn't support transparency in windows
		QImage image2(pm.size(), QImage::Format_ARGB32);
		image2.fill(Qt::white);
		QPainter painter(&image2);
		painter.drawImage(0, 0, pm);

		mimeData->setImageData(image2);
#else
		mimeData->setImageData(pm);
#endif

	}

	if ( ! html.isEmpty())
		mimeData->setHtml(html);

	if (mimeData->hasImage() || mimeData->hasHtml())
	{
		QClipboard *clipboard = QApplication::clipboard();
		clipboard->setMimeData(mimeData, QClipboard::Clipboard);
	}
}


void ResultsJsInterface::displayMessageFromResults(QString msg)
{
	MessageForwarder::showWarning(tr("Results Warning"), msg);
}

void ResultsJsInterface::setStatus(Analysis *analysis)
{
	int id = analysis->id();
	QString status = analysis->statusQ();

	runJavaScript("window.setStatus(" + QString::number(id) + ", '" + status + "')");
}

void ResultsJsInterface::changeTitle(Analysis *analysis)
{
    int id = analysis->id();
	QString title = tq(analysis->title());
	
	Log::log() << " void ResultsJsInterface::changeTitle(Analysis *analysis)" << std::endl;

    runJavaScript("window.changeTitle(" + QString::number(id) + ", '" + escapeJavascriptString(title) + "')");
}

void ResultsJsInterface::changeDataSpec(Analysis *analysis)
{
	int		id			= analysis->id();
	QString	dataSpec	= analysis->dataSpec();

	runJavaScript("window.changeDataSpec(" + QString::number(id) + ", '" + escapeJavascriptString(dataSpec) + "')");
}

void ResultsJsInterface::overwriteUserdata(Analysis *analysis)
{
	size_t id = analysis->id();
	QString userData = tq(analysis->userData().toStyledString());

	runJavaScript("window.overwriteUserdata(" + QString::number(id) + ", JSON.parse('" + escapeJavascriptString(userData) + "'))");
}

void ResultsJsInterface::showAnalysis(int id)
{
	runJavaScript("window.select(" + QString::number(id) + ", true)");
}

void ResultsJsInterface::exportSelected(const QString &filename)
{
	runJavaScript("window.exportHTML('" + filename + "');");
}

void ResultsJsInterface::exportAnalysisHTML(int analysisId)
{
	QString defaultName = QString("analysis_%1.html").arg(analysisId);
	QString filename = QFileDialog::getSaveFileName(
		nullptr, tr("Export Analysis"), defaultName,
		tr("HTML files (*.html)"));

	if (filename.isEmpty()) return;

	runJavaScript(QString("window.exportAnalysisHTML(%1, '%2');")
		.arg(analysisId)
		.arg(escapeJavascriptString(filename)));
}

void ResultsJsInterface::analysisChanged(Analysis *analysis)
{
	runJavaScript("window.analysisChanged(JSON.parse('" + escapeJavascriptString(tq(analysis->asJSON().toStyledString())) + "'));");
}

void ResultsJsInterface::setResultsMeta(const QString & str)
{
	runJavaScript("window.setResultsMeta(JSON.parse('" + escapeJavascriptString(str) + "'));");
}

void ResultsJsInterface::resetResults()
{
	emit resultsPageUrlChanged(_resultsPageUrl);
}

void ResultsJsInterface::setRSyntax(int id, const QString &syntax)
{
	runJavaScript("window.setRSyntaxText(" + QString::number(id) + ", '" + escapeJavascriptString(syntax) + "')");
}

void ResultsJsInterface::unselect()
{
	runJavaScript("window.unselect()");
}

void ResultsJsInterface::removeAnalysis(Analysis *analysis)
{
	runJavaScript("window.removeAnalysisTrigger(" + QString::number(analysis->id()) + ")");
}

void ResultsJsInterface::removeAnalyses()
{
	runJavaScript("window.removeAllAnalyses()");
}

void ResultsJsInterface::moveAnalyses(quint64 fromId, quint64 toId)
{
	runJavaScript("window.moveAnalyses(" + QString::number(fromId) + "," + QString::number(toId) + ")");
}

void ResultsJsInterface::showInstruction()
{
	runJavaScript("window.showInstructions()");
}

void ResultsJsInterface::exportPreviewHTML()
{
	DataSetPackage::pkg()->setWaitingForReady();
	runJavaScript("window.exportHTML('%PREVIEW%');");
}

void ResultsJsInterface::exportHTML()
{
	DataSetPackage::pkg()->setWaitingForReady();
	runJavaScript("window.exportHTML('%EXPORT%');");
}

QString ResultsJsInterface::escapeJavascriptString(const QString &str)
{
	QString out;
	static QRegularExpression rx("(\\r|\\n|\\\\|\"|\')");
	int pos = 0, lastPos = 0;

	for (const QRegularExpressionMatch &match : rx.globalMatch(str))
	{
		QString matched = match.captured(1);
		pos = match.capturedStart(1);
		out += str.mid(lastPos, pos - lastPos);
		switch (matched.at(0).unicode())
		{
		case '\r':
			out += "\\r";
			break;
		case '\n':
			out += "\\n";
			break;
		case '"':
			out += "\\\"";
			break;
		case '\'':
			out += "\\'";
			break;
		case '\\':
			out += "\\\\";
			break;
		}
		pos++;
		lastPos = pos;
	}
	out += str.mid(lastPos);
	return out;
}


void ResultsJsInterface::setResultsMetaFromJavascript(QString json)
{
	emit resultsMetaChanged(json);
	emit packageModified();
}

void ResultsJsInterface::setAllUserDataFromJavascript(QString json)
{
	emit allUserDataChanged(json);
	emit packageModified();
}

void ResultsJsInterface::setResultsPageUrl(QString resultsPageUrl)
{
	if (_resultsPageUrl == resultsPageUrl)
		return;

	_resultsPageUrl = resultsPageUrl;
	emit resultsPageUrlChanged(_resultsPageUrl);
}

void ResultsJsInterface::analysisEditImage(int id, QString options)
{
	std::string opts = fq(options);
	Json::Value converted = Json::objectValue;

	Json::Reader().parse(opts, converted);

	std::string type = converted.get("type", "").asString();

	if		(type == "resize"		) emit analysisResizeImage(id, options);
	else if	(type == "interactive"	) emit showPlotEditor(id, options);
}

void ResultsJsInterface::setThemeCss(QString themeName)
{
	if(_resultsLoaded)
		runJavaScript("window.setTheme(\"" + themeName + "\");");
}

void ResultsJsInterface::setFontFamily()
{
	if (_resultsLoaded)
	{
		QString font = PreferencesModel::prefs()->resultFont(true);
		runJavaScript("window.setFontFamily(\"" + escapeJavascriptString(font) + "\");");
	}
}

void ResultsJsInterface::jsLog(QString msg)
{
	Log::log() << "JS: " << msg.toStdString() << std::endl;
}

void ResultsJsInterface::setLocale(QString localeId, bool thousandSeps)
{
	runJavaScript("window.setLocale('" + escapeJavascriptString(localeId) + "', " + (thousandSeps ? "true" : "false") + ");");
}

void ResultsJsInterface::runJavaScript(const QString & js)
{
	if(_resultsLoaded)	emit runJavaScriptSignal(js);
	else				_delayedJs.push(js);
}


void ResultsJsInterface::dequeueJsQueue()
{
	Log::log() << "ResultsJsInterface::dequeueJsQueue has " + std::to_string(_delayedJs.size()) + " waiting Js-scripts." << std::endl;
	
	while(_delayedJs.size() > 0)
	{
		Log::log() << "Running delayed script: '" <<_delayedJs.front() << "'." << std::endl;
		emit runJavaScriptSignalQueued(_delayedJs.front());
		_delayedJs.pop();
	}
}
