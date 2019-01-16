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
#include <QStringBuilder>

#ifdef __WIN32__
#include <QPainter>
#endif

#include "utilities/qutils.h"
#include "appinfo.h"
#include "tempfiles.h"
#include <functional>
#include "timers.h"
#include "utilities/settings.h"
#include <QMimeData>
#include <QAction>
#include "gui/messageforwarder.h"
#include <QApplication>

ResultsJsInterface::ResultsJsInterface(QObject *parent) : QObject(parent)
{
	//setChannel(new QQmlWebChannel(this));
	//_analysisMenu = new QMenu(_mainWindow);
	std::cout << "connect(_analysisMenu, &QMenu::aboutToHide, this, &ResultsJsInterface::menuHidding); not being done anymore" << std::endl;
}


void ResultsJsInterface::setZoom(double zoom)
{
	if(zoom == _webViewZoom)
		return;

	_webViewZoom = zoom;
	emit zoomChanged(zoom);

	QString js = "window.setZoom(" + QString::number(zoom) + ")";
	emit runJavaScript(js);
}

void ResultsJsInterface::zoomIn()
{
	setZoom(_webViewZoom + 0.2);
}

void ResultsJsInterface::zoomOut()
{
	if (_webViewZoom >= 0.4)
		setZoom(_webViewZoom - 0.2);
}

void ResultsJsInterface::zoomReset()
{
	setZoom(1);
}


void ResultsJsInterface::resultsPageLoaded(bool succes)
{
	if (succes)
	{
		QString version = tq(AppInfo::version.asString());

#ifdef JASP_DEBUG
		version+="-Debug";
#endif

		emit runJavaScript("window.setAppVersion('" + version + "')");

		setGlobalJsValues();

		emit runJavaScript("window.getPPI()");
	}
}

void ResultsJsInterface::showAnalysesMenu(QString options)
{
	std::cout << "showAnalysesMenu must be done in QML" << std::endl;

	/*
	Json::Value menuOptions;

	Json::Reader parser;
	parser.parse(fq(options), menuOptions);

	QIcon _copyIcon = QIcon(":/icons/copy.png");
	QIcon _citeIcon = QIcon(":/icons/cite.png");
	QIcon _codeIcon = QIcon(":/icons/code-icon.png");
	QIcon _collapseIcon = QIcon(":/icons/collapse.png");
	QIcon _expandIcon = QIcon(":/icons/expand.png");
	QIcon _saveImageIcon = QIcon(":/icons/document-save-as.png");
	QIcon _editImageIcon = QIcon(":/icons/editImage.png");

	_analysisMenu->clear();

	QString objName = tq(menuOptions["objectName"].asString());

	if (menuOptions["hasCollapse"].asBool())
	{
		Json::Value collapseOptions = menuOptions["collapseOptions"];
		QIcon icon = collapseOptions["collapsed"].asBool() ? _expandIcon : _collapseIcon;
		_analysisMenu->addAction(icon, tq(collapseOptions["menuText"].asString()), this, SLOT(collapseSelected()));
		_analysisMenu->addSeparator();
	}

	if (menuOptions["hasEditTitle"].asBool())
	{
		_analysisMenu->addAction("Edit Title", this, SLOT(editTitleSelected()));
		_analysisMenu->addSeparator();
	}

	if (menuOptions["hasCopy"].asBool())
		_analysisMenu->addAction(_copyIcon, "Copy", this, SLOT(copySelected()));

	if (menuOptions["hasLaTeXCode"].asBool())  // TODO: || menuOptions["hasPlainText"].asBool())
	{
		_copySpecialMenu = _analysisMenu->addMenu(tr("&Copy special"));

		_copySpecialMenu->addAction(_codeIcon, "LaTeX code", this, SLOT(latexCodeSelected()));

		QAction *copyTextAction = new QAction("Copy text");
		// connect(copyTextAction, SIGNAL(triggered), this, SLOT(copyTextSelected));
		copyTextAction->setEnabled(false);
		_copySpecialMenu->addAction(copyTextAction);
	}

	if (menuOptions["hasCite"].asBool())
	{
		_analysisMenu->addSeparator();
		_analysisMenu->addAction(_citeIcon, "Copy Citations", this, SLOT(citeSelected()));
	}

	if (menuOptions["hasSaveImg"].asBool())
	{
		_analysisMenu->addAction(_saveImageIcon, "Save Image As", this, SLOT(saveImage()));
	}
#ifdef JASP_DEBUG
    if (menuOptions["hasEditImg"].asBool())
    {
        _analysisMenu->addAction(_editImageIcon, "Edit Image", this, SLOT(editImage()));
    }
#endif

	if (menuOptions["hasNotes"].asBool())
	{
		_analysisMenu->addSeparator();

		Json::Value noteOptions = menuOptions["noteOptions"];

		for (Json::ValueIterator iter = noteOptions.begin(); iter != noteOptions.end(); iter++)
		{
			Json::Value noteOption = *iter;
			QAction *a1 = _analysisMenu->addAction(tq(noteOption["menuText"].asString()), this, SLOT(noteSelected()));

			a1->setDisabled(noteOption["visible"].asBool());


			QString call = QString("window.notesMenuClicked('%1', %2);").arg(tq(noteOption["key"].asString())).arg(noteOption["visible"].asBool() ? "false" : "true");

			a1->setData(call);
		}
	}


	if (menuOptions["hasRemove"].asBool())
	{
		_analysisMenu->addSeparator();
		_analysisMenu->addAction("Remove " + objName, this, SLOT(removeSelected()));
	}

	if (menuOptions["hasRemoveAllAnalyses"].asBool())
	{
		_analysisMenu->addSeparator();
		_analysisMenu->addAction("Remove All ", _mainWindow, SLOT(removeAllAnalyses()));
	}

	if (menuOptions["hasRefreshAllAnalyses"].asBool())
	{
		_analysisMenu->addSeparator();
		_analysisMenu->addAction("Refresh All ", _mainWindow, SLOT(refreshAllAnalyses()));
	}

	QPoint point = _webViewResults->mapToGlobal(QPoint(round(menuOptions["rX"].asInt() * _webViewZoom), round(menuOptions["rY"].asInt() * _webViewZoom)));

	_analysisMenu->move(point);
	_analysisMenu->show();*/
}

void ResultsJsInterface::collapseSelected()
{
	emit runJavaScript("window.collapseMenuClicked();");
}

void ResultsJsInterface::removeSelected()
{
	emit runJavaScript("window.removeMenuClicked();");
}

void ResultsJsInterface::editTitleSelected()
{
	emit runJavaScript("window.editTitleMenuClicked();");
	emit packageModified();
}

void ResultsJsInterface::copySelected()
{
	TempFiles::purgeClipboard();
	emit runJavaScript("window.copyMenuClicked();");
}

void ResultsJsInterface::citeSelected()
{
	TempFiles::purgeClipboard();
	emit runJavaScript("window.citeMenuClicked();");
}

void ResultsJsInterface::latexCodeSelected()
{
	TempFiles::purgeClipboard();
	emit runJavaScript("window.latexCodeMenuClicked();");
}

void ResultsJsInterface::getDefaultPPI()
{
	runJavaScript("window.getPPI(true)");
}

void ResultsJsInterface::saveImage()
{
	emit runJavaScript("window.saveImageClicked();");
}

void ResultsJsInterface::editImage()
{
	emit runJavaScript("window.editImageClicked();");
}

void ResultsJsInterface::noteSelected()
{
	QAction *action = (QAction *)this->sender();
	QString call = action->data().toString();

	emit runJavaScript(call);
	emit packageModified();
}

void ResultsJsInterface::simulatedMouseClick(int x, int y, int count)
{
	std::cout << "We are NOT going to do weird stuff like simulating mouse clicks to operate an internal webbrowser...." << std::endl;
	/*
	int diff = count;
	while (diff >= 2)
	{
		QMouseEvent * clickEvent = new QMouseEvent ((QEvent::MouseButtonDblClick), QPoint(x * _webViewZoom, y * _webViewZoom),
			Qt::LeftButton,
			Qt::LeftButton,
			Qt::NoModifier   );

		qApp->postEvent((QObject*)_webViewResults,(QEvent *)clickEvent);

		diff -= 2;
	}

	if (diff != 0)
	{
		QMouseEvent * clickEvent1 = new QMouseEvent ((QEvent::MouseButtonPress), QPoint(x * _webViewZoom, y * _webViewZoom),
			Qt::LeftButton,
			Qt::LeftButton,
			Qt::NoModifier   );

		qApp->postEvent((QObject*)_webViewResults,(QEvent *)clickEvent1);

		QMouseEvent * clickEvent2 = new QMouseEvent ((QEvent::MouseButtonRelease), QPoint(x * _webViewZoom, y * _webViewZoom),
			Qt::LeftButton,
			Qt::LeftButton,
			Qt::NoModifier   );

		qApp->postEvent((QObject*)_webViewResults,(QEvent *)clickEvent2);
	}*/
}

void ResultsJsInterface::setExactPValuesHandler(bool exact)
{
	QString exactPValueString = (exact ? "true" : "false");
	QString js = "window.globSet.pExact = " + exactPValueString;
	js += "; window.reRenderAnalyses();";
	emit runJavaScript(js);
}

void ResultsJsInterface::setFixDecimalsHandler(QString numDecimals)
{
	if (numDecimals == "")
		numDecimals = "\"\"";
	QString js = "window.globSet.decimals = " + numDecimals + "; window.reRenderAnalyses();";
	emit runJavaScript(js);
}

void ResultsJsInterface::setGlobalJsValues()
{
	bool exactPValues = Settings::value(Settings::EXACT_PVALUES).toBool();
	QString exactPValueString = (exactPValues ? "true" : "false");
	QString numDecimals = Settings::value(Settings::NUM_DECIMALS).toString();
	QString tempFolder = "file:///" + tq(TempFiles::sessionDirName());

	QString js = "window.globSet.pExact = " + exactPValueString;
	js += "; window.globSet.decimals = " + (numDecimals.isEmpty() ? "\"\"" : numDecimals);
	js += "; window.globSet.tempFolder = \"" + tempFolder + "/\"";
	emit runJavaScript(js);
}

void ResultsJsInterface::saveTempImage(int id, QString path, QByteArray data)
{
	QByteArray byteArray = QByteArray::fromBase64(data);

	QString fullpath = tq(TempFiles::createSpecific_clipboard(fq(path)));

	QFile file(fullpath);
	file.open(QIODevice::WriteOnly);
	file.write(byteArray);
	file.close();

	QString eval = QString("window.imageSaved({ id: %1, fullPath: '%2'});").arg(id).arg(fullpath);
	emit runJavaScript(eval);
}

void ResultsJsInterface::analysisImageEditedHandler(Analysis *analysis)
{
	Json::Value imgJson = analysis->getImgResults();
	QString	results = tq(imgJson.toStyledString());
	results = escapeJavascriptString(results);
	results = "window.modifySelectedImage(" + QString::number(analysis->id()) + ", JSON.parse('" + results + "'));";
	emit runJavaScript(results);

    return;
}

void ResultsJsInterface::menuHidding()
{
	emit runJavaScript("window.analysisMenuHidden();");
}

void ResultsJsInterface::getImageInBase64(int id, const QString &path)
{
	QString fullPath = tq(TempFiles::sessionDirName()) + "/" + path;
	QFile *file = new QFile(fullPath);
	file->open(QIODevice::ReadOnly);
	QByteArray image = file->readAll();
	QString result = QString(image.toBase64());

	QString eval = QString("window.convertToBase64Done({ id: %1, result: '%2'});").arg(id).arg(result);
	emit runJavaScript(eval);

}

void ResultsJsInterface::pushToClipboard(const QString &mimeType, const QString &data, const QString &html)
{
	QMimeData *mimeData = new QMimeData();

	if (mimeType == "text/plain")
		mimeData->setText(data);

	if ( ! html.isEmpty())
		mimeData->setHtml(html);

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
#ifdef __WIN32__ //needed because jpegs/clipboard doesn't support transparency in windows
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
	MessageForwarder::showWarning("Results Warning", msg);
}

void ResultsJsInterface::showAnalysis(int id)
{
	emit runJavaScript("window.select(" % QString::number(id) % ")");
}

void ResultsJsInterface::exportSelected(const QString &filename)
{
	emit runJavaScript("window.exportHTML('" + filename + "');");
}

void ResultsJsInterface::analysisChanged(Analysis *analysis)
{
	Json::Value analysisJson	= analysis->asJSON();
	analysisJson["userdata"]	= analysis->userData();
	QString results				= tq(analysisJson.toStyledString());
	results						= "window.analysisChanged(JSON.parse('" + escapeJavascriptString(results) + "'));";

	emit runJavaScript(results);
}

void ResultsJsInterface::setResultsMeta(QString str)
{
	QString results = escapeJavascriptString(str);
	results = "window.setResultsMeta(JSON.parse('" + results + "'));";
	emit runJavaScript(results);
}

void ResultsJsInterface::unselect()
{
	emit runJavaScript("window.unselect()");
}

void ResultsJsInterface::removeAnalysis(Analysis *analysis)
{
	emit runJavaScript("window.remove(" % QString::number(analysis->id()) % ")");
}

Json::Value &ResultsJsInterface::getResultsMeta()
{
	QEventLoop loop;

	runJavaScript("window.getResultsMeta()");
	connect(this, &ResultsJsInterface::getResultsMetaCompleted, &loop, &QEventLoop::quit);
	loop.exec();

	return _resultsMeta;
}

QVariant &ResultsJsInterface::getAllUserData()
{
	QEventLoop loop;

	runJavaScript("window.getAllUserData()");
	connect(this, &ResultsJsInterface::getAllUserDataCompleted, &loop, &QEventLoop::quit);
	loop.exec();

	return _allUserData;
}

void ResultsJsInterface::showInstruction()
{
	emit runJavaScript("window.showInstructions()");
}

void ResultsJsInterface::exportPreviewHTML()
{
	emit runJavaScript("window.exportHTML('%PREVIEW%');");
}

void ResultsJsInterface::exportHTML()
{
	emit runJavaScript("window.exportHTML('%EXPORT%');");
}

QString ResultsJsInterface::escapeJavascriptString(const QString &str)
{
	QString out;
	QRegExp rx("(\\r|\\n|\\\\|\"|\')");
	int pos = 0, lastPos = 0;

	while ((pos = rx.indexIn(str, pos)) != -1)
	{
		out += str.mid(lastPos, pos - lastPos);

		switch (rx.cap(1).at(0).unicode())
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

void ResultsJsInterface::setPPI(int ppi)
{
	_webViewZoom = 1;
	if(!_loadedResultsFirstTime)
		emit resultsPageLoadedPpi(true, ppi);

	emit ppiChanged(ppi);

	_loadedResultsFirstTime = true;
}

void ResultsJsInterface::setResultsMetaFromJavascript(QString json)
{
	Json::Reader().parse(json.toStdString(), _resultsMeta);
	emit getResultsMetaCompleted();
}

void ResultsJsInterface::setAllUserDataFromJavascript(QString json)
{
	_allUserData = json;
	emit getAllUserDataCompleted();
}

void ResultsJsInterface::setResultsPageUrl(QString resultsPageUrl)
{
	if (_resultsPageUrl == resultsPageUrl)
		return;

	_resultsPageUrl = resultsPageUrl;
	emit resultsPageUrlChanged(_resultsPageUrl);
}
