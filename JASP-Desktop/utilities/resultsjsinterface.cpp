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

#include <QWebEngineHistory>
#include <QMessageBox>
#include <QClipboard>
#include <QStringBuilder>

#include "utilities/qutils.h"
#include "appinfo.h"
#include "tempfiles.h"
#include "analysis/analysis.h"
#include <functional>
#include "utilities/settings.h"
#include <QApplication>

ResultsJsInterface::ResultsJsInterface(QWidget *parent) : QObject(parent)
{
	_mainWindow = dynamic_cast<MainWindow *>(parent);
	_webViewResults = _mainWindow->getWebViewResults();
	_webViewResults->setContextMenuPolicy(Qt::NoContextMenu);

	_channel = new QWebChannel(this);
	_channel->registerObject(QStringLiteral("jasp"), this);
	_webViewResults->page()->setWebChannel(_channel);

	_analysisMenu = new QMenu(_mainWindow);
	connect(_analysisMenu, SIGNAL(aboutToHide()), this, SLOT(menuHidding()));

	_webViewResults->setUrl(QUrl(QString("qrc:///core/index.html")));
	connect(_webViewResults, SIGNAL(loadFinished(bool)), this, SLOT(resultsPageLoaded(bool)));
// TODO: This signal does not exist anymore, the WebEngine does not support paintEvent anymore, and WebView used this function
// to emit the scrollValuesChanged signal.
//	connect(_webViewResults, SIGNAL(scrollValueChanged()), this, SLOT(scrollValueChangedHandle()));

}

void ResultsJsInterface::setZoom(double zoom)
{
	_webViewZoom = zoom;
	QString js = "window.setZoom(" + QString::number(zoom) + ")";
	runJavaScript(js);
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


void ResultsJsInterface::resultsPageLoaded(bool success)
{
	// clear history, to prevent backspace from going 'back'
	_webViewResults->history()->clear();

	if (success)
	{
		QString version = tq(AppInfo::version.asString());
		runJavaScript("window.setAppVersion('" + version + "')");
#ifdef JASP_DEBUG
		version+="-Debug";
#endif

		setGlobalJsValues();

		runJavaScript("window.getPPI()", std::bind(&ResultsJsInterface::cbSetPPI, this, std::placeholders::_1));
	}
	else
	{
		std::cout << "Could not load result page!" << std::endl;
		std::cout.flush();
	}
}

void ResultsJsInterface::showAnalysesMenu(QString options)
{
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
	_analysisMenu->show();
}

void ResultsJsInterface::collapseSelected()
{
	runJavaScript("window.collapseMenuClicked();");
}

void ResultsJsInterface::removeSelected()
{
	runJavaScript("window.removeMenuClicked();");
}

void ResultsJsInterface::editTitleSelected()
{
	runJavaScript("window.editTitleMenuClicked();");
	_mainWindow->setPackageModified();
}

void ResultsJsInterface::copySelected()
{
	TempFiles::purgeClipboard();
	runJavaScript("window.copyMenuClicked();");
}

void ResultsJsInterface::citeSelected()
{
	TempFiles::purgeClipboard();
	runJavaScript("window.citeMenuClicked();");
}

void ResultsJsInterface::latexCodeSelected()
{
	TempFiles::purgeClipboard();
	runJavaScript("window.latexCodeMenuClicked();");
}

void ResultsJsInterface::saveImage()
{
	runJavaScript("window.saveImageClicked();");
}

void ResultsJsInterface::editImage()
{
    runJavaScript("window.editImageClicked();");
}

void ResultsJsInterface::noteSelected()
{
	QAction *action = (QAction *)this->sender();
	QString call = action->data().toString();

	runJavaScript(call);

	_mainWindow->setPackageModified();
}

void ResultsJsInterface::simulatedMouseClick(int x, int y, int count) {

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
	}
}

void ResultsJsInterface::scrollValueChangedHandle()
{
	if ( ! _analysisMenu->isHidden())
		_analysisMenu->hide();
}
void ResultsJsInterface::setExactPValuesHandler(bool exact)
{
	QString exactPValueString = (exact ? "true" : "false");
	QString js = "window.globSet.pExact = " + exactPValueString;
	js += "; window.reRenderAnalyses();";
	runJavaScript(js);
}

void ResultsJsInterface::setFixDecimalsHandler(QString numDecimals)
{
	if (numDecimals == "")
		numDecimals = "\"\"";
	QString js = "window.globSet.decimals = " + numDecimals + "; window.reRenderAnalyses();";
	runJavaScript(js);
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
	runJavaScript(js);
}

void ResultsJsInterface::analysisUnselected()
{
	_mainWindow->analysisUnselectedHandler();
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
	runJavaScript(eval);
}

void ResultsJsInterface::analysisImageEditedHandler(Analysis *analysis)
{
	Json::Value imgJson = analysis->getImgResults();
	QString	results = tq(imgJson.toStyledString());
	results = escapeJavascriptString(results);
	results = "window.modifySelectedImage(" + QString::number(analysis->id()) + ", JSON.parse('" + results + "'));";
	runJavaScript(results);

    return;
}

void ResultsJsInterface::menuHidding()
{
	runJavaScript("window.analysisMenuHidden();");
}

void ResultsJsInterface::analysisChangedDownstream(int id, QString options)
{
	_mainWindow->analysisChangedDownstreamHandler(id, options);
}

void ResultsJsInterface::resultsDocumentChanged()
{
	_mainWindow->setPackageModified();
}

void ResultsJsInterface::updateUserData(int id, QString key)
{
	_mainWindow->setPackageModified();
}

void ResultsJsInterface::saveTextToFile(const QString &filename, const QString &data)
{
	_mainWindow->saveTextToFileHandler(filename, data);
}

void ResultsJsInterface::analysisSelected(int id)
{
	_mainWindow->analysisSelectedHandler(id);
}

void ResultsJsInterface::analysisSaveImage(int id, QString options)
{
	_mainWindow->analysisSaveImageHandler(id, options);
}

void ResultsJsInterface::analysisEditImage(int id, QString options)
{
	_mainWindow->analysisEditImageHandler(id, options);
}

void ResultsJsInterface::removeAnalysisRequest(int id)
{
	_mainWindow->removeAnalysisRequestHandler(id);
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
	QMessageBox::warning(_mainWindow, "Results Warning", msg);
}

void ResultsJsInterface::showAnalysis(int id)
{
	runJavaScript("window.select(" % QString::number(id) % ")");
}

void ResultsJsInterface::exportSelected(const QString &filename)
{
	runJavaScript("window.exportHTML('" + filename + "');");
}

void ResultsJsInterface::analysisChanged(Analysis *analysis)
{
	Json::Value analysisJson = analysis->asJSON();
	analysisJson["userdata"] = analysis->userData();
	QString results = tq(analysisJson.toStyledString());

	results = escapeJavascriptString(results);
	results = "window.analysisChanged(JSON.parse('" + results + "'));";
	runJavaScript(results);
}

void ResultsJsInterface::setResultsMeta(QString str)
{
	QString results = escapeJavascriptString(str);
	results = "window.setResultsMeta(JSON.parse('" + results + "'));";
	runJavaScript(results);
}

void ResultsJsInterface::unselect()
{
	runJavaScript("window.unselect()");
}

void ResultsJsInterface::removeAnalysis(Analysis *analysis)
{
	runJavaScript("window.remove(" % QString::number(analysis->id()) % ")");
}

Json::Value &ResultsJsInterface::getResultsMeta()
{
	QEventLoop loop;

	runJavaScript("window.getResultsMeta()", std::bind(&ResultsJsInterface::cbSetResultstMeta, this, std::placeholders::_1));
	connect(this, SIGNAL(getResultsMetaCompleted()), &loop, SLOT(quit()));
	loop.exec();

	return _resultsMeta;
}

QVariant &ResultsJsInterface::getAllUserData()
{
	QEventLoop loop;

	runJavaScript("window.getAllUserData()", std::bind(&ResultsJsInterface::cbSetAllUserData, this, std::placeholders::_1));
	connect(this, SIGNAL(getAllUserDataCompleted()), &loop, SLOT(quit()));
	loop.exec();

	return _allUserData;
}

void ResultsJsInterface::showInstruction()
{
	runJavaScript("window.showInstructions()");
}

void ResultsJsInterface::exportPreviewHTML()
{
	runJavaScript("window.exportHTML('%PREVIEW%');");
}

void ResultsJsInterface::exportHTML()
{
	runJavaScript("window.exportHTML('%EXPORT%');");
}

void ResultsJsInterface::openFileTab()
{
	_mainWindow->setCurrentTab("first");
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

void ResultsJsInterface::runJavaScript(const QString &str)
{
	_webViewResults->page()->runJavaScript(str);
}

void ResultsJsInterface::runJavaScript(const QString &str, std::function<void(const QVariant&)> cb)
{
	_webViewResults->page()->runJavaScript(str,  [cb] (const QVariant &result) { cb(result); });
}

void ResultsJsInterface::cbSetPPI(const QVariant &vppi)
{
	bool success;
	int ppi = vppi.toInt(&success);
	if (success == false)
	{
		std::cout << "Could not get PPI: " << vppi.toString().toStdString() << "." << std::endl;
		std::cout.flush();
		ppi = 96;
	}
	else
	{
		std::cout << "PPI: " << vppi.toString().toStdString() << "." << std::endl;
		std::cout.flush();

	}

	_webViewZoom = 1;

	_mainWindow->resultsPageLoaded(success, ppi);
}

void ResultsJsInterface::cbSetResultstMeta(const QVariant &vMetaData)
{
	Json::Reader parser;
	parser.parse(fq(vMetaData.toString()), _resultsMeta);

	emit getResultsMetaCompleted();
}

void ResultsJsInterface::cbSetAllUserData(const QVariant &vAllUserData)
{
	_allUserData = vAllUserData;
	emit getAllUserDataCompleted();
}
