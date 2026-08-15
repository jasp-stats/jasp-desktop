#include "plotschemehandler.h"
#include "tempfiles.h"
#include <QDir>
#include <QFileInfo>

PlotSchemeHandler::PlotSchemeHandler(QObject *parent) : QWebEngineUrlSchemeHandler(parent)
{
	QQuickWebEngineProfile::defaultProfile()->installUrlSchemeHandler("plot", this);
}

void PlotSchemeHandler::createUrlScheme()
{
	QWebEngineUrlScheme plotScheme = QWebEngineUrlScheme("plot");
	plotScheme.setFlags(QWebEngineUrlScheme::ContentSecurityPolicyIgnored | QWebEngineUrlScheme::CorsEnabled | QWebEngineUrlScheme::FetchApiAllowed);
	plotScheme.setSyntax(QWebEngineUrlScheme::Syntax::Path);
	QWebEngineUrlScheme::registerScheme(plotScheme);
}

void PlotSchemeHandler::requestStarted(QWebEngineUrlRequestJob *request)
{
	QUrl	fileUrl		= request->requestUrl();
	QString filePath	= QString::fromStdString(TempFiles::sessionDirName()) + fileUrl.toString(QUrl::RemoveScheme | QUrl::RemoveQuery);
	//Maybe we could remove the whole ?rev=number thing because we are not caching anything here. But maybe webengine does, Im leaving it for now to avoid too many changes.

	QString contentType;
	if(filePath.endsWith(".png", Qt::CaseInsensitive))
		contentType = "image/png";
	else if(filePath.endsWith(".json", Qt::CaseInsensitive))
		contentType = "application/json";
	else
	{
		request->fail(QWebEngineUrlRequestJob::Error::UrlInvalid);
		return;
	}

	QFileInfo sessionDir(QString::fromStdString(TempFiles::sessionDirName()));
	QFileInfo requestedFile(filePath);
	const QString resourcesPath	= QDir::cleanPath(sessionDir.canonicalFilePath() + QDir::separator() + "resources");
	const QString requestedPath	= QDir::cleanPath(requestedFile.canonicalFilePath());

	if(requestedPath.isEmpty() || resourcesPath.isEmpty() || !requestedPath.startsWith(resourcesPath + QDir::separator()))
	{
		request->fail(QWebEngineUrlRequestJob::Error::UrlNotFound);
		return;
	}

	QFile * file = new QFile(requestedPath, request);
	file->open(QIODevice::ReadOnly);

	request->reply(contentType.toUtf8(), file);
}
