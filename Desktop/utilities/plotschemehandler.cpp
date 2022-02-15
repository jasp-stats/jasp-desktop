#include "plotschemehandler.h"

PlotSchemeHandler::PlotSchemeHandler(QObject *parent) : QWebEngineUrlSchemeHandler(parent)
{
	QQuickWebEngineProfile::defaultProfile()->installUrlSchemeHandler("plot", this);
}

void PlotSchemeHandler::createUrlScheme()
{
	QWebEngineUrlScheme plotScheme = QWebEngineUrlScheme("plot");
	plotScheme.setFlags(QWebEngineUrlScheme::ContentSecurityPolicyIgnored);
	plotScheme.setSyntax(QWebEngineUrlScheme::Syntax::Path);
	QWebEngineUrlScheme::registerScheme(plotScheme);
}

void PlotSchemeHandler::requestStarted(QWebEngineUrlRequestJob *request)
{
	QUrl	fileUrl		= request->requestUrl();
	QString filePath	= QString::fromStdString(TempFiles::sessionDirName()) + fileUrl.toString(QUrl::RemoveScheme | QUrl::RemoveQuery);
	//Maybe we could remove the whole ?rev=number thing because we are not caching anything here. But maybe webengine does, Im leaving it for now to avoid too many changes.

	if(filePath.indexOf(".png") == -1)
	{
		request->fail(QWebEngineUrlRequestJob::Error::UrlInvalid);
		return;
	}

	QFile * png = new QFile(filePath, request);
	if(!png->exists())
	{
		request->fail(QWebEngineUrlRequestJob::Error::UrlNotFound);
		return;
	}
	png->open(QIODevice::ReadOnly);

	request->reply("image/png", png);
}
