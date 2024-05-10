#include "imgschemehandler.h"
#include <QWebEngineUrlScheme>
#include <QQuickWebEngineProfile>
#include <QWebEngineUrlRequestJob>
#include <QFile>
#include <QIODevice>
#include <iostream>
#include "modules/dynamicmodules.h"
#include "log.h"

using namespace Modules;

ImgSchemeHandler::ImgSchemeHandler(QObject *parent)
	: QWebEngineUrlSchemeHandler{parent}
{
    std::cout << "ImgSchemeHandler for QtWebengine" << std::endl;

	QQuickWebEngineProfile::defaultProfile()->installUrlSchemeHandler("img", this);
}

void ImgSchemeHandler::createUrlScheme()
{
	QWebEngineUrlScheme imgScheme = QWebEngineUrlScheme("img");
	imgScheme.setFlags(QWebEngineUrlScheme::ContentSecurityPolicyIgnored);
	imgScheme.setSyntax(QWebEngineUrlScheme::Syntax::Path);
	QWebEngineUrlScheme::registerScheme(imgScheme);
}

void ImgSchemeHandler::requestStarted(QWebEngineUrlRequestJob *request)
{
	QString filePath	= request->requestUrl().toString(QUrl::RemoveScheme | QUrl::RemoveQuery);

	Log::log() << "ImgSchemeHandler got file: " << filePath << std::endl;

	QStringList filePathElements = filePath.split("/");

	DynamicModule * mod = filePathElements.size() > 0 ? DynamicModules::dynMods()->dynamicModuleLowerCased(filePathElements[0])  : nullptr;

	if(!mod)
	{
		Log::log() <<  "Url is invalid, #" << filePathElements.size() << " elements, and first is: " << (filePathElements.size() > 0 ? filePathElements[0] : "...") << std::endl;
		request->fail(QWebEngineUrlRequestJob::Error::UrlInvalid);
		return;
	}

	if(filePath.indexOf(".png") == -1 && filePath.indexOf(".svg") == -1)
	{
		Log::log() << "Url is invalid because it doesnt end in .svg or .png" << std::endl;
		request->fail(QWebEngineUrlRequestJob::Error::UrlInvalid);
		return;
	}

	//So its both correctly identifying a module and refers to a png or svg. Now to load it
	const QString moduleName = filePathElements[0];
	filePathElements.remove(0);
	filePath =  tq(DynamicModules::dynMods()->dynamicModuleLowerCased(moduleName)->moduleInstFolder()) + filePathElements.join(QDir::separator());

	Log::log() << "Is img from module " << moduleName << " and reconstructed imgpath is: " << filePath << std::endl;

	QFile * img = new QFile(filePath, request);
	if(!img->exists())
		{
			request->fail(QWebEngineUrlRequestJob::Error::UrlNotFound);
			return;
		}
	img->open(QIODevice::ReadOnly);

	request->reply(filePath.indexOf(".png") != -1 ? "image/png" : "image/svg", img);
}
