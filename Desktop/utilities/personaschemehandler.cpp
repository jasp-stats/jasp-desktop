//
// PersonaSchemeHandler - serves persona image files via jaspPersona:// scheme.
//

#include "personaschemehandler.h"
#include <QWebEngineUrlScheme>
#include <QQuickWebEngineProfile>
#include <QWebEngineUrlRequestJob>
#include <QFile>
#include <QDir>
#include "log.h"

PersonaSchemeHandler::PersonaSchemeHandler(QObject *parent)
	: QWebEngineUrlSchemeHandler{parent}
{
	QQuickWebEngineProfile::defaultProfile()->installUrlSchemeHandler("jaspPersona", this);
}

void PersonaSchemeHandler::createUrlScheme()
{
	QWebEngineUrlScheme scheme("jaspPersona");
	scheme.setFlags(QWebEngineUrlScheme::ContentSecurityPolicyIgnored);
	scheme.setSyntax(QWebEngineUrlScheme::Syntax::Path);
	QWebEngineUrlScheme::registerScheme(scheme);
}

void PersonaSchemeHandler::requestStarted(QWebEngineUrlRequestJob *request)
{
	QString fileName = request->requestUrl().toString(QUrl::RemoveScheme | QUrl::RemoveQuery);

	// Strip leading slashes
	while (fileName.startsWith('/'))
		fileName = fileName.mid(1);

	// Reconstruct the full filesystem path from the personas directory
	QString fullPath = m_personasDir + QDir::separator() + fileName;

	Log::log() << "PersonaSchemeHandler serving: " << fullPath.toStdString() << std::endl;

	QFile *file = new QFile(fullPath, request);
	if (!file->exists()) {
		// Try the resources persona images directory as fallback
		if (!m_resourcesPersonasDir.isEmpty()) {
			QString fallbackPath = m_resourcesPersonasDir + QDir::separator() + fileName;
			delete file;
			file = new QFile(fallbackPath, request);
			if (file->exists()) {
				fullPath = fallbackPath;
				Log::log() << "PersonaSchemeHandler serving (fallback): " << fullPath.toStdString() << std::endl;
			}
		}
	}
	if (!file->exists()) {
		Log::log() << "PersonaSchemeHandler: file not found: " << fullPath.toStdString() << std::endl;
		request->fail(QWebEngineUrlRequestJob::Error::UrlNotFound);
		return;
	}

	file->open(QIODevice::ReadOnly);

	// Determine MIME type from extension
	QString mime = "image/png";
	if (fileName.endsWith(".jpg") || fileName.endsWith(".jpeg"))
		mime = "image/jpeg";
	else if (fileName.endsWith(".gif"))
		mime = "image/gif";
	else if (fileName.endsWith(".svg"))
		mime = "image/svg+xml";

	request->reply(mime.toUtf8(), file);
}
