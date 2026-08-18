//
// PersonaSchemeHandler - serves persona image files via jaspPersona:// scheme.
//

#ifndef PERSONASCHEMEHANDLER_H
#define PERSONASCHEMEHANDLER_H

#include <QWebEngineUrlSchemeHandler>
#include <QString>

class PersonaSchemeHandler : public QWebEngineUrlSchemeHandler
{
public:
	explicit PersonaSchemeHandler(QObject *parent = nullptr);

	static void createUrlScheme();

	void setPersonasDir(const QString &dir) { m_personasDir = dir; }
	void setResourcesPersonasDir(const QString &dir) { m_resourcesPersonasDir = dir; }

protected:
	void requestStarted(QWebEngineUrlRequestJob *request) override;

private:
	QString m_personasDir;
	QString m_resourcesPersonasDir;
};

#endif // PERSONASCHEMEHANDLER_H
