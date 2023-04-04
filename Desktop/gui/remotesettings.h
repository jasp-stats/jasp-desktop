#ifndef REMOTESETTINGS_H
#define REMOTESETTINGS_H

#include <QNetworkAccessManager>
#include <QObject>

class RemoteSettings : public QObject
{
	Q_OBJECT
public:
	explicit RemoteSettings(QObject *parent = nullptr);

	void processRemoteSettings();
	bool patchPresent(const QString module, const QString analysis, QString& path);

public slots:
	void remoteChanged(QString remoteURL);

signals:
	void remoteSettingsProcessed(QString results);

private slots:
//	void downloadFinished(QNetworkReply* reply);
	void sslErrors(const QList<QSslError> &errors);

private:
	void processTasks();
	void parse(const QString& settings);
//	int32_t getVersion(const QString& settings);

	QNetworkAccessManager	_networkManager;
//	int32_t _version = -1;
	QSet<QString> _patches;

	const QString configurationFilename = "remote_settings.conf";
//	static QRegularExpression versionRE;

};

#endif // REMOTESETTINGS_H
