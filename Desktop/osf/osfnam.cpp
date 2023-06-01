#include "osfnam.h"

#include <QNetworkRequest>

//#include "lrnamreply.h"


OSFNAM::OSFNAM(QObject *parent)
	: QNetworkAccessManager(parent)
{
}

QNetworkReply *OSFNAM::createRequest(QNetworkAccessManager::Operation op, const QNetworkRequest &request, QIODevice *outgoingData)
{

	QNetworkRequest authRequest = QNetworkRequest(request);

	authRequest.setRawHeader("Authorization", _authHeader);

	return QNetworkAccessManager::createRequest(op, authRequest, outgoingData);
}

void OSFNAM::osfAuthentication(QString username, QString password){
	_username = username;
	updateAuthHeader(password);
}

void OSFNAM::updateAuthHeader(QString password)
{
	_authHeader = "Basic " + QByteArray(QString("%1:%2").arg(_username).arg(password).toLatin1()).toBase64();
}

QString OSFNAM::username()
{
	return _username;
}
