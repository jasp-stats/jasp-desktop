#ifndef OSFNAM_H
#define OSFNAM_H

#include <QNetworkAccessManager>

#include "common.h"

class OSFNAM : public QNetworkAccessManager
{
	Q_OBJECT

public:
	OSFNAM(QObject *parent = 0);

	void osfAuthentication(QString username, QString password);
	QString username();

private:
	QByteArray _authHeader;
	QString _username;

	void updateAuthHeader(QString password);


protected:
	QNetworkReply *createRequest(Operation op, const QNetworkRequest &request, QIODevice *outgoingData) OVERRIDE;

};

#endif // OSFNAM_H
