#ifndef LRNAM_H
#define LRNAM_H

#include <QNetworkAccessManager>

#include "common.h"

class LRNAM : public QNetworkAccessManager
{
	Q_OBJECT

public:
	LRNAM(QObject *parent = 0);

protected:
	QNetworkReply *createRequest(Operation op, const QNetworkRequest &request, QIODevice *outgoingData) OVERRIDE;

};

#endif // LRNAM_H
