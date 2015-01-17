
#include "lrnam.h"

#include <QNetworkRequest>

#include "lrnamreply.h"

LRNAM::LRNAM(QObject *parent)
	: QNetworkAccessManager(parent)
{

}

QNetworkReply *LRNAM::createRequest(QNetworkAccessManager::Operation op, const QNetworkRequest &request, QIODevice *outgoingData)
{
	if (op == QNetworkAccessManager::GetOperation && request.url().isLocalFile())
	{
		QString path = request.url().toLocalFile();
		return new LRNAMReply(path);
	}
	else
	{
		return QNetworkAccessManager::createRequest(op, request, outgoingData);
	}
}

