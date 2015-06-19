
#include "lrnam.h"

#include <QNetworkRequest>

#include "lrnamreply.h"


LRNAM::LRNAM(const QString &baseResourceDirectory, QObject *parent)
	: QNetworkAccessManager(parent)
{
	_baseResourceDirectory = baseResourceDirectory;
}

QNetworkReply *LRNAM::createRequest(QNetworkAccessManager::Operation op, const QNetworkRequest &request, QIODevice *outgoingData)
{	
	QUrl url = request.url();

	QString path = url.path();
	if (op == QNetworkAccessManager::GetOperation && url.isLocalFile())
	{
			path = url.toLocalFile();
			return new LRNAMReply(path);
	}
	else if (path.startsWith("/core/resources/"))
	{
		path = _baseResourceDirectory + path.mid(5);
		return new LRNAMReply(path);
	}
	else
	{
		return QNetworkAccessManager::createRequest(op, request, outgoingData);
	}
}

