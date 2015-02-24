
#include "lrnamreply.h"

#include <QFileInfo>
#include <QTimer>

#include "base64.h"
#include "qutils.h"

LRNAMReply::LRNAMReply(const QString &path, QObject *parent)
	: QNetworkReply(parent)
{
	QFileInfo info(path);
	setHeader(QNetworkRequest::ContentLengthHeader, info.size());

	int slashPos = path.lastIndexOf("/");
	int dotPos = path.lastIndexOf('.');

	if (dotPos != -1 && dotPos > slashPos)
	{
		QString extension = path.mid(dotPos + 1);

		if (extension == "svg")
		{
			setHeader(QNetworkRequest::ContentTypeHeader, "image/svg+xml");
		}
		else if (extension == "png")
		{
			setHeader(QNetworkRequest::ContentTypeHeader, "image/png");
		}
	}

	_file.setFileName(path);
	if (_file.open(QFile::ReadOnly))
	{
		open(ReadOnly | Unbuffered);

		// delay signals until after listeners have attached
		QTimer::singleShot(0, this, SLOT(emitFinished()));
	}
	else
	{
		QTimer::singleShot(0, this, SLOT(emitError()));
	}
}

void LRNAMReply::abort()
{
	_file.close();
}

qint64 LRNAMReply::bytesAvailable() const
{
	return _file.bytesAvailable();
}

bool LRNAMReply::isSequential() const
{
	return _file.isSequential();
}

qint64 LRNAMReply::readData(char *data, qint64 maxSize)
{
	return _file.read(data, maxSize);
}

void LRNAMReply::emitFinished()
{
	emit readyRead();
	emit finished();
}

void LRNAMReply::emitError()
{
	emit error(QNetworkReply::ContentNotFoundError);
}

