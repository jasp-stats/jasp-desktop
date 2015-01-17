#ifndef LRNAMREPLY_H
#define LRNAMREPLY_H

#include <QNetworkReply>

#include <QFile>

#include "common.h"

class LRNAMReply : public QNetworkReply
{
	Q_OBJECT

public:
	LRNAMReply(const QString &path, QObject *parent = 0);

	virtual void abort() OVERRIDE;
	virtual qint64 bytesAvailable() const OVERRIDE;
	virtual bool isSequential() const OVERRIDE;
	virtual qint64 readData(char * data, qint64 maxSize);

private:

	QFile _file;

private slots:
	void emitFinished();
	void emitError();

};

#endif // LRNAMREPLY_H
