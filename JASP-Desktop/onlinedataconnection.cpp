#include "onlinedataconnection.h"

#include <QNetworkRequest>
#include <QNetworkReply>

OnlineDataConnection::OnlineDataConnection(QString id, QNetworkAccessManager *manager, QObject *parent):
	QObject(parent)
{
	_id = id;
	_manager = manager;
}


QNetworkAccessManager* OnlineDataConnection::manager() const
{
	return _manager;
}

QString OnlineDataConnection::id() const
{
	return _id;
}

bool OnlineDataConnection::error() const
{
	return _error;
}

QString OnlineDataConnection::errorMessage() const
{
	return _errorMsg;
}

void OnlineDataConnection::beginUploadFile(QUrl url, QString sourcePath) {

	//_uploadConnectionType = _dataNode->uploadConnectionType();

	_uploadFile = new QFile(sourcePath, this);
	if(_uploadFile->open(QFile::ReadOnly))
		retryUploadFile(url);
}

bool OnlineDataConnection::retryUploadFile(QUrl url) {

	if (_uploadFile == NULL || _uploadFile->isOpen() == false)
		return false;

	QNetworkRequest request(url);

	if (_uploadFile->pos() != 0)
		_uploadFile->reset();

	QNetworkReply *reply;
	//if (_uploadConnectionType == OnlineDataNode::Put)
		reply = _manager->put(request, _uploadFile);
	//else
	//	reply = _manager->post(request, _uploadFile);

	connect(reply, SIGNAL(finished()), this, SLOT(uploadFinished()));

	return true;
}

void OnlineDataConnection::uploadFinished() {

	QNetworkReply *reply = (QNetworkReply*)this->sender();

	bool isFinished = true;

	_error = false;
	_errorMsg = "";

	if(reply->error())
		setError(reply->errorString());
	else
	{
		int status = reply->attribute(QNetworkRequest::HttpStatusCodeAttribute).toInt();
		if (status == 301 || status == 302)
		{
			 QUrl url = reply->attribute(QNetworkRequest::RedirectionTargetAttribute).toUrl();
			 if (retryUploadFile(url))
				isFinished = false;
		}
	}

	reply->deleteLater();

	if (isFinished)
	{
		_uploadFile->close();
		_uploadFile->deleteLater();

		emit finished();
	}
}


void OnlineDataConnection::beginDownloadFile(QUrl url, QString destination) {

	_downloadDest = destination;
	//_downloadConnectionType = _dataNode->downloadConnectionType();

	QNetworkRequest request(url);

	QNetworkReply* reply = _manager->get(request);

	connect(reply, SIGNAL(finished()), this, SLOT(downloadFinished()));
}

void OnlineDataConnection::downloadFinished() {

	QNetworkReply *reply = (QNetworkReply*)this->sender();

	_error = false;
	_errorMsg = "";

	bool isFinished = true;

	if(reply->error())
		setError(reply->errorString());
	else
	{
		int status = reply->attribute(QNetworkRequest::HttpStatusCodeAttribute).toInt();
		if (status == 301 || status == 302)
		{
			 QUrl url = reply->attribute(QNetworkRequest::RedirectionTargetAttribute).toUrl();
			 beginDownloadFile(url, _downloadDest);
			 isFinished = false;
		}
		else {

			QFile *file = new QFile(_downloadDest);
			if(file->open(QFile::WriteOnly))
			{
				file->write(reply->readAll());
				file->flush();
				file->close();

				_downloadDest = "";
			}
			else
				setError("Cannot open file for saving downloaded data.");

			delete file;
		}
	}

	reply->deleteLater();

	if (isFinished)
		emit finished();
}

void OnlineDataConnection::setError(QString msg)
{
	_error = true;
	_errorMsg = msg;
}
