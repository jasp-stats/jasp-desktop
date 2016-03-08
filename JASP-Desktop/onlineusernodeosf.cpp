#include "onlineusernodeosf.h"

#include <QJsonDocument>
#include <QJsonObject>
#include <QUrl>
#include <QNetworkRequest>
#include <QNetworkReply>
#include <QByteArray>
#include <QEventLoop>
#include <stdexcept>
#include <QMessageBox>


using namespace std;

OnlineUserNodeOSF::OnlineUserNodeOSF(QNetworkAccessManager *manager, QString id, QObject *parent):
	OnlineUserNode(manager, id, parent)
{
}

void OnlineUserNodeOSF::initialise() {

	startInit();

	QUrl url = QUrl(_path);

	QNetworkRequest request(url);
	request.setHeader(QNetworkRequest::ContentTypeHeader, "application/vnd.api+json");
	request.setRawHeader("Accept", "application/vnd.api+json");

	QNetworkReply* reply = _manager->get(request);

	connect(reply, SIGNAL(finished()), this, SLOT(nodeInfoReceived()));
}


void OnlineUserNodeOSF::nodeInfoReceived() {

	QNetworkReply *reply = (QNetworkReply*)this->sender();

	bool success = false;

	if (reply->error() != QNetworkReply::NoError)
		setError(true, reply->errorString());
	else
	{
		QByteArray data = reply->readAll();
		QString dataString = (QString) data;

		QJsonParseError error;
		QJsonDocument doc = QJsonDocument::fromJson(dataString.toUtf8(), &error);

		QJsonObject json = doc.object();

		QJsonObject dataObj = json.value("data").toObject();
		//_userId = dataObj.value("id").toString();

		QJsonObject attributes = dataObj.value("attributes").toObject();
		_fullname = attributes.value("full_name").toString();

		success = true;
	}

	reply->deleteLater();

	endInit(success);
}

bool OnlineUserNodeOSF::authenticationSuccessful(QNetworkAccessManager *manager)
{
	QUrl url = QUrl("https://api.osf.io/v2/users/me/");

	QEventLoop loop;
	QNetworkRequest request(url);
	request.setHeader(QNetworkRequest::ContentTypeHeader, "application/vnd.api+json");
	request.setRawHeader("Accept", "application/vnd.api+json");

	QNetworkReply* reply = manager->get(request);

	connect(reply, SIGNAL(finished()), &loop, SLOT(quit()));

	loop.exec();

	QNetworkReply::NetworkError error = reply->error();
	if (error != QNetworkReply::NoError && error != QNetworkReply::AuthenticationRequiredError) //Authorisation message will be shown later on
		QMessageBox::warning(NULL, "OSF Warning", reply->errorString());

	delete reply;

	return error == false;
}
