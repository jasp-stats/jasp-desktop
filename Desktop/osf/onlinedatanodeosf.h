#ifndef ONLINEDATANODEOSF_H
#define ONLINEDATANODEOSF_H

#include "onlinedatanode.h"
#include "common.h"

///
/// Fills in the interface of OnlineDataNode for OSF
class OnlineDataNodeOSF: public OnlineDataNode
{
	Q_OBJECT

public:
	OnlineDataNodeOSF(QString localPath, QNetworkAccessManager *manager, QString id, QObject *parent = 0);

	virtual void initialise() OVERRIDE;

	virtual QString getUploadPath() const OVERRIDE;
	virtual QString getUploadPath(QString filename) const OVERRIDE;
	virtual QString getDownloadPath() const OVERRIDE;
	virtual QString getNewFolderPath(QString folderName) const OVERRIDE;
	virtual QString getDeletePath() const OVERRIDE;

	virtual bool beginDownloadFile() OVERRIDE;
	virtual bool beginUploadFile() OVERRIDE;
	virtual bool beginUploadFile(QString name) OVERRIDE;
	virtual bool beginNewFolder(QString name) OVERRIDE;

signals:
	void progress(const QString &status, int progress);

private slots:
	void nodeInfoReceived();
	//void checkFinished();

private:

	bool searchList(QString searchName, OnlineDataNode::Kind kind, QJsonArray arrayObject, QJsonObject &nodeObject);
	bool interpretNode(QJsonObject nodeObject);
	OnlineDataNode::Kind parseKind(QString kind);
	void processUrl(QUrl url);
	QString getContentsUrl(QJsonObject nodeObject);
	void populateNodeData(QJsonObject nodeObject);
	QString getBaseUrl(QJsonObject nodeObject);

	QStringList _subPath;

	OnlineDataNode::Kind _dataKind = OnlineDataNode::Unknown;
	QString _expectedName = "";

};

#endif // ONLINEDATANODEOSF_H
