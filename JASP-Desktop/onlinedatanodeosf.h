#ifndef ONLINEDATANODEOSF_H
#define ONLINEDATANODEOSF_H

#include "onlinedatanode.h"
#include "common.h"

class OnlineDataNodeOSF: public OnlineDataNode
{
	Q_OBJECT

public:
	OnlineDataNodeOSF(QNetworkAccessManager *manager, QString id, QObject *parent = 0);

	virtual void getNodeInfo() const OVERRIDE;

	virtual QString getUploadPath() const OVERRIDE;
	virtual QString getUploadPath(QString filename) const OVERRIDE;
	virtual QString getDownloadPath() const OVERRIDE;
	virtual QString getNewFolderPath(QString folderName) const OVERRIDE;
	virtual QString getDeletePath() const OVERRIDE;

private slots:
	void nodeInfoReceived();

};

#endif // ONLINEDATANODEOSF_H
