#include "onlinedatanode.h"

OnlineDataNode::OnlineDataNode(QString localPath, QNetworkAccessManager *manager, QString id, QObject *parent):
	OnlineNode(manager, id, parent)
{
	_localPath = localPath;
	_localFile.setFileName(localPath);
}

QString OnlineDataNode::getUploadPath() const
{
	return _uploadPath;
}

QString OnlineDataNode::getDeletePath() const
{
	return _deletePath;
}

QString OnlineDataNode::getDownloadPath() const
{
	return _downloadPath;
}

QString OnlineDataNode::name() const
{
	return _name;
}

bool OnlineDataNode::kind() const
{
	return _kind;
}

OnlineDataNode::ConnectionType OnlineDataNode::downloadConnectionType() const {

	return _downloadConnectionType;
}

OnlineDataNode::ConnectionType OnlineDataNode::uploadConnectionType() const {

	return _uploadConnectionType;
}

void OnlineDataNode::prepareAction(OnlineDataNode::Action action, const QString &data)
{
	_preparedAction = action;
	_preparedData = data;
}

QString OnlineDataNode::getLocalPath()
{
	return _localPath;
}

QString OnlineDataNode::getActionPath() const
{
	if (_preparedAction == OnlineDataNode::NewFile)
		return getUploadPath(_preparedData);
	else if (_preparedAction == OnlineDataNode::NewFolder)
		return getNewFolderPath(_preparedData);
	else if (_preparedAction == OnlineDataNode::Upload)
		return getUploadPath();
	else if (_preparedAction == OnlineDataNode::Download)
		return getDownloadPath();
	else
		return "";
}


bool OnlineDataNode::beginAction() {

	if (_preparedAction == OnlineDataNode::Upload)
		beginUploadFile();
	else if (_preparedAction == OnlineDataNode::NewFile)
		beginUploadFile(_preparedData);
	else if (_preparedAction == OnlineDataNode::Download)
		beginDownloadFile();
	else if (_preparedAction == OnlineDataNode::NewFolder)
		beginNewFolder(_preparedData);
	else
		return false;

	return true;
}

bool OnlineDataNode::processAction(Action action, const QString &data)
{
	prepareAction(action, data);
	initialise();
}
