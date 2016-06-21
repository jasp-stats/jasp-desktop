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

void OnlineDataNode::setActionFilter(ActionFilter *filter)
{
	_actionFilter = filter;
}

OnlineDataNode::ActionFilter* OnlineDataNode::actionFilter()
{
	return _actionFilter;
}

void OnlineDataNode::deleteActionFilter()
{
	if (_actionFilter != NULL)
	{
		delete _actionFilter;
		_actionFilter = NULL;
	}
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

QString OnlineDataNode::md5() const
{
	return _md5;
}

bool OnlineDataNode::exists()
{
	return _exists && _inited;
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

	if (_actionFilter == NULL || _actionFilter->call(this, _actionFilter) == true)
	{
		if (_preparedAction == OnlineDataNode::Upload)
			return beginUploadFile();
		else if (_preparedAction == OnlineDataNode::NewFile)
			return beginUploadFile(_preparedData);
		else if (_preparedAction == OnlineDataNode::Download)
			return beginDownloadFile();
		else if (_preparedAction == OnlineDataNode::NewFolder)
			return beginNewFolder(_preparedData);
	}

	return false;
}

bool OnlineDataNode::processAction(Action action, const QString &data)
{
	prepareAction(action, data);
	initialise();

	return true;
}
