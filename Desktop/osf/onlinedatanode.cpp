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
	switch(_preparedAction)
	{
	case OnlineDataNode::NewFile:		return getUploadPath(_preparedData);
	case OnlineDataNode::NewFolder:		return getNewFolderPath(_preparedData);
	case OnlineDataNode::Upload:		return getUploadPath();
	case OnlineDataNode::Download:		return getDownloadPath();
	default:							return "";
	}
}

bool OnlineDataNode::beginAction() {

	if (_actionFilter == NULL || _actionFilter->call(this, _actionFilter) == true)
	{
		switch(_preparedAction)
		{
		case OnlineDataNode::Upload:	return beginUploadFile();
		case OnlineDataNode::NewFile:	return beginUploadFile(_preparedData);
		case OnlineDataNode::Download:	return beginDownloadFile();
		case OnlineDataNode::NewFolder:	return beginNewFolder(_preparedData);
		default:						return false;
		}
	}

	return false;
}

bool OnlineDataNode::processAction(Action action, const QString &data)
{
	prepareAction(action, data);
	initialise();

	return true;
}
