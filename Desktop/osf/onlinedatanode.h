#ifndef ONLINEDATANODE_H
#define ONLINEDATANODE_H

#include <QString>
#include <QObject>
#include <QNetworkAccessManager>
#include <QFile>
#include "onlinenode.h"
#include "common.h"

///
/// Represents a single node in a directory/file structure on a server
class OnlineDataNode: public OnlineNode
{
	Q_OBJECT

public:

	enum Kind {Unknown, File, Folder};
	enum Action {None, NewFile, NewFolder, Upload, Download};

	class ActionFilter {

	public:

		ActionFilter(bool (*callback)(OnlineDataNode*, OnlineDataNode::ActionFilter*), QVariant _arg1, QVariant _arg2, QVariant _arg3)
		{
			call = callback;
			arg1 = _arg1;
			arg2 = _arg2;
			arg3 = _arg3;
		}

		ActionFilter(bool (*callback)(OnlineDataNode*, OnlineDataNode::ActionFilter*), QVariant _arg1, QVariant _arg2)
		{
			call = callback;
			arg1 = _arg1;
			arg2 = _arg2;
		}

		ActionFilter(bool (*callback)(OnlineDataNode*, OnlineDataNode::ActionFilter*), QVariant _arg1)
		{
			call = callback;
			arg1 = _arg1;
		}

		ActionFilter(bool (*callback)(OnlineDataNode*, OnlineDataNode::ActionFilter*))
		{
			call = callback;
		}

		bool (*call)(OnlineDataNode*, OnlineDataNode::ActionFilter*);

		QVariant arg1;
		QVariant arg2;
		QVariant arg3;
	};

	OnlineDataNode(QString localPath, QNetworkAccessManager *manager, QString id, QObject *parent = 0);

	virtual QString getUploadPath() const;
	virtual QString getUploadPath(QString filename) const = 0;
	virtual QString getDownloadPath() const;
	virtual QString getNewFolderPath(QString folderName) const = 0;
	virtual QString getDeletePath() const;

	virtual bool beginDownloadFile() = 0;
	virtual bool beginUploadFile() = 0;
	virtual bool beginUploadFile(QString name) = 0;
	virtual bool beginNewFolder(QString name) = 0;

	virtual bool beginAction() OVERRIDE;

	bool processAction(Action action, const QString &data);

	QString name() const;
	bool kind() const;
	QString md5() const;

	void prepareAction(Action action, const QString &data);

	void setActionFilter(ActionFilter *filter);
	ActionFilter* actionFilter();
	void deleteActionFilter();

	virtual QString getActionPath() const OVERRIDE;

	QString getLocalPath();

	bool exists();

protected:
	QString _uploadPath;
	QString _downloadPath;
	QString _newFolderPath;
	QString _deletePath;
	QString _md5;
	
	QString _name;
	OnlineDataNode::Kind _kind;

	Action _preparedAction = OnlineDataNode::None;
	QString _preparedData;
	ActionFilter *_actionFilter = NULL;

	QString _localPath;
	QFile _localFile;

	bool _exists = true;

};

#endif // ONLINEDATANODE_H
