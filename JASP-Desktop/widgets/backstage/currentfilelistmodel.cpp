#include "currentfilelistmodel.h"
#include "fsentrywidget.h"
#include <QFileInfo>
#include <QDir>

CurrentFileListModel::CurrentFileListModel(QObject *parent)
	: QAbstractListModel(parent)
{
	_fsbmCurrentFile = new FSBMCurrentFile(this);
	_fsbmCurrentFile->refresh();
	_iconsources = FSEntryWidget::sourcesIcons();
	
	connect(this, SIGNAL(syncFile(FileEvent *)), parent, SLOT(syncFile(FileEvent *)));
}

int CurrentFileListModel::rowCount(const QModelIndex &parent) const
{
	// For list models only the root node (an invalid parent) should return the list's size. For all
	// other (valid) parents, rowCount() should return 0 so that it does not become a tree model.
	if (parent.isValid())
		return 0;
	
	return  _fsbmCurrentFile->entries().count();
}

QVariant CurrentFileListModel::data(const QModelIndex &index, int role) const
{
	if (!index.isValid())
		return QVariant();
	
	//Get the FileSystemEntryList
	FSBMCurrentFile::FileSystemEntryList fileEntryList = _fsbmCurrentFile->entries();
	
	//Get the FileEntry
	FSEntry item = fileEntryList[index.row()];
	
	switch (role)
	{
	case NameRole:
		return QVariant(item.name);
	case PathRole:
		return QVariant(item.path);
	case FolderRole:
		{QFileInfo  fi(item.path);
		return QVariant(fi.path() + QDir::separator());}
	case TypeRole:
		return QVariant(item.entryType);
	case IconSourceRole:
		return QVariant("qrc"+_iconsources[item.entryType]);
	default:
		return QVariant(QStringLiteral("Unknown role"));
	}
}

bool CurrentFileListModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
	//Set and fill the FileSystemEntryList
	FSBMCurrentFile::FileSystemEntryList fileEntryList = _fsbmCurrentFile->entries();
	
	if (index.row() < 0 || index.row() >= fileEntryList.count())
		return false;
	
	if (data(index, role) != value)
	{
		//Get the FileEntry
		FSEntry item = fileEntryList[index.row()];
		switch (role)
		{
		case NameRole:
			item.name = value.toString();
			break;
		case PathRole:
			item.path = value.toString();
			break;
		case TypeRole:
			item.entryType = static_cast<FSEntry::EntryType> (value.toInt());
			break;
		case IconSourceRole: //No need for some change
		case FolderRole :
			break;
		}
		
		emit dataChanged(index, index, QVector<int>() << role);
		return true;
	}
	 return false;	

}

Qt::ItemFlags CurrentFileListModel::flags(const QModelIndex &index) const
{
	if (!index.isValid())
		return Qt::NoItemFlags;
	
	return Qt::ItemIsEditable; // FIXME: Implement me!
}	

QHash<int, QByteArray> CurrentFileListModel::roleNames() const
{
	QHash<int, QByteArray> names;
	names[NameRole] = "name";
	names[PathRole] = "path";
	names[FolderRole] = "folder";
	names[TypeRole] ="type";
	names[IconSourceRole] ="iconsource";
	return names;
}

FSBMCurrentFile *CurrentFileListModel::getCurrentFileFSBModel()
{
	return _fsbmCurrentFile;
}

void CurrentFileListModel::setCurrentFilePath(const QString &newcurrent)
{
	beginResetModel();
	
	_fsbmCurrentFile->setCurrent(newcurrent);
	_fsbmCurrentFile->refresh();
	
	endResetModel();	
}

void CurrentFileListModel::syncFile(const QString &path)
{
	FileEvent *event = new FileEvent(this->parent(), FileEvent::FileSyncData);
	event->setPath(path);

	emit syncFile(event);
}
