#include "recentfileslistmodel.h"
#include "fsentrywidget.h"
#include <QFileInfo>
#include <QDir>

RecentFilesListModel::RecentFilesListModel(QObject *parent)
		: QAbstractListModel(parent)
{
	_fsbmRecentFiles = new FSBMRecentFiles(this);
	_fsbmRecentFiles->refresh();
	_iconsources = FSEntryWidget::sourcesIcons();

	connect(this, SIGNAL(openFile(FileEvent *)), parent, SLOT(openFile(FileEvent *)));

}

int RecentFilesListModel::rowCount(const QModelIndex &parent) const
{
	// For list models only the root node (an invalid parent) should return the list's size. For all
	// other (valid) parents, rowCount() should return 0 so that it does not become a tree model.

	if (parent.isValid())
		return 0;

	return _fsbmRecentFiles->entries().count();
}

QVariant RecentFilesListModel::data(const QModelIndex &index, int role) const
{
	if (!index.isValid())
		return QVariant();

	//Get the FileSystemEntryList
	FSBMRecentFiles::FileSystemEntryList fileEntryList = _fsbmRecentFiles->entries();

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

bool RecentFilesListModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
	//Set and fill the FileSystemEntryList
	FSBMRecentFiles::FileSystemEntryList fileEntryList = _fsbmRecentFiles->entries();

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

Qt::ItemFlags RecentFilesListModel::flags(const QModelIndex &index) const
{
	if (!index.isValid())
		return Qt::NoItemFlags;

	return Qt::ItemIsEditable; // FIXME: Implement me!
}

QHash<int, QByteArray> RecentFilesListModel::roleNames() const
{
	QHash<int, QByteArray> names;
	names[NameRole] = "name";
	names[PathRole] = "path";
	names[FolderRole] = "folder";
	names[TypeRole] ="type";
	names[IconSourceRole] ="iconsource";
	return names;
}

void RecentFilesListModel::addRecentFilePath(const QString &newpath)
{
	
	beginResetModel();
	
	_fsbmRecentFiles->addRecent(newpath);
	_fsbmRecentFiles->refresh();
	
	endResetModel();
	
}

//Slots
void RecentFilesListModel::openFile(const QString &path)
{
	FileEvent *event = new FileEvent(this->parent(), FileEvent::FileOpen);
	event->setPath(path);

	emit openFile(event);
}
