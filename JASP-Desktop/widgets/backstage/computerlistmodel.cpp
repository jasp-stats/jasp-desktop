#include "computerlistmodel.h"
#include "fsentrywidget.h"
#include <QFileInfo>
#include <QDir>

ComputerListModel::ComputerListModel(QObject *parent)
	: QAbstractListModel(parent)
{
	_fsbmRecentFoilders = new FSBMRecentFolders();
	_fsbmRecentFoilders->refresh();
	_iconsources = FSEntryWidget::sourcesIcons();
	
}

int ComputerListModel::rowCount(const QModelIndex &parent) const
{
	// For list models only the root node (an invalid parent) should return the list's size. For all
	// other (valid) parents, rowCount() should return 0 so that it does not become a tree model.
	if (parent.isValid())
		return 0;
	
	return _fsbmRecentFoilders->entries().count();
}

QVariant ComputerListModel::data(const QModelIndex &index, int role) const
{
	if (!index.isValid())
		return QVariant();
	
	//Get the FileSystemEntryList
	FSBMRecentFolders::FileSystemEntryList fileEntryList = _fsbmRecentFoilders->entries();

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

QHash<int, QByteArray> ComputerListModel::roleNames() const
{
	QHash<int, QByteArray> names;
	names[NameRole] = "name";
	names[PathRole] = "path";
	names[FolderRole] = "folder";
	names[TypeRole] ="type";
	names[IconSourceRole] ="iconsource";
	return names;
}

QString ComputerListModel::getMostRecent()
{
	return _fsbmRecentFoilders->mostRecent();
}

void ComputerListModel::addRecentFolder(const QString &newpath)
{
	beginResetModel();
	
	_fsbmRecentFoilders->addRecent(newpath);
	_fsbmRecentFoilders->refresh();
	
	endResetModel();
}

void ComputerListModel::refresh()
{
	beginResetModel();
	
	_fsbmRecentFoilders->refresh();
	
	endResetModel();
}

//Slots

