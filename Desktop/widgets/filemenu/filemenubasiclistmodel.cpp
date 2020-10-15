#include "filemenubasiclistmodel.h"
#include <QFileInfo>
#include <QDir>
#include <QTimer>
#include "log.h"
#include "qquick/jasptheme.h"

FileMenuBasicListModel::FileMenuBasicListModel(QObject *parent, FileSystem * model) : QAbstractListModel(parent), _model(model)
{

}

int FileMenuBasicListModel::rowCount(const QModelIndex &parent) const
{
	// For list models only the root node (an invalid parent) should return the list's size. For all
	// other (valid) parents, rowCount() should return 0 so that it does not become a tree model.

	if (parent.isValid())
		return 0;

	return _model->entries().count();

}

QVariant FileMenuBasicListModel::data(const QModelIndex &index, int role) const
{
	if (!index.isValid())
		return QVariant();

	auto fileEntryList	= _model->entries();
	auto item			= fileEntryList[index.row()];


	switch (role)
	{
	case NameRole:					return item.name;
	case PathRole:					return item.path;
	case DescriptionRole:			return item.description;
	case TypeRole:					return item.entryType;
	case AssociatedDataFileRole:	return QFileInfo(item.associatedDataFile).fileName();
	case IconSourceRole:			return JaspTheme::currentIconPath() + FileSystemEntry::sourcesIcons()[item.entryType];
	case DataIconSourceRole:		return JaspTheme::currentIconPath() + FileSystemEntry::sourcesIcons()[FileSystemEntry::CSV];
	case DirRole:
	{
		if (QFileInfo(item.path).path().toLower().startsWith("http:") || QFileInfo(item.path).path().toLower().startsWith("https:"))
			return QFileInfo (item.path).path();
		else
		{
			QString location = QDir::toNativeSeparators(QFileInfo (item.path).path()) ;
			while (location.endsWith(QDir::separator())) location.chop(1);
			return location + QDir::separator();
		}
	}
	case ActionRole:				return _openFileWhenClicked ? "open" : "sync";
	default:						return QStringLiteral("Me know nothing");
	}

}

bool FileMenuBasicListModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
	auto fileEntryList = _model->entries();

	if (index.row() < 0 || index.row() >= fileEntryList.count())
		return false;

	if (data(index, role) != value)
	{
		auto item = fileEntryList[index.row()];
		switch (role)
		{
		case NameRole:					item.name					= value.toString();									break;
		case PathRole:					item.path					= value.toString();									break;
		case DescriptionRole:			item.description			= value.toString();									break;
		case TypeRole:					item.entryType				= static_cast<FileSystemEntry::EntryType> (value.toInt());	break;
		case AssociatedDataFileRole:	item.associatedDataFile	= value.toString();									break;
		case IconSourceRole:			//Do nothing
		case DataIconSourceRole:
		case DirRole:					break;
		}

		emit dataChanged(index, index, QVector<int>() << role);
		return true;
	}

	return false;

}

Qt::ItemFlags FileMenuBasicListModel::flags(const QModelIndex &index) const
{
	if (!index.isValid())
		return Qt::NoItemFlags;

	return Qt::ItemIsEditable; // FIXME: Implement me! ??? Why would we want to edit this?
}

void FileMenuBasicListModel::changePath(const QString& name, const QString& path)
{
	Log::log() << "Override basicListModel::changePath!" << std::endl;
}

void FileMenuBasicListModel::changePathCrumbIndex(const int& index)
{
	Log::log() << "Override basicListModel::changePathCrumbIndex!" << std::endl;
}

void FileMenuBasicListModel::openFile(const QString& path)
{
	Log::log() << "Override basicListModel::openFile!" << std::endl;
}

void FileMenuBasicListModel::saveFile(const QString& path)
{
	Log::log() << "Override basicListModel::saveFile!" << std::endl;
}

QMutex FileMenuBasicListModel::_opening;

bool FileMenuBasicListModel::mayOpen()
{
	if (_opening.tryLock())
	{
		QTimer::singleShot(500, this, &FileMenuBasicListModel::resetOpening);
		return true;
	}
	else
		return false;
}

void FileMenuBasicListModel::resetOpening()
{
	_opening.unlock();
}

