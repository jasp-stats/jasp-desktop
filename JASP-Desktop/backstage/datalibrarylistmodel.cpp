#include "datalibrarylistmodel.h"
#include "fsentrywidget.h"
#include <QFileInfo>
#include <QDir>

DataLibraryListModel::DataLibraryListModel(QObject *parent)
	: QAbstractListModel(parent)
{
	_fsbmDataLibrary = new FSBMDataLibrary(this,  FSBMDataLibrary::rootelementname );
	_fsbmDataLibrary->refresh();
	_iconsources = FSEntryWidget::sourcesIcons();

	connect(this, SIGNAL(openFile(FileEvent *)), parent, SLOT(openFile(FileEvent *)));	//connect(_dataLibraryBreadCrumbsListModel, SIGNAL(indexChanged(const int &)), this, SLOT(changePath(const int &)));
}

int DataLibraryListModel::rowCount(const QModelIndex &parent) const
{
	// For list models only the root node (an invalid parent) should return the list's size. For all
	// other (valid) parents, rowCount() should return 0 so that it does not become a tree model.

	if (parent.isValid())
		return 0;

	return _fsbmDataLibrary->entries().count();

}

QVariant DataLibraryListModel::data(const QModelIndex &index, int role) const
{
	if (!index.isValid())
		return QVariant();

	//Set and fill the FileSystemEntryList
	FSBMDataLibrary::FileSystemExtendedEntryList fileEntryList = _fsbmDataLibrary->entries();

	//Get the FileEntry
	ExtendedFSEntry item = fileEntryList[index.row()];
	//QFileInfo  fi(item.associated_datafile);

	switch (role)
	{
	case NameRole:
		return QVariant(item.name);
	case PathRole:
		return QVariant(item.path);
	case DescriptionRole:
		return QVariant(item.description);
	case TypeRole:
		return QVariant(item.entryType);
	case AssociatedDataFileRole:
		{QFileInfo  fi(item.associated_datafile);
		return QVariant(fi.fileName());}
	case IconSourceRole:
		return QVariant("qrc"+_iconsources[item.entryType]);
	case DataIconSourceRole:
		return QVariant("qrc"+_iconsources[FSEntry::CSV]);		
	case DirRole:
		{QFileInfo  fi(item.associated_datafile);
		return QVariant(fi.path() + QDir::separator());}
	default:
		return QVariant(QStringLiteral("Me know nothing"));
	}

}

bool DataLibraryListModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
	//Get the FileSystemEntryList
	FSBMDataLibrary::FileSystemExtendedEntryList fileEntryList = _fsbmDataLibrary->entries();

	if (index.row() < 0 || index.row() >= fileEntryList.count())
        return false;

	if (data(index, role) != value)
	{
		//Get the FileEntry
		ExtendedFSEntry item = fileEntryList[index.row()];
		switch (role)
		{
		case NameRole:
			item.name = value.toString();
			break;
		case PathRole:
			item.path = value.toString();
			break;
		case DescriptionRole:
			item.description = value.toString();
			break;
		case TypeRole:
			item.entryType = static_cast<FSEntry::EntryType> (value.toInt());
			break;
		case AssociatedDataFileRole:
			item.associated_datafile = value.toString();
			break;
		case IconSourceRole: //No need for some change
		case DataIconSourceRole: 
		case DirRole:
			break;
		}

		emit dataChanged(index, index, QVector<int>() << role);
		return true;
	}

	return false;

}

Qt::ItemFlags DataLibraryListModel::flags(const QModelIndex &index) const
{
	if (!index.isValid())
		return Qt::NoItemFlags;

	return Qt::ItemIsEditable; // FIXME: Implement me!
}

QHash<int, QByteArray> DataLibraryListModel::roleNames() const
{
	QHash<int, QByteArray> names;
	names[NameRole] = "name";
	names[DescriptionRole] ="description";
	names[PathRole] = "path";
	names[TypeRole] ="type";
	names[AssociatedDataFileRole] ="associated_datafile";
	names[IconSourceRole] ="iconsource";
	names[DataIconSourceRole] ="dataiconsource";
	names[DirRole] ="dirpath";
	return names;
}

void DataLibraryListModel::setBreadCrumbsListModel(DataLibraryBreadCrumbsListModel *dataLibraryBreadCrumbsModel)
{
	_dataLibraryBreadCrumbsListModel = dataLibraryBreadCrumbsModel;
}


void DataLibraryListModel::changePath(const QString &name, const QString &path)
{
	// Called from datalibrarylist
	beginResetModel();	
	_dataLibraryBreadCrumbsListModel->appendCrumb(name , path);
	
	_fsbmDataLibrary->setPath(path);
	_fsbmDataLibrary->refresh();

	endResetModel();
}

void DataLibraryListModel::changePath(const int &index)
{
	// Called from breadcrumbs
	beginResetModel();	
	
	QString path;
	
	path = _dataLibraryBreadCrumbsListModel->switchCrumb(index);
	
	_fsbmDataLibrary->setPath(path);
	_fsbmDataLibrary->refresh();
	
	endResetModel();
	
}

void DataLibraryListModel::openFile(const QString &path)
{
	FileEvent *event = new FileEvent(this->parent(), FileEvent::FileOpen);
	event->setPath(path);
	event->setReadOnly();

	emit openFile(event);
}
