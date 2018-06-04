#include "datalibrarylistmodel.h"
#include "fsentrywidget.h"
#include <QtDebug>
#include <QFileInfo>
#include <QDir>

DataLibraryListModel::DataLibraryListModel(QObject *parent)
	: QAbstractListModel(parent)
{
	_fsbmExampleModel = new FSBMExamples(this,  FSBMExamples::rootelementname );
	_fsbmExampleModel->refresh();
	_iconsources = FSEntryWidget::sourcesIcons();

	connect(this, SIGNAL(openFile(FileEvent *)), parent, SLOT(openFile(FileEvent *)));

}

int DataLibraryListModel::rowCount(const QModelIndex &parent) const
{
	// For list models only the root node (an invalid parent) should return the list's size. For all
	// other (valid) parents, rowCount() should return 0 so that it does not become a tree model.

	if (parent.isValid())
		return 0;

	return _fsbmExampleModel->entries().count();

}

QVariant DataLibraryListModel::data(const QModelIndex &index, int role) const
{
	if (!index.isValid())
		return QVariant();

	//Set and fill the FileSystemEntryList
	FSBMExamples::FileSystemExtendedEntryList fileEntryList = _fsbmExampleModel->entries();

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
	FSBMExamples::FileSystemExtendedEntryList fileEntryList = _fsbmExampleModel->entries();

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

void DataLibraryListModel::setDataLibraryBreadCrumbsModel(DataLibraryBreadCrumbsModel *dataLibraryBreadCrumbsModel)
{
	_dataLibraryBreadCrumbsModel = dataLibraryBreadCrumbsModel;
}

void DataLibraryListModel::changePath(const QString &name, const QString &path)
{
	beginResetModel();	
	_dataLibraryBreadCrumbsModel->appendCrumb(name , path);
	
	_fsbmExampleModel->setPath(path);
	_fsbmExampleModel->refresh();

	endResetModel();
}

void DataLibraryListModel::changePath(const int &index)
{
	beginResetModel();	
	
	QString path;
	
	path = _dataLibraryBreadCrumbsModel->switchCrumb(index);
	
	_fsbmExampleModel->setPath(path);
	_fsbmExampleModel->refresh();
	
	endResetModel();
	
}

void DataLibraryListModel::openFile(const QString &path)
{
	FileEvent *event = new FileEvent(this->parent(), FileEvent::FileOpen);
	event->setPath(path);
	event->setReadOnly();

	emit openFile(event);
}
