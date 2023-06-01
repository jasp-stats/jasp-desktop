#include "datalibrarybreadcrumbsmodel.h"
#include "datalibraryfilesystem.h"

#define CAT "Categories"

DataLibraryBreadCrumbsListModel::DataLibraryBreadCrumbsListModel(QObject *parent, const QChar sep)
	: QAbstractListModel(parent), _separator(sep)
{	
	_crumbNameList.append(getTranslaterRootElement());
	_physicalPathList.append(DataLibraryFileSystem::rootelementname);
}

int DataLibraryBreadCrumbsListModel::rowCount(const QModelIndex &parent) const
{
	// For list models only the root node (an invalid parent) should return the list's size. For all
	// other (valid) parents, rowCount() should return 0 so that it does not become a tree model.
	if (parent.isValid())
		return 0;
	
	return parent.isValid() ? 0 : _crumbNameList.count();
	
}

QVariant DataLibraryBreadCrumbsListModel::data(const QModelIndex &index, int role) const
{
	
	if (!index.isValid())
		return QVariant();
	
	if (index.row() < 0 || index.row() >= _crumbNameList.count())
        return QVariant();
	
    switch (role) {
        case NameRole:
            return _crumbNameList[index.row()];
         default:
            return QVariant();
    }
	
}

bool DataLibraryBreadCrumbsListModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if (index.row() < 0 || index.row() >= _crumbNameList.count())
        return false;
	
	if (data(index, role) != value) {
		_crumbNameList[index.row()] = value.toString();
		emit dataChanged(index, index, QVector<int>() << role);
		return true;
	}
	return false;
	
}

Qt::ItemFlags DataLibraryBreadCrumbsListModel::flags(const QModelIndex &index) const
{
	if (!index.isValid())
		return Qt::NoItemFlags;
	
	return Qt::ItemIsEditable; // FIXME: Implement me!
}

QHash<int, QByteArray> DataLibraryBreadCrumbsListModel::roleNames() const
{
	
	QHash<int, QByteArray> names;
    names.insert(NameRole, "name");
    return names;
	
}

void DataLibraryBreadCrumbsListModel::setSeparator(const QChar &separator)
{
	_separator = separator;
}

void DataLibraryBreadCrumbsListModel::appendCrumb(const QString &crumbname, const QString &path)
{
	const int lastRowIndex = _crumbNameList.size();
    
	beginInsertRows(QModelIndex(), lastRowIndex, lastRowIndex);
	_crumbNameList.append(crumbname);
	_physicalPathList.append(path);
	endInsertRows();
}

QString DataLibraryBreadCrumbsListModel::switchCrumb(const int &index)
{
	removeCrumbsAfterIndex(index+1);
	return _physicalPathList.at(index);
}

void DataLibraryBreadCrumbsListModel::refresh()
{
	beginResetModel();

	_crumbNameList.clear();
	_crumbNameList.append(getTranslaterRootElement());
	switchCrumb(0);
	emit crumbIndexChanged(0);

	endResetModel();

}


bool DataLibraryBreadCrumbsListModel::removeCrumbsAfterIndex(int index)
{
	const int lastRowIndex = _crumbNameList.size() - 1;	
	
	if (index > lastRowIndex )
		return false;
	
	beginRemoveRows(QModelIndex(), index, lastRowIndex);
	for (int ix = lastRowIndex; ix >= index ; --ix )
	{
		_crumbNameList.removeLast();
		_physicalPathList.removeLast();
	}
    endRemoveRows();
	
	return true;
	
}

QString DataLibraryBreadCrumbsListModel::getTranslaterRootElement()
{
	return tr("Categories");
}

void DataLibraryBreadCrumbsListModel::indexChanged(const int &index)
{
	emit crumbIndexChanged(index);
}







