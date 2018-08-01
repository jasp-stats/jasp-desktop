#include "datalibrarybreadcrumbsmodel.h"
#include "fsbmexamples.h"
#include <QMessageBox>

DataLibraryBreadCrumbsModel::DataLibraryBreadCrumbsModel(QObject *parent)
	: QAbstractListModel(parent)
{	
	_crumbNameList.append(FSBMExamples::rootelementname);
	_physicalPathList.append(FSBMExamples::rootelementname);
}

int DataLibraryBreadCrumbsModel::rowCount(const QModelIndex &parent) const
{
	// For list models only the root node (an invalid parent) should return the list's size. For all
	// other (valid) parents, rowCount() should return 0 so that it does not become a tree model.
	if (parent.isValid())
		return 0;
	
	return parent.isValid() ? 0 : _crumbNameList.count();
	
}

QVariant DataLibraryBreadCrumbsModel::data(const QModelIndex &index, int role) const
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

bool DataLibraryBreadCrumbsModel::setData(const QModelIndex &index, const QVariant &value, int role)
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

Qt::ItemFlags DataLibraryBreadCrumbsModel::flags(const QModelIndex &index) const
{
	if (!index.isValid())
		return Qt::NoItemFlags;
	
	return Qt::ItemIsEditable; // FIXME: Implement me!
}

QHash<int, QByteArray> DataLibraryBreadCrumbsModel::roleNames() const
{
	
	QHash<int, QByteArray> names;
    names.insert(NameRole, "name");
    return names;
	
}

void DataLibraryBreadCrumbsModel::appendCrumb(const QString &crumbname, const QString &path)
{
	const int lastRowIndex = _crumbNameList.size();
    
	beginInsertRows(QModelIndex(), lastRowIndex, lastRowIndex);
	_crumbNameList.append(crumbname);
	_physicalPathList.append(path);
	endInsertRows();
}

QString DataLibraryBreadCrumbsModel::switchCrumb(const int &index)
{
	removeCrumbsAfterIndex(index+1);
	return _physicalPathList.at(index);
}


bool DataLibraryBreadCrumbsModel::removeCrumbsAfterIndex(int index)
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


void DataLibraryBreadCrumbsModel::removeLastCrumb()
{
	if (_crumbNameList.isEmpty())
        return;
	
    const int lastRowIndex = (_crumbNameList.size() - 1);

	beginRemoveRows(QModelIndex(), lastRowIndex, lastRowIndex);
    _crumbNameList.removeLast();
    endRemoveRows();
	
}

void DataLibraryBreadCrumbsModel::setSeperator(const QChar &separator)
{
	_separator = separator;
}






