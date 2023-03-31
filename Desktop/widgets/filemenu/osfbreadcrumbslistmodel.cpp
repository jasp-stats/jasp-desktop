#include "osfbreadcrumbslistmodel.h"
#include "osffilesystem.h"

OSFBreadCrumbsListModel::OSFBreadCrumbsListModel(QObject *parent, const QChar separator)
	: QAbstractListModel(parent), _separator(separator)
{
	_crumbNameList.append(OSFFileSystem::rootelementname);
	_physicalPathList.append(OSFFileSystem::rootelementname);
}


int OSFBreadCrumbsListModel::rowCount(const QModelIndex &parent) const
{
	// For list models only the root node (an invalid parent) should return the list's size. For all
	// other (valid) parents, rowCount() should return 0 so that it does not become a tree model.
	if (parent.isValid())
		return 0;
	
	return parent.isValid() ? 0 : _crumbNameList.count();
}

QVariant OSFBreadCrumbsListModel::data(const QModelIndex &index, int role) const
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

QHash<int, QByteArray> OSFBreadCrumbsListModel::roleNames() const
{
	QHash<int, QByteArray> names;
    names.insert(NameRole, "name");
    return names;
	
}

void OSFBreadCrumbsListModel::setSeparator(const QChar &separator)
{
	_separator = separator;
}

void OSFBreadCrumbsListModel::appendCrumb(const QString &crumbname, const QString &path)
{
	const int lastRowIndex = _crumbNameList.size();
	
	beginInsertRows(QModelIndex(), lastRowIndex, lastRowIndex);
	_crumbNameList.append(crumbname);
	_physicalPathList.append(path);
	endInsertRows();
}

QString OSFBreadCrumbsListModel::switchCrumb(const int &index)
{
	removeCrumbsAfterIndex(index+1);
	return _physicalPathList.at(index);
}

bool OSFBreadCrumbsListModel::removeCrumbsAfterIndex(int index)
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

void OSFBreadCrumbsListModel::indexChanged(const int &index)
{
	emit crumbIndexChanged(index);
}


