#include "datalibrarybreadcrumbsmodel.h"
#include "fsbmexamples.h"
#include <QMessageBox>

DataLibraryBreadCrumbsModel::DataLibraryBreadCrumbsModel(QObject *parent)
	: QAbstractListModel(parent)
{	
	_pathList.append(FSBMExamples::rootelementname);	
}

int DataLibraryBreadCrumbsModel::rowCount(const QModelIndex &parent) const
{
	// For list models only the root node (an invalid parent) should return the list's size. For all
	// other (valid) parents, rowCount() should return 0 so that it does not become a tree model.
	if (parent.isValid())
		return 0;
	
	return parent.isValid() ? 0 : _pathList.count();
	
}

QVariant DataLibraryBreadCrumbsModel::data(const QModelIndex &index, int role) const
{
	
	if (!index.isValid())
		return QVariant();
	
	if (index.row() < 0 || index.row() >= _pathList.count())
        return QVariant();
	
    switch (role) {
        case NameRole:
            return _pathList[index.row()];
         default:
            return QVariant();
    }
	
}

bool DataLibraryBreadCrumbsModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if (index.row() < 0 || index.row() >= _pathList.count())
        return false;
	
	if (data(index, role) != value) {
		_pathList[index.row()] = value.toString();
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

void DataLibraryBreadCrumbsModel::appendCrumb(QString crumb)
{
	const int lastRowIndex = _pathList.size();
    
	beginInsertRows(QModelIndex(), lastRowIndex, lastRowIndex);
	_pathList.append(crumb);
	endInsertRows();
}

QString DataLibraryBreadCrumbsModel::changeCrumb(QString crumb)
{
	//Set the latest breadcrumb to crumb and remove others if necessary.
	//If crumb is not in the list add this one.
	
	QString path = "";
	
	//Check if crumb is in the list, we expect no doubles
	int index = _pathList.indexOf(crumb);
	
	if (index == -1) //crumb not found
		appendCrumb(crumb);	
	else
	{
		if (index+1 < _pathList.count())
			removeCrumbsAfterIndex(index+1);
	}
	
	path = _pathList.join(_separator);
	return path;
	
}

bool DataLibraryBreadCrumbsModel::removeCrumbsAfterIndex(int index)
{
	const int lastRowIndex = _pathList.size() - 1;	
	
	if (index > lastRowIndex )
		return false;
	
	beginRemoveRows(QModelIndex(), index, lastRowIndex);
	for (int ix = lastRowIndex; ix >= index ; --ix )
		_pathList.removeLast();
    endRemoveRows();
	
	return true;
	
}


void DataLibraryBreadCrumbsModel::removeLastCrumb()
{
	if (_pathList.isEmpty())
        return;
	
    const int lastRowIndex = (_pathList.size() - 1);

	beginRemoveRows(QModelIndex(), lastRowIndex, lastRowIndex);
    _pathList.removeLast();
    endRemoveRows();
	
}

void DataLibraryBreadCrumbsModel::handleCrumbClicked(int index)
{
//	QMessageBox msgBox;	
//	QString txt = "Index : " + QString::number(index) + " belonging crumb :" + _dataList[index];
//	msgBox.setText(txt);
//	msgBox.setInformativeText(txt);
//	msgBox.setStandardButtons(QMessageBox::Ok);
//	msgBox.exec();
}

void DataLibraryBreadCrumbsModel::setSeperator(const QChar &separator)
{
	_separator = separator;
}






