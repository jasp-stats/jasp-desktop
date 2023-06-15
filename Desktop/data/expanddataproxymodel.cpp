#include "expanddataproxymodel.h"
#include "datasettablemodel.h"

ExpandDataProxyModel::ExpandDataProxyModel(QObject *parent)
	: QObject{parent}
{
}

int ExpandDataProxyModel::rowCount(bool includeVirtuals) const
{
	if (!_sourceModel)
		return 0;
	return _sourceModel->rowCount() + (includeVirtuals && _expandDataSet ? EXTRA_ROWS : 0);
}

int ExpandDataProxyModel::columnCount(bool includeVirtuals) const
{
	if (!_sourceModel)
		return 0;
	return _sourceModel->columnCount() + (includeVirtuals && _expandDataSet ? EXTRA_COLS : 0);
}

QVariant ExpandDataProxyModel::data(int row, int col, int role) const
{
	if (!_sourceModel || role == -1) // Role not defined
		return QVariant();

	if (col < _sourceModel->columnCount() && row < _sourceModel->rowCount())
		return _sourceModel->data(_sourceModel->index(row, col), role);

	if (role == getRole("selected"))
		return false;
	else if (role == getRole("lines"))
	{
		if (col == columnCount() - 1)
			col = _sourceModel->columnCount() - 1;
		else if (col >= _sourceModel->columnCount())
			col = _sourceModel->columnCount() - 2;
		if (col < 0) col = 0;
		if (row == rowCount() - 1)
			row = _sourceModel->rowCount() - 1;
		else if (row >= _sourceModel->rowCount())
			row = _sourceModel->rowCount() - 2;
		if (row < 0) row = 0;

		return _sourceModel->data(_sourceModel->index(row, col), role);
	}
	else if (role == getRole("value"))
		return "";
	else if (role == getRole("itemInputValue"))
		return "string";

	return QVariant();
}

QVariant ExpandDataProxyModel::headerData(int section, Qt::Orientation orientation, int role) const
{
	if (!_sourceModel || role == -1) // Role not defined
		return QVariant();

	if (orientation == Qt::Orientation::Horizontal)
	{
		if (section < _sourceModel->columnCount())
			return _sourceModel->headerData(section, orientation, role);
		else
		{
			if (role == getRole("columnIsComputed"))
				return false;
			else if (role == getRole("computedColumnIsInvalidated"))
				return false;
			else if (role == getRole("filter"))
				return false;
			else if (role == getRole("computedColumnError"))
				return "";
			else if (role == getRole("columnType"))
				return int(columnType::unknown);
			else if (role == getRole("maxColString"))
				return "XXXXXXXXXXX";
			else if (role == Qt::DisplayRole)
				return "";
		}
	}
	else if (orientation == Qt::Orientation::Vertical)
	{
		if (section < _sourceModel->rowCount())
			return _sourceModel->headerData(section, orientation, role);
		else
			return section + 1;
	}

	return QVariant();
}

Qt::ItemFlags ExpandDataProxyModel::flags(int row, int column) const
{
	if (!_sourceModel)
		return Qt::NoItemFlags;

	if (column < _sourceModel->columnCount() && row < _sourceModel->rowCount())
		return _sourceModel->flags(index(row, column));

	return Qt::ItemIsSelectable | Qt::ItemIsEnabled | Qt::ItemIsEditable;
}

QModelIndex ExpandDataProxyModel::index(int row, int column, const QModelIndex &parent) const
{
	if (!_sourceModel)
		return QModelIndex();

	return _sourceModel->index(row, column, parent);
}

bool ExpandDataProxyModel::filtered(int row, int column) const
{
	if (!_sourceModel)
		return false;

	if (column < _sourceModel->columnCount() && row < _sourceModel->rowCount())
	{
		QModelIndex ind(_sourceModel->index(row, column));
		return _sourceModel->data(ind, getRole("filter")).toBool();
	}

	return true;
}

bool ExpandDataProxyModel::isRowVirtual(int row) const
{
	if (!_sourceModel)
		return false;

	return row >= _sourceModel->rowCount();
}

bool ExpandDataProxyModel::isColumnVirtual(int col) const
{
	if (!_sourceModel)
		return false;

	return col >= _sourceModel->columnCount();
}

void ExpandDataProxyModel::setSourceModel(QAbstractItemModel *sourceModel)
{
	if(_sourceModel != sourceModel)
		_sourceModel = sourceModel;

	_setRolenames();
}

void ExpandDataProxyModel::_setRolenames()
{
	_roleNameToRole.clear();

	if(_sourceModel == nullptr) return;

	auto roleNames = _sourceModel->roleNames();

	for(auto rn : roleNames.keys())
		_roleNameToRole[roleNames[rn].toStdString()] = rn;
}

int ExpandDataProxyModel::getRole(const std::string &roleName) const
{
	auto it = _roleNameToRole.find(roleName);
	if (it == _roleNameToRole.end())
		return -1;
	else
		return it->second;
}

void ExpandDataProxyModel::removeRows(int start, int count)
{
	if (!_sourceModel)
		return;

	_sourceModel->removeRows(start, count);
}

void ExpandDataProxyModel::removeColumns(int start, int count)
{
	if (!_sourceModel)
		return;

	_sourceModel->removeColumns(start, count);
}

void ExpandDataProxyModel::removeRow(int row)
{
	if (!_sourceModel)
		return;

	if (row >= 0 && row < _sourceModel->columnCount())
		_sourceModel->removeRow(row);
}

void ExpandDataProxyModel::removeColumn(int col)
{
	if (!_sourceModel)
		return;

	if (col >= 0 && col < _sourceModel->columnCount())
		_sourceModel->removeColumn(col);
}

void ExpandDataProxyModel::insertRow(int row)
{
	if (!_sourceModel)
		return;

	_sourceModel->insertRow(row);
}

void ExpandDataProxyModel::insertColumn(int col)
{
	if (!_sourceModel)
		return;

	_sourceModel->insertColumn(col);
}

QString ExpandDataProxyModel::insertColumnSpecial(int col, bool computed, bool R)
{
	DataSetTableModel * dataSetTable = dynamic_cast<DataSetTableModel *>(_sourceModel);

	if (dataSetTable)
		return dataSetTable->insertColumnSpecial(col, computed, R);

	return "";
}

void ExpandDataProxyModel::_expandIfNecessary(int row, int col)
{
	if (!_sourceModel || row < 0 || col < 0 || row >= rowCount() || col >= columnCount())
		return;

	for (int colNr = _sourceModel->columnCount(); colNr <= col; colNr++)
		insertColumnSpecial(colNr, false, false);
	for (int rowNr = _sourceModel->rowCount(); rowNr <= row; rowNr++)
		insertRow(rowNr);
}

bool ExpandDataProxyModel::setData(int row, int col, const QVariant &value, int role)
{
	if (!_sourceModel || row < 0 || col < 0)
		return false;

	_expandIfNecessary(row, col);

	return _sourceModel->setData(index(row, col), value, role);
}

void ExpandDataProxyModel::pasteSpreadsheet(int row, int col, const std::vector<std::vector<QString>> & cells, QStringList newColNames)
{
	DataSetTableModel* dataSetTable = qobject_cast<DataSetTableModel*>(_sourceModel);

	if (!dataSetTable || row < 0 || col < 0)
		return;

	_expandIfNecessary(row, col);

	dataSetTable->pasteSpreadsheet(row, col, cells, newColNames);
}
