#include "expanddataproxymodel.h"
#include "datasettablemodel.h"

ExpandDataProxyModel::ExpandDataProxyModel(QObject *parent)
	: QObject{parent}
{
	_undoStack = DataSetPackage::pkg()->undoStack();
	connect(_undoStack, &QUndoStack::indexChanged, this, &ExpandDataProxyModel::undoChanged) ;
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

	switch(role)
	{
	case int(dataPkgRoles::selected):				return false;
	case int(dataPkgRoles::lines):
	{
		DataSetTableModel * dataSetTable = dynamic_cast<DataSetTableModel *>(_sourceModel);

		if (row < _sourceModel->rowCount() && dataSetTable && dataSetTable->showInactive() && !DataSetPackage::pkg()->getRowFilter(row))
			return DataSetPackage::getDataSetViewLines(false, false, false, false);
		return DataSetPackage::getDataSetViewLines(col>0, row>0, true, true);
	}
	case int(dataPkgRoles::value):					return "";
	case int(dataPkgRoles::columnType):				return int(columnType::scale);
//	case int(dataPkgRoles::itemInputValue):			return "string"; ???
	default:										return QVariant();
	}

	return QVariant(); //gcc might complain some more I guess?
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
			switch(role)
			{
			case int(dataPkgRoles::columnIsComputed):				return false;
			case int(dataPkgRoles::computedColumnIsInvalidated):	return false;
			case int(dataPkgRoles::filter):							return false;
			case int(dataPkgRoles::computedColumnError):			return "";
			case int(dataPkgRoles::columnType):						return int(columnType::unknown);
			case int(dataPkgRoles::maxColString):					return "XXXXXXXXXXX";
			default:												return "";
			}
	}
	else if (orientation == Qt::Orientation::Vertical)
	{
		if (section < _sourceModel->rowCount())
			return _sourceModel->headerData(section, orientation, role);
		else if (section == 0 && role == int(dataPkgRoles::maxRowHeaderString))
			return "XXXX";
		else
			return  DataSetPackage::pkg()->dataRowCount() + (section - _sourceModel->rowCount()) + 1;
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

	for(const auto & rn : roleNames.keys())
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
	if (!_sourceModel || count <= 0 || start < 0 || start >= _sourceModel->rowCount())
		return;

	if (start + count > _sourceModel->rowCount())
		count = _sourceModel->rowCount() - start;

	_undoStack->pushCommand(new RemoveRowsCommand(_sourceModel, start, count));
}

void ExpandDataProxyModel::removeRowGroups(std::vector<std::pair<int, int> > groups)
{
	int rows = 0;
	for(const auto & startCount : groups)
		rows += startCount.second; 
	
	if(!rows)
		return;
	
	_undoStack->startMacro(tr("Remove %1 rows").arg(rows));
	for(const auto & startCount : groups)
		_undoStack->pushCommand(new RemoveRowsCommand(_sourceModel, startCount.first, startCount.second));
	
	_undoStack->endMacro();
}

void ExpandDataProxyModel::removeColumns(int start, int count)
{
	if (!_sourceModel || count <= 0 || start < 0 || start >= _sourceModel->columnCount())
		return;

	if (start + count > _sourceModel->columnCount())
		count = _sourceModel->columnCount() - start;
	
	_undoStack->pushCommand(new RemoveColumnsCommand(_sourceModel, start, count));
}

void ExpandDataProxyModel::removeColumnGroups(std::vector<std::pair<int, int> > groups)
{
	int cols = 0;
	for(const auto & startCount : groups)
		cols += startCount.second; 
	
	if(!cols)
		return;
	
	_undoStack->startMacro(tr("Remove %1 columns").arg(cols));
	for(const auto & startCount : groups)
		new RemoveColumnsCommand(_sourceModel, startCount.first, startCount.second);
	
	_undoStack->endMacro();
}

void ExpandDataProxyModel::insertRows(int row, int count)
{
	if (!_sourceModel)
		return;

	_undoStack->pushCommand(new InsertRowsCommand(_sourceModel, row, count));
}


void ExpandDataProxyModel::insertColumns(int col, int count)
{
	if (!_sourceModel)
		return;

	_undoStack->pushCommand(new InsertColumnsCommand(_sourceModel, col, count));
}


void ExpandDataProxyModel::insertColumn(int col, bool computed, bool R)
{
	if (!_sourceModel)
		return;

	QMap<QString, QVariant> props;
	if (computed)
		props["computed"] = int(R ? computedColumnType::rCode : computedColumnType::constructorCode);
	_undoStack->pushCommand(new InsertColumnCommand(_sourceModel, col, props));
}

void ExpandDataProxyModel::resize(int row, int col, bool onlyExpand, const QString& undoText)
{
	if (!_sourceModel || row < 0 || col < 0)
		return;

	if (onlyExpand)
	{
		if (col < _sourceModel->columnCount() && row < _sourceModel->rowCount()) return;
	}
	else
	{
		if (col == (_sourceModel->columnCount() - 1) && row == (_sourceModel->rowCount() - 1)) return;
	}

	_undoStack->startMacro(undoText);

	if(col >= _sourceModel->columnCount())
	{	
		int colNr = _sourceModel->columnCount(),
			colC  = 1 + col - colNr;
		
		if(colC > 0)
			insertColumns(colNr, colC);
	}
	else if (!onlyExpand && col < (_sourceModel->columnCount() - 1))
		removeColumns(col + 1, _sourceModel->columnCount() - col - 1);

	if(row >= _sourceModel->rowCount())
	{	
		int rowNr = _sourceModel->rowCount(),
			rowC  = 1 + row - rowNr;
		
		if(rowC > 0)
			insertRows(rowNr, rowC);
	}
	else if (!onlyExpand && row < (_sourceModel->rowCount() - 1))
		removeRows(row + 1, _sourceModel->rowCount() - row - 1);

	if (!undoText.isEmpty())
		_undoStack->endMacro();
}

void ExpandDataProxyModel::setData(int row, int col, const QVariant &value, int role)
{
	if (!_sourceModel || row < 0 || col < 0)
		return;

	resize(row, col);
	_undoStack->endMacro(new SetDataCommand(_sourceModel, row, col, value, role));
}

void ExpandDataProxyModel::pasteSpreadsheet(int row, int col, const std::vector<std::vector<QString>> & values, const std::vector<std::vector<QString>> & labels, const QStringList & colNames, const std::vector<boolvec> & selected)
{
	if (!_sourceModel || row < 0 || col < 0 || values.size() == 0 || values[0].size() == 0 )
		return;

	resize(row + values[0].size() - 1, col + values.size() - 1);
	_undoStack->endMacro(new PasteSpreadsheetCommand(_sourceModel, row, col, values, labels, selected, colNames));
}

int ExpandDataProxyModel::setColumnType(intset columnIndexes, int columnType)
{
	_undoStack->pushCommand(new SetColumnTypeCommand(_sourceModel, columnIndexes, columnType));

	return columnType; //it always works
}

void ExpandDataProxyModel::columnReverseValues(intset columnIndexes)
{
	_undoStack->pushCommand(new ColumnReverseValuesCommand(_sourceModel, columnIndexes));
}

void ExpandDataProxyModel::columnautoSortByValues(intset columnIndexes)
{
    _undoStack->pushCommand(new ColumnToggleAutoSortByValuesCommand(_sourceModel, columnIndexes));
}

void ExpandDataProxyModel::copyColumns(int startCol, const std::vector<Json::Value>& copiedColumns)
{
	if (!_sourceModel || startCol < 0 || copiedColumns.size() == 0)
		return;

	resize(0, startCol + copiedColumns.size() - 1);
	_undoStack->endMacro(new CopyColumnsCommand(_sourceModel, startCol, copiedColumns));
}

Json::Value ExpandDataProxyModel::serializedColumn(int col)
{
	Json::Value result;
	if (col < _sourceModel->columnCount())
	{
		QString colName = _sourceModel->headerData(col, Qt::Orientation::Horizontal).toString();
		if (!colName.isEmpty())
			result = DataSetPackage::pkg()->serializeColumn(colName.toStdString());
	}

	return result;
}
