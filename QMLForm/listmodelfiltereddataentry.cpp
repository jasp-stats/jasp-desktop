#include "listmodelfiltereddataentry.h"
#include "analysisform.h"
#include "tableviewbase.h"
#include "qutils.h"
#include "log.h"
#include "jaspcontrol.h"

ListModelFilteredDataEntry::ListModelFilteredDataEntry(TableViewBase * parent)
	: ListModelTableViewBase(parent)
{
	_keepRowsOnReset = false;
	setAcceptedRowsTrue();

	connect(this,					&ListModelFilteredDataEntry::filterChanged,		this, &ListModelFilteredDataEntry::runFilter				);
	connect(infoProviderModel(),	&QAbstractItemModel::modelReset,				this, &ListModelFilteredDataEntry::dataSetChangedHandler,	Qt::QueuedConnection	);
	connect(_tableView,				SIGNAL(filterSignal(QString)),					this, SLOT(setFilter(QString))								);
	connect(_tableView,				SIGNAL(colNameSignal(QString)),					this, SLOT(setColName(QString))								);
	connect(_tableView,				SIGNAL(extraColSignal(QString)),				this, SLOT(setExtraCol(QString))							);

}

void ListModelFilteredDataEntry::dataSetChangedHandler()
{
	setAcceptedRowsTrue();
	runFilter(_tableTerms.filter);
}

void ListModelFilteredDataEntry::setFilter(QString filter)
{
	if (_tableTerms.filter == filter)
		return;

	_tableTerms.filter = filter;
	emit filterChanged(_tableTerms.filter);
}

void ListModelFilteredDataEntry::runFilter(QString filter)
{
	//std::cout << "ListModelFilteredDataEntry::runFilter(" << filter.toStdString() << ")" << std::endl;

	if (getDataSetRowCount() > 0)
		runRScript(	"filterResult <- {" + filter + "};"										"\n"
					"if(!is.logical(filterResult)) filterResult <- rep(TRUE, rowcount);"	"\n"
					"return(paste0(sep=' ', collapse='', as.character(filterResult)));"		"\n"
		);
}

size_t ListModelFilteredDataEntry::getDataSetRowCount() const
{
	return requestInfo(VariableInfo::RowCount).toUInt();
}

void ListModelFilteredDataEntry::rScriptDoneHandler(const QString & result)
{
	Log::log() << "ListModelFilteredDataEntry::rScriptDoneHandler: " << result << std::endl;

	QStringList values = result.split(' ');

	size_t dataSetRows = getDataSetRowCount();

	auto newRows = std::vector<bool>(dataSetRows, true);

	if (dataSetRows == 0)
		return;

	size_t i = 0;
	for (const QString & value : values)
		if (value == "TRUE" || value == "FALSE")
		{
			if(i < dataSetRows)
				newRows[i] = value == "TRUE";
			i++;
		}


	setAcceptedRows(i == dataSetRows ? newRows : std::vector<bool>(dataSetRows, true));
}

void ListModelFilteredDataEntry::setAcceptedRows(std::vector<bool> newRows)
{
	//std::cout << "setAcceptedRows(# newRows == " << newRows.size() << ")" << std::endl;
	bool changed = newRows.size() != _acceptedRows.size();

	if (changed)
		_acceptedRows = newRows;
	else
		for (size_t i=0; i<_acceptedRows.size(); i++)
			if (_acceptedRows[i] != newRows[i])
			{
				_acceptedRows[i] = newRows[i];
				changed = true;
			}

	if (changed)
	{
		emit acceptedRowsChanged();
		fillTable();
	}
}

void ListModelFilteredDataEntry::itemChanged(int column, int row, QVariant value, QString)
{
	if (column != _editableColumn)
		return;

	//std::cout << "ListModelFilteredDataEntry::itemChanged(" << column << ", " << row << ", " << value << ")" << std::endl;

	//If changing this function also take a look at it's counterpart in ListModelTableViewBase
	if (column > -1 && column < columnCount() && row > -1 && row < _tableTerms.rowNames.length())
	{
		if (_tableTerms.values[0][row] != value)
		{
			bool gotLarger							= _tableTerms.values[0][row].toString().size() != value.toString().size();
			_tableTerms.values[0][row]							= value.toDouble();
			_enteredValues[_filteredRowToData[size_t(row)]] = value.toDouble();

			emit dataChanged(index(row, column), index(row, column), { Qt::DisplayRole });

			if (gotLarger)
				emit headerDataChanged(Qt::Orientation::Horizontal, column, column);


		}
	}
}

Qt::ItemFlags ListModelFilteredDataEntry::flags(const QModelIndex & index) const
{

	return Qt::ItemIsEnabled | (index.column() == _editableColumn ? (Qt::ItemIsSelectable | Qt::ItemIsEditable) : Qt::NoItemFlags );
}


void ListModelFilteredDataEntry::sourceTermsReset()
{
	//std::cout << "ListModelFilteredDataEntry::sourceTermsChanged(Terms *, Terms *)" << std::endl;

	Terms sourceTerms		= getSourceTerms();
	QString colName			= (_editableColumn >= 0 && _editableColumn < _tableTerms.colNames.size()) ? _tableTerms.colNames[_editableColumn] : "";
	_dataColumns			= sourceTerms.asQList();
	_tableTerms.colNames	= _dataColumns;

	if (_tableTerms.extraCol != "" && !_tableTerms.colNames.contains(_tableTerms.extraCol))
		_tableTerms.colNames.push_back(_tableTerms.extraCol);

	if (!colName.isEmpty())
	{
		_editableColumn	= _tableTerms.colNames.size();
		if (!_tableTerms.colNames.contains(colName))
			_tableTerms.colNames.push_back(colName);
	}
	else
		_editableColumn = -1;

	fillTable();
}

void ListModelFilteredDataEntry::initialValuesChanged()
{
	_initialValues.clear();
	if (_tableView->initialValuesControl())
	{
		const Terms& terms = _tableView->initialValuesControl()->model()->terms();
		if (terms.size() > 0)
		{
			QList<QVariant> values = requestInfo(VariableInfo::DoubleValues, terms[0].asQString()).toList();
			for (const QVariant& value : values)
				_initialValues.push_back(value.toDouble());
		}
	}
	fillTable();
}

void ListModelFilteredDataEntry::initTableTerms(const TableTerms& terms)
{
	//std::cout << "ListModelFilteredDataEntry::initValues(OptionsTable * bindHere)" << std::endl;

	if (terms.values.size() > 1)
		Log::log() << "Too many values in ListModelFilteredDataEntry" << std::endl;

	if (terms.values.size() == 0)
	{
		fillTable();
		return;
	}

	_tableTerms = terms;
	setFilter(_tableTerms.filter);
	setColName(_tableTerms.colName);
	setExtraCol(_tableTerms.extraCol);

	_acceptedRows = std::vector<bool>(getDataSetRowCount(), false);

	_enteredValues.clear();

	_dataColumns	= _tableTerms.colNames;

	if (_tableTerms.extraCol != "" && !_tableTerms.colNames.contains(_tableTerms.extraCol))
		_tableTerms.colNames.push_back(_tableTerms.extraCol);

	_editableColumn = _tableTerms.colName.isEmpty() ? -1 : _tableTerms.colNames.size();

	if (!_tableTerms.colName.isEmpty() && !_tableTerms.colNames.contains(_tableTerms.colName))
		_tableTerms.colNames.push_back(_tableTerms.colName);

	int valIndex = 0;
	for (int rowIndex : _tableTerms.rowIndices)
	{
		size_t row = static_cast<size_t>(rowIndex) - 1;
		if (valIndex < _tableTerms.values[0].size())
			_enteredValues[row] = _tableTerms.values[0][valIndex].toDouble();
		else
			_tableTerms.values[0].push_back(_tableView->defaultValue());
		_acceptedRows[row]	= true;
		valIndex++;
	}

	fillTable();
}

void ListModelFilteredDataEntry::fillTable()
{
	beginResetModel();

	_filteredRowToData.clear();
	_tableTerms.rowNames.clear();
	_tableTerms.values.clear();

	size_t dataRows = getDataSetRowCount();

	if (_acceptedRows.size() != dataRows)
		_acceptedRows = std::vector<bool>(dataRows, true);


	_tableTerms.values.push_back({});

	for (size_t row=0; row<dataRows; row++)
		if (_acceptedRows[row])
		{
			_filteredRowToData.push_back(row);

			QVariant val = _tableView->defaultValue();
			if (_enteredValues.count(row) > 0)		val = _enteredValues[row];
			else if (_initialValues.size() > row)	val = _initialValues[row];

			_tableTerms.values[0].push_back(val);
			_tableTerms.rowNames.push_back(tq(std::to_string(row + 1)));

		}

	_editableColumn = _tableTerms.colName.isEmpty() ? -1 : (columnCount() - 1);
	endResetModel();

	emit columnCountChanged();
	emit rowCountChanged();
}

QVariant ListModelFilteredDataEntry::data(const QModelIndex &index, int role) const
{
	int		column	= index.column(),
			row		= index.row();

	if(row < 0 || row > _tableTerms.rowNames.size())
		return QVariant();

	if (role != Qt::DisplayRole)
		return ListModelTableViewBase::data(index, role);

	if(column == _editableColumn)
		return QVariant(_tableTerms.values[0][row]);

	if(getDataSetRowCount() == 0 || column > _tableTerms.colNames.size() || column < 0)
		return QVariant();

	std::string colName = _tableTerms.colNames[column].toStdString();
	size_t rowData		= _filteredRowToData[static_cast<size_t>(row)];
	return requestInfo(VariableInfo::Value, tq(colName), rowData);
}


int ListModelFilteredDataEntry::getMaximumColumnWidthInCharacters(size_t column) const
{
	int colIndex = int(column);

	if (colIndex == _editableColumn)
		return ListModelTableViewBase::getMaximumColumnWidthInCharacters(0);



	if (colIndex < _tableTerms.colNames.size() && colIndex >= 0)
	{
		QString colName	= _tableTerms.colNames[colIndex];
		return requestInfo(VariableInfo::MaxWidth, colName).toInt();
	}


	return 6;
}

void ListModelFilteredDataEntry::setColName(QString colName)
{
	if (_tableTerms.colName == colName)
		return;

	if (_tableTerms.colName.isEmpty())
	{
		if (!_tableTerms.colNames.contains(colName))
			_tableTerms.colNames.push_back(colName);
		_editableColumn = _tableTerms.colNames.size() - 1;
	}
	else if (colName.isEmpty())
	{
		if (_tableTerms.colNames.size() > 0) _tableTerms.colNames.pop_back();
		_editableColumn = -1;
	}
	else if (_tableTerms.colNames.size() > _editableColumn)
	{
		if (_editableColumn >= 0)
			_tableTerms.colNames[_editableColumn]	= colName;
		else
			Log::log() << "Warning: editableColumn is negative!" << std::endl;
	}

	_tableTerms.colName = colName;
	emit colNameChanged(_tableTerms.colName);
	refresh();

	if (_editableColumn >= 0)
		emit headerDataChanged(Qt::Horizontal, _editableColumn, _editableColumn);

}

void ListModelFilteredDataEntry::setExtraCol(QString extraCol)
{
	if (_tableTerms.extraCol == extraCol)
		return;

	//std::cout << "ListModelFilteredDataEntry::setExtraCol("<< extraCol.toStdString() <<")" << std::endl;

	QString oldExtraCol = _tableTerms.extraCol;

	_tableTerms.extraCol = extraCol;


	beginResetModel();
	if (extraCol == "" && _tableTerms.colNames.size() > _editableColumn && _editableColumn > 0 && _tableTerms.colNames[_editableColumn - 1] == oldExtraCol)
	{
		//std::cout << "Leegmaken!" << std::endl;

		_tableTerms.colNames.erase(_tableTerms.colNames.begin() + _editableColumn - 1);
		_editableColumn--;

		//emit headerDataChanged(Qt::Horizontal, _editableColumn, _colNames.size() + 1);
		//emit dataChanged(index(0, _editableColumn), index(static_cast<int>(getDataSetRowCount()), _colNames.size() + 1));

	}
	else if (oldExtraCol == "" && _tableTerms.colNames.size() > 0)
	{
		//std::cout << "Volmaken!" << std::endl;

		if (_editableColumn >= 0 && !_tableTerms.colName.isEmpty())
		{
			_tableTerms.colNames[_editableColumn] = extraCol;
			_editableColumn++;
			_tableTerms.colNames.push_back(_tableTerms.colName);
		}
		else if (!_tableTerms.colNames.contains(extraCol))
			_tableTerms.colNames.push_back(extraCol);


		//emit headerDataChanged(Qt::Horizontal, _editableColumn - 1, _colNames.size());
		//emit dataChanged(index(0, _editableColumn - 1), index(static_cast<int>(getDataSetRowCount()), _colNames.size()));

	}
	else if (oldExtraCol != "" && extraCol != "" && _tableTerms.colNames.size() > 0)
	{
		_tableTerms.colNames[_tableTerms.colNames.size() - 1] = extraCol;
		//emit headerDataChanged(Qt::Horizontal, _editableColumn - 1, _editableColumn - 1);
		//emit dataChanged(index(0, _editableColumn - 1), index(static_cast<int>(getDataSetRowCount()), _editableColumn - 1));
	}

	endResetModel();

	emit columnCountChanged();
	emit extraColChanged(_tableTerms.extraCol);
}

void ListModelFilteredDataEntry::refreshModel()
{
	ListModel::refresh();

	runFilter(_tableTerms.filter);
}
