#include "listmodelfiltereddataentry.h"
#include "columnutils.h"
#include "controls/tableviewbase.h"
#include "utilities/qutils.h"
#include "log.h"
#include "controls/jaspcontrol.h"
#include "filter.h"


ListModelFilteredDataEntry::ListModelFilteredDataEntry(TableViewBase * parent)
	: ListModelTableViewBase(parent)
{
	_keepRowsOnReset = false;

	connect(this,					&ListModelFilteredDataEntry::filterChanged,		this, &ListModelFilteredDataEntry::runFilter				);
	connect(_tableView,				SIGNAL(filterSignal(QString)),					this, SLOT(setFilter(QString))								);
	connect(_tableView,				SIGNAL(colNameSignal(QString)),					this, SLOT(setColName(QString))								);
	connect(_tableView,				SIGNAL(extraColSignal(QString)),				this, SLOT(setExtraCol(QString))							);

	static int counter = 0;
	do
	{
		_filterName = "ListModelFilteredDataEntry_" + std::to_string(counter++);
	}
	while(!Filter::filterNameIsFree(_filterName));
	
	connect(VariableInfo::info(),	&VariableInfo::dataSetChanged,					this, &ListModelFilteredDataEntry::dataSetChangedHandler);
}

ListModelFilteredDataEntry::~ListModelFilteredDataEntry()
{
	if(_filter)
		_filter->dbDelete();
	delete _filter;
	_filter = nullptr;
}

void ListModelFilteredDataEntry::dataSetChangedHandler()
{
	runFilter();
}

size_t ListModelFilteredDataEntry::getDataSetRowCount() const
{
	return requestInfo(VariableInfo::DataSetRowCount).toUInt();
}

void ListModelFilteredDataEntry::setFilter(QString filter)
{
	if (_tableTerms.filter == filter)
		return;

	_tableTerms.filter = filter;
	emit filterChanged(_tableTerms.filter);
	
	if(_filter)
		_filter->setRFilter("filterResult <- {" + filter.toStdString() + "};"						"\n"
							"if(!is.logical(filterResult)) filterResult <- rep(TRUE, rowcount);"	"\n"
							"  return(filterResult);"												"\n");
}

void ListModelFilteredDataEntry::runFilter()
{
	if(!_filter) //prob still need to bind
		return;
	
	runFilterByName(tq(_filter->name()));
}

void ListModelFilteredDataEntry::filterDoneHandler(const QString &name, const QString & error)
{
	if(name.toStdString() != _filter->name())
		return;

	Log::log() << "ListModelFilteredDataEntry::filterDoneHandler for " << name << " and error '" << error << "'" << std::endl;

	_filter->checkForUpdates();

	setAcceptedRows(_filter->filtered());
	
	if(!error.isEmpty())
		_tableView->addControlWarning(tr("Filter had error '%1'").arg(error));
	else
		_tableView->clearControlError();
	
	if(_filter->filteredRowCount() == 0)
		runFilter();
	else
		informDataSetOfInitialValues();
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
	
	if(terms.filterName.isEmpty())
	{
		//We dont apparently have a previous filterName, so this is a fresh one, we need a new filter!
		assert(!_filter && !_filterName.empty());
		_filter = new Filter(VariableInfo::info()->dataSet(), _filterName, true);
	}
	else if(!_filter)
	{
		_filterName = fq(terms.filterName);
		_filter		= new Filter(VariableInfo::info()->dataSet(), _filterName, true);
	}

	if (terms.colName.isEmpty())
	{
		fillTable();
		return;
	}

	_tableTerms = terms;
	_filterName = _tableTerms.filterName.isEmpty() ? _filterName : fq(_tableTerms.filterName);
	
	setFilter(	_tableTerms.filter	);
	setColName(	_tableTerms.colName	);
	setExtraCol(_tableTerms.extraCol);

	_acceptedRows = _filter->filtered();

	_dataColumns	= _tableTerms.colNames;

	if (_tableTerms.extraCol != "" && !_tableTerms.colNames.contains(_tableTerms.extraCol))
		_tableTerms.colNames.push_back(_tableTerms.extraCol);

	_editableColumn = _tableTerms.colName.isEmpty() ? -1 : _tableTerms.colNames.size();

	if (!_tableTerms.colName.isEmpty() && !_tableTerms.colNames.contains(_tableTerms.colName))
		_tableTerms.colNames.push_back(_tableTerms.colName);
	
	fillTable();
	
	runFilter();
}

void ListModelFilteredDataEntry::fillTable()
{
	beginResetModel();

	_filteredRowToData.clear();
	_tableTerms.rowNames.clear();
	_tableTerms.values.clear();
	
	if(_filter)
		_filter->checkForUpdates();
	
	size_t dataRows = _filter && _filter->filtered().size() > 0 ? _filter->filtered().size() : getDataSetRowCount();

	if (_acceptedRows.size() != dataRows)
		_acceptedRows = std::vector<bool>(dataRows, true);

	_tableTerms.values.push_back({});

	
	
	for (size_t row=0; row<dataRows; row++)
		if (_acceptedRows[row])
		{
			_filteredRowToData.push_back(row);
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

	if(getDataSetRowCount() == 0 || column > _tableTerms.colNames.size() || column < 0)
		return QVariant();

	//Otherwise get it from the data:
	QString colName		= _tableTerms.colNames[column];
	size_t rowData		= _filteredRowToData[static_cast<size_t>(row)];
	return requestInfo(VariableInfo::DataSetValue, colName, rowData);
}


void ListModelFilteredDataEntry::itemChanged(int column, int row, QVariant value, QString)
{
	if (column != _editableColumn)
		return;

	//std::cout << "ListModelFilteredDataEntry::itemChanged(" << column << ", " << row << ", " << value << ")" << std::endl;

	//If changing this function also take a look at it's counterpart in ListModelTableViewBase
	if (column > -1 && column < columnCount() && row > -1 && row < _tableTerms.rowNames.length())
	{
		//Otherwise get it from the data:
		QString colName		= _tableTerms.colNames[column];
		size_t rowData		= _filteredRowToData[static_cast<size_t>(row)];
		sendInfo(VariableInfo::DataSetValue, colName, rowData, value);
	}
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
	
	bool	wasEmpty = _tableTerms.colName ==  "";
	QString oldName  = _tableTerms.colName;

	if (_tableTerms.colName.isEmpty())
	{
		if (!_tableTerms.colNames.contains(colName))
			_tableTerms.colNames.push_back(colName);
		_editableColumn = _tableTerms.colNames.size() - 1;
	}
	else if (colName.isEmpty())
	{
		if(!wasEmpty)
			emit requestComputedColumnDestruction(fq(_tableTerms.colName));

		if (_tableTerms.colNames.size() > 0)
			_tableTerms.colNames.pop_back();

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

	
	if(!wasEmpty && oldName != _tableTerms.colName)
		emit requestComputedColumnDestruction(fq(oldName));
	
	emit requestComputedColumnCreation(fq(_tableTerms.colName));
	emit colNameChanged(_tableTerms.colName);
	
	refresh();

	if (_editableColumn >= 0)
		emit headerDataChanged(Qt::Horizontal, _editableColumn, _editableColumn);
	
	_informOnce = true;
}

void ListModelFilteredDataEntry::informDataSetOfInitialValues()
{
	//Make sure we set the initial values to at least the values shown in the filtered data entry
	if(_tableTerms.colName != "" && _informOnce)
	{
		bool somethingFilled = false;
		QVariantList vals;
		for(size_t i=0; i<_initialValues.size(); i++)
		{
			vals.append(_acceptedRows[i] ? tq(ColumnUtils::doubleToString(_initialValues[i])) : "");
			if(_acceptedRows[i])
				somethingFilled = true;
		}
		
		if(somethingFilled)
		{
			sendInfo(VariableInfo::DataSetValues, _tableTerms.colName, 0, vals);
			_informOnce = false;
			refresh();
		}
	}
		
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
}
