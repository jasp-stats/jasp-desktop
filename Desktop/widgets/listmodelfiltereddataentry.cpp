#include "listmodelfiltereddataentry.h"
#include "analysis/options/optiondoublearray.h"
#include "analysis/options/optionintegerarray.h"
#include "analysis/options/optionstring.h"
#include "analysis/analysisform.h"
#include "tableviewbase.h"
#include "utilities/qutils.h"
#include <QQuickItem>
#include "log.h"
#include "analysis/jaspcontrol.h"
#include "data/columnsmodel.h"

ListModelFilteredDataEntry::ListModelFilteredDataEntry(TableViewBase * parent, QString tableType)
	: ListModelTableViewBase(parent, tableType)
{
	setAcceptedRowsTrue();

	setFilter(	_tableView->property("filter").toString());
	setColName(	_tableView->property("colName").toString());
	setExtraCol(_tableView->property("extraCol").toString());

	_tableView->setProperty("itemType", "double"); //Force itemtype to be double

	connect(this,				&ListModelFilteredDataEntry::filterChanged,		this, &ListModelFilteredDataEntry::runFilter										);
	connect(_tableView,			SIGNAL(filterSignal(QString)),					this, SLOT(setFilter(QString))														);
	connect(_tableView,			SIGNAL(colNameSignal(QString)),					this, SLOT(setColName(QString))														);
	connect(_tableView,			SIGNAL(extraColSignal(QString)),				this, SLOT(setExtraCol(QString))													);
	connect(this,				&ListModelFilteredDataEntry::filterChanged,		[&](){ _tableView->setProperty("filter",	_filter);	}						);
	connect(this,				&ListModelFilteredDataEntry::colNameChanged,	[&](){ _tableView->setProperty("colName",	_colName);	}						);

	if(_colNames.size() == 0 && !_colName.isEmpty())
		_colNames.push_back(_colName);


}

//TODO: This is not called anymore, but should be handled by termsChangedHandler
/*void ListModelFilteredDataEntry::dataSetChangedHandler()
{
	//std::cout << "ListModelFilteredDataEntry::dataSetChangedHandler()" << std::endl;
	setAcceptedRowsTrue();
	runFilter(_filter);
}*/

void ListModelFilteredDataEntry::setFilter(QString filter)
{
	if (_filter == filter)
		return;

	_filter = filter;
	emit filterChanged(_filter);
}

void ListModelFilteredDataEntry::runFilter(QString filter)
{
	//std::cout << "ListModelFilteredDataEntry::runFilter(" << filter.toStdString() << ")" << std::endl;

	if(getDataSetRowCount() > 0)
		runRScript(	"filterResult <- {" + filter + "};"										"\n"
					"if(!is.logical(filterResult)) filterResult <- rep(TRUE, rowcount);"	"\n"
					"return(paste0(sep=' ', collapse='', as.character(filterResult)));"		"\n"
		);
}

size_t ListModelFilteredDataEntry::getDataSetRowCount() const
{
	return size_t(ColumnsModel::singleton()->rowCount());
}

void ListModelFilteredDataEntry::rScriptDoneHandler(const QString & result)
{
	Log::log() << "ListModelFilteredDataEntry::rScriptDoneHandler: " << result << std::endl;

	QStringList values = result.split(' ');

	size_t dataSetRows = getDataSetRowCount();

	auto newRows = std::vector<bool>(dataSetRows, true);

	if(dataSetRows == 0)
		return;

	size_t i = 0;
	for(const QString & value : values)
		if(value == "TRUE" || value == "FALSE")
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

	if(changed)
		_acceptedRows = newRows;
	else
		for(size_t i=0; i<_acceptedRows.size(); i++)
			if(_acceptedRows[i] != newRows[i])
			{
				_acceptedRows[i] = newRows[i];
				changed = true;
			}

	if(changed)
	{
		emit acceptedRowsChanged();
		fillTable();
	}
}

void ListModelFilteredDataEntry::itemChanged(int column, int row, QVariant value, QString)
{
	if(column != _editableColumn)
		return;

	//std::cout << "ListModelFilteredDataEntry::itemChanged(" << column << ", " << row << ", " << value << ")" << std::endl;

	//If changing this function also take a look at it's counterpart in ListModelTableViewBase
	if (column > -1 && column < columnCount() && row > -1 && row < _rowNames.length())
	{
		if (_values[0][row] != value)
		{
			bool gotLarger							= _values[0][row].toString().size() != value.toString().size();
			_values[0][row]							= value.toDouble();
			_enteredValues[_filteredRowToData[row]] = value.toDouble();

			emit dataChanged(index(row, column), index(row, column), { Qt::DisplayRole });

			if(gotLarger)
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

	Terms sourceTerms	= getSourceTerms();
	QString colName		= (_editableColumn >= 0 && _editableColumn < _colNames.size()) ? _colNames[_editableColumn] : "";
	_dataColumns		= sourceTerms.asQList();
	_colNames			= _dataColumns;

	if(_extraCol != "")
		_colNames.push_back(_extraCol);

	if (!colName.isEmpty())
	{
		_editableColumn		= _colNames.size();
		_colNames.push_back(colName);
	}
	else
		_editableColumn = -1;

	_columnCount		= _colNames.size();

	fillTable();
}

OptionsTable * ListModelFilteredDataEntry::createOption()
{
	Options* optsTemplate =			new Options();
	optsTemplate->add("colName",	new OptionString());
	optsTemplate->add("filter",		new OptionString());
	optsTemplate->add("values",		new OptionDoubleArray());
	optsTemplate->add("rowIndices",	new OptionIntegerArray());
	optsTemplate->add("dataCols",	new OptionVariables());
	optsTemplate->add("extraCol",	new OptionVariables());

	return new OptionsTable(optsTemplate);
}

void ListModelFilteredDataEntry::initValues(OptionsTable * bindHere)
{
	//std::cout << "ListModelFilteredDataEntry::initValues(OptionsTable * bindHere)" << std::endl;

	_boundTo = bindHere;

	std::vector<Options *>	options = bindHere->value();

	if(options.size() > 1)
		addControlError(tr("Too many rows in OptionsTable for ListModelFilteredDataEntry"));

	if(options.size() == 0)
	{
		//addControlError("Not a single row in OptionsTable for ListModelFilteredDataEntry!");
		fillTable();
		return;
	}

	_acceptedRows = std::vector<bool>(getDataSetRowCount(), false);

	_enteredValues.clear();
		 _colNames.clear();
		 _rowNames.clear();
		   _values.clear();


	Options * firstRow = options[0];

	OptionString		*	optionFilter		= static_cast<OptionString			* >(firstRow->get("filter"));
	OptionDoubleArray	*	optionValues		= static_cast<OptionDoubleArray		* >(firstRow->get("values"));
	OptionString		*	optionColName		= static_cast<OptionString			* >(firstRow->get("colName"));
	OptionVariables		*	optionDataCols		= static_cast<OptionVariables		* >(firstRow->get("dataCols"));
	OptionVariables		*	optionExtraCol		= static_cast<OptionVariables		* >(firstRow->get("extraCol"));
	OptionIntegerArray	*	optionRowIndices	= static_cast<OptionIntegerArray	* >(firstRow->get("rowIndices"));

	_extraCol = tq(optionExtraCol->variables().size() > 0 ? optionExtraCol->variables()[0] : "");

	_dataColumns	= tql(optionDataCols->variables());
	_colNames		= _dataColumns;

	if(_extraCol != "")
		_colNames.push_back(_extraCol);

	_columnCount	= size_t(_dataColumns.size() + 1);
	_colName		= tq(optionColName->value());
	_editableColumn = _colName.isEmpty() ? -1 : _colNames.size();

	if (!_colName.isEmpty()) _colNames.push_back(_colName);

	QVector<QVariant> tempvalues;
	for (QVariant val : optionValues->value())
		tempvalues.push_back(val);
	_values.push_back(tempvalues);

	int valIndex = 0;
	for(int rowIndex : optionRowIndices->value())
	{
		size_t row = static_cast<size_t>(rowIndex) - 1;

		_enteredValues[row] = _values[0][valIndex++].toDouble();
		_acceptedRows[row]	= true;
	}


	fillTable();
	setFilter(tq(optionFilter->value()));
}

void ListModelFilteredDataEntry::fillTable()
{
	//std::cout << "ListModelFilteredDataEntry::fillTable()" << std::endl;

	beginResetModel();

	_filteredRowToData.clear();
			 _rowNames.clear();
			   _values.clear();

	size_t dataRows = getDataSetRowCount();

	if(_acceptedRows.size() != dataRows)
		_acceptedRows = std::vector<bool>(true, dataRows);


	_values.push_back({});

	for(size_t row=0; row<dataRows; row++)
		if(_acceptedRows[row])
		{
			_filteredRowToData.push_back(row);
					_values[0].push_back(_enteredValues[row]);
					 _rowNames.push_back(tq(std::to_string(row + 1)));

		}

	_columnCount	= _colNames.size();
	_editableColumn = _colName.isEmpty() ? -1 : (_columnCount - 1);
	endResetModel();

	emit columnCountChanged();
	emit rowCountChanged();
}

void ListModelFilteredDataEntry::modelChangedSlot()
{
	//std::cout << "ListModelFilteredDataEntry::modelChangedSlot()" << std::endl;

	if (_boundTo)
	{
		std::vector<int> stdRowIndices;

		for (size_t index : _filteredRowToData)
			stdRowIndices.push_back(static_cast<int>(index + 1));

		Options* options =			new Options();
		if (!_colName.isEmpty())
			options->add("colName",	new OptionString(_colName.toStdString()));
		options->add("filter",		new OptionString(_filter.toStdString()));
		options->add("rowIndices",	new OptionIntegerArray(stdRowIndices));
		
		std::vector<double> tempvalues;
		for (QVariant val : _values[0])
			tempvalues.push_back(val.toDouble());
		options->add("values",		new OptionDoubleArray(tempvalues));
		options->add("dataCols",	new OptionVariables(fq(_dataColumns)));
		options->add("extraCol",	new OptionVariables({fq(_extraCol)}));

		_boundTo->setValue({options});
	}
}


QVariant ListModelFilteredDataEntry::data(const QModelIndex &index, int role) const
{
	int		column	= index.column(),
			row		= index.row();

	if(row < 0 || row > _rowNames.size())
		return QVariant();

	if (role != Qt::DisplayRole)
		return ListModelTableViewBase::data(index, role);

	if(column == _editableColumn)
		return QVariant(_values[0][row]);

	if(getDataSetRowCount() == 0 || column > _colNames.size() || column < 0)
		return QVariant();

	std::string colName = _colNames[column].toStdString();
	size_t rowData		= _filteredRowToData[static_cast<size_t>(row)];

	ColumnsModel* columnsModel = ColumnsModel::singleton();

	int colIndex = columnsModel->getColumnIndex(colName);

	return columnsModel->data(columnsModel->index(int(rowData), colIndex, columnsModel->parentModelForType(parIdxType::data)));
}


int ListModelFilteredDataEntry::getMaximumColumnWidthInCharacters(size_t column) const
{
	int colIndex = int(column);

	if(colIndex == _editableColumn)
		return ListModelTableViewBase::getMaximumColumnWidthInCharacters(0);


	ColumnsModel* columnsModel = ColumnsModel::singleton();

	if(!(columnsModel->rowCount() >= 0 || colIndex > _colNames.size() || column < 0))
	{
		std::string colName		= _colNames[colIndex].toStdString();
		int			colIndex	= columnsModel->getColumnIndex(colName);

		if(colIndex > -1)
			return int(columnsModel->getMaximumColumnWidthInCharacters(colIndex));
	}


	return 6;
}

void ListModelFilteredDataEntry::setColName(QString colName)
{
	if (_colName == colName)
		return;

	if (_colName.isEmpty())
	{
		_colNames.push_back(colName);
		_editableColumn = _colNames.size() - 1;
	}
	else if (colName.isEmpty())
	{
		if (_colNames.size() > 0) _colNames.pop_back();
		_editableColumn = -1;
	}
	else if (_colNames.size() > _editableColumn)
	{
		if (_editableColumn >= 0)
			_colNames[_editableColumn]	= colName;
		else
			Log::log() << "Warning: editableColumn is negative!" << std::endl;
	}

	_colName = colName;
	emit colNameChanged(_colName);
	refresh();

	if (_editableColumn >= 0)
		emit headerDataChanged(Qt::Horizontal, _editableColumn, _editableColumn);

}

void ListModelFilteredDataEntry::setExtraCol(QString extraCol)
{
	if (_extraCol == extraCol)
		return;

	//std::cout << "ListModelFilteredDataEntry::setExtraCol("<< extraCol.toStdString() <<")" << std::endl;

	QString oldExtraCol = _extraCol;

	_extraCol = extraCol;


	beginResetModel();
	if(extraCol == "" && _colNames.size() > _editableColumn && _editableColumn > 0 && _colNames[_editableColumn - 1] == oldExtraCol)
	{
		//std::cout << "Leegmaken!" << std::endl;

		_colNames.erase(_colNames.begin() + _editableColumn - 1);
		_editableColumn--;
		_columnCount = _colNames.size();

		//emit headerDataChanged(Qt::Horizontal, _editableColumn, _colNames.size() + 1);
		//emit dataChanged(index(0, _editableColumn), index(static_cast<int>(getDataSetRowCount()), _colNames.size() + 1));

	}
	else if(oldExtraCol == "" && _colNames.size() > 0)
	{
		//std::cout << "Volmaken!" << std::endl;

		if (_editableColumn >= 0 && !_colName.isEmpty())
		{
			_colNames[_editableColumn] = extraCol;
			_editableColumn++;
			_colNames.push_back(_colName);
		}
		else
			_colNames.push_back(extraCol);

		_columnCount = _colNames.size();

		//emit headerDataChanged(Qt::Horizontal, _editableColumn - 1, _colNames.size());
		//emit dataChanged(index(0, _editableColumn - 1), index(static_cast<int>(getDataSetRowCount()), _colNames.size()));

	}
	else if(oldExtraCol != "" && extraCol != "" && _colNames.size() > 0)
	{
		_colNames[_colNames.size() - 1] = extraCol;
		//emit headerDataChanged(Qt::Horizontal, _editableColumn - 1, _editableColumn - 1);
		//emit dataChanged(index(0, _editableColumn - 1), index(static_cast<int>(getDataSetRowCount()), _editableColumn - 1));
	}

	endResetModel();

	emit columnCountChanged();
	emit extraColChanged(_extraCol);
}

void ListModelFilteredDataEntry::refreshModel()
{
	ListModel::refresh();

	runFilter(_filter);
}
