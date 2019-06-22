#include "listmodelfiltereddataentry.h"
#include "analysis/options/optiondoublearray.h"
#include "analysis/options/optionintegerarray.h"
#include "analysis/options/optionstring.h"
#include "analysis/analysisform.h"
#include "boundqmltableview.h"
#include "utilities/qutils.h"
#include <QQuickItem>

ListModelFilteredDataEntry::ListModelFilteredDataEntry(BoundQMLTableView * parent, QString tableType)
	: ListModelTableViewBase(parent, tableType)
{
	setAcceptedRowsTrue();

	setFilter(_tableView->getItemProperty("filter").toString());
	setColName(_tableView->getItemProperty("colName").toString());
	setExtraCol(_tableView->getItemProperty("extraCol").toString());

	connect(_tableView->item(), SIGNAL(filterSignal(QString)),					this, SLOT(setFilter(QString))														);
	connect(_tableView->item(), SIGNAL(colNameSignal(QString)),					this, SLOT(setColName(QString))														);
	connect(_tableView->item(), SIGNAL(extraColSignal(QString)),				this, SLOT(setExtraCol(QString))													);
	connect(_tableView->form(), &AnalysisForm::dataSetChanged,					this, &ListModelFilteredDataEntry::dataSetChangedHandler,	Qt::QueuedConnection	);
	connect(this,				&ListModelFilteredDataEntry::filterChanged,		[&](){ _tableView->setItemProperty("filter",	_filter);	}						);
	connect(this,				&ListModelFilteredDataEntry::colNameChanged,	[&](){ _tableView->setItemProperty("colName",	_colName);	}						);

	if(_colNames.size() == 0)
		_colNames.push_back(_colName);


}

void ListModelFilteredDataEntry::dataSetChangedHandler()
{
	setAcceptedRowsTrue();
	runFilter(_filter);
}

void ListModelFilteredDataEntry::setFilter(QString filter)
{
	if (_filter == filter)
		return;

	_filter = filter;
	emit filterChanged(_filter);

	runFilter(filter);
}

void ListModelFilteredDataEntry::runFilter(QString filter)
{
	if(getDataSetRowCount() > 0)
		runRScript(	"filterResult <- {" + filter + "};"										"\n"
					"if(!is.logical(filterResult)) filterResult <- rep(TRUE, rowcount);"	"\n"
					"return(paste0(sep=' ', collapse='', as.character(filterResult)));"		"\n"
		);
}

size_t ListModelFilteredDataEntry::getDataSetRowCount()
{
	DataSet	*	dataset		= _tableView->form()->getDataSet();
	size_t		dataSetRows = dataset == nullptr ? 0 : dataset->rowCount();

	return dataSetRows;
}

void ListModelFilteredDataEntry::rScriptDoneHandler(const QString & result)
{
	//std::cout << "ListModelFilteredDataEntry::rScriptDoneHandler: " << result.toStdString() << std::endl;

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
		emit modelChanged();
	}
}

void ListModelFilteredDataEntry::itemChanged(int column, int row, double value)
{
	if(column != _editableColumn)
		return;

	//If changing this function also take a look at it's counterpart in ListModelTableViewBase
	if (column > -1 && column < columnCount() && row > -1 && row < _rowNames.length())
	{
		if (_values[0][row] != value)
		{
			bool gotLarger							= QVariant(_values[0][row]).toString().size() != QVariant(value).toString().size();
			_values[0][row]							= value;
			_enteredValues[_filteredRowToData[row]] = value;

			emit dataChanged(index(row, column), index(row, column), { Qt::DisplayRole });
			emit modelChanged();

			if(gotLarger)
				emit headerDataChanged(Qt::Orientation::Horizontal, column, column);


		}
	}
}

Qt::ItemFlags ListModelFilteredDataEntry::flags(const QModelIndex & index) const
{

	return Qt::ItemIsEnabled | (index.column() == _editableColumn ? (Qt::ItemIsSelectable | Qt::ItemIsEditable) : Qt::NoItemFlags );
}


void ListModelFilteredDataEntry::sourceTermsChanged(Terms *, Terms *)
{
	Terms sourceTerms	= getSourceTerms();
	QString colName		= _colNames[_editableColumn];
	_dataColumns		= sourceTerms.asVector();
	_colNames			= tq(_dataColumns);

	if(_extraCol != "")
		_colNames.push_back(_extraCol);

	_editableColumn		= _colNames.size();
	_columnCount		= _dataColumns.size() + 1;

	_colNames.push_back(colName);

	fillTable();

	emit modelChanged();
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
	_boundTo = bindHere;

	std::vector<Options *>	options = bindHere->value();

	if(options.size() > 1)
		addError("Too many rows in OptionsTable for ListModelFilteredDataEntry");

	if(options.size() == 0)
	{
		//addError("Not a single row in OptionsTable for ListModelFilteredDataEntry!");
		fillTable();
		emit modelChanged();
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

	_dataColumns	= optionDataCols->variables();
	_colNames		= tq(_dataColumns);

	if(_extraCol != "")
		_colNames.push_back(_extraCol);

	_editableColumn = _colNames.size();
	_columnCount	= _dataColumns.size() + 1;
	_colName		= tq(optionColName->value());

	_colNames.push_back(_colName);
	  _values.push_back(tq(optionValues->value()));

	int valIndex = 0;
	for(int rowIndex : optionRowIndices->value())
	{
		size_t row = static_cast<size_t>(rowIndex);

		_enteredValues[row] = _values[0][valIndex++];
		_acceptedRows[row]	= true;
	}


	fillTable();
	setFilter(tq(optionFilter->value()));
}

void ListModelFilteredDataEntry::fillTable()
{
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
	_editableColumn = _columnCount - 1;
	endResetModel();

	emit columnCountChanged();
	emit rowCountChanged();
}

void ListModelFilteredDataEntry::modelChangedSlot()
{
	if (_boundTo)
	{
		std::vector<int> stdRowIndices;

		for (size_t index : _filteredRowToData)
			stdRowIndices.push_back(static_cast<int>(index));

		Options* options =			new Options();
		options->add("colName",		new OptionString(_colName.toStdString()));
		options->add("filter",		new OptionString(_filter.toStdString()));
		options->add("rowIndices",	new OptionIntegerArray(stdRowIndices));
		options->add("values",		new OptionDoubleArray(_values[0].toStdVector()));
		options->add("dataCols",	new OptionVariables(_dataColumns));
		options->add("extraCol",	new OptionVariables({fq(_extraCol)}));

		_boundTo->setValue({options});
	}
}


QVariant ListModelFilteredDataEntry::data(const QModelIndex &index, int role) const
{
	if (_rowNames.length() == 0)
		return QVariant();

	int		column	= index.column(),
			row		= index.row();

	if(row < 0 || row > _rowNames.size())
		return QVariant();

	if (role != Qt::DisplayRole)
		return ListModelTableViewBase::data(index, role);

	if(column == _editableColumn)
		return QVariant(_values[0][row]);

	DataSet	* dataset = _tableView->form()->getDataSet();

	if(dataset == nullptr || column > _colNames.size() || column < 0)
		return QVariant();

	try
	{
		std::string colName = _colNames[column].toStdString();
		size_t rowData		= _filteredRowToData[static_cast<size_t>(row)];

		if(rowData > dataset->rowCount())
			return QVariant();

		return tq(dataset->column(colName)[static_cast<int>(rowData)]);
	}
	catch(columnNotFound &)
	{
		return QVariant();
	}
}


int ListModelFilteredDataEntry::getMaximumColumnWidthInCharacters(size_t column) const
{
	if(column == _editableColumn)
		return ListModelTableViewBase::getMaximumColumnWidthInCharacters(0);

	int returnThis = 6;

	DataSet	* dataset = _tableView->form()->getDataSet();

	if(!(dataset == nullptr || column > _colNames.size() || column < 0))
		try
		{
			std::string colName = _colNames[column].toStdString();

			returnThis = dataset->getMaximumColumnWidthInCharacters(dataset->getColumnIndex(colName));
		}
		catch(columnNotFound &)	{}

	return returnThis;
}

void ListModelFilteredDataEntry::setColName(QString colName)
{
	if (_colName == colName)
		return;

	_colName = colName;
	emit colNameChanged(_colName);

	if(_colNames.size() > _editableColumn && _editableColumn >= 0)
	{
		bool changed				= _colNames[_editableColumn] == _colName;
		_colNames[_editableColumn]	= _colName;

		if(changed)
			emit modelChanged();
	}
}

void ListModelFilteredDataEntry::setExtraCol(QString extraCol)
{
	if (_extraCol == extraCol)
		return;

	if(extraCol == "" && _colNames.size() > _editableColumn && _editableColumn > 0 && _colNames[_editableColumn - 1] == _extraCol)
	{
		_colNames.erase(_colNames.begin() + _editableColumn - 1);
		_editableColumn--;

		emit headerDataChanged(Qt::Horizontal, _editableColumn, _colNames.size() + 1);
		emit dataChanged(index(0, _editableColumn), index(static_cast<int>(getDataSetRowCount()), _colNames.size() + 1));
	}
	else if(_extraCol == "" && _colNames.size() > 0)
	{
		_colNames.insert(_editableColumn, extraCol);
		_editableColumn++;

		emit headerDataChanged(Qt::Horizontal, _editableColumn - 1, _colNames.size());
		emit dataChanged(index(0, _editableColumn - 1), index(static_cast<int>(getDataSetRowCount()), _colNames.size()));
	}
	else if(_extraCol != "" && extraCol != "" && _colNames.size() > _editableColumn)
	{
		_colNames[_editableColumn - 1] = extraCol;
		emit headerDataChanged(Qt::Horizontal, _editableColumn - 1, _editableColumn - 1);
		emit dataChanged(index(0, _editableColumn - 1), index(static_cast<int>(getDataSetRowCount()), _editableColumn - 1));
	}

	_extraCol = extraCol;

	emit extraColChanged(_extraCol);
	emit modelChanged();
}
