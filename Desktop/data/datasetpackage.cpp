//
// Copyright (C) 2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "log.h"
#include "utilities/qutils.h"
#include "sharedmemory.h"
#include <QThread>
#include "engine/enginesync.h"
#include "qquick/jasptheme.h"
#include "columnencoder.h"
#include "timers.h"
#include "utilities/appdirs.h"
#include "utils.h"
#include "gui/messageforwarder.h"
#include "utilities/settings.h"
#include "modules/ribbonmodel.h"

#define ENUM_DECLARATION_CPP
#include "datasetpackage.h"


DataSetPackage * DataSetPackage::_singleton = nullptr;

DataSetPackage::DataSetPackage(QObject * parent) : QAbstractItemModel(parent)
{
	if(_singleton) throw std::runtime_error("DataSetPackage can be constructed only once!");
	_singleton = this;
	//True init is done in setEngineSync!

	connect(this,						&DataSetPackage::isModifiedChanged,		this, &DataSetPackage::windowTitleChanged);
	connect(this,						&DataSetPackage::loadedChanged,			this, &DataSetPackage::windowTitleChanged);
	connect(this,						&DataSetPackage::currentFileChanged,	this, &DataSetPackage::windowTitleChanged);
	connect(this,						&DataSetPackage::folderChanged,			this, &DataSetPackage::windowTitleChanged);
	connect(this,						&DataSetPackage::currentFileChanged,	this, &DataSetPackage::nameChanged);
	connect(this,						&DataSetPackage::dataModeChanged,		this, &DataSetPackage::logDataModeChanged);
}

void DataSetPackage::setEngineSync(EngineSync * engineSync)
{
	_engineSync = engineSync;

	//These signals should *ONLY* be called from a different thread than _engineSync!
	connect(this,	&DataSetPackage::pauseEnginesSignal,	_engineSync,	&EngineSync::pauseEngines,		Qt::BlockingQueuedConnection);
	connect(this,	&DataSetPackage::resumeEnginesSignal,	_engineSync,	&EngineSync::resumeEngines,		Qt::BlockingQueuedConnection);

	reset();
}

bool DataSetPackage::isThisTheSameThreadAsEngineSync()
{
	return	QThread::currentThread() == _engineSync->thread();
}

void DataSetPackage::pauseEngines()
{
	if(isThisTheSameThreadAsEngineSync())	_engineSync->pauseEngines(true);
	else									emit pauseEnginesSignal(true);
}

void DataSetPackage::resumeEngines()
{
	if(isThisTheSameThreadAsEngineSync())	_engineSync->resumeEngines();
	else									emit resumeEnginesSignal();

	ColumnEncoder::setCurrentColumnNames(getColumnNames()); //Same place as in engine, should be fine right?
}

void DataSetPackage::reset()
{
	beginLoadingData();
	setDataSet(nullptr);
	_archiveVersion				= Version();
	_dataArchiveVersion			= Version();
	_analysesHTML				= std::string();
	_analysesData				= Json::arrayValue;
	_warningMessage				= std::string();
	_hasAnalysesWithoutData		= false;
	_analysesHTMLReady			= false;
	_isArchive					= false;
	_dataFilter					= DEFAULT_FILTER;
	_filterConstructorJSON		= DEFAULT_FILTER_JSON;
	_computedColumns.reset();
	_filterShouldRunInit		= false;

	setLoaded(false);
	setModified(false);
	setCurrentFile("");

	resetEmptyValues();
	endLoadingData();
}


void DataSetPackage::setDataSet(DataSet * dataSet)
{
	if(_dataSet == dataSet)
		return;

	if(_dataSet && !dataSet)
		freeDataSet();

	_dataSet = dataSet;
}

void DataSetPackage::createDataSet()
{
	setDataSet(SharedMemory::createDataSet()); //Why would we do this here but the free in the asyncloader?
}

void DataSetPackage::freeDataSet()
{
	if(_dataSet)
		emit freeDatasetSignal(_dataSet);
	_dataSet = nullptr;
}


void DataSetPackage::generateEmptyData()
{
	if(_dataSet || isLoaded())
	{
		Log::log() << "void DataSetPackage::generateEmptyData() called but dataset already exists, ignoring it." << std::endl;
		return;
	}

	beginLoadingData();
	createDataSet();
	setDataSetSize(1, 1);
	initColumnAsScale(0, freeNewColumnName(0), { NAN });
	endLoadingData();
	emit newDataLoaded();
	resetAllFilters();
}

//Some debugprinting
void DataSetPackage::logDataModeChanged(bool dataMode)
{
	Log::log() << "Data Mode " << (dataMode ? "on" : "off") << "!" << std::endl;
	beginResetModel();
	endResetModel();
}

QModelIndex DataSetPackage::index(int row, int column, const QModelIndex &parent) const
{
	parIdxType * pointer = 0;

	if(!parent.isValid())
	{
		pointer += row; //this index will be the root for data/labels/filter indices and will remember what type it is through the pointer

		if(parIdxType(row) == parIdxType::label)
			pointer += column;
	}
	else
		pointer = static_cast<parIdxType*>(parent.internalPointer());

	return createIndex(row, column, static_cast<void*>(pointer));
}

parIdxType DataSetPackage::parentIndexTypeIs(const QModelIndex &index) const
{
	if(!index.isValid())
		return parIdxType::root;

	uint64_t notPointer = reinterpret_cast<uint64_t>(index.internalPointer());
	if(notPointer < uint64_t(parIdxType::label))
	{
		parIdxType returnThis = parIdxType(notPointer);

		/*if(notPointer > uint64_t(parIdxType::label))
			Log::log() << "???" << std::endl;*/

		return returnThis;
	}

	return parIdxType::label; //The label also encodes which column it is.
}

QModelIndex DataSetPackage::parent(const QModelIndex & index) const
{
	parIdxType	parentType		= parentIndexTypeIs(index);
	int			parentColumn	= 0;

	if(parentType == parIdxType::label)
	{
		uint64_t	notPointer		= reinterpret_cast<uint64_t>(index.internalPointer());
					parentColumn	= notPointer - uint64_t(parIdxType::label);
	}

	return parentModelForType(parentType, parentColumn);
}



QModelIndex DataSetPackage::parentModelForType(parIdxType type, int column) const
{
	if(type == parIdxType::root || column < 0)
		return QModelIndex();

	return index(int(type), column, QModelIndex());
}

int DataSetPackage::rowCount(const QModelIndex & parent) const
{
	if(!_dataSet) return 0;

	switch(parentIndexTypeIs(parent))
	{
	case parIdxType::label:
	{
		if(parent.column() >= _dataSet->columnCount()) return 0;

		Column & col = _dataSet->columns()[parent.column()];
		if(col.getColumnType() == columnType::scale)
			return 0;

		int labelSize = col.labels().size();
		return labelSize;
	}
	case parIdxType::filter:
	default:
	case parIdxType::root:		//return int(parIdxType::leaf); Its more logical to get the actual datasize
	case parIdxType::data:		return !_dataSet ? 0 : _dataSet->rowCount();
	}

	return 0; // <- because gcc is stupid
}

int DataSetPackage::columnCount(const QModelIndex &parent) const
{
	switch(parentIndexTypeIs(parent))
	{
	case parIdxType::filter:	return 1;
	case parIdxType::label:		return 1; //The parent index has a column index in it that tells you which actual column was selected!
	default:
	case parIdxType::root:		//Default is columnCount of data because it makes programming easier. I do hope it doesn't mess up the use of the tree-like-structure of the data though
	case parIdxType::data:		return _dataSet == nullptr ? 0 : _dataSet->columnCount();
	}

	return 0; // <- because gcc is stupid
}

bool DataSetPackage::getRowFilter(int row) const
{
	QModelIndex filterParent(parentModelForType(parIdxType::filter));

	return data(this->index(row, 0, filterParent)).toBool();
}

QVariant DataSetPackage::getDataSetViewLines(bool up, bool left, bool down, bool right) const
{
	return			(left ?		1 : 0) +
					(right ?	2 : 0) +
					(up ?		4 : 0) +
					(down ?		8 : 0);
}

QVariant DataSetPackage::data(const QModelIndex &index, int role) const
{
	if(!index.isValid()) return QVariant();

	parIdxType parentType = parentIndexTypeIs(index);
	
	if(role == int(specialRoles::selected))
		return false; //DataSetPackage doesnt know anything about selected, only LabelModel does (now)

	switch(parentType)
	{
	default:
		return QVariant();

	case parIdxType::filter:
		if(_dataSet == nullptr || index.row() < 0 || index.row() >= _dataSet->filterVector().size())
			return true;
		return _dataSet->filterVector()[index.row()];

	case parIdxType::data:
		if(_dataSet == nullptr || index.column() >= _dataSet->columnCount() || index.row() >= _dataSet->rowCount())
			return QVariant(); // if there is no data then it doesn't matter what role we play

	// 	Log::log() << "Data requested for col " << index.column() << " and row " << index.row() << std::endl;
		
		switch(role)
		{
		case Qt::DisplayRole:
		case int(specialRoles::label):				return tq(_dataSet->column(index.column())[index.row()]);
		case int(specialRoles::value):				return tq(_dataSet->column(index.column()).getOriginalValue(index.row()));
		case int(specialRoles::filter):				return getRowFilter(index.row());
		case int(specialRoles::columnType):			return int(_dataSet->columns()[index.column()].getColumnType());
		case int(specialRoles::lines):
		{
			bool	iAmActive		= getRowFilter(index.row()),
					belowMeIsActive = index.row() < rowCount() - 1	&& data(this->index(index.row() + 1, index.column(), index.parent()), int(specialRoles::filter)).toBool();

			return getDataSetViewLines(
				iAmActive,
				iAmActive,
				iAmActive && !belowMeIsActive,
				iAmActive && index.column() == columnCount(index.parent()) - 1 //always draw left line and right line only if last col
			);
		}
		}

	case parIdxType::label:
	{
		int parRowCount = rowCount(index.parent());

		if(!_dataSet || index.row() >= parRowCount)
			return QVariant(); // if there is no data then it doesn't matter what role we play

		//We know which column we need through the parent index!
		Labels & labels = _dataSet->column(index.parent().column()).labels();

		switch(role)
		{
		case int(specialRoles::filter):				return labels[index.row()].filterAllows();
		case int(specialRoles::value):				return tq(labels.getValueFromRow(index.row()));
		case int(specialRoles::lines):				return getDataSetViewLines(index.row() == 0, index.column() == 0, true, true);
		case Qt::DisplayRole:
		case int(specialRoles::label):				return tq(labels.getLabelFromRow(index.row()));
		default:									return QVariant();
		}
	}
	}

	return QVariant(); // <- because gcc is stupid
}

size_t DataSetPackage::getMaximumColumnWidthInCharacters(int columnIndex) const
{
	return _dataSet ? _dataSet->getMaximumColumnWidthInCharacters(columnIndex) : 0;
}

QVariant DataSetPackage::headerData(int section, Qt::Orientation orientation, int role)	const
{
	if (!_dataSet || section < 0 || section >= (orientation == Qt::Horizontal ? columnCount() : rowCount()))
		return QVariant();

	switch(role)
	{
	case int(specialRoles::maxColString):
	{
		//calculate some kind of maximum string to give views an expectation of the width needed for a column
		QString dummyText = headerData(section, orientation, Qt::DisplayRole).toString() + "XXXXX" + (isColumnComputed(section) ? "XXXXX" : ""); //Bit of padding for filtersymbol and columnIcon
		int colWidth = getMaximumColumnWidthInCharacters(section);

		while(colWidth > dummyText.length())
			dummyText += "X";

		return dummyText;
	}
	case int(specialRoles::maxRowHeaderString):				return QString::number(_dataSet->maxRowCount()) + "XXX";
	case int(specialRoles::filter):							return getColumnHasFilter(section) || isColumnUsedInEasyFilter(section);
	case Qt::DisplayRole:									return orientation == Qt::Horizontal ? tq(_dataSet->column(section).name()) : QVariant(section);
	case Qt::TextAlignmentRole:								return QVariant(Qt::AlignCenter);
	case int(specialRoles::labelsHasFilter):				return getColumnHasFilter(section);
	case int(specialRoles::columnIsComputed):				return isColumnComputed(section);
	case int(specialRoles::computedColumnError):			return tq(getComputedColumnError(section));
	case int(specialRoles::computedColumnIsInvalidated):	return isColumnInvalidated(section);
	}

	return QVariant();
}

bool DataSetPackage::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if(!index.isValid() || !_dataSet) return false;

	parIdxType parentType = parentIndexTypeIs(index);

	switch(parentType)
	{
	default:
		return false;

	case parIdxType::filter:
		if(index.row() < 0 || index.row() >= _dataSet->filterVector().size() || value.type() != QMetaType::Bool)
			return false;

		if(_dataSet->filterVector()[index.row()] != value.toBool())
		{
			_dataSet->filterVector()[index.row()] = value.toBool();

			emit dataChanged(DataSetPackage::index(index.row(), 0, parentModelForType(parIdxType::filter)),		DataSetPackage::index(index.row(), columnCount(index.parent()), parentModelForType(parIdxType::filter)));	//Emit dataChanged for filter
			emit dataChanged(DataSetPackage::index(index.row(), 0, parentModelForType(parIdxType::data)),		DataSetPackage::index(index.row(), columnCount(),				parentModelForType(parIdxType::data)));		//Emit dataChanged for data
			return true;
		}
		else
			return false;

	case parIdxType::data:
		pasteSpreadsheet(index.row(), index.column(), {{value.toString()}});
		return true;

	case parIdxType::label:
	{
		int parColCount = columnCount(index.parent()),
			parRowCount = rowCount(index.parent());

		if(!_dataSet || index.column() >= parColCount || index.row() >= parRowCount || index.column() < 0 || index.row() < 0)
			return false;

		//We know which column we need through the parent index!
		int columnIndex = index.parent().column();
		Labels				&	labels	= _dataSet->column(columnIndex).labels();
		switch(role)
		{
		case int(specialRoles::filter):
			if(value.type() != QMetaType::Bool) return false;
			return setAllowFilterOnLabel(index, value.toBool());

		case int(specialRoles::value):
			return false;

		default:
		{
			QString originalLabel = tq(labels.getLabelFromRow(index.row()));
			if(labels.setLabelFromRow(index.row(), value.toString().toStdString()))
			{
				QModelIndex parent	= index.parent();
				size_t		row		= index.row(),
							col		= parent.column();

				emit dataChanged(DataSetPackage::index(row, 0, parent), DataSetPackage::index(row, columnCount(parent), parent));	//Emit dataChanged for filter

				parent = parentModelForType(parIdxType::data);
				emit dataChanged(DataSetPackage::index(0, col, parent), DataSetPackage::index(rowCount(), col, parent), { Qt::DisplayRole });

				emit labelChanged(tq(getColumnName(col)), originalLabel, tq(labels.getLabelFromRow(index.row())));
				return true;
			}
			break;
		}
		}
	}
	}

	return false;
}


void DataSetPackage::resetFilterAllows(size_t columnIndex)
{
	if(!_dataSet) return;

	_dataSet->column(columnIndex).resetFilter();

	emit labelFilterChanged();

	QModelIndex parentModel = parentModelForType(parIdxType::data);
	emit dataChanged(DataSetPackage::index(0, columnIndex,	parentModel),	DataSetPackage::index(rowCount(), columnIndex, parentModel), {int(specialRoles::filter)} );

	parentModel = parentModelForType(parIdxType::label, columnIndex);
	emit dataChanged(DataSetPackage::index(0, 0,	parentModel),			DataSetPackage::index(rowCount(parentModel), columnCount(parentModel), parentModel), {int(specialRoles::filter)} );


	emit filteredOutChanged(columnIndex);
}

bool DataSetPackage::setAllowFilterOnLabel(const QModelIndex & index, bool newAllowValue)
{
	if(parentIndexTypeIs(index.parent()) != parIdxType::label)
		return false;

	bool atLeastOneRemains = newAllowValue;


	QModelIndex parent	= index.parent();
	size_t		row		= index.row(),
				col		= parent.column();


	if(col > columnCount() || row > rowCount(parent))
		return false;

	Column & column = _dataSet->column(col);
	Labels & labels = column.labels();

	if(!atLeastOneRemains) //Do not let the user uncheck every single one because that is useless, the user wants to uncheck row so lets see if there is another one left after that.
		for(size_t i=0; i< labels.size(); i++)
			if(i != row && labels[i].filterAllows())
			{
				atLeastOneRemains = true;
				break;
			}
			else if(i == row && labels[i].filterAllows() == newAllowValue) //Did not change!
				return true;

	if(atLeastOneRemains)
	{
		bool before = column.hasFilter();
		labels[row].setFilterAllows(newAllowValue);

		if(before != column.hasFilter())
			notifyColumnFilterStatusChanged(col);


		emit labelFilterChanged();
		emit dataChanged(DataSetPackage::index(row, 0, parent),	DataSetPackage::index(row, columnCount(parent), parent));	//Emit dataChanged for filter
		emit filteredOutChanged(col);

		return true;
	}
	else
		return false;

	return atLeastOneRemains;

}

int DataSetPackage::filteredOut(size_t col) const
{
	if(!_dataSet || col > columnCount())
		return 0; //or -1?

	Labels &	labels		= _dataSet->column(col).labels();
	int			filteredOut = 0;

	for(const Label & label : labels)
		if(!label.filterAllows())
			filteredOut++;

	return filteredOut;
}

Qt::ItemFlags DataSetPackage::flags(const QModelIndex &index) const
{
	return Qt::ItemIsSelectable | Qt::ItemIsEnabled | (parentIndexTypeIs(index) != parIdxType::data ? Qt::ItemIsEditable : Qt::NoItemFlags);
}

QHash<int, QByteArray> DataSetPackage::roleNames() const
{
	static bool						set = false;
	static QHash<int, QByteArray> roles = QAbstractItemModel::roleNames ();

	if(!set)
	{
		roles[int(specialRoles::value)]							= QString("value").toUtf8();
		roles[int(specialRoles::lines)]							= QString("lines").toUtf8();
		roles[int(specialRoles::label)]							= QString("label").toUtf8();
		roles[int(specialRoles::filter)]						= QString("filter").toUtf8();
		roles[int(specialRoles::selected)]						= QString("selected").toUtf8();
		roles[int(specialRoles::columnType)]					= QString("columnType").toUtf8();
		roles[int(specialRoles::maxColString)]					= QString("maxColString").toUtf8();
		roles[int(specialRoles::labelsHasFilter)]				= QString("labelsHasFilter").toUtf8();
		roles[int(specialRoles::columnIsComputed)]				= QString("columnIsComputed").toUtf8();
		roles[int(specialRoles::maxRowHeaderString)]			= QString("maxRowHeaderString").toUtf8();
		roles[int(specialRoles::columnWidthFallback)]			= QString("columnWidthFallback").toUtf8();
		roles[int(specialRoles::computedColumnError)]			= QString("computedColumnError").toUtf8();
		roles[int(specialRoles::computedColumnIsInvalidated)]	= QString("computedColumnIsInvalidated").toUtf8();

		set = true;
	}

	return roles;
}

void DataSetPackage::setModified(bool value)
{
	if ((!value || _isLoaded || _hasAnalysesWithoutData) && value != _isModified)
	{
		_isModified = value;
		emit isModifiedChanged();
	}
}

void DataSetPackage::setLoaded(bool loaded)
{
	if(loaded == _isLoaded)
		return;

	_isLoaded						= loaded;

	emit loadedChanged();
}

size_t DataSetPackage::findIndexByName(std::string name) const
{
	if(!_dataSet)
		throw columnNotFound(name);

	return _dataSet->columns().findIndexByName(name);
}

bool DataSetPackage::isColumnNameFree(std::string name) const
{
	try			{ findIndexByName(name); return false;}
	catch(...)	{ }

	return true;
}

bool DataSetPackage::isColumnComputed(size_t colIndex) const
{
	try
	{
		const Column & normalCol = _dataSet->columns().at(colIndex);
		_computedColumns.findIndexByName(normalCol.name());
		return true;
	}
	catch(...) {}

	return false;

}

bool DataSetPackage::isColumnComputed(std::string name) const
{
	try
	{
		_computedColumns.findIndexByName(name);
		return true;
	}
	catch(...) {}

	return false;

}

bool DataSetPackage::isColumnInvalidated(size_t colIndex) const
{
	try
	{
		const Column & normalCol		= _dataSet->columns().at(colIndex);
		const ComputedColumn & compCol	= _computedColumns[normalCol.name()];

		return compCol.isInvalidated();
	}
	catch(...) {}

	return false;
}

std::string DataSetPackage::getComputedColumnError(size_t colIndex) const
{
	try
	{
		const Column & normalCol		= _dataSet->columns().at(colIndex);
		const ComputedColumn & compCol	= _computedColumns[normalCol.name()];

		return compCol.error();
	}
	catch(...) {}

	return "";
}

void DataSetPackage::setColumnsUsedInEasyFilter(std::set<std::string> usedColumns)
{
	std::set<std::string> toUpdate(usedColumns);

	for(const auto & nameAndUsed : _columnNameUsedInEasyFilter)
		if(nameAndUsed.second)
			toUpdate.insert(nameAndUsed.first);

	_columnNameUsedInEasyFilter.clear();

	for(const std::string & col : usedColumns)
		_columnNameUsedInEasyFilter[col] = true;

	if(_dataSet)
		for(const std::string & col: toUpdate)
			try { notifyColumnFilterStatusChanged(findIndexByName(col)); } catch(columnNotFound &) {}
}


bool DataSetPackage::isColumnUsedInEasyFilter(int column) const
{
	if(_dataSet != nullptr && size_t(column) < _dataSet->columnCount())
	{
		std::string colName = _dataSet->column(column).name();
		return _columnNameUsedInEasyFilter.count(colName) > 0 && _columnNameUsedInEasyFilter.at(colName);
	}
	return false;
}

void DataSetPackage::notifyColumnFilterStatusChanged(int columnIndex)
{
	emit columnsFilteredCountChanged();
	emit headerDataChanged(Qt::Horizontal, columnIndex, columnIndex);
}


QVariant DataSetPackage::getColumnTypesWithCorrespondingIcon() const
{
	static QVariantList ColumnTypeAndIcons;

	//enum ColumnType { unknown = 0, nominal = 1, nominalText = 2, ordinal = 4, scale = 8 };

	if(ColumnTypeAndIcons.size() == 0)
	{
		ColumnTypeAndIcons.push_back("");
		ColumnTypeAndIcons.push_back("variable-nominal.png");
		ColumnTypeAndIcons.push_back("variable-nominal-text.png");
		ColumnTypeAndIcons.push_back("");
		ColumnTypeAndIcons.push_back("variable-ordinal.png");
		ColumnTypeAndIcons.push_back("");
		ColumnTypeAndIcons.push_back("");
		ColumnTypeAndIcons.push_back("");
		ColumnTypeAndIcons.push_back("variable-scale.png");
	}

	return QVariant(ColumnTypeAndIcons);
}


QVariant DataSetPackage::getColumnTitle(int column) const
{
	if(_dataSet != nullptr && column >= 0 && size_t(column) < _dataSet->columnCount())
		return tq(_dataSet->column(column).name());

	return QVariant();
}

QVariant DataSetPackage::getColumnIcon(int column) const
{
	if(_dataSet != nullptr && column >= 0 && size_t(column) < _dataSet->columnCount())
		return QVariant(int(_dataSet->column(column).getColumnType()));

	return QVariant(-1);
}

bool DataSetPackage::getColumnHasFilter(int column) const
{
	if(_dataSet != nullptr && column >= 0 && size_t(column) < _dataSet->columnCount())
		return _dataSet->column(column).hasFilter();
	
	return false;
}


int DataSetPackage::columnsFilteredCount()
{
	if(_dataSet == nullptr) return 0;

	int colsFiltered = 0;

	for(auto & col : _dataSet->columns())
		if(col.hasFilter())
			colsFiltered++;

	return colsFiltered;
}

void DataSetPackage::resetAllFilters()
{
	for(auto & col : _dataSet->columns())
		col.resetFilter();

	emit allFiltersReset();
	emit columnsFilteredCountChanged();
	emit headerDataChanged(Qt::Horizontal, 0, columnCount());
}

bool DataSetPackage::setColumnType(int columnIndex, columnType newColumnType)
{
	if (_dataSet == nullptr)
		return true;

	columnTypeChangeResult	feedback;

	enlargeDataSetIfNecessary([&](){ feedback = _dataSet->column(columnIndex).changeColumnType(newColumnType); }, "setColumnType");

	if (feedback == columnTypeChangeResult::changed) //Everything went splendidly
	{
		emit headerDataChanged(Qt::Orientation::Horizontal, columnIndex, columnIndex);
		emit columnDataTypeChanged(tq(_dataSet->column(columnIndex).name()));
	}
	else
	{
		QString informUser = tr("Something went wrong converting columntype, but it is unclear what.");

		switch(feedback)
		{
		default:	break; //default text already set above.
		case columnTypeChangeResult::cannotConvertDoubleValueToInteger: //Aka something failed converting to ordinal because to nominal would yield nominalText
			informUser = tr("Failed to convert column to ordinal: values contain decimals.");
			break;

		case columnTypeChangeResult::cannotConvertStringValueToDouble: //Aka something failed converting to scaler from nominaltext
			informUser = tr("Failed to convert column to continuous: values contain text.");
			break;

		case columnTypeChangeResult::cannotConvertStringValueToInteger: //Aka something failed converting to ordinal from nominaltext
			informUser = tr("Failed to convert column to ordinal: values contain text or decimals.");
			break;
		}

		MessageForwarder::showWarning("Changing column type failed", informUser);
	}

	return feedback == columnTypeChangeResult::changed;
}

void DataSetPackage::refreshColumn(QString columnName)
{
	if(!_dataSet) return;

	int colIndex = getColumnIndex(columnName);

	if(colIndex >= 0)
	{
		QModelIndex p = parentModelForType(parIdxType::data);
		emit dataChanged(index(0, colIndex, p), index(rowCount(p), colIndex, p));
		emit headerDataChanged(Qt::Horizontal, colIndex, colIndex);
	}
}

void DataSetPackage::columnWasOverwritten(std::string columnName, std::string)
{
	for(size_t col=0; col<_dataSet->columns().columnCount(); col++)
		if(_dataSet->columns()[col].name() == columnName)
			emit dataChanged(index(0, col, parentModelForType(parIdxType::data)), index(rowCount()-1, col, parentModelForType(parIdxType::data)));
}

int DataSetPackage::setColumnTypeFromQML(int columnIndex, int newColumnType)
{
	setColumnType(columnIndex, columnType(newColumnType));
	return int(getColumnType(columnIndex));
}

void DataSetPackage::beginSynchingData(bool informEngines)
{
	beginLoadingData(informEngines);
	_synchingData = true;
}

void DataSetPackage::endSynchingDataChangedColumns(std::vector<std::string>	&	changedColumns, bool hasNewColumns, bool informEngines)
{
	 std::vector<std::string>				missingColumns;
	 std::map<std::string, std::string>		changeNameColumns;

	endSynchingData(changedColumns, missingColumns, changeNameColumns, hasNewColumns, informEngines);
}

void DataSetPackage::endSynchingData(std::vector<std::string>			&	changedColumns,
									 std::vector<std::string>			&	missingColumns,
									 std::map<std::string, std::string>	&	changeNameColumns,  //origname -> newname
									 bool									rowCountChanged,
									 bool									hasNewColumns,
									 bool									informEngines)
{

	endLoadingData(informEngines);
	_synchingData = false;
	//We convert all of this stuff to qt containers even though this takes time etc. Because it needs to go through a (queued) connection and it might not work otherwise
	emit datasetChanged(tql(changedColumns), tql(missingColumns), tq(changeNameColumns), rowCountChanged, hasNewColumns);
}


void DataSetPackage::beginLoadingData(bool informEngines)
{
	JASPTIMER_SCOPE(DataSetPackage::beginLoadingData);

	_enginesLoadedAtBeginSync = informEngines && !enginesInitializing();

	if(_enginesLoadedAtBeginSync)
		pauseEngines();

	beginResetModel();
}

void DataSetPackage::endLoadingData(bool informEngines)
{
	JASPTIMER_SCOPE(DataSetPackage::endLoadingData);

	endResetModel();

	if(_enginesLoadedAtBeginSync && informEngines)
		resumeEngines();

	emit modelInit();
	emit dataSetChanged();
}

void DataSetPackage::setDataSetSize(size_t columnCount, size_t rowCount)
{
	enlargeDataSetIfNecessary([&]()
	{
		_dataSet->setColumnCount(columnCount);

		if (rowCount > 0)
			_dataSet->setRowCount(rowCount);
	}, "setDataSetSize");
}

bool DataSetPackage::initColumnAsScale(size_t colNo, std::string newName, const std::vector<double> & values)
{
	bool out = false;

	enlargeDataSetIfNecessary([&]()
	{
		Column &column = _dataSet->column(colNo);
		column.setName(newName);
		out = column.setColumnAsScale(values);
	}, "initColumnAsScale");

	emit columnNamesChanged();

	return out;
}

std::map<int, std::string> DataSetPackage::initColumnAsNominalText(size_t colNo, std::string newName, const std::vector<std::string> & values, const std::map<std::string, std::string> & labels)
{
	std::map<int, std::string> out;

	enlargeDataSetIfNecessary([&]()
	{
		Column &column = _dataSet->column(colNo);
		column.setName(newName);
		out = column.setColumnAsNominalText(values, labels);
	}, "initColumnAsNominalText");

	emit columnNamesChanged();

	return out;
}

bool DataSetPackage::initColumnAsNominalOrOrdinal(size_t colNo, std::string newName, const std::vector<int> & values, bool is_ordinal)
{
	bool out = false;

	enlargeDataSetIfNecessary([&]()
	{
		Column &column = _dataSet->column(colNo);
		column.setName(newName);
		out = column.setColumnAsNominalOrOrdinal(values, is_ordinal);
	}, "initColumnAsNominalOrOrdinal");

	emit columnNamesChanged();

	return out;
}

bool DataSetPackage::initColumnAsNominalOrOrdinal(size_t colNo, std::string newName, const std::vector<int> & values, const std::map<int, std::string> &uniqueValues, bool is_ordinal)
{
	bool out = false;

	enlargeDataSetIfNecessary([&]()
	{
		Column &column = _dataSet->column(colNo);
		column.setName(newName);
		out = column.setColumnAsNominalOrOrdinal(values, uniqueValues, is_ordinal);
	}, "initColumnAsNominalOrOrdinal");

	emit columnNamesChanged();

	return out;
}

bool DataSetPackage::initColumnAsScale(QVariant colID, std::string newName, const std::vector<double> & values)
{
	if(colID.type() == QMetaType::Int || colID.type() == QMetaType::UInt)
	{
		int colNo = colID.type() == QMetaType::Int ? colID.toInt() : colID.toUInt();
		return initColumnAsScale(colNo, newName, values);
	}
	else
		return initColumnAsScale(colID.toString().toStdString(), newName, values);
}

std::map<int, std::string> DataSetPackage::initColumnAsNominalText(QVariant colID, std::string newName, const std::vector<std::string> & values, const std::map<std::string, std::string> & labels)
{
	if(colID.type() == QMetaType::Int || colID.type() == QMetaType::UInt)
	{
		int colNo = colID.type() == QMetaType::Int ? colID.toInt() : colID.toUInt();
		return initColumnAsNominalText(colNo, newName, values, labels);
	}
	else
		return initColumnAsNominalText(colID.toString().toStdString(), newName, values, labels);
}

bool DataSetPackage::initColumnAsNominalOrOrdinal(	QVariant colID, std::string newName, const std::vector<int> & values, bool is_ordinal)
{
	if(colID.type() == QMetaType::Int || colID.type() == QMetaType::UInt)
	{
		int colNo = colID.type() == QMetaType::Int ? colID.toInt() : colID.toUInt();
		return initColumnAsNominalOrOrdinal(colNo, newName, values, is_ordinal);
	}
	else
		return initColumnAsNominalOrOrdinal(colID.toString().toStdString(), newName, values, is_ordinal);
}

bool DataSetPackage::initColumnAsNominalOrOrdinal(	QVariant colID, std::string newName, const std::vector<int> & values, const std::map<int, std::string> &uniqueValues, bool is_ordinal)
{
	if(colID.type() == QMetaType::Int || colID.type() == QMetaType::UInt)
	{
		int colNo = colID.type() == QMetaType::Int ? colID.toInt() : colID.toUInt();
		return initColumnAsNominalOrOrdinal(colNo, newName, values, uniqueValues, is_ordinal);
	}
	else
		return initColumnAsNominalOrOrdinal(colID.toString().toStdString(), newName, values, uniqueValues, is_ordinal);
}


void DataSetPackage::initColumnWithStrings(QVariant colId, std::string newName, const std::vector<std::string> &values)
{
	// interpret the column as a datatype
	std::set<int>				uniqueValues;
	std::vector<int>			intValues;
	std::vector<double>			doubleValues;
	std::map<int, std::string>	emptyValuesMap;

	//If less unique integers than the thresholdScale then we think it must be ordinal: https://github.com/jasp-stats/INTERNAL-jasp/issues/270
	bool	useCustomThreshold	= Settings::value(Settings::USE_CUSTOM_THRESHOLD_SCALE).toBool();
	size_t	thresholdScale		= (useCustomThreshold ? Settings::value(Settings::THRESHOLD_SCALE) : Settings::defaultValue(Settings::THRESHOLD_SCALE)).toUInt();

	bool valuesAreIntegers		= Utils::convertVecToInt(values, intValues, uniqueValues, emptyValuesMap);
	
	size_t minIntForThresh		= thresholdScale > 2 ? 2 : 0;

	auto isNominalInt			= [&](){ return valuesAreIntegers && uniqueValues.size() == minIntForThresh; };
	auto isOrdinal				= [&](){ return valuesAreIntegers && uniqueValues.size() >  minIntForThresh && uniqueValues.size() <= thresholdScale; };
	auto isScalar				= [&](){ return Utils::convertVecToDouble(values, doubleValues, emptyValuesMap); };

	if		(isOrdinal())					initColumnAsNominalOrOrdinal(	colId,	newName,	intValues,		true	);
	else if	(isNominalInt())				initColumnAsNominalOrOrdinal(	colId,	newName,	intValues,		false	);
	else if	(isScalar())					initColumnAsScale(				colId,	newName,	doubleValues	);
	else				emptyValuesMap =	initColumnAsNominalText(		colId,	newName,	values			);

	storeInEmptyValues(newName, emptyValuesMap);
}

void DataSetPackage::enlargeDataSetIfNecessary(std::function<void()> tryThis, const char * callerText)
{
	while(true)
	{
		try	{ tryThis(); return; }
		catch (boost::interprocess::bad_alloc &)
		{
			try							{ setDataSet(SharedMemory::enlargeDataSet(_dataSet)); }
			catch (std::exception &)	{ throw std::runtime_error("Out of memory: this data set is too large for your computer's available memory");	}
		}
		catch (std::exception & e)	{ Log::log() << "std::exception in enlargeDataSetIfNecessary for " << callerText << ": " << e.what() << std::endl;	return;}
		catch (...)					{ Log::log() << "something went wrong while enlargeDataSetIfNecessary for " << callerText << "..." << std::endl;	return;}

	}
}

std::vector<std::string> DataSetPackage::getColumnNames(bool includeComputed)
{
	std::vector<std::string> names;

	if(_dataSet)
		for(const Column & col : _dataSet->columns())
			if(includeComputed || !isColumnComputed(col.name()))
				names.push_back(col.name());

	return names;
}

bool DataSetPackage::isColumnDifferentFromStringValues(std::string columnName, std::vector<std::string> strVals)
{
	try
	{
		Column & col = _dataSet->column(columnName);
		return col.isColumnDifferentFromStringValues(strVals);
	}
	catch(columnNotFound & ) { }

	return true;
}

void DataSetPackage::renameColumn(std::string oldColumnName, std::string newColumnName)
{
	try
	{
		Column & col = _dataSet->column(oldColumnName);
		col.setName(newColumnName);
		emit columnNamesChanged();
	}
	catch(...)
	{
		Log::log() << "Couldn't rename column from '" << oldColumnName << "' to '" << newColumnName << "'" << std::endl;
	}
}

void DataSetPackage::writeDataSetToOStream(std::ostream & out, bool includeComputed)
{
	std::vector<Column*> cols;

	int columnCount = _dataSet->columnCount();
	for (int i = 0; i < columnCount; i++)
	{
		Column &column		= _dataSet->column(i);
		std::string name	= column.name();

		if(!isColumnComputed(name) || includeComputed)
			cols.push_back(&column);
	}


	for (size_t i = 0; i < cols.size(); i++)
	{
		Column *column		= cols[i];
		std::string name	= column->name();

		if (stringUtils::escapeValue(name))	out << '"' << name << '"';
		else								out << name;

		if (i < cols.size()-1)	out << ",";
		else					out << "\n";

	}

	size_t rows = rowCount();

	for (size_t r = 0; r < rows; r++)
		for (size_t i = 0; i < cols.size(); i++)
		{
			Column *column = cols[i];

			std::string value = column->getOriginalValue(r);
			if (value != ".")
			{
				if (stringUtils::escapeValue(value))	out << '"' << value << '"';
				else									out << value;
			}

			if (i < cols.size()-1)		out << ",";
			else if (r != rows-1)		out << "\n";
		}
}

std::string DataSetPackage::getColumnTypeNameForJASPFile(columnType columnType)
{
	switch(columnType)
	{
	case columnType::nominal:			return "Nominal";
	case columnType::nominalText:		return "NominalText";
	case columnType::ordinal:			return "Ordinal";
	case columnType::scale:				return "Continuous";
	default:							return "Unknown";
	}
}

columnType DataSetPackage::parseColumnTypeForJASPFile(std::string name)
{
	if (name == "Nominal")				return  columnType::nominal;
	else if (name == "NominalText")		return  columnType::nominalText;
	else if (name == "Ordinal")			return  columnType::ordinal;
	else if (name == "Continuous")		return  columnType::scale;
	else								return  columnType::unknown;
}

Json::Value DataSetPackage::columnToJsonForJASPFile(size_t columnIndex, Json::Value & labelsData, size_t & dataSize)
{
	Column &column					= _dataSet->column(columnIndex);
	std::string name				= column.name();
	Json::Value columnMetaData		= Json::Value(Json::objectValue);
	columnMetaData["name"]			= Json::Value(name);
	columnMetaData["measureType"]	= Json::Value(getColumnTypeNameForJASPFile(column.getColumnType()));

	if(column.getColumnType() == columnType::scale)
	{
		   columnMetaData["type"] = Json::Value("number");
		   dataSize += sizeof(double) * rowCount();
	}
	else
	{
		   columnMetaData["type"] = Json::Value("integer");
		   dataSize += sizeof(int) * rowCount();
	}


	if (column.getColumnType() != columnType::scale)
	{
		Labels &labels = column.labels();

		if (labels.size() > 0)
		{
			Json::Value &columnLabelData	= labelsData[name];
			Json::Value &labelsMetaData		= columnLabelData["labels"];
			int labelIndex = 0;

			for (const Label &label : labels)
			{
				Json::Value keyValueFilterPair(Json::arrayValue);

				keyValueFilterPair.append(label.value());
				keyValueFilterPair.append(label.text());
				keyValueFilterPair.append(label.filterAllows());

				labelsMetaData.append(keyValueFilterPair);
				labelIndex += 1;
			}

			Json::Value &orgStringValuesMetaData	= columnLabelData["orgStringValues"];
			std::map<int, std::string> &orgLabels	= labels.getOrgStringValues();
			for (const std::pair<int, std::string> &pair : orgLabels)
			{
				Json::Value keyValuePair(Json::arrayValue);
				keyValuePair.append(pair.first);
				keyValuePair.append(pair.second);
				orgStringValuesMetaData.append(keyValuePair);
			}
		}
	}

	return columnMetaData;
}

void DataSetPackage::columnLabelsFromJsonForJASPFile(Json::Value xData, Json::Value columnDesc, size_t columnIndex, std::map<std::string, std::map<int, int> > & mapNominalTextValues)
{
	std::string name					= columnDesc["name"].asString();
	Json::Value &orgStringValuesDesc	= columnDesc["orgStringValues"];
	Json::Value &labelsDesc				= columnDesc["labels"];
	std::map<int, int>& mapValues		= mapNominalTextValues[name];	// This is needed for old JASP file where factor keys where not filled in the right way

	if (labelsDesc.isNull() &&  ! xData.isNull())
	{
		Json::Value &columnlabelData = xData[name];

		if (!columnlabelData.isNull())
		{
			labelsDesc			= columnlabelData["labels"];
			orgStringValuesDesc = columnlabelData["orgStringValues"];
		}
	}

	enlargeDataSetIfNecessary([&]()
	{
		Column &column = _dataSet->column(columnIndex);
		columnType columnType = parseColumnTypeForJASPFile(columnDesc["measureType"].asString());

		column.setName(name);
		column.setColumnType(columnType);

		Labels &labels = column.labels();
		labels.clear();
		int index = 1;

		for (Json::Value keyValueFilterTrip : labelsDesc)
		{
			int zero		= 0; //MSVC complains on int(0) with: error C2668: 'Json::Value::get': ambiguous call to overloaded function
			int key			= keyValueFilterTrip.get(zero,		Json::nullValue).asInt();
			std::string val = keyValueFilterTrip.get(1,			Json::nullValue).asString();
			bool fil		= keyValueFilterTrip.get(2,			true).asBool();
			int labelValue	= key;

			if (columnType == columnType::nominalText)
			{
				labelValue		= index;
				mapValues[key]	= labelValue;
			}

			labels.add(labelValue, val, fil, columnType == columnType::nominalText);

			index++;
		}

		if (!orgStringValuesDesc.isNull())
		{
			for (Json::Value keyValuePair : orgStringValuesDesc)
			{
				int zero		= 0; //MSVC complains on int(0) with: error C2668: 'Json::Value::get': ambiguous call to overloaded function
				int key			= keyValuePair.get(zero,	Json::nullValue).asInt();
				std::string val = keyValuePair.get(1,		Json::nullValue).asString();
				if (mapValues.find(key) != mapValues.end())
					key = mapValues[key];
				else
					Log::log() << "Cannot find key " << key << std::flush;
				labels.setOrgStringValues(key, val);
			}
		}

	}, "columnLabelsFromJsonForJASPFile");

	emit columnNamesChanged();
}

std::vector<int> DataSetPackage::getColumnDataInts(size_t columnIndex)
{
	if(_dataSet == nullptr) return {};

	Column & col = _dataSet->column(columnIndex);
	return std::vector<int>(col.AsInts.begin(), col.AsInts.end());
}

std::vector<double> DataSetPackage::getColumnDataDbls(size_t columnIndex)
{
	if(_dataSet == nullptr) return {};

	Column & col = _dataSet->column(columnIndex);
	return std::vector<double>(col.AsDoubles.begin(), col.AsDoubles.end());
}

std::vector<std::string> DataSetPackage::getColumnDataStrs(size_t columnIndex)
{
	if(_dataSet == nullptr) return {};

	Column & col = _dataSet->column(columnIndex);
	
	std::vector<std::string> out;
	
	for(size_t r=0; r<col.rowCount(); r++)
	{
		std::string value = col.getOriginalValue(r);
		if (value != ".")
		{
			out.push_back(value);
			/*if (stringUtils::escapeValue(value))	out << '"' + value + '"';
			else									out << value;*/
		}
		else
			out.push_back("");
	}
	
	return out;
}

void DataSetPackage::setColumnName(size_t columnIndex, const std::string & newName)
{
	if(!_dataSet)
		return;

	std::string oldName = getColumnName(columnIndex);

	beginResetModel();
	_dataSet->column(columnIndex).setName(newName);
	endResetModel();

	emit datasetChanged({}, {}, QMap<QString, QString>({{tq(oldName), tq(newName)}}), false, false);
}



void DataSetPackage::setColumnDataInts(size_t columnIndex, std::vector<int> ints)
{
	Column & col = _dataSet->column(columnIndex);
	Labels & lab = col.labels();

	for(size_t r = 0; r<ints.size(); r++)
	{
		int value = ints[r];

		//Maybe something went wrong somewhere and we do not have labels for all values...
		try
		{
			if (value != std::numeric_limits<int>::lowest())
				lab.getLabelObjectFromKey(value);
		}
		catch (const labelNotFound &)
		{
			Log::log() << "Value '" << value << "' in column '" << col.name() << "' did not have a corresponding label, adding one now.\n";
			lab.add(value, std::to_string(value), true, col.getColumnType() == columnType::nominalText);
		}

		col.setValue(r, value);
	}
}


void DataSetPackage::setColumnDataDbls(size_t columnIndex, std::vector<double> dbls)
{
	Column & col = _dataSet->column(columnIndex);
	Labels & lab = col.labels();

	for(size_t r = 0; r<dbls.size(); r++)
		col.setValue(r, dbls[r]);
}

void DataSetPackage::emptyValuesChangedHandler()
{
	if (isLoaded())
	{
		beginSynchingData();
		std::map<std::string, std::map<int, std::string> > emptyValuesChanged;
		std::vector<std::string> colChanged;

		enlargeDataSetIfNecessary([&](){ emptyValuesChanged = _dataSet->resetEmptyValues(emptyValuesMap()); }, "emptyValuesChangedHandler");

		for (auto it : emptyValuesChanged)
		{
			colChanged.push_back(it.first);
			storeInEmptyValues(it.first, it.second);
		}

		endSynchingDataChangedColumns(colChanged);
	}
}

bool DataSetPackage::setFilterData(std::string filter, std::vector<bool> filterResult)
{
	setDataFilter(filter);

	bool someFilterValueChanged = _dataSet ? _dataSet->setFilterVector(filterResult) : false;

	if(someFilterValueChanged) //We could also send exactly those cells that were changed if we were feeling particularly inclined to write the code...
	{
		//emit dataChanged(index(0, 0, parentModelForType(parIdxType::filter)),	index(rowCount(), 0,				parentModelForType(parIdxType::filter)));
		//This actually lets the whole application freeze when a filter is undone... -> emit dataChanged(index(0, 0, parentModelForType(parIdxType::data)),		index(rowCount(), columnCount(),	parentModelForType(parIdxType::data)));

		beginResetModel();
		endResetModel();
	}

	return someFilterValueChanged;
}

columnType DataSetPackage::getColumnType(std::string columnName)	const
{
	int colIndex = getColumnIndex(columnName);

	if(colIndex > -1)	return getColumnType(colIndex);
	else				return columnType::unknown;
}

QStringList DataSetPackage::getColumnLabelsAsStringList(std::string columnName)	const
{
	int colIndex = getColumnIndex(columnName);

	if(colIndex > -1)	return getColumnLabelsAsStringList(colIndex);
	else				return QStringList();;
}

QStringList DataSetPackage::getColumnLabelsAsStringList(size_t columnIndex)	const
{
	QStringList list;
	if(columnIndex < 0 || columnIndex >= columnCount()) return list;

	for(const Label & label : _dataSet->column(columnIndex).labels())
		list.append(tq(label.text()));

	return list;
}

void DataSetPackage::labelMoveRows(size_t column, std::vector<size_t> rows, bool up)
{
	Labels & labels = _dataSet->column(column).labels();

	std::vector<Label> new_labels(labels.begin(), labels.end());

	int mod = up ? -1 : 1;

	std::sort(rows.begin(), rows.end(), [&](const size_t & l, const size_t & r) { return up ? l < r : r < l; });

	for (size_t row : rows)
		if(int(row) + mod < 0 || int(row) + mod >= int(labels.size()))
			return; //Because we can't move out of our labels for obvious reasons

	std::set<size_t> rowsChanged;

	for (size_t row : rows)
	{
		iter_swap(new_labels.begin() + row, new_labels.begin() + (row + mod));
		rowsChanged.insert(row);
		rowsChanged.insert(row + mod);
	}

	labels.set(new_labels);
	QModelIndex p = parentModelForType(parIdxType::label, column);

	for(size_t row: rowsChanged)
		emit dataChanged(index(row, 0, p), index(row, columnCount(p), p));

	emit labelsReordered(tq(getColumnName(column)));
}

void DataSetPackage::labelReverse(size_t column)
{
	Labels & labels = _dataSet->column(column).labels();

	std::vector<Label> new_labels(labels.begin(), labels.end());

	std::reverse(new_labels.begin(), new_labels.end());
	labels.set(new_labels);
	QModelIndex p = parentModelForType(parIdxType::label, column);

	emit dataChanged(index(0, 0, p), index(rowCount(p), columnCount(p), p));
	emit labelsReordered(tq(getColumnName(column)));
}

void DataSetPackage::columnSetDefaultValues(std::string columnName, columnType columnType)
{
	if(!_dataSet) return;

	int colIndex = getColumnIndex(columnName);

	if(colIndex >= 0)
	{
		QModelIndex p = parentModelForType(parIdxType::data);
		_dataSet->column(colIndex).setDefaultValues(columnType);
		emit dataChanged(index(0, colIndex, p), index(rowCount(p), colIndex, p));
		emit headerDataChanged(Qt::Horizontal, colIndex, colIndex);
	}
}

std::string DataSetPackage::freeNewColumnName(size_t startHere)
{
	const QString nameBase = tr("Column %1");

	while(true)
	{
		const std::string newColName = fq(nameBase.arg(++startHere));
		if(isColumnNameFree(newColName))
			return newColName;
	}
}

void DataSetPackage::unicifyColumnNames()
{
	for(int c=0; c<columnCount(); c++)
	{
		std::string name = getColumnName(c);

		for(int c1=c+1; c1<columnCount(); c1++)
			if(getColumnName(c1) == name)
			{
				std::string newName = name + "'";
				while(!isColumnNameFree(newName))
					newName = newName + "'";
				setColumnName(c1, newName);
			}
	}
}

void DataSetPackage::resizeData(size_t rows, size_t cols)
{
	auto	namesBefore = tql(getColumnNames(false));
	bool	rowsChanged = int(rows) != rowCount(),
			newCols		= int(cols) >  columnCount(),
			newRows		= int(rows) >  rowCount();
	size_t	oriRows		= rowCount(),
			oriCols		= columnCount();

	beginSynchingData(false); //I assume this is all being called in dataMode, so the engines will be informed once we are done
	setDataSetSize(cols, rows);

	for(size_t c=newRows ? 0 : oriCols; c<cols; c++)
	{
		std::vector<std::string>	colVals = getColumnDataStrs(c);
		std::string					colName = getColumnName(c);

		for(size_t r=oriRows; r<rows; r++)
			colVals[r] = "";

		initColumnWithStrings(int(c), colName == "" ? freeNewColumnName(c) : colName, colVals);
	}

	auto namesAfter = getColumnNames(false);

	for(const std::string & n : namesAfter)
		namesBefore.removeAll(tq(n));

	stringvec	changed = rowsChanged ? namesAfter : std::vector<std::string>(),
				missing = fq(namesBefore);

	std::map<std::string, std::string>	changeNameColumns;

	endSynchingData(changed, missing, changeNameColumns, rowsChanged, newCols, false);

}

void DataSetPackage::pasteSpreadsheet(size_t row, size_t col, const std::vector<std::vector<QString>> & cells, QStringList newColNames)
{
	int		rowMax			= ( cells.size() > 0 ? cells[0].size() : 0), 
			colMax			= cells.size();
	bool	rowCountChanged = int(row + rowMax) > rowCount()	,
			colCountChanged = int(col + colMax) > columnCount()	;
	
	setSynchingExternally(false); //Don't synch with external file after editing

	//beginResetModel();
	beginSynchingData(false);
	
	if(colCountChanged || rowCountChanged)	
		setDataSetSize(std::max(size_t(columnCount()), colMax + col), std::max(size_t(rowCount()), rowMax + row));



	stringvec colNames = getColumnNames(false);
	
	std::vector<std::string> changed;

	for(int c=0; c<colMax; c++)
	{
		int							dataCol = c +  col;
		std::vector<std::string>	colVals = getColumnDataStrs(dataCol);

		for(int r=0; r<rowMax; r++)
		{
			std::string cellVal = fq(cells[c][r]);
			colVals[r + row] = cellVal;
		}

		std::string colName = getColumnName(dataCol),
					newName = newColNames.size() > c && newColNames[c] != "" ? fq(newColNames[c]) : colName == "" ? freeNewColumnName(dataCol) : colName;
		
		initColumnWithStrings(dataCol, newName, colVals);

		if(newName != "")
			changed.push_back(newName);

	}

	std::map<std::string, std::string>		changeNameColumns;
	if(newColNames.size() > 0)
	{
		unicifyColumnNames();

		stringvec colNamesNew = getColumnNames(false);

		for(size_t i=0; i<colNames.size() && i < colNamesNew.size(); i++)
			if(colNames[i] != colNamesNew[i])
				changeNameColumns[colNames[i]] = colNamesNew[i];
	}
	

	std::vector<std::string>				missingColumns;


   endSynchingData(changed, missingColumns, changeNameColumns, rowCountChanged, colCountChanged, false);

	if(isLoaded()) setModified(true);
}

void DataSetPackage::columnInsert(size_t column)
{
	setSynchingExternally(false); //Don't synch with external file after editing
	beginSynchingData(false);

	_dataSet->columns().insertColumn(column);
	setColumnName(column, freeNewColumnName(column));

	stringvec changed({getColumnName(column)});
	endSynchingDataChangedColumns(changed, true, false);
}

void DataSetPackage::columnDelete(size_t column)
{
	setSynchingExternally(false); //Don't synch with external file after editing
	beginSynchingData(false);

	std::vector<std::string>				changed;
	std::map<std::string, std::string>		changeNameColumns;
	std::vector<std::string>				missingColumns({getColumnName(column)});

	_dataSet->columns().removeColumn(column);

	endSynchingData(changed, missingColumns, changeNameColumns, false, true, false);
}

void DataSetPackage::rowInsert(size_t row)
{
	setSynchingExternally(false); //Don't synch with external file after editing
	beginSynchingData(false);
	stringvec changed;

	setDataSetSize(columnCount(), rowCount()+1);

	for(int c=0; c<columnCount(); c++)
	{
		const std::string name = getColumnName(c);
		changed.push_back(name);

		std::vector<std::string>	colVals = getColumnDataStrs(c);

		colVals.insert(colVals.begin() + row, "");

		if(int(colVals.size()) > rowCount())
		{
			Log::log() << "ASSUMPTION CORRECT! I guess?" << std::endl;
			colVals.resize(rowCount());
		}
		initColumnWithStrings(c, name, colVals);
	}

	std::map<std::string, std::string>		changeNameColumns;
	std::vector<std::string>				missingColumns;

	endSynchingData(changed, missingColumns, changeNameColumns, true, false, false);
}



void DataSetPackage::rowDelete(size_t row)
{
	setSynchingExternally(false); //Don't synch with external file after editing
	beginSynchingData(false);
	stringvec changed;

	for(int c=0; c<columnCount(); c++)
	{
		const std::string name = getColumnName(c);
		changed.push_back(name);

		std::vector<std::string>	colVals = getColumnDataStrs(c);

		colVals.erase(colVals.begin() + row);
		colVals.push_back("");

		initColumnWithStrings(c, name, colVals);
	}

	setDataSetSize(columnCount(), rowCount()-1);

	std::map<std::string, std::string>		changeNameColumns;
	std::vector<std::string>				missingColumns;

	endSynchingData(changed, missingColumns, changeNameColumns, true, false, false);
}

bool DataSetPackage::createColumn(std::string name, columnType columnType)
{
	if(getColumnIndex(name) >= 0) return false;

	size_t newColumnIndex	= columnCount();

	beginResetModel();
	setDataSetColumnCount(newColumnIndex + 1);

	_dataSet->columns().initializeColumnAs(newColumnIndex, name)->setDefaultValues(columnType);
	endResetModel();

	pauseEngines();
	resumeEngines();

	return true;
}

void DataSetPackage::removeColumn(std::string name)
{
	int colIndex = getColumnIndex(name);
	if(colIndex == -1) return;

	emit columnAboutToBeRemoved(colIndex);

	beginResetModel();
	_dataSet->columns().removeColumn(name);
	endResetModel();

	if(isLoaded()) setModified(true);

	if (!_synchingData) // If wwe are synchronising, the datasetChanged is already called
		emit datasetChanged({}, {tq(name)}, {}, false, false);
}

std::vector<bool> DataSetPackage::filterVector()
{
	std::vector<bool> out;

	if(_dataSet)
	{
		out.reserve(_dataSet->filterVector().size());
		for(bool f : _dataSet->filterVector())
			out.push_back(f);
	}

	return out;
}

bool DataSetPackage::synchingExternally() const
{
	return PreferencesModel::prefs()->dataAutoSynchronization() && _dataFilePath != "";
}

void DataSetPackage::setSynchingExternally(bool synchingExternally_)
{
	if(synchingExternally() == synchingExternally_)
		return;

	if (!synchingExternally_)
	{
		PreferencesModel::prefs()->setDataAutoSynchronization(false);
	}
	else
	{
		if(_dataFilePath == "")
			emit askUserForExternalDataFile();
		else
			PreferencesModel::prefs()->setDataAutoSynchronization(true);
	}

	emit synchingExternallyChanged();
}



void DataSetPackage::setCurrentFile(QString currentFile)
{
	if (_currentFile == currentFile)
		return;

	_currentFile = currentFile;
	emit currentFileChanged();

	QFileInfo	file(_currentFile);
	QUrl		url(_currentFile);

#ifdef _WIN32
	setFolder(file.exists() ? file.absolutePath().replace('/', '\\')	: url.isValid() ? "OSF" : "");
#else
	setFolder(file.exists() ? file.absolutePath()						: url.isValid() ? "OSF" : "");
#endif
}

void DataSetPackage::setFolder(QString folder)
{
	//Remove the last part if it is the name of the file regardless of extension
	QString _name	= name();
	int		i		= _name.size();
	for(; i < folder.size(); i++)
		if(folder.right(i).startsWith(_name))
		{
			folder = folder.left(folder.size() - i);
			break;
		}
#ifdef _WIN32
		else if(folder.right(i).contains('\\'))	break;
#else
		else if(folder.right(i).contains('/'))	break;
#endif

	if (_folder == folder)
		return;

	_folder = folder;
	emit folderChanged();
}

QString DataSetPackage::name() const
{
	QFileInfo	file(_currentFile);

	if(file.completeBaseName() != "")
		return file.completeBaseName();

	return "JASP";
}

bool DataSetPackage::dataMode() const
{
	return RibbonModel::singleton()->dataMode();
}

QModelIndex DataSetPackage::lastCurrentCell()
{
 throw std::runtime_error("Not implemented!");
}

QString DataSetPackage::windowTitle() const
{
	QString name	= DataSetPackage::name(),
			folder	= DataSetPackage::folder();
	
#ifdef _WIN32
	if(folder.startsWith(AppDirs::examples().replace('/', '\\')))
#else
	if(folder.startsWith(AppDirs::examples()))
#endif
		folder = "";

	folder = folder == "" ? "" : "      (" + folder + ")";

	return name + (isModified() ? "*" : "") + folder;
}

// This function can be called from a different thread then where the underlying value for isReady() is set, but I don't think a mutex or whatever is necessary here. What could go wrong with checking a boolean?
// Also this was already the case, so I'm not making things worse here...
void DataSetPackage::waitForExportResultsReady() 
{ 
	int maxSleepTime	= 10000,
		sleepTime		= 100,
		delay			= 0;
	
	while (!isReady())
	{
		if (delay > maxSleepTime)
			break;
		
		Utils::sleep(sleepTime);
		delay += sleepTime;
	}
	
	if(!isReady())
		Log::log() << "Results were not exported properly!" << std::endl; //Should we maybe create a dummy result that explains something went wrong with the upload? Should we abort saving? What is going on?
}
