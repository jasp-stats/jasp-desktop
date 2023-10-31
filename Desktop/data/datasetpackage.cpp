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

#include "datasetpackage.h"
#include "log.h"
#include "utilities/qutils.h"
#include <QThread>
#include "engine/enginesync.h"
#include "jasptheme.h"
#include "columnencoder.h"
#include "timers.h"
#include "utilities/appdirs.h"
#include "utils.h"
#include "columnutils.h"
#include "utilities/messageforwarder.h"
#include "datasetpackagesubnodemodel.h"
#include "databaseconnectioninfo.h"
#include "utilities/settings.h"
#include "modules/ribbonmodel.h"

//Im having problems getting the proxy models to play nicely with beginRemoveRows etc
//So just reset the whole thing as that is what happens in datasetview
//#define ROUGH_RESET

DataSetPackage * DataSetPackage::_singleton = nullptr;

DataSetPackage::DataSetPackage(QObject * parent) : QAbstractItemModel(parent)
{
	if(_singleton) throw std::runtime_error("DataSetPackage can be constructed only once!");
	_singleton = this;
	//True init is done in setEngineSync!
	
	_db			= new DatabaseInterface(true);

	_dataSet	= new DataSet(); //We create one here to make sure filter() etc can actually work
	setDefaultWorkspaceEmptyValues();
	
	connect(this, &DataSetPackage::isModifiedChanged,	this, &DataSetPackage::windowTitleChanged);
	connect(this, &DataSetPackage::loadedChanged,		this, &DataSetPackage::windowTitleChanged);
	connect(this, &DataSetPackage::currentFileChanged,	this, &DataSetPackage::windowTitleChanged);
	connect(this, &DataSetPackage::folderChanged,		this, &DataSetPackage::windowTitleChanged);
	connect(this, &DataSetPackage::currentFileChanged,	this, &DataSetPackage::nameChanged);
	connect(this, &DataSetPackage::dataModeChanged,		this, &DataSetPackage::onDataModeChanged);

	_dataSubModel	= new SubNodeModel("data",		_dataSet->dataNode());
	_filterSubModel = new SubNodeModel("filters",	_dataSet->filtersNode());
	_labelsSubModel = new SubNodeModel("labels");
	
	connect(&_databaseIntervalSyncher, &QTimer::timeout, this, &DataSetPackage::synchingIntervalPassed);

	_undoStack = new UndoStack(this);
}

DataSetPackage::~DataSetPackage() 
{ 
	_singleton = nullptr; 
	_databaseIntervalSyncher.stop();
}

Filter * DataSetPackage::filter()
{
	return !pkg()->_dataSet ? nullptr : pkg()->_dataSet->filter();
}

void DataSetPackage::setEngineSync(EngineSync * engineSync)
{
	_engineSync = engineSync;

	//These signals should *ONLY* be called from a different thread than _engineSync!
	connect(this,	&DataSetPackage::enginesPrepareForDataSignal,	_engineSync,	&EngineSync::enginesPrepareForData,	Qt::BlockingQueuedConnection);
	connect(this,	&DataSetPackage::enginesReceiveNewDataSignal,	_engineSync,	&EngineSync::enginesReceiveNewData,	Qt::BlockingQueuedConnection);

	reset();
}

bool DataSetPackage::isThisTheSameThreadAsEngineSync()
{
	return	QThread::currentThread() == _engineSync->thread();
}

void DataSetPackage::enginesPrepareForData()
{
	if(_dataMode)
		return;

	if(isThisTheSameThreadAsEngineSync())	_engineSync->enginesPrepareForData();
	else									emit enginesPrepareForDataSignal();
}

void DataSetPackage::enginesReceiveNewData()
{
	if(!_dataMode)
	{
		if(isThisTheSameThreadAsEngineSync())	_engineSync->enginesReceiveNewData();
		else									emit enginesReceiveNewDataSignal();
	}

	ColumnEncoder::setCurrentColumnNames(getColumnNames()); //Same place as in engine, should be fine right?
}

bool DataSetPackage::dataSetBaseNodeStillExists(DataSetBaseNode *node) const
{
	return _dataSet && _dataSet->nodeStillExists(node);
}

void DataSetPackage::reset(bool newDataSet)
{
	Log::log() << "DataSetPackage::reset()" << std::endl;
	_databaseIntervalSyncher.stop();
	
	beginLoadingData();

	if(newDataSet)	createDataSet();
	else			deleteDataSet();

	_archiveVersion				= Version();
	_jaspVersion				= Version();
	_analysesHTML				= QString();
	_analysesData				= Json::arrayValue;
	_warningMessage				= std::string();
	_hasAnalysesWithoutData		= false;
	_analysesHTMLReady			= false;
	_database					= Json::nullValue;
	_isJaspFile					= false;
	_filterShouldRunInit		= false;
	_dataMode					= false;
	_manualEdits				= false;

	_columnNameUsedInEasyFilter.clear();

	setLoaded(false);
	setModified(false);
	setSynchingExternally(false); //Default is off, AsyncLoader::loadPackage(...) will turn it on for non-jasp
	setCurrentFile("");

	endLoadingData();
}

void DataSetPackage::generateEmptyData()
{
	if(isLoaded())
	{
		Log::log() << "void DataSetPackage::generateEmptyData() called but dataset already loaded, ignoring it." << std::endl;
		return;
	}

	const int INIT_COL = 3;
	const int INIT_ROW = 10;

	beginLoadingData();

	if(!_dataSet)
		createDataSet();
	setDataSetSize(INIT_COL, INIT_ROW);
	doublevec emptyValues(INIT_ROW, NAN);
	initColumnAsScale(0, freeNewColumnName(0), emptyValues);
	initColumnAsScale(1, freeNewColumnName(1), emptyValues);
	initColumnAsScale(2, freeNewColumnName(2), emptyValues);

	endLoadingData();
	emit newDataLoaded();
	resetAllFilters();
	setSynchingExternally(false);
}

//Some debugprinting
void DataSetPackage::onDataModeChanged(bool dataMode)
{
	Log::log() << "Data Mode " << (dataMode ? "on" : "off") << "!" << std::endl;
	_dataMode = dataMode;

	beginResetModel();
	endResetModel();
}

DataSetBaseNode * DataSetPackage::indexPointerToNode(const QModelIndex & index) const
{
	DataSetBaseNode * node = static_cast<DataSetBaseNode*>(index.internalPointer());
	
	//Below was when I was trying to use dataChanged for columnModel setData updates. but it got messy, so instead we do reset but we do not need to loop over all nodes anymore everytime we convert something
	return node;
	//Sometimes the proxymodels seem to return pointers to destroyed objects, so lets check even if it gives some overhead...
	//return dataSetBaseNodeStillExists(node) ? node : nullptr;
}

/// the following hierarchy is used (Where parents point to children):
/// QModelIndex()/root -> DataSet_N* Where each DataSet is located at row 0 and column N (0-based)
/// DataSet_N -> Data (0r,0c) and Filters(1r,0c)
/// Filters -> Filter_N* where column is filterIndex and row value-index in the filtered-bools
/// Data -> Directly to the data but each column (dfwith any row) can also be used as a parent for getting the Labels
/// Data[*r, Nc] -> Labels
QModelIndex DataSetPackage::index(int row, int column, const QModelIndex &parent) const
{
	const void * pointer = nullptr;

	if(parent.isValid()) //Top of hierarchy (parent != valid) has no pointer
	{
		if(!parent.internalPointer()) //Parent has no pointer stored so this must be a dataSet
		{
			//Currently we only have a single dataSet but in the future there will be more.
			//Then they will be differentiated here by row
			pointer = dynamic_cast<const void*>(_dataSet);
		}
		else
		{
			DataSetBaseNode * parentNode = indexPointerToNode(parent);
			
			switch(parentNode->nodeType())
			{
			case dataSetBaseNodeType::dataSet:
			{
				DataSet * data = dynamic_cast<DataSet*>(parentNode);
				// if row 0 it is "data" else "filters"
				pointer = row == 0 ? dynamic_cast<const void*>(data->dataNode()) : dynamic_cast<const void*>(data->filtersNode());
				break;
			}
				
			case dataSetBaseNodeType::data:
			{
				DataSet * data = dynamic_cast<DataSet*>(parentNode->parent());
				pointer = dynamic_cast<const void*>(data->column(column));
				break;
			}
				
			case dataSetBaseNodeType::filters:
			{
				//Later on we should support multiple filters here by selecting a filter per column
				DataSet * data = dynamic_cast<DataSet*>(parentNode->parent());
				pointer = dynamic_cast<const void*>(data->filter());
				break;
			}
				
			case dataSetBaseNodeType::column:
			{
				Column * col = dynamic_cast<Column*>(parentNode);
				pointer = dynamic_cast<const void*>(col->labels()[row]);
				break;
			}
				
			case dataSetBaseNodeType::label:	//Label & Filter cant be a parentnode
			case dataSetBaseNodeType::filter:
			default:
				throw std::runtime_error("Somehow a label, filter or unknown DataSetBaseNode was passed as parent for an index... This is not allowed.");
				break;
			}
		}
	}
	
	return createIndex(row, column, pointer);
}

///Used to get the parent for a DataSetPackageSubNodeModel
QModelIndex DataSetPackage::indexForSubNode(DataSetBaseNode * node) const
{
	if(node)
		switch(node->nodeType())
		{
		case dataSetBaseNodeType::dataSet:
			return createIndex(0, 0, dynamic_cast<void *>(_dataSet));

		case dataSetBaseNodeType::data:
			return createIndex(0, 0, dynamic_cast<void *>(_dataSet->dataNode()));

		case dataSetBaseNodeType::filters:
			return createIndex(1, 0, dynamic_cast<void *>(_dataSet->filtersNode()));

		case dataSetBaseNodeType::column:
		{
			Column * col = dynamic_cast<Column*>(node);
			if (col)
				return createIndex(0, col->data()->columnIndex(col), dynamic_cast<void *>(col));
			else
				return QModelIndex();
		}

		case dataSetBaseNodeType::label: //Doesnt really make sense to have this as the parent of a subnodemodel but whatever
		{
			Label	* lab = dynamic_cast<Label*>( node);
			Column	* col = lab ? dynamic_cast<Column*>(node->parent()) : nullptr;
			int		i = col ? col->labelIndex(lab) : -1;

			return createIndex(i, 0, dynamic_cast<void*>(lab));
		}

		case dataSetBaseNodeType::filter: //Doesnt really make sense to have this as the parent of a subnodemodel but whatever
		{
			return createIndex(0, 0, dynamic_cast<void*>(_dataSet->filter()));
		}

		default:
			break;
		}

	return QModelIndex();
}

QModelIndex DataSetPackage::parent(const QModelIndex & index) const
{
	if(!index.isValid())
		return QModelIndex();

	
	DataSetBaseNode * node = indexPointerToNode(index);

	if(!node)
		return QModelIndex();
	
	switch(node->nodeType())
	{
	case dataSetBaseNodeType::filters: [[fallthrough]];
	case dataSetBaseNodeType::dataSet:
		return QModelIndex();
		
	case dataSetBaseNodeType::data:
		return indexForSubNode(_dataSet);

	case dataSetBaseNodeType::column:
		return indexForSubNode(_dataSet->dataNode());
		
	case dataSetBaseNodeType::label:
	{
	//	Label	* label	= dynamic_cast<Label*>(node);
		Column	* col	= dynamic_cast<Column*>(node->parent());
		
		return indexForSubNode(col);
	}
		
		
	case dataSetBaseNodeType::filter:
		return indexForSubNode(_dataSet->filtersNode());
		
	default:
		break;
	}
	
	return QModelIndex(); //Shouldnt get here though
}

int DataSetPackage::rowCount(const QModelIndex & parent) const
{
	if(!parent.isValid())
		return 1; //There is only a "column" of DataSets as topnodes
	
	
	DataSetBaseNode * node = indexPointerToNode(parent);

	if(!node)
		return 1;
	
	switch(node->nodeType())
	{
	case dataSetBaseNodeType::dataSet:
		return 2; //data + filters
		
	case dataSetBaseNodeType::data:
	case dataSetBaseNodeType::filters:
	{
		DataSet * data = dynamic_cast<DataSet*>(node->parent());

		return data ? data->rowCount() : 0;
	}
		
	case dataSetBaseNodeType::column:
	{
		Column * col = dynamic_cast<Column*>(node);
		
		if(!col || col->type() == columnType::scale)
			return 0;

		return col->labels().size();
	}
		
	case dataSetBaseNodeType::filter:
	case dataSetBaseNodeType::label:
		return 1;
	
	case dataSetBaseNodeType::unknown:
		return 0;
	}

	return 0; // <- because gcc is stupid
}

int DataSetPackage::columnCount(const QModelIndex &parent) const
{
	if(!parent.isValid())
		return 1; //There is only a "row" of DataSets as topnodes, and currently a single column because a single DataSet	
	
	DataSetBaseNode * node = indexPointerToNode(parent);

	if(!node)
		return 1;
	
	switch(node->nodeType())
	{
	case dataSetBaseNodeType::dataSet:
	{
		return 1; //data + filters are on rows
	}
	case dataSetBaseNodeType::data:
	{
		DataSet * data = dynamic_cast<DataSet*>(node->parent());
		return data->columnCount();
	}
		
	case dataSetBaseNodeType::filters:
	{
		return 1; //change when implementing multiple filters
	}
		
	case dataSetBaseNodeType::column:
	{
		Column * col = dynamic_cast<Column*>(node);
		
		if(col->type() == columnType::scale)
			return 0;

		return 1;
	}
		
	case dataSetBaseNodeType::filter:
	case dataSetBaseNodeType::label:

		return 1;
	
	case dataSetBaseNodeType::unknown:
		return 0;
	}

	return 0; // <- because gcc is stupid
}

bool DataSetPackage::getRowFilter(int row) const
{
	return !_dataSet ? false : data(this->index(row, 0, indexForSubNode(_dataSet->filtersNode()))).toBool();
}

QVariant DataSetPackage::getDataSetViewLines(bool up, bool left, bool down, bool right)
{
	return			(left ?		1 : 0) +
					(right ?	2 : 0) +
					(up ?		4 : 0) +
					(down ?		8 : 0);
}

int DataSetPackage::dataRowCount() const 
{ 
	return !_dataSet ? 0 : rowCount(indexForSubNode(_dataSet->dataNode()));
}

int DataSetPackage::dataColumnCount() const 
{ 
	return !_dataSet ? 0 : columnCount(indexForSubNode(_dataSet->dataNode()));
}

QVariant DataSetPackage::data(const QModelIndex &index, int role) const
{
    JASPTIMER_SCOPE(DataSetPackage::data);
    
	if(!index.isValid())
		return QVariant();

	if(role == int(specialRoles::selected))
		return false; //DataSetPackage doesnt know anything about selected, only ColumnModel does (now)

	DataSetBaseNode *	node		= indexPointerToNode(index);
//					*	parentNode	= !index.parent().isValid() ? nullptr : indexPointerToNode(index.parent());

	if(!node)
		return QVariant();// : QVariant(tq("DataSet_" + std::to_string(dynamic_cast<DataSet*>(node)->id())));
	
	switch(node->nodeType())
	{
	default:
		return QVariant();

	case dataSetBaseNodeType::filter:
	{
		Filter * filter = dynamic_cast<Filter*>(node);
		if(index.row() < 0 || index.row() >= int(filter->filtered().size()))
			return true;
		
		return  QVariant(filter->filtered()[index.row()]);
	}

	case dataSetBaseNodeType::column:
	{
		Column	* column	= dynamic_cast<Column*>(node);
		DataSet * dataSet	= column ? column->data() : nullptr;

		if(!dataSet || index.row() >= int(dataSet->rowCount()))
			return QVariant(); // if there is no data then it doesn't matter what role we play

		switch(role)
		{
		case Qt::DisplayRole:						[[fallthrough]];
		case int(specialRoles::label):				return tq((*column)[index.row()]);
		case int(specialRoles::description):		return tq(column->description());
		case int(specialRoles::labelsStrList):		return getColumnLabelsAsStringList(column->name());
		case int(specialRoles::valuesDblList):		return getColumnValuesAsDoubleList(getColumnIndex(column->name()));
		case int(specialRoles::inEasyFilter):		return isColumnUsedInEasyFilter(column->name());
		case int(specialRoles::value):				return tq(column->getValue(index.row()));
		case int(specialRoles::name):				return tq(column->name());
		case int(specialRoles::title):				return tq(column->title());
		case int(specialRoles::filter):				return getRowFilter(index.row());
		case int(specialRoles::columnType):			return int(column->type());
		case int(specialRoles::computedColumnType):	return int(column->codeType());
		case int(specialRoles::columnPkgIndex):		return index.column();
		case int(specialRoles::lines):
		{
			bool	iAmActive		= getRowFilter(index.row()),
					belowMeIsActive = index.row() < column->rowCount() - 1	&& data(this->index(index.row() + 1, index.column(), index.parent()), int(specialRoles::filter)).toBool();

			return getDataSetViewLines(
				iAmActive,
				iAmActive,
				iAmActive && !belowMeIsActive,
				iAmActive && index.column() == columnCount(index.parent()) - 1 //always draw left line and right line only if last col
			);
		}
		}
	}
		
	case dataSetBaseNodeType::label:
	{
		int			parRowCount = rowCount(index.parent());
	//	Label	*	label		= dynamic_cast<Label*>(node);
		Column	*	column		= dynamic_cast<Column*>(node->parent());
		

		if(!_dataSet || index.row() >= parRowCount)
			return QVariant(); // if there is no data then it doesn't matter what role we play

		const Labels & labels = column->labels();

		switch(role)
		{
		case int(specialRoles::filter):				return labels[index.row()]->filterAllows();
		case int(specialRoles::value):				return tq(labels[index.row()]->originalValueAsString(true));
		case int(specialRoles::description):		return tq(labels[index.row()]->description());
		case int(specialRoles::labelsStrList):		return getColumnLabelsAsStringList(column->name());
		case int(specialRoles::valuesDblList):		return getColumnValuesAsDoubleList(getColumnIndex(column->name()));
		case int(specialRoles::lines):				return getDataSetViewLines(index.row() == 0, index.column() == 0, true, true);
		case Qt::DisplayRole:
		case int(specialRoles::label):				return tq(labels[index.row()]->label());
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
	if (!_dataSet || section < 0 || section >= (orientation == Qt::Horizontal ? dataColumnCount() : dataRowCount()))
		return QVariant();
    
    JASPTIMER_SCOPE(DataSetPackage::headerData);

	if(orientation == Qt::Vertical)
		switch(role)
		{
		default:
			return QVariant();

		case int(specialRoles::maxRowHeaderString):
			return QString::number(_dataSet->rowCount()) + "XXX";

		case Qt::DisplayRole:
			return QVariant(section + 1);
		}
	else
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
		case int(specialRoles::maxRowHeaderString):				return QString::number(_dataSet->rowCount()) + "XXX";
		case int(specialRoles::filter):							return		!_dataSet || !_dataSet->column(section) ? false							: _dataSet->column(section)->hasFilter() || isColumnUsedInEasyFilter(_dataSet->column(section)->name());
		case Qt::DisplayRole:									return tq(	!_dataSet || !_dataSet->column(section) ? "?"							: _dataSet->column(section)->name());
		case Qt::TextAlignmentRole:								return QVariant(Qt::AlignCenter);
		case int(specialRoles::labelsHasFilter):				return		!_dataSet || !_dataSet->column(section) ? false							: _dataSet->column(section)->hasFilter();
		case int(specialRoles::columnIsComputed):				return		!_dataSet || !_dataSet->column(section) ? false							: _dataSet->column(section)->isComputed() && _dataSet->column(section)->codeType() != computedColumnType::analysisNotComputed;
		case int(specialRoles::computedColumnError):			return tq(	!_dataSet || !_dataSet->column(section) ? "?"							: _dataSet->column(section)->error());
		case int(specialRoles::computedColumnIsInvalidated):	return		!_dataSet || !_dataSet->column(section) ? false							: _dataSet->column(section)->invalidated();
		case int(specialRoles::columnType):						return int(	!_dataSet || !_dataSet->column(section) ? columnType::unknown			: _dataSet->column(section)->type());
		case int(specialRoles::computedColumnType):				return int(	!_dataSet || !_dataSet->column(section) ? computedColumnType::notComputed	: _dataSet->column(section)->codeType());
		case int(specialRoles::description):					return tq(	!_dataSet || !_dataSet->column(section) ? "?"							: _dataSet->column(section)->description());
		case int(specialRoles::title):							return tq(	!_dataSet || !_dataSet->column(section) ? "?"							: _dataSet->column(section)->title());
		}

	return QVariant();
}

bool DataSetPackage::setData(const QModelIndex &index, const QVariant &value, int role)
{
    JASPTIMER_SCOPE(DataSetPackage::setData);
    
	if(!index.isValid() || !_dataSet) return false;

	DataSetBaseNode * node = indexPointerToNode(index);
	
	if(!node)
		return false;

	switch(node->nodeType())
	{
	default:
		return false;

	/*case dataSetBaseNodeType::filter:
		 Honestly cant see the usecase right now as filter will be overwritten repeatedly anyway
		  
		  
		  if(index.row() < 0 || index.row() >= int(_dataSet->filter()->filtered()().size()) || value.typeId() != QMetaType::Bool)
			return false;

		if(_dataSet->filter()->filtered()()[index.row()] != value.toBool())
		{
			_dataSet->filter()->setFilterRowTo(index.row(), value.toBool());

			emit dataChanged(DataSetPackage::index(index.row(), 0, parentModelForType(parIdxType::filter)),		DataSetPackage::index(index.row(), columnCount(index.parent()), parentModelForType(parIdxType::filter)));	//Emit dataChanged for filter
			emit dataChanged(DataSetPackage::index(index.row(), 0, parentModelForType(parIdxType::data)),		DataSetPackage::index(index.row(), dataColumnCount(),				parentModelForType(parIdxType::data)));		//Emit dataChanged for data
			return true;
		}
		else
		return false;*/

	case dataSetBaseNodeType::column:
		if(node)
		{
			Column	* column	= dynamic_cast<Column*>(node);
			//DataSet * data		= column->data();

			if(role == Qt::DisplayRole || role == Qt::EditRole || role == int(specialRoles::value))
			{
				bool changed = false;
				bool typeChanged = false;

				const std::string val = fq(value.toString());

				if(column->setStringValueToRowIfItFits(index.row(), val == ColumnUtils::emptyValue ? "" : val, changed, typeChanged))
				{
					if(changed)
					{
						JASPTIMER_SCOPE(DataSetPackage::setData reset model);

						setManualEdits(true); //Don't synch with external file after editing

						//beginResetModel();
						//beginSynchingData(false);

						stringvec	changedCols = {column->name()},
									missing;
						strstrmap	changeName;

						//endSynchingData(changedCols, missing, changeName, false, false, false);

						emit dataChanged(DataSetPackage::index(index.row(), index.column(), index.parent()), DataSetPackage::index(index.row(), index.column(), index.parent()));
						emit datasetChanged(tq(changedCols), tq(missing), tq(changeName), false, false);

						emit labelsReordered(tq(column->name()));

						if(typeChanged)
							emit columnDataTypeChanged(tq(column->name()));

						//emit label dataChanged just in case
						//QModelIndex parent = indexForSubNode(column);
						//emit dataChanged(DataSetPackage::index(0, 0, parent), DataSetPackage::index(rowCount(parent)-1, columnCount(parent)-1, parent), { Qt::DisplayRole });
					}
				}
				else
				{
					JASPTIMER_SCOPE(DataSetPackage::setData pasteSpreadsheet);
					column->rememberOriginalColumnType();
					pasteSpreadsheet(index.row(), index.column(), {{value.toString()}});
					changed = true;
					emit columnDataTypeChanged(tq(column->name()));
				}

				if(changed && column->hasFilter())
				{
					emit labelFilterChanged();
					emit runFilter();
				}


			}
			else
			{
				bool aChange = false;

				switch(role)
				{
				case int(specialRoles::description):
					column->setDescription(value.toString().toStdString());
					aChange = true;
					break;

				case int(specialRoles::title):
					column->setTitle(value.toString().toStdString());
					aChange = true;
					break;

				case int(specialRoles::columnType):
					{
						if(value.toInt() >= int(columnType::unknown) && value.toInt() <= int(columnType::scale))
						{
							columnType converted = static_cast<columnType>(value.toInt());
							if(setColumnType(index.column(), converted))
							{
								aChange = true;
								emit columnDataTypeChanged(tq(column->name()));
							}
							break;
						}
					}


				}

				if(aChange)
				{
					beginResetModel();
					endResetModel();
					setManualEdits(true);
				}
			}

			return true;
		}
		else
			return false;
	
	case dataSetBaseNodeType::label:
	{
		Column * column = dynamic_cast<Column*>(node->parent());
		
		int parColCount = columnCount(index.parent()),
			parRowCount = rowCount(index.parent());

		if(!_dataSet || index.column() >= parColCount || index.row() >= parRowCount || index.column() < 0 || index.row() < 0)
			return false;


		const Labels	&	labels		= column->labels();
		
		switch(role)
		{
		case int(specialRoles::filter):
			if(value.typeId() != QMetaType::Bool) 
				return false;

			setManualEdits(true);
			
			return setAllowFilterOnLabel(index, value.toBool());

		case int(specialRoles::description):
			setManualEdits(true);
			return setDescriptionOnLabel(index, value.toString());

		case int(specialRoles::value):
			return false;

		default:
		{
			QString originalLabel = tq(labels[index.row()]->label(true));
			
			if(labels[index.row()]->setLabel(value.toString().toStdString()))
			{
				beginSynchingData(false);
				QModelIndex parent	= index.parent();
				size_t		row		= index.row(),
							col		= parent.column();

				/*
				 the proxymodles can have non-existent pointers stored in their source-indices, so lets roughly reset all 
				 emit dataChanged(DataSetPackage::index(row, 0, parent), DataSetPackage::index(row, columnCount(parent), parent));

				parent = indexForSubNode(column->data()->dataNode());
				emit dataChanged(DataSetPackage::index(0, col, parent), DataSetPackage::index(rowCount(), col, parent), { Qt::DisplayRole });

				parent = indexForSubNode(column);
				emit dataChanged(DataSetPackage::index(0, 0, parent), DataSetPackage::index(rowCount(parent), columnCount(parent), parent), { Qt::DisplayRole });

				emit labelChanged(tq(getColumnName(col)), originalLabel, tq(labels[index.row()]->label(true)));*/
				
				stringvec changedCols = {column->name()};
				endSynchingDataChangedColumns(changedCols, false, false);
				//setManualEdits(true); // A label change is not a data change, so don't set manual edit: this would unset synchronisation if it was on.
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

	_dataSet->column(columnIndex)->resetFilter();

	emit labelFilterChanged();

	QModelIndex parentModel = indexForSubNode(_dataSet->dataNode());
	emit dataChanged(DataSetPackage::index(0, columnIndex,	parentModel),	DataSetPackage::index(rowCount() - 1, columnIndex, parentModel), {int(specialRoles::filter)} );

	parentModel = indexForSubNode(_dataSet->column(columnIndex));
	emit dataChanged(DataSetPackage::index(0, 0,	parentModel),			DataSetPackage::index(rowCount(parentModel) - 1, columnCount(parentModel) - 1, parentModel), {int(specialRoles::filter)} );


	emit filteredOutChanged(columnIndex);
}

bool DataSetPackage::setDescriptionOnLabel(const QModelIndex & index, const QString & newDescription)
{
	Label  * label  = dynamic_cast<Label*>(indexPointerToNode(index));
	Column * column = dynamic_cast<Column*>(label->parent());

	if(!column)
		return false;

	QModelIndex parent	= index.parent();
	size_t		row		= index.row();


	if(int(row) > rowCount(parent))
		return false;

	const Labels	& labels = column->labels();

	int col = column->data()->columnIndex(column);

	labels[row]->setDescription(newDescription.toStdString());

	emit dataChanged(DataSetPackage::index(row, 0, parent),	DataSetPackage::index(row, columnCount(parent), parent), {int(specialRoles::description)});	//Emit dataChanged for filter

	return true;


}

bool DataSetPackage::setAllowFilterOnLabel(const QModelIndex & index, bool newAllowValue)
{
	JASPTIMER_SCOPE(DataSetPackage::setAllowFilterOnLabel);

	Label  * label  = dynamic_cast<Label*>(indexPointerToNode(index));
	Column * column = dynamic_cast<Column*>(label->parent());
	
	if(!column)
		return false;

	bool atLeastOneRemains = newAllowValue;


	QModelIndex parent	= index.parent();
	size_t		row		= index.row();


	if(int(row) > rowCount(parent))
		return false;

	const Labels	& labels = column->labels();

	if(!atLeastOneRemains) //Do not let the user uncheck every single one because that is useless, the user wants to uncheck row so lets see if there is another one left after that.
		for(size_t i=0; i< labels.size(); i++)
		{
			if(i != row && labels[i]->filterAllows())
			{
				atLeastOneRemains = true;
				break;
			}
			else if(i == row && labels[i]->filterAllows() == newAllowValue) //Did not change!
				return true;
		}

	if(atLeastOneRemains)
	{
		int col = column->data()->columnIndex(column);

		bool before = column->hasFilter();
		labels[row]->setFilterAllows(newAllowValue);

		if(before != column->hasFilter())
			notifyColumnFilterStatusChanged(col); //basically resetModel now

		emit labelFilterChanged();
		QModelIndex columnParentNode = indexForSubNode(column);
		emit dataChanged(DataSetPackage::index(row, 0, columnParentNode),	DataSetPackage::index(row, columnCount(columnParentNode), columnParentNode), { int(specialRoles::filter) });
		emit filteredOutChanged(col);

		return true;
	}
	else
		return false;
}

int DataSetPackage::filteredOut(size_t col) const
{
	if(!_dataSet || int(col) >= dataColumnCount())
		return 0; //or -1?

	const Column * column = _dataSet->column(col);
	if(!column)
		return 0;

	const Labels &	labels		= column->labels();
	int			filteredOut = 0;

	for(const Label * label : labels)
		if(!label->filterAllows())
			filteredOut++;

	return filteredOut;
}

Qt::ItemFlags DataSetPackage::flags(const QModelIndex &index) const
{
	const auto *	node		= indexPointerToNode(index);
	bool			isDataNode	= node && (node->nodeType() == dataSetBaseNodeType::data || node->nodeType() == dataSetBaseNodeType::column),
					isEditable	= !isDataNode || (_dataMode && !isColumnComputed(index.column()));

	return Qt::ItemIsSelectable | Qt::ItemIsEnabled | (isEditable ? Qt::ItemIsEditable : Qt::NoItemFlags);
}

QHash<int, QByteArray> DataSetPackage::roleNames() const
{
	static bool						set = false;
	static QHash<int, QByteArray> roles = QAbstractItemModel::roleNames ();

	if(!set)
	{
		for(const auto & enumString : dataPkgRolesToStringMap())
			roles[int(enumString.first)] = QString::fromStdString(enumString.second).toUtf8();

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

QString DataSetPackage::description() const
{
	return tq(_dataSet ? _dataSet->description() : "");
}

void DataSetPackage::setDescription(const QString &description)
{
	if (!_dataSet) return;

	_dataSet->setDescription(fq(description));

	emit descriptionChanged();
}

int DataSetPackage::findIndexByName(const std::string & name) const
{
	return _dataSet->getColumnIndex(name);
}

bool DataSetPackage::isColumnNameFree(const std::string & name) const
{
	return findIndexByName(name) == -1;
}

bool DataSetPackage::isColumnComputed(size_t colIndex) const
{
	const Column * normalCol = _dataSet->columns().at(colIndex);
	
	return normalCol->isComputed();
}

bool DataSetPackage::isColumnComputed(const std::string & name) const
{
	const Column * normalCol = _dataSet->column(name);

	return normalCol && normalCol->isComputed();
}

bool DataSetPackage::isColumnAnalysisNotComputed(const std::string & name) const
{
	const Column * normalCol = _dataSet->column(name);

	return normalCol && normalCol->codeType() == computedColumnType::analysisNotComputed;
}

bool DataSetPackage::isColumnInvalidated(size_t colIndex) const
{
	return colIndex <= dataColumnCount() && _dataSet->columns().at(colIndex)->invalidated();
}

std::string DataSetPackage::getComputedColumnError(size_t colIndex) const
{
	return colIndex >= dataColumnCount() ? "" : _dataSet->columns().at(colIndex)->error();
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
		{
			int idx = findIndexByName(col);
			if(idx >= 0)
				notifyColumnFilterStatusChanged(idx);
		}
}


bool DataSetPackage::isColumnUsedInEasyFilter(const std::string & colName) const
{
	return _columnNameUsedInEasyFilter.count(colName) > 0 && _columnNameUsedInEasyFilter.at(colName);
}

void DataSetPackage::notifyColumnFilterStatusChanged(int columnIndex)
{
	JASPTIMER_SCOPE(DataSetPackage::notifyColumnFilterStatusChanged);

	emit columnsFilteredCountChanged();
	//emit headerDataChanged(Qt::Horizontal, columnIndex, columnIndex); //this keeps crashing jasp and i dont know why
	beginResetModel();
	endResetModel();
}


QVariant DataSetPackage::getColumnTypesWithIcons() const
{
	static QVariantList ColumnTypeAndIcons;

	if(ColumnTypeAndIcons.size() == 0)
	{
		ColumnTypeAndIcons.push_back("");
		ColumnTypeAndIcons.push_back("variable-nominal.png");
		ColumnTypeAndIcons.push_back("variable-nominal-text.png");
		ColumnTypeAndIcons.push_back("variable-ordinal.png");
		ColumnTypeAndIcons.push_back("variable-scale.png");
	}

	return QVariant(ColumnTypeAndIcons);
}

int DataSetPackage::columnsFilteredCount()
{
	if(_dataSet == nullptr) return 0;

	int colsFiltered = 0;

	for(Column * col : _dataSet->columns())
		if(col->hasFilter())
			colsFiltered++;

	return colsFiltered;
}

void DataSetPackage::resetAllFilters()
{
	for(Column * col : _dataSet->columns())
		col->resetFilter();

	emit allFiltersReset();
	emit columnsFilteredCountChanged();
	//this is only used in conjunction with a reset so dont do: emit headerDataChanged(Qt::Horizontal, 0, columnCount());
}

bool DataSetPackage::setColumnType(int columnIndex, columnType newColumnType)
{
	if (_dataSet == nullptr)
		return true;

	columnTypeChangeResult	feedback;

	feedback = _dataSet->column(columnIndex)->changeType(newColumnType);

	if (feedback == columnTypeChangeResult::changed) //Everything went splendidly
	{
		beginResetModel();
		endResetModel();
		emit columnDataTypeChanged(tq(_dataSet->column(columnIndex)->name()));
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

		case columnTypeChangeResult::generatedFromAnalysis: //Some outputted column from ML or so
			informUser = tr("This column is generated by an analysis and its type is fixed.");
			break;
		}

		emit showWarning("Changing column type failed", informUser);
	}

	return feedback == columnTypeChangeResult::changed;
}

void DataSetPackage::refreshColumn(QString columnName)
{
	beginResetModel();
	endResetModel();

	return;
/*
	if(!_dataSet) return;

	int colIndex = getColumnIndex(columnName);

	if(colIndex >= 0)
	{
		QModelIndex p = indexForSubNode(_dataSet->dataNode());
		emit dataChanged(index(0, colIndex, p), index(rowCount(p), colIndex, p));
		emit headerDataChanged(Qt::Horizontal, colIndex, colIndex);
	}*/
}

void DataSetPackage::columnWasOverwritten(const std::string & columnName, const std::string &)
{
	for(size_t col=0; col<_dataSet->columns().size(); col++)
		if(_dataSet->columns()[col]->name() == columnName)
			emit dataChanged(index(0, col, indexForSubNode(_dataSet->dataNode())), index(rowCount()-1, col, indexForSubNode(_dataSet->dataNode())) );
}

void DataSetPackage::beginSynchingData(bool informEngines)
{
	beginLoadingData(informEngines);
	_synchingData = true;
}

void DataSetPackage::endSynchingDataChangedColumns(stringvec &	changedColumns, bool hasNewColumns, bool informEngines)
{
	 stringvec				missingColumns;
	 strstrmap		changeNameColumns;

	endSynchingData(changedColumns, missingColumns, changeNameColumns, hasNewColumns, informEngines);
}

void DataSetPackage::endSynchingData(	stringvec		&	changedColumns,
										stringvec		&	missingColumns,
										strstrmap		&	changeNameColumns,  //origname -> newname
										bool				rowCountChanged,
										bool				hasNewColumns,
										bool				informEngines)
{

	endLoadingData(informEngines);
	_synchingData = false;
	//We convert all of this stuff to qt containers even though this takes time etc. Because it needs to go through a (queued) connection and it might not work otherwise
	emit datasetChanged(tq(changedColumns), tq(missingColumns), tq(changeNameColumns), rowCountChanged, hasNewColumns);

	setManualEdits(false);
}


void DataSetPackage::beginLoadingData(bool informEngines)
{
	JASPTIMER_SCOPE(DataSetPackage::beginLoadingData);

	enginesPrepareForData();
	beginResetModel();
}

void DataSetPackage::endLoadingData(bool informEngines)
{
	JASPTIMER_SCOPE(DataSetPackage::endLoadingData);

	Log::log() << "DataSetPackage::endLoadingData" << std::endl;

	endResetModel();
	enginesReceiveNewData();

	emit modelInit();
}

void DataSetPackage::setDataSetSize(size_t columnCount, size_t rowCount)
{
	
	JASPTIMER_SCOPE(DataSetPackage::setDataSetSize);
		
	_dataSet->setColumnCount(columnCount);
	_dataSet->setRowCount(rowCount);
}

void DataSetPackage::dbDelete()
{
	JASPTIMER_SCOPE(DataSetPackage::dbDelete);
	if(_dataSet && _dataSet->id() != -1)
		_dataSet->dbDelete();
}

void DataSetPackage::createDataSet()
{
	JASPTIMER_SCOPE(DataSetPackage::createDataSet);
					
	dbDelete();
	deleteDataSet();
	_dataSet = new DataSet();
	setDefaultWorkspaceEmptyValues();
	_dataSubModel->selectNode(_dataSet->dataNode());
	_filterSubModel->selectNode(_dataSet->filtersNode());
	
	_dataSet->setModifiedCallback([&](){ setModified(true); }); //DataSet and co dont use Qt so instead we just use a callback
}

void DataSetPackage::loadDataSet(std::function<void(float)> progressCallback)
{
	if(_dataSet)
		deleteDataSet(); //no dbDelete necessary cause we just copied an old sqlite file here from the JASP file

	_dataSet = new DataSet(0);
	_dataSet->dbLoad(1, jaspVersion(), progressCallback); //Right now there can only be a dataSet with ID==1 so lets keep it simple
	_dataSubModel->selectNode(_dataSet->dataNode());
	_filterSubModel->selectNode(_dataSet->filtersNode());

	DataSetPackage::pkg()->initializeComputedColumns();

	emit synchingExternallyChanged(synchingExternally());
}

void DataSetPackage::deleteDataSet()
{
	JASPTIMER_SCOPE(DataSetPackage::deleteDataSet);


	_dataSubModel->selectNode(nullptr);
	_filterSubModel->selectNode(nullptr);
	
	delete _dataSet;
	_dataSet = nullptr;
	_undoStack->clear();
}

bool DataSetPackage::initColumnAsScale(size_t colNo, const std::string & newName, const doublevec & values)
{
	JASPTIMER_SCOPE(DataSetPackage::initColumnAsScale);
	
	bool result = _dataSet->columns()[colNo]->initAsScale(colNo, newName, values);

	return result;
}

intstrmap DataSetPackage::initColumnAsNominalText(size_t colNo, const std::string & newName, const stringvec & values, const strstrmap & labels)
{
	JASPTIMER_SCOPE(DataSetPackage::initColumnAsNominalText);
	
	intstrmap result =  _dataSet->columns()[colNo]->initAsNominalText(colNo, newName, values, labels);

	return result;
}

bool DataSetPackage::initColumnAsNominalOrOrdinal(size_t colNo, const std::string & newName, const intvec & values, bool is_ordinal)
{
	JASPTIMER_SCOPE(DataSetPackage::initColumnAsNominalOrOrdinal);
	
	bool result =  _dataSet->columns()[colNo]->initAsNominalOrOrdinal(colNo, newName, values, is_ordinal);

	return result;
}

bool DataSetPackage::initColumnAsNominalOrOrdinal(size_t colNo, const std::string & newName, const intvec & values, const intstrmap &uniqueValues, bool is_ordinal)
{
	JASPTIMER_SCOPE(DataSetPackage::initColumnAsNominalOrOrdinal with uniqueValues);
	
	bool result =  _dataSet->columns()[colNo]->initAsNominalOrOrdinal(colNo, newName, values, uniqueValues, is_ordinal);


	return result;
}

int DataSetPackage::getColIndex(QVariant colID)
{
	if(colID.typeId() == QMetaType::Int || colID.typeId() == QMetaType::UInt)
		return colID.typeId() == QMetaType::Int ? colID.toInt() : colID.toUInt();

	else
		return _dataSet->getColumnIndex(fq(colID.toString()));
}

bool DataSetPackage::convertVecToInt(int colIndex, const std::vector<std::string> &values, std::vector<int> &intValues, std::set<int> &uniqueValues, std::map<int, std::string> &emptyValuesMap)
{
	JASPTIMER_SCOPE(ColumnUtils::convertVecToInt);

	emptyValuesMap.clear();
	uniqueValues.clear();
	intValues.clear();
	intValues.reserve(values.size());

	int row = 0;

	for (const std::string &value : values)
	{
		int intValue = std::numeric_limits<int>::lowest();

		if (_dataSet->column(colIndex)->convertValueToIntForImport(value, intValue))
		{
			if (intValue != std::numeric_limits<int>::lowest())	uniqueValues.insert(intValue);
			else if (!value.empty())							emptyValuesMap.insert(make_pair(row, value));

			intValues.push_back(intValue);
		}
		else
		{
			std::vector<int>().swap(intValues); //this clears intValues and guarentees its memory is released
			return false;
		}

		row++;
	}

	return true;
}


bool DataSetPackage::convertVecToDouble(int colIndex, const stringvec & values, doublevec & doubleValues, intstrmap & emptyValuesMap)
{
	JASPTIMER_SCOPE(DataSetPackage::convertVecToDouble);

	emptyValuesMap.clear();
	doubleValues.clear();
	doubleValues.resize(values.size());

	int row = 0;
	for (const std::string & value : values)
	{
		double doubleValue = static_cast<double>(NAN);

		if (_dataSet->column(colIndex)->convertValueToDoubleForImport(value, doubleValue))
		{
			doubleValues[row] = doubleValue;

			if (std::isnan(doubleValue) && value != ColumnUtils::emptyValue)
				emptyValuesMap.insert(std::make_pair(row, value));
		}
		else
		{
			std::vector<double>().swap(doubleValues); //this clears doubleValues and guarentees its memory is released
			return false;
		}

		row++;
	}

	return true;
}



void DataSetPackage::initColumnWithStrings(QVariant colId, const std::string & newName, const stringvec &values, const std::string & title, columnType desiredType)
{
	JASPTIMER_SCOPE(DataSetPackage::initColumnWithStrings);
	
	// interpret the column as a datatype
	intset		uniqueValues;
	intvec		intValues;
	doublevec	doubleValues;
	intstrmap	emptyValuesMap;
	int			colIndex = getColIndex(colId);

	//If less unique integers than the thresholdScale then we think it must be ordinal: https://github.com/jasp-stats/INTERNAL-jasp/issues/270
	bool	useCustomThreshold	= Settings::value(Settings::USE_CUSTOM_THRESHOLD_SCALE).toBool();
	size_t	thresholdScale		= (useCustomThreshold ? Settings::value(Settings::THRESHOLD_SCALE) : Settings::defaultValue(Settings::THRESHOLD_SCALE)).toUInt();

	JASPTIMER_RESUME(DataSetPackage::initColumnWithStrings preamble);
	bool valuesAreIntegers		= convertVecToInt(colIndex, values, intValues, uniqueValues, emptyValuesMap);
		
	size_t minIntForThresh		= thresholdScale > 2 ? 2 : 0;

	auto isNominalInt			= [&](){ return valuesAreIntegers && (desiredType == columnType::nominal || uniqueValues.size() == minIntForThresh); };
	auto isOrdinal				= [&](){ return valuesAreIntegers && (desiredType == columnType::ordinal || (uniqueValues.size() >  minIntForThresh && uniqueValues.size() <= thresholdScale)); };
	auto isScalar				= [&](){ return convertVecToDouble(colIndex, values, doubleValues, emptyValuesMap); };
	
	JASPTIMER_STOP(DataSetPackage::initColumnWithStrings preamble);

	JASPTIMER_RESUME(DataSetPackage::initColumnWithStrings followup - initing columns);
	
	if		(isOrdinal())					initColumnAsNominalOrOrdinal(	colIndex,	newName,	intValues,		true	);
	else if	(isNominalInt())				initColumnAsNominalOrOrdinal(	colIndex,	newName,	intValues,		false	);
	else if	(isScalar())					initColumnAsScale(				colIndex,	newName,	doubleValues	);
	else				emptyValuesMap =	initColumnAsNominalText(		colIndex,	newName,	values			);
	
	JASPTIMER_STOP(DataSetPackage::initColumnWithStrings followup - initing columns);

	storeMissingData(newName, emptyValuesMap);
}

void DataSetPackage::initializeComputedColumns()
{
	for(const Column * col : dataSet()->columns())
		emit checkForDependentColumnsToBeSent(tq(col->name()));
}


stringvec DataSetPackage::getColumnNames()
{
	stringvec names;

	if(_dataSet)
		for(const Column * col : _dataSet->columns())
				names.push_back(col->name());

	return names;
}

bool DataSetPackage::isColumnDifferentFromStringValues(const std::string & columnName, const stringvec & strVals)
{
	Column * col = _dataSet->column(columnName);
	
	if(col)
		return col->isColumnDifferentFromStringValues(strVals);

	return true;
}

void DataSetPackage::renameColumn(const std::string & oldColumnName, const std::string & newColumnName)
{
	try
	{
		Column * col = _dataSet->column(oldColumnName);
		col->setName(newColumnName);
	}
	catch(...)
	{
		Log::log() << "Couldn't rename column from '" << oldColumnName << "' to '" << newColumnName << "'" << std::endl;
	}
}

void DataSetPackage::writeDataSetToOStream(std::ostream & out, bool includeComputed)
{
	std::vector<const Column*> cols;

	//Add a UTF-8 BOM
	out.put(0xEF);
	out.put(0xBB);
	out.put(0xBF);

	int columnCount = _dataSet->columnCount();

	for (int i = 0; i < columnCount; i++)
	{
		Column		*	column	= _dataSet->column(i);

		if(!column->isComputed() || includeComputed)
			cols.push_back(column);
	}


	for (size_t i = 0; i < cols.size(); i++)
	{
		const Column *	column	= cols[i];
		std::string		name	= column->name();

		if (stringUtils::escapeValue(name))	out << '"' << name << '"';
		else								out << name;

		if (i < cols.size()-1)	out << ",";
		else					out << "\n";

	}

	size_t rows = _dataSet->rowCount();

	for (size_t r = 0; r < rows; r++)
		for (size_t i = 0; i < cols.size(); i++)
		{
			const Column *column = cols[i];

			std::string value = column->getValue(r);
			if (value != "")
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

columnType DataSetPackage::parseColumnTypeForJASPFile(const std::string & name)
{
	if (name == "Nominal")				return  columnType::nominal;
	else if (name == "NominalText")		return  columnType::nominalText;
	else if (name == "Ordinal")			return  columnType::ordinal;
	else if (name == "Continuous")		return  columnType::scale;
	else								return  columnType::unknown;
}

Json::Value DataSetPackage::columnToJsonForJASPFile(size_t columnIndex, Json::Value & labelsData, size_t & dataSize)
{
	Column		*	column			= _dataSet->column(columnIndex);
	std::string		name			= column->name();
	Json::Value		columnMetaData	= Json::Value(Json::objectValue);
	columnMetaData["name"]			= Json::Value(name);
	columnMetaData["measureType"]	= Json::Value(getColumnTypeNameForJASPFile(column->type()));

	if(column->type() == columnType::scale)
	{
		   columnMetaData["type"] = Json::Value("number");
		   dataSize += sizeof(double) * rowCount();
	}
	else
	{
		   columnMetaData["type"] = Json::Value("integer");
		   dataSize += sizeof(int) * rowCount();
	}


	if (column->type() != columnType::scale && column->labels().size() > 0)
	{
		Json::Value &columnLabelData	= labelsData[name];
		Json::Value &labelsMetaData		= columnLabelData["labels"];
		int labelIndex = 0;

		for (const Label * label : column->labels())
		{
			Json::Value keyValueFilterPair(Json::arrayValue);

			keyValueFilterPair.append(label->value());
			keyValueFilterPair.append(label->label());
			keyValueFilterPair.append(label->filterAllows());

			labelsMetaData.append(keyValueFilterPair);
			labelIndex += 1;
		}

		/*Json::Value &orgStringValuesMetaData	= columnLabelData["orgStringValues"];
		intstrmap &orgLabels	= labels.getOrgStringValues();
		for (const auto & pair : orgLabels)
		{
			Json::Value keyValuePair(Json::arrayValue);
			keyValuePair.append(pair.first);
			keyValuePair.append(pair.second);
			orgStringValuesMetaData.append(keyValuePair);
		}*/
		
		MessageForwarder::showWarning("Not including original value strings cause this function shouldnt be used at all anyway!");
	}


	return columnMetaData;
}

void DataSetPackage::columnLabelsFromJsonForJASPFile(Json::Value xData, Json::Value columnDesc, size_t columnIndex, std::map<std::string, std::map<int, int> > & mapNominalTextValues)
{
	std::string     name				= columnDesc["name"].asString();
	Json::Value &   orgStringValuesDesc	= columnDesc["orgStringValues"],
				&   labelsDesc			= columnDesc["labels"];
	intintmap   &   mapValues           = mapNominalTextValues[name];	// This is needed for *really old* JASP files where factor keys where not filled in the right way

	if (labelsDesc.isNull() &&  ! xData.isNull())
	{
		Json::Value &columnlabelData = xData[name];

		if (!columnlabelData.isNull())
		{
			labelsDesc			= columnlabelData["labels"];
			orgStringValuesDesc = columnlabelData["orgStringValues"];
		}
	}


	Column *column = _dataSet->column(columnIndex);
	columnType columnType = parseColumnTypeForJASPFile(columnDesc["measureType"].asString());

	column->setName(name);
	column->setType(columnType);

	int nomTextIndex = 0;

	//Log::log() << "For column '" << name << "' we get labelsDesc: '" << labelsDesc.toStyledString() << "'" << std::endl;

	for (Json::Value & keyValueFilterTrip : labelsDesc)
	{
		int zero			= 0; //MSVC complains on int(0) with: error C2668: 'Json::Value::get': ambiguous call to overloaded function
		int key				= keyValueFilterTrip.get(zero,		Json::nullValue).asInt();
		std::string label	= keyValueFilterTrip.get(1,			Json::nullValue).asString();
		bool filterAllow	= keyValueFilterTrip.get(2,			true).asBool();
		int labelValue		= key;

		//Log::log() << "For keyValueFilterTrip '" << keyValueFilterTrip.toStyledString() << "' we managed to extract key " << key << ", label '" << label << "' and filterAllow " << (filterAllow ? "yes" : "no") << std::endl;

		if (columnType == columnType::nominalText)
		{
			labelValue		= nomTextIndex++;
			mapValues[key]	= labelValue;
		}

		_dataSet->columns()[columnIndex]->labelsAdd(labelValue, label, filterAllow, "", columnType == columnType::nominalText ? Json::Value(label) : Json::Value(key)); //temporarily set something as original value, later it might be a real origValue
	}

	if (!orgStringValuesDesc.isNull())
	{
		for (Json::Value & keyValuePair : orgStringValuesDesc)
		{
			int zero		= 0; //MSVC complains on int(0) with: error C2668: 'Json::Value::get': ambiguous call to overloaded function
			int key			= keyValuePair.get(zero,	Json::nullValue).asInt();
			std::string val = keyValuePair.get(1,		Json::nullValue).asString();

			if (mapValues.find(key) != mapValues.end())
				key = mapValues[key];
			else
				Log::log() << "Cannot find key " << key << std::flush;

			Label* label = _dataSet->columns()[columnIndex]->labelByValue(key);
			if (label)
				label->setOriginalValue(val);
			else
				Log::log() << "Cannot find label for key " << key << std::endl;
		}
	}
}

intvec DataSetPackage::getColumnDataInts(size_t columnIndex)
{
	if(_dataSet == nullptr) return {};

	return _dataSet->columns()[columnIndex]->ints();
}

doublevec DataSetPackage::getColumnDataDbls(size_t columnIndex)
{
	
	return !_dataSet || !_dataSet->columns()[columnIndex] ? doublevec() : _dataSet->columns()[columnIndex]->dbls();
	
}

stringvec DataSetPackage::getColumnDataStrs(size_t columnIndex)
{
	JASPTIMER_SCOPE(DataSetPackage::getColumnDataStrs);

	if(_dataSet == nullptr)
		return {};

	Column * col = _dataSet->column(columnIndex);
	
	stringvec out;
	out.reserve(_dataSet->rowCount());
	
	for(size_t r=0; r<col->rowCount(); r++)
	{
		std::string value = col->getValue(r);	
		out.push_back(value != "." ? value : "");
	}

	return out;
}

void DataSetPackage::setColumnName(size_t columnIndex, const std::string & newName, bool resetModel)
{
	if(!_dataSet)
		return;

	Column* column = _dataSet->column(columnIndex);
	if (!column)
		return;

	std::string oldName = getColumnName(columnIndex);

	if(resetModel)
		beginResetModel();

	column->setName(newName);

	if(resetModel)
		endResetModel();

	emit datasetChanged({}, {}, QMap<QString, QString>({{tq(oldName), tq(newName)}}), false, false);
}

void DataSetPackage::setColumnTitle(size_t columnIndex, const std::string & newTitle)
{
	if(!_dataSet)
		return;

	Column* column = _dataSet->column(columnIndex);
	if (!column)
		return;

	column->setTitle(newTitle);
}

void DataSetPackage::setColumnDescription(size_t columnIndex, const std::string & newDescription)
{
	if(!_dataSet)
		return;

	Column* column = _dataSet->column(columnIndex);
	if (!column)
		return;

	column->setDescription(newDescription);
}

void DataSetPackage::setColumnAsComputed(size_t columnIndex, computedColumnType type)
{
	if(!_dataSet)
		return;

	Column* column = _dataSet->column(columnIndex);
	if (!column)
		return;

	column->setCodeType(type);

	emit dataChanged(index(0, columnIndex), index(rowCount() - 1, columnIndex));
}

void DataSetPackage::setColumnHasCustomEmptyValues(size_t columnIndex, bool hasCustomEmptyValue)
{
	if(!_dataSet)
		return;

	Column* column = _dataSet->column(columnIndex);
	if (!column || column->hasCustomEmptyValues() == hasCustomEmptyValue)
		return;

	column->setHasCustomEmptyValues(hasCustomEmptyValue);

	emit dataChanged(index(0, columnIndex), index(rowCount() - 1, columnIndex));

	if(!hasCustomEmptyValue)
		resetMissingData({column});
}

void DataSetPackage::setColumnCustomEmptyValues(size_t columnIndex, const stringset& customEmptyValues)
{
	if(!_dataSet)
		return;

	Column* column = _dataSet->column(columnIndex);
	if (!column || column->emptyValues() == customEmptyValues)
		return;

	column->setCustomEmptyValues(customEmptyValues);
	resetMissingData({column});

	emit dataChanged(index(0, columnIndex), index(rowCount() - 1, columnIndex));

}

void DataSetPackage::setColumnDataInts(size_t columnIndex, const intvec & ints)
{
	JASPTIMER_SCOPE(DataSetPackage::setColumnDataInts);

	Column * col = _dataSet->column(columnIndex);
	if (!col)
		return;

	for(int value : ints)
	{
		//Maybe something went wrong somewhere and we do not have labels for all values...
		if (value != std::numeric_limits<int>::lowest() && !col->labelByValue(value))
		{
			Log::log() << "Value '" << value << "' in column '" << col->name() << "' did not have a corresponding label, adding one now.\n";
			col->labelsAdd(value, std::to_string(value), true, "", value);
		}
	}

	col->setValues(ints);
	col->incRevision();
}


void DataSetPackage::setColumnDataDbls(size_t columnIndex, const doublevec & dbls)
{
	JASPTIMER_SCOPE(DataSetPackage::setColumnDataDbls);
	Column * col = _dataSet->column(columnIndex);

	if (!col)
		return;

	col->setValues(dbls);
	col->incRevision();
}

bool DataSetPackage::setFilterData(const std::string & rFilter, const boolvec & filterResult)
{
	filter()->setRFilter(rFilter);

	bool someFilterValueChanged = filter()->setFilterVector(filterResult);
	
	if(_dataSet)
		_dataSet->filter()->setFilterVector(filterResult);

	if(someFilterValueChanged) //We could also send exactly those cells that were changed if we were feeling particularly inclined to write the code...
	{
		//emit dataChanged(index(0, 0, parentModelForType(parIdxType::filter)),	index(rowCount(), 0,				parentModelForType(parIdxType::filter)));
		//This actually lets the whole application freeze when a filter is undone... -> emit dataChanged(index(0, 0, parentModelForType(parIdxType::data)),		index(rowCount(), columnCount(),	parentModelForType(parIdxType::data)));

		beginResetModel();
		endResetModel();
	}

	return someFilterValueChanged;
}

columnType DataSetPackage::getColumnType(size_t columnIndex) const
{
	return _dataSet && _dataSet->column(columnIndex) ? _dataSet->column(columnIndex)->type() : columnType::unknown;
}

std::string DataSetPackage::getColumnName(size_t columnIndex) const
{
	return _dataSet && _dataSet->column(columnIndex) ? _dataSet->column(columnIndex)->name() : "";
}

QStringList DataSetPackage::getColumnLabelsAsStringList(const std::string & columnName)	const
{
	int colIndex = getColumnIndex(columnName);

	if(colIndex > -1)	return getColumnLabelsAsStringList(colIndex);
	else				return QStringList();;
}

QStringList DataSetPackage::getColumnLabelsAsStringList(size_t columnIndex)	const
{
	QStringList list;
	if(columnIndex < 0 || columnIndex >= dataColumnCount()) 
		return list;

	for (const Label * label : _dataSet->columns()[columnIndex]->labels())
		list.append(tq(label->label()));

	return list;
}


boolvec DataSetPackage::getColumnFilterAllows(size_t columnIndex) const 
{
	boolvec list;
	if(columnIndex < 0 || columnIndex >= dataColumnCount()) 
		return list;

	for (const Label * label : _dataSet->columns()[columnIndex]->labels())
		list.push_back(label->filterAllows());

	return list;
}

stringvec DataSetPackage::getColumnLabelsAsStrVec(size_t columnIndex) const
{
	stringvec list;
	if(columnIndex < 0 || columnIndex >= dataColumnCount()) 
		return list;

	for (const Label * label : _dataSet->columns()[columnIndex]->labels())
		list.push_back(label->label());

	return list;
}


QList<QVariant> DataSetPackage::getColumnValuesAsDoubleList(size_t columnIndex)	const
{
	QList<QVariant> list;
	if(columnIndex < 0 || columnIndex >= dataColumnCount()) return list;

	for (double value : _dataSet->columns()[columnIndex]->dbls())
		list.append(value);

	return list;
}

bool DataSetPackage::labelNeedsFilter(size_t columnIndex) const
{
	if(columnIndex < 0 || columnIndex >= dataColumnCount()) 
		return false;
			
	return _dataSet->columns()[columnIndex]->hasFilter();
}


void DataSetPackage::labelMoveRows(size_t colIdx, std::vector<size_t> rows, bool up)
{
	Column		*	column	= _dataSet->columns()[colIdx];
	sizetset	rowsChanged = column->labelsMoveRows(rows, up);

	if(rowsChanged.size())
	{
		QModelIndex p = indexForSubNode(column);

		for(size_t row : rowsChanged)
			emit dataChanged(index(0, 0, p), index(rowCount(p) - 1 , columnCount(p) - 1, p));

		emit labelsReordered(tq(column->name()));
	}
}

void DataSetPackage::labelReverse(size_t colIdx)
{
	Column		*	column	= _dataSet->columns()[colIdx];

	column->labelsReverse();

	QModelIndex p = indexForSubNode(column);

	emit dataChanged(index(0, 0, p), index(rowCount(p) - 1, columnCount(p) - 1, p));
	emit labelsReordered(tq(column->name()));
}

void DataSetPackage::columnSetDefaultValues(const std::string & columnName, columnType columnType, bool emitSignals)
{
	if(!_dataSet)
		return;

	int colIndex = getColumnIndex(columnName);

	if(colIndex >= 0)
	{
		Column * column = _dataSet->columns()[colIndex];
		column->setDefaultValues(columnType);

		QModelIndex p = indexForSubNode(_dataSet->dataNode());


		if(emitSignals)
		{
			emit dataChanged(index(0, colIndex, p), index(rowCount(p), colIndex, p));
			emit headerDataChanged(Qt::Horizontal, colIndex, colIndex);
		}
	}
}

std::string DataSetPackage::freeNewColumnName(size_t startHere)
{
	const QString nameBase = tr("Column %1");

	while(true)
	{
		const std::string & newColName = fq(nameBase.arg(++startHere));
		if(isColumnNameFree(newColName))
			return newColName;
	}
}

Json::Value DataSetPackage::serializeColumn(const std::string& columnName) const
{
	Column*	column	= _dataSet->column(columnName);
	return column ? column->serialize() : Json::nullValue;
}

void DataSetPackage::deserializeColumn(const std::string& columnName, const Json::Value& col)
{
	Column		*	column	= _dataSet->column(columnName);
	column->deserialize(col);
	emit datasetChanged({tq(columnName)}, {}, {}, false, false);
}

const stringset& DataSetPackage::workspaceEmptyValues() const
{
	static stringset emptyVec;
	return _dataSet ? _dataSet->workspaceEmptyValues() : emptyVec;
}

void DataSetPackage::setDefaultWorkspaceEmptyValues()
{
	stringset values;
	for (const QString& value : PreferencesModel::prefs()->emptyValues())
		 values.insert(fq(value));
	setWorkspaceEmptyValues(values);
}

void DataSetPackage::resetMissingData(const std::vector<Column*>& cols, bool reset)
{
	if (isLoaded())
	{
		if (reset)
			beginSynchingData();

		stringvec colChanged;

		for (const auto & it : _dataSet->resetMissingData(cols))
		{
			colChanged.push_back(it.first);
			storeMissingData(it.first, it.second);
		}

		if (reset)
			endSynchingDataChangedColumns(colChanged);
	}
}


void DataSetPackage::setWorkspaceEmptyValues(const stringset &emptyValues, bool reset)
{
	if (!_dataSet) return;

	_dataSet->setWorkspaceEmptyValues(emptyValues);

	resetMissingData(_dataSet->columns(), reset);

	emit workspaceEmptyValuesChanged();
}

void DataSetPackage::pasteSpreadsheet(size_t row, size_t col, const std::vector<std::vector<QString>> & cells, intvec coltypes)
{
	JASPTIMER_SCOPE(DataSetPackage::pasteSpreadsheet);

	int		rowMax			= ( cells.size() > 0 ? cells[0].size() : 0), 
			colMax			= cells.size();
	bool	rowCountChanged = int(row + rowMax) > dataRowCount()	,
			colCountChanged = int(col + colMax) > dataColumnCount()	;

	beginSynchingData(false);
	
	if(colCountChanged || rowCountChanged)	
		setDataSetSize(std::max(size_t(dataColumnCount()), colMax + col), std::max(size_t(dataRowCount()), rowMax + row));

	stringvec colNames = getColumnNames();
	
	stringvec changed;

	for(int c=0; c<colMax; c++)
	{
		int			dataCol		= c + col;
		stringvec	colVals		= getColumnDataStrs(dataCol);
		columnType	desiredType	= coltypes.size() > c ? columnType(coltypes[c]) : columnType::unknown;

		for(int r=0; r<rowMax; r++)
		{
			std::string cellVal = fq(cells[c][r]);
			if (desiredType != columnType::unknown && desiredType != columnType::scale)
			{
				Label* label = _dataSet->columns()[dataCol]->labelByDisplay(cellVal);
				if (label)
					cellVal = (desiredType == columnType::nominalText) ? label->originalValue().asString() : std::to_string(label->value());
			}
			colVals[r + row] = cellVal == ColumnUtils::emptyValue ? "" : cellVal;
		}

		std::string colName = getColumnName(dataCol);
		initColumnWithStrings(dataCol, colName, colVals, "", desiredType);

		changed.push_back(colName);

	}

	strstrmap		changeNameColumns;
	stringvec		missingColumns;

	endSynchingData(changed, missingColumns, changeNameColumns, rowCountChanged, colCountChanged, false);
	setManualEdits(true); //set manual edits here so external synching is turned off, endSynchingData also just reset it, so thats why it is way down here
}

QString DataSetPackage::insertColumnSpecial(int column, const QMap<QString, QVariant>& props)
{
	if(column < 0)
		column = 0;

	if(column > dataColumnCount())
		column = dataColumnCount(); //the column will be created if necessary but only if it is in a logical place. So the end of the vector

	setManualEdits(true); //Don't synch with external file after editing
#ifdef ROUGH_RESET
	beginResetModel();
#else
	beginInsertColumns(indexForSubNode(_dataSet->dataNode()), column, column);
#endif

	stringvec changed;

	_dataSet->insertColumn(column);

	const std::string & name = props.contains("name") ? fq(props["name"].toString()) : freeNewColumnName(column);
	columnType type = props.contains("type") ? columnType(props["type"].toInt()) : columnType::scale;
	setColumnName(column, name, false);
	_dataSet->column(column)->setDefaultValues(type);

	if(props.contains("computed"))
		_dataSet->column(column)->setCodeType(computedColumnType(props["computed"].toInt()));

	_dataSet->incRevision();

	changed.push_back(name);

#ifdef ROUGH_RESET
	endResetModel();
#else
	endInsertColumns();
#endif

	strstrmap		changeNameColumns;
	stringvec		missingColumns;

	emit datasetChanged(tq(changed), tq(missingColumns), tq(changeNameColumns), false, true);

	return QString::fromStdString(name);
}

QString DataSetPackage::appendColumnSpecial(const QMap<QString, QVariant>& props)
{
	return insertColumnSpecial(dataColumnCount(), props);
}


bool DataSetPackage::insertColumns(int column, int count, const QModelIndex & aparent)
{
	if(column > dataColumnCount())
		column = dataColumnCount(); //the column will be created if necessary but only if it is in a logical place. So the end of the vector

	setManualEdits(true); //Don't synch with external file after editing
#ifdef ROUGH_RESET
	beginResetModel();
#else
	beginInsertColumns(indexForSubNode(_dataSet->dataNode()), column, column + count - 1);
#endif

	stringvec changed;

	for(int c = column; c<column+count; c++)
	{
		_dataSet->insertColumn(c);
		const std::string & name = freeNewColumnName(c);
		setColumnName(c, name, false);
		_dataSet->column(c)->setDefaultValues(columnType::scale);

		changed.push_back(name);
	}
#ifdef ROUGH_RESET
	endResetModel();
#else
	endInsertColumns();
#endif

	strstrmap		changeNameColumns;
	stringvec		missingColumns;

	emit datasetChanged(tq(changed), tq(missingColumns), tq(changeNameColumns), true, false);

	return true;
}

bool DataSetPackage::removeColumns(int column, int count, const QModelIndex & aparent)
{
	if(column == -1)
		return false;

	emit columnsBeingRemoved(column, count);

	setManualEdits(true); //Don't synch with external file after editing
#ifdef ROUGH_RESET
	beginResetModel();
#else
	beginRemoveColumns(indexForSubNode(_dataSet->dataNode()), column, column + count - 1);
#endif

	stringvec	changed;
	strstrmap	changeNameColumns;
	stringvec	missingColumns;

	for(int c = column + count; c>column; c--)
	{
		missingColumns.push_back(getColumnName(c - 1));
		_dataSet->removeColumn(c - 1);
	}
#ifdef ROUGH_RESET
	endResetModel();
#else
	endRemoveColumns();
#endif
	emit datasetChanged(tq(changed), tq(missingColumns), tq(changeNameColumns), false, true);

	return true;
}

bool DataSetPackage::insertRows(int row, int count, const QModelIndex & aparent)
{
	if(row > dataRowCount())
		row = dataRowCount();

	setManualEdits(true); //Don't synch with external file after editing
#ifdef ROUGH_RESET
	beginResetModel();
#else
	beginInsertRows(indexForSubNode(_dataSet->dataNode()), row, row + count - 1);
#endif
	stringvec changed;


	dataSet()->beginBatchedToDB();

	for(int c=0; c<dataColumnCount(); c++)
	{
		const std::string & name = getColumnName(c);
		changed.push_back(name);

		for(int r=row; r<row+count; r++)
			dataSet()->column(c)->rowInsertEmptyVal(r);
	}

	dataSet()->setRowCount(dataSet()->rowCount() + count);
	dataSet()->incRevision();
	dataSet()->endBatchedToDB();
#ifdef ROUGH_RESET
	endResetModel();
#else
	endInsertRows();
#endif
	strstrmap		changeNameColumns;
	stringvec		missingColumns;

	emit datasetChanged(tq(changed), tq(missingColumns), tq(changeNameColumns), true, false);

	return true;
}

bool DataSetPackage::removeRows(int row, int count, const QModelIndex & aparent)
{
	if(row == -1)
		return false;

	setManualEdits(true); //Don't synch with external file after editing
#ifdef ROUGH_RESET
	beginResetModel();
#else
	beginRemoveRows(indexForSubNode(_dataSet->dataNode()), row, row + count - 1);
#endif
	stringvec changed;

	dataSet()->beginBatchedToDB();
	
	for(int c=0; c<dataColumnCount(); c++)
	{
		const std::string & name = getColumnName(c);
		changed.push_back(name);
	
		for(int r=row+count; r>row; r--)
			dataSet()->column(c)->rowDelete(r-1);
	}

	setDataSetSize(dataColumnCount(), dataRowCount()-count);
	dataSet()->incRevision();
	dataSet()->endBatchedToDB();

	strstrmap		changeNameColumns;
	stringvec		missingColumns;
#ifdef ROUGH_RESET
	endResetModel();
#else
	endRemoveRows();
#endif
	emit datasetChanged(tq(changed), tq(missingColumns), tq(changeNameColumns), true, false);

	return true;
}

void DataSetPackage::storeMissingData(const std::string & columnName, const intstrmap & emptyValues)
{
	JASPTIMER_SCOPE(DataSetPackage::storeInEmptyValues);
	_dataSet->setMissingData(columnName, emptyValues);
}

Column * DataSetPackage::createColumn(const std::string & name, columnType columnType)
{
	if(getColumnIndex(name) >= 0)
		return nullptr;

	setModified(true);

	size_t newColumnIndex	= dataColumnCount();

	enginesPrepareForData();
	beginResetModel();
	_dataSet->insertColumn(newColumnIndex);
	_dataSet->column(newColumnIndex)->setName(name);
	_dataSet->column(newColumnIndex)->setDefaultValues(columnType);
	endResetModel();
	enginesReceiveNewData();

	return _dataSet->column(newColumnIndex);
}

void DataSetPackage::removeColumn(const std::string & name)
{
	int colIndex = getColumnIndex(name);
	if(colIndex == -1) return;

	removeColumns(colIndex, 1);
}

bool DataSetPackage::columnExists(Column *column)
{
	if(_dataSet)
		for(const Column * col : _dataSet->columns())
			if(col == column)
				return true;
		
	return false;
}

boolvec DataSetPackage::filterVector()
{
	boolvec out;

	if(_dataSet)
		out = boolvec(_dataSet->filter()->filtered().begin(), _dataSet->filter()->filtered().end());
	
	return out;
}

void DataSetPackage::databaseStopSynching()
{
	_databaseIntervalSyncher.stop();	
}

void DataSetPackage::databaseStartSynching(bool syncImmediately)
{
	if(_database == Json::nullValue)
		throw std::runtime_error("Cannot start synching with a database if we arent connected to a database...");
	
	_databaseIntervalSyncher.stop(); //Is this even necessary? Probaly not but lets do it just in case

	DatabaseConnectionInfo dbCI(_database);
	
	if(dbCI._interval > 0)
	{
		if(dbCI._hadPassword && !dbCI._rememberMe && dbCI._password == "")
		{
			bool tryAgain = true, couldConnect;

			while(tryAgain)
			{
				dbCI._password	= MessageForwarder::askPassword(tr("Database Password"), dbCI._username != "" ? tr("The databaseconnection needs a password for user '%1'").arg(dbCI._username) : tr("The databaseconnection needs a password"));
				tryAgain		= dbCI.connect() ? false : MessageForwarder::showYesNo(tr("Connection failed"), tr("Could not connect to database because of '%1', want to try a different password?").arg(dbCI.lastError()));
			}

			if(!dbCI.connected())
			{
				MessageForwarder::showWarning(tr("Could not connect to the database so synchronizing will be disabled."));
				return;
			}
			else
				_database = dbCI.toJson();
		}
	
		_databaseIntervalSyncher.setInterval(1000 * 60 * dbCI._interval);
		_databaseIntervalSyncher.start();
		
		if(syncImmediately)
			emit synchingIntervalPassed();

		emit synchingExternallyChanged(synchingExternally());
	}
}

bool DataSetPackage::synchingExternally() const
{
	return _dataSet && _dataSet->dataFileSynch() && (!_dataSet->dataFilePath().empty() || (_database != Json::nullValue && _databaseIntervalSyncher.isActive()));
}

void DataSetPackage::setSynchingExternallyFriendly(bool synchingExternally)
{
	if (synchingExternally && emit askUserForExternalDataFile())
	{
		setSynchingExternally(synchingExternally);
		setModified(true); //Perhaps someone would like to save the fact that it shouldnt be synchronized
	}
	else if(!synchingExternally)
	{
		if(_dataSet && _dataSet->dataFileSynch())
			setSynchingExternally(false);
		setModified(true);
	}
}

void DataSetPackage::setSynchingExternally(bool synchingExternally)
{	
	if(_dataSet)
		_dataSet->setDataFileSynch(synchingExternally);

	emit synchingExternallyChanged(DataSetPackage::synchingExternally());
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

void DataSetPackage::setMissingData(Json::Value &emptyValues)
{ 
	// This is used only for old jasp file that did not have a sqlite database
	_dataSet->setEmptyValuesJson(emptyValues);
}

bool DataSetPackage::currentFileIsExample() const
{
	return currentFile().startsWith(AppDirs::examples());
}

void DataSetPackage::setDataFilePath(std::string filePath)				
{
	if(!_dataSet || _dataSet->dataFilePath() == filePath)
		return;	

	_dataSet->setDataFilePath(filePath);
	if (tq(filePath).startsWith(AppDirs::examples()))
		setDataFileReadOnly(true);

	emit synchingExternallyChanged(synchingExternally());
}

void DataSetPackage::setDatabaseJson(const Json::Value &dbInfo)		
{
	_database						= dbInfo;			
	Log::log() << "DataSetPackage::setDatabaseJson got:" << dbInfo << std::endl;

	_dataSet->setDatabaseJson(_database.toStyledString());
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


void DataSetPackage::checkComputedColumnDependenciesForAnalysis(Analysis * analysis)
{
	if(!_dataSet)
		return;
	
	for(Column * col : _dataSet->columns())
		if(col->isComputedByAnalysis(analysis->id()))
			col->setDependsOn(analysis->usedVariables());
		
}

Column * DataSetPackage::createComputedColumn(const std::string & name, columnType type, computedColumnType desiredType, Analysis * analysis)
{
	QString nameTemp = insertColumnSpecial(dataColumnCount(), { std::make_pair("computed", int(desiredType)) });

	Column	* newComputedColumn = DataSetPackage::pkg()->dataSet()->column(nameTemp.toStdString());

	beginResetModel();

	newComputedColumn->setName(name);
	newComputedColumn->setType(type);

	if(analysis)
		newComputedColumn->setAnalysisId(analysis->id());

	endResetModel();

	return newComputedColumn;
}

Column * DataSetPackage::requestComputedColumnCreation(const std::string & columnName, Analysis * analysis)
{
	if(!DataSetPackage::pkg()->isColumnNameFree(columnName))
		return nullptr;

	return createComputedColumn(columnName, columnType::scale, computedColumnType::analysis, analysis);
}

void DataSetPackage::requestColumnCreation(const std::string & columnName, Analysis * analysis, columnType type)
{
	if(DataSetPackage::pkg()->isColumnNameFree(columnName))
		createComputedColumn(columnName, type, computedColumnType::analysisNotComputed, analysis);
}


void DataSetPackage::requestComputedColumnDestruction(const std::string& columnName)
{
	if(columnName.empty())
		return;

	Column * col = dataSet()->column(columnName);

	if(!col || !col->isComputed())
		return;

	removeColumn(columnName);

	emit checkForDependentColumnsToBeSent(tq(columnName));
}

void DataSetPackage::checkDataSetForUpdates()
{
	if(!_dataSet)
		return;

	stringvec changedCols;

	if(_dataSet->checkForUpdates(&changedCols))
	{
		refresh();

		emit datasetChanged(tq(changedCols), {}, {}, false, false);
	}
}

bool DataSetPackage::manualEdits() const
{
	return _manualEdits;
}

void DataSetPackage::setManualEdits(bool newManualEdits)
{
	if (_manualEdits == newManualEdits)
		return;

	_manualEdits = newManualEdits;

	if(_manualEdits)
		setSynchingExternally(false);

	emit manualEditsChanged();
}

