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
#include "filtermodel.h"
#include <ranges>
#include "variableinfo.h"

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
	
	connect(&_databaseIntervalSyncher,	&QTimer::timeout, this, &DataSetPackage::synchingIntervalPassed);
	connect(&_delayedRefreshTimer,		&QTimer::timeout, this, &DataSetPackage::delayedRefresh);

	_undoStack = new UndoStack(this);
}

DataSetPackage::~DataSetPackage() 
{ 
	_databaseIntervalSyncher.stop();
	_delayedRefreshTimer.stop();
	
	_singleton = nullptr; 
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
	_delayedRefreshTimer.stop();
	
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

	const int INIT_COL = 1;
	const int INIT_ROW = 1;

	beginLoadingData();

	if(!_dataSet)
		createDataSet();
	setDataSetSize(INIT_COL, INIT_ROW);
	doublevec emptyValues(INIT_ROW, EmptyValues::missingValueDouble);
	initColumnWithStrings(0, freeNewColumnName(0), {""});

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
				Column	* col	= dynamic_cast<Column*>(parentNode);
				Label	* lab	= col->labelByIndexNotEmpty(row);
				pointer			= dynamic_cast<const void*>(lab ? lab : col->labelDoubleDummy());
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
			int		i = col ? col->labelIndexNonEmpty(lab) : -1;

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
		
		return !col ? 0 : col->labelsTempCount();
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
		//Column * col = dynamic_cast<Column*>(node);
		
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
		case Qt::DisplayRole:									return tq(column->getDisplay(index.row()));
		case int(specialRoles::label):							return tq(column->getLabel(index.row(), false, true));
		case int(specialRoles::value):							return tq(column->getValue(index.row()));
		case int(specialRoles::name):							return tq(column->name());
		case int(specialRoles::title):							return tq(column->title());
		case int(specialRoles::filter):							return getRowFilter(index.row());
		case int(specialRoles::columnType):						return int(column->type());
		case int(specialRoles::description):					return tq(column->description());
		case int(specialRoles::inEasyFilter):					return isColumnUsedInEasyFilter(column->name());
		case int(specialRoles::shadowDisplay):					return tq(column->getShadow(index.row()));
		case int(specialRoles::valuesDblList):					return getColumnValuesAsDoubleList(getColumnIndex(column->name()));
		case int(specialRoles::nonFilteredNumericValuesCount):	return column->nonFilteredNumericsCount();
		case int(specialRoles::nonFilteredLevels):				return tql(column->nonFilteredLevels());
		case int(specialRoles::computedColumnType):				return int(column->codeType());
		case int(specialRoles::columnPkgIndex):					return index.column();
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
		case int(specialRoles::nonFilteredNumericValuesCount):	return column->nonFilteredNumericsCount();
		case int(specialRoles::nonFilteredLevels):				return tql(column->nonFilteredLevels());
		case int(specialRoles::valuesDblList):					return getColumnValuesAsDoubleList(getColumnIndex(column->name()));
		case int(specialRoles::description):					return index.row() >= labels.size() ? "" : tq(labels[index.row()]->description());
		case int(specialRoles::filter):							return index.row() >= labels.size() || labels[index.row()]->filterAllows();
		case int(specialRoles::value):							return tq(column->labelsTempValue(index.row()));
		case int(specialRoles::lines):							return getDataSetViewLines(index.row() == 0, index.column() == 0, true, true);
		case int(specialRoles::label):							[[fallthrough]];
		case Qt::DisplayRole:									return tq(column->labelsTempDisplay(index.row()));
		default:												return QVariant();
		}
	}
	}

	return QVariant(); // <- because gcc is stupid
}

qsizetype DataSetPackage::getMaximumColumnWidthInCharacters(int columnIndex) const
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
	{
		Column * col = _dataSet ? _dataSet->column(section) : nullptr;
		
		switch(role)
		{
		case int(specialRoles::maxColString):
		{
			//calculate some kind of maximum string to give views an expectation of the width needed for a column
			bool		hasFilter	= col && (col->hasFilter() || isColumnUsedInEasyFilter(col->name()));
			QString		dummyText	= headerData(section, orientation, int(specialRoles::maxColumnHeaderString)).toString() + (isColumnComputed(section) ? "XXX" : "") + (hasFilter ? "XXX" : ""); //Bit of padding for hamburger, filtersymbol and columnIcon
			qsizetype	colWidth	= getMaximumColumnWidthInCharacters(section);

			while(colWidth > dummyText.length())
				dummyText += "X";

			return dummyText;
		}
		case int(specialRoles::maxColumnHeaderString):			return headerData(section, orientation, Qt::DisplayRole).toString() + "XXX";
		case int(specialRoles::maxRowHeaderString):				return QString::number(_dataSet ? _dataSet->rowCount() : 0 )		+ "XXX";
		case Qt::TextAlignmentRole:								return QVariant(Qt::AlignCenter);
		case int(specialRoles::filter):							return		!col ? false							: col->hasFilter() || isColumnUsedInEasyFilter(col->name());
		case Qt::DisplayRole:									return tq(	!col ? "?"								: col->name());
		
		case int(specialRoles::labelsHasFilter):				return		!col ? false							: col->hasFilter();
		case int(specialRoles::columnIsComputed):				return		!col ? false							: col->isComputed() && col->codeType() != computedColumnType::analysisNotComputed;
		case int(specialRoles::computedColumnError):			return tq(	!col ? "?"								: col->error());
		case int(specialRoles::computedColumnIsInvalidated):	return		!col ? false							: col->invalidated();
		case int(specialRoles::columnType):						return int(	!col ? columnType::unknown				: col->type());
		case int(specialRoles::computedColumnType):				return int(	!col ? computedColumnType::notComputed	: col->codeType());
		case int(specialRoles::description):					return tq(	!col ? "?"								: col->description());
		case int(specialRoles::title):							return tq(	!col ? "?"								: col->title());
		case int(specialRoles::previewScale):
		case int(specialRoles::previewOrdinal):					
		case int(specialRoles::previewNominal):					
		{
			columnType colTypeWanted = 
					role == int(specialRoles::previewNominal) 
					? columnType::nominal 
					: role == int(specialRoles::previewOrdinal)
					? columnType::ordinal
					: columnType::scale;
			
			stringvec preview = !col ? stringvec() : col->previewTransform(colTypeWanted);
			
			if(preview.size() != 4)
				return QVariant();
			
			QString	levelsTotal		= tq(preview[0]),
					levelsNums		= tq(preview[1]),
					vals			= tq(preview[2]),
					empties			= tq(preview[3]);
			
			if(colTypeWanted == columnType::scale)
				return	tr("There are %1 total levels, of which %2 have a numeric value.\nAs a '%3' it looks like: %4\n%5")
						.arg(levelsTotal)
						.arg(levelsNums)
						.arg(VariableInfo::getTypeFriendly(colTypeWanted))
						.arg(vals)
						.arg(
							empties == "" 
							? "" 
							: tr("Implicit missing values: %1").arg(empties)
						);
			else
				return tr("There are %1 total levels.\nAs a '%2' it looks like: %3")
					.arg(levelsTotal)
					.arg(VariableInfo::getTypeFriendly(colTypeWanted))
					.arg(vals);
		}
		}
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

	case dataSetBaseNodeType::column:
		if(node)
		{
			Column	* column	= dynamic_cast<Column*>(node);
			//DataSet * data		= column->data();

			if(role == Qt::DisplayRole || role == Qt::EditRole || role == int(specialRoles::value) || role == int(specialRoles::valueLabelPair) || role == int(specialRoles::valuesStrList))
			{				
				bool				isPair	= role == int(specialRoles::valueLabelPair),
									isVals	= role == int(specialRoles::valuesStrList);
				QVariantList		listVar	= isPair || isVals ? value.toList()	: QVariantList{ value };
				bool				aChange = false;
				
				if(!isVals)
				{
					const std::string	val		= fq(listVar[0].toString()),
										label	= fq(isPair ? listVar[1].toString() : "");
										aChange	= !isPair	
												? column->setStringValue(index.row(), val == EmptyValues::displayString() ? "" : val)
												: column->setValue(index.row(), val, label);
				}
				else //Its a list of values, for instance "intial values"
				{
					int r=0;
					for(const QVariant & val : listVar)
						if(column->setStringValue(index.row() + r++, fq(val.toString() == tq(EmptyValues::displayString()) ? "" : val.toString())))
							aChange = true;
				}
				
				if(aChange)
				{
						JASPTIMER_SCOPE(DataSetPackage::setData reset model);

						setManualEdits(true); //Don't synch with external file after editing
						
						column->labelsRemoveOrphans();
						column->labelsTempReset();
						column->labelsHandleAutoSort();

						stringvec	changedCols = {column->name()};
	
						refresh();
						emit datasetChanged(tq(changedCols), {}, {}, false, false);
						emit labelsReordered(tq(column->name()));
						
						if(column->hasFilter())
						{
							emit labelFilterChanged();
							emit runFilter();
						}
				}
				
				return true;
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
					if(value.toInt() >= int(columnType::unknown) && value.toInt() <= int(columnType::scale))
					{
						columnType converted = static_cast<columnType>(value.toInt());
						if(converted != column->type() && setColumnType(index.column(), converted))
						{
							aChange = true;
							emit columnDataTypeChanged(tq(column->name()));
						}
					}
					break;
				}

				if(aChange)
				{
					beginResetModel();
					endResetModel();
					setManualEdits(true);
				}
				return true;
			}
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
			return setLabelAllowFilter(index, value.toBool());

		case int(specialRoles::description):
			setManualEdits(true);
			return setLabelDescription(index, value.toString());

		case int(specialRoles::value):
			return setLabelValue(index,  value.toString());

		case int(specialRoles::label):
			return setLabelDisplay(index, value.toString());
			
		default:
			return false;
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

bool DataSetPackage::setLabelDescription(const QModelIndex & index, const QString & newDescription)
{
	Label		*	label	= dynamic_cast<Label*>(indexPointerToNode(index));
	Column		*	column	= dynamic_cast<Column*>(label->parent());
	QModelIndex		parent	= index.parent();
	
	if(!column || index.row() > rowCount(parent))
		return false;
	
	if(column->labelDoubleDummy() == label)
		label = column->replaceDoublesTillLabelsRowWithLabels(index.row());

	label->setDescription(newDescription.toStdString());
	
	emit dataChanged(DataSetPackage::index(index.row(), 0, parent),	DataSetPackage::index(index.row(), columnCount(parent), parent), {int(specialRoles::description)});	//Emit dataChanged for filter

	return true;
}

bool DataSetPackage::setLabelDisplay(const QModelIndex &index, const QString &newLabel)
{
	Label			*	label		= dynamic_cast<Label*>(indexPointerToNode(index));
	Column			*	column		= dynamic_cast<Column*>(label->parent());
	QModelIndex			parent		= index.parent();
	stringvec			changedCols	;
	bool				aChange		= false,
						setManual	= false;
	
	if(!column || index.row() > rowCount(parent))
		return false;
	
	beginSynchingData(false);
	
	if(column->labelDoubleDummy() == label)
	{
		label	= column->replaceDoublesTillLabelsRowWithLabels(index.row());
		aChange = true;
	}
	
	if(label->setLabel(newLabel.toStdString()))
	{
		aChange = true;
		
		if(dataFileCanHaveLabels())
			setManual = true;
	}
	
	if(aChange)
		changedCols = {column->name()};
	
	endSynchingDataChangedColumns(changedCols, false, false);

	if(setManual)
		setManualEdits(true);

	return aChange;
}

bool DataSetPackage::setLabelValue(const QModelIndex &index, const QString &newLabelValue)
{
	Label			*	label		= dynamic_cast<Label*>(indexPointerToNode(index));
	Column			*	column		= dynamic_cast<Column*>(label->parent());
	QModelIndex			parent		= index.parent();
	stringvec			changedCols	;
	bool				aChange		= false,
						aNumber		= false;
	
	if(!column || index.row() > rowCount(parent))
		return false;
	
	beginSynchingData(false);
	
	Json::Value originalValue = newLabelValue.toStdString();

	int		anInteger;
	double	aDouble;

	if(	(aNumber =	ColumnUtils::getDoubleValue(newLabelValue.toStdString(), aDouble))	)	originalValue = aDouble;
	if(				ColumnUtils::getIntValue(	newLabelValue.toStdString(), anInteger)	)	originalValue = anInteger;
	
	
	if(column->labelDoubleDummy() == label)
	{
		int		replaceTill	= -1;
		double	oldDouble	= column->labelsTempValueDouble(index.row());
				
		if(aNumber)
		{
			int newHasRow	= column->labelsDoubleValueIsTempLabelRow(aDouble);
					
			if(!Utils::isEqual(aDouble, oldDouble))
			{
				assert(newHasRow != index.row()); //Because it shouldnt be the same after all
				replaceTill = std::max(index.row(), newHasRow);
			}
					
			if(replaceTill < 0 && column->replaceDoubleLabelFromRowWithDouble(index.row(), aDouble))
			{
				changedCols = {column->name()};
				endSynchingDataChangedColumns(changedCols, false, false);
				
				setManualEdits(true); //A value change is a manual edit for sure as that changes the data itself
				return true;
			}
		}
		
		//if no a number then we will have to replace everything anyway because we wont be able to sort otherwise
		if(replaceTill == -1 && column->autoSortByValue())
				replaceTill = column->labelsTempCount();
		
		label	= column->replaceDoublesTillLabelsRowWithLabels(replaceTill > -1 ? replaceTill : index.row(), oldDouble);
		aChange = true;
	}
	
	{
		// Here we will overwrite the original value with the new origval.
		// but if the label is the same as the original value we want to make the users life easier and replace it as well.
		// this makes sense if the user is changing a string or number. But if the user is recoding, so turning values from str => dbl
		// then we dont want to do this, because then the label should be different afterwards.
		
		//summarized:
		// if orgval == label then: 
		// if (oldorigval == dbl && newOrigVal == dbl) || (olorigval != dbl && newOrigVal != dbl)  then replace both
		// if neworigval == dbl and oldorigval != dbl then replace only value
		
		// But only if we are allowed to change both because of https://github.com/jasp-stats/INTERNAL-jasp/issues/2680 (allow editing of only value/label and disable the other one for computed columns
		// which means that if this column is a computed column of scale type we are only allowed to change the label and only the value for the other types.
		// so in this case this means that if it is a computed column, and of type !scale we do *not* also update the label when updating the value. Because otherwise it would override the data from the computed column...
		
		bool dontSetLabel = label->originalValueAsString(false) != label->labelDisplay() || (originalValue.isDouble() && !label->originalValue().isDouble());
		
		if(!dontSetLabel && column->isComputed() && column->type() != columnType::scale)
			dontSetLabel = true;
		
		if(dontSetLabel)	aChange = label->setOriginalValue(originalValue)	||	aChange;
		else				aChange = label->setOrigValLabel(originalValue)		||	aChange;
	}
	
	column->labelsHandleAutoSort();
	
	if(aChange)
		changedCols = {column->name()};

	endSynchingDataChangedColumns(changedCols, false, false);
	
	if(aChange)
		setManualEdits(true); //A value change is a manual edit for sure as that changes the data itself

	return aChange;
}

bool DataSetPackage::setLabelAllowFilter(const QModelIndex & index, bool newAllowValue)
{
	JASPTIMER_SCOPE(DataSetPackage::setAllowFilterOnLabel);
	
	Column * column = nullptr;

	{
		Label  *	label  = dynamic_cast<Label*>(indexPointerToNode(index));
					column = dynamic_cast<Column*>(label->parent());
		
		if(column->labelDoubleDummy() == label)
			column->replaceDoublesTillLabelsRowWithLabels(index.row());
	}		
	
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
	
	atLeastOneRemains = atLeastOneRemains || column->labelsTempCount() > labels.size();

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

bool DataSetPackage::dataFileCanHaveLabels() const
{ 
	return _dataSet && !tq(_dataSet->dataFilePath()).endsWith(".csv");  
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
		ColumnTypeAndIcons.push_back("variable-scale.svg");
		ColumnTypeAndIcons.push_back("variable-ordinal.svg");
		ColumnTypeAndIcons.push_back("variable-nominal.svg");
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

void DataSetPackage::resetFilterCounters()
{
	for(Column * col : _dataSet->columns())
		col->nonFilteredCountersReset();
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
	return setColumnTypes({columnIndex}, newColumnType);
}

bool DataSetPackage::setColumnTypes(intset columnIndexes, columnType newColumnType)
{
	if (_dataSet == nullptr)
		return true;
	
	bool somethingChanged = false;

	for(int columnIndex : columnIndexes)
	{
		Column *col = _dataSet->column(columnIndex);
	
		if (col->type() == newColumnType)
			continue;
	
	
		//the only possible "fail" is when an analysis made the column and thus decides the type
		//the user might bet
		if(col->changeType(newColumnType) == columnTypeChangeResult::generatedFromAnalysis)
		{
			emit showWarning(tr("Changing column type failed"), tr("The column '%1' is generated by an analysis and its type is fixed.").arg(tq(col->name())));
			continue;
		}
	
		emit columnDataTypeChanged(tq(_dataSet->column(columnIndex)->name()));
		somethingChanged = true;
	}
	
	if(somethingChanged)
		refreshWithDelay();

	return somethingChanged;
}

void DataSetPackage::refreshWithDelay()
{
	_delayedRefreshTimer.setSingleShot(true);
	_delayedRefreshTimer.setInterval(100);
	_delayedRefreshTimer.start();	
}

void DataSetPackage::delayedRefresh()
{
	refresh();	
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

void DataSetPackage::endSynchingData(	const stringvec	&	changedColumns,
										const stringvec	&	missingColumns,
										const strstrmap	&	changeNameColumns,  //origname -> newname
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

void DataSetPackage::resetVariableTypes()
{
	for (Column * col : _dataSet->columns())
	{
		columnType guessedType = col->resetValues(PreferencesModel::prefs()->thresholdScale());
		
		if(guessedType != col->type() && col->changeType(guessedType) == columnTypeChangeResult::changed)
		{
			emit columnDataTypeChanged(tq(col->name()));
			refreshWithDelay();
		}
	}
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
	
	_dataSet->filter()->setRFilter(FilterModel::defaultRFilter());
	
	_dataSet->setModifiedCallback([&](){ setModified(true); }); //DataSet and co dont use Qt so instead we just use a callback
}

void DataSetPackage::loadDataSet(std::function<void(float)> progressCallback)
{
	if(_dataSet)
		deleteDataSet(); //no dbDelete necessary cause we just copied an old sqlite file here from the JASP file
	
	_db->close();
	_db->load();		
	_db->upgradeDBFromVersion(_jaspVersion);
	
	bool do019Upgrade = _jaspVersion < "0.19"; // A tweak needs to be made to the data as its loaded, see https://github.com/jasp-stats/jasp-desktop/pull/5367
	
	_dataSet = new DataSet(0);
	_dataSet->dbLoad(1, progressCallback, do019Upgrade); //Right now there can only be a dataSet with ID==1 so lets keep it simple
	if (do019Upgrade)
	{
		// In 0.18.3 and before, there was a bug with the order of dataFilePath and description in the database.
		// dataFilePath was set empty and description has dataFilePath.
		if (dataFilePath().empty())
		{
			QFileInfo fileInfo(description());
			if (fileInfo.isFile())
				setDataFilePath(fq(description()));
		}
	}
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

int DataSetPackage::getColIndex(QVariant colID)
{
	if(colID.typeId() == QMetaType::Int || colID.typeId() == QMetaType::UInt)
		return colID.typeId() == QMetaType::Int ? colID.toInt() : colID.toUInt();

	else
		return _dataSet->getColumnIndex(fq(colID.toString()));
}

bool DataSetPackage::initColumnWithStrings(QVariant colId, const std::string & newName, const stringvec &values, const stringvec & labels, const std::string & title, columnType desiredType, const stringset & emptyValues)
{
	JASPTIMER_SCOPE(DataSetPackage::initColumnWithStrings);
	
	int			colIndex		=	getColIndex(colId),
				threshold		=	Settings::value(Settings::THRESHOLD_SCALE).toInt();
	Column	*	column			=	_dataSet->columns()[colIndex];
				column			->	setHasCustomEmptyValues(emptyValues.size());
				column			->	setCustomEmptyValues(emptyValues);
				column			->	setName(newName);
				column			->	setTitle(title);
				column			->	beginBatchedLabelsDB();
	bool		anyChanges		=	title != column->title() || newName != column->name();
	columnType	prevType		=	column->type(),
				suggestedType	=	column->setValues(values, labels,	threshold, &anyChanges);  //If less unique integers than the thresholdScale then we think it must be ordinal: https://github.com/jasp-stats/INTERNAL-jasp/issues/270
				column			->	setType(column->type() != columnType::unknown ? column->type() : desiredType == columnType::unknown ? suggestedType : desiredType);
				column			->	endBatchedLabelsDB();
				
	if(PreferencesModel::prefs()->orderByValueByDefault())
		column->labelsOrderByValue();
	
	return anyChanges || column->type() != prevType;
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

bool DataSetPackage::isColumnDifferentFromStringValues(const std::string & columnName, const std::string & title, const stringvec & strVals, const stringvec & strLabs, const stringset & strEmptyVals)
{
	Column * col = _dataSet->column(columnName);
	
	if(col)
		return col->isColumnDifferentFromStringValues(title, strVals, strLabs, strEmptyVals);

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

	setManualEdits(true);
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
	
	refresh();
}

void DataSetPackage::setColumnDescription(size_t columnIndex, const std::string & newDescription)
{
	if(!_dataSet)
		return;

	Column* column = _dataSet->column(columnIndex);
	if (!column)
		return;

	column->setDescription(newDescription);
	refresh();
}

void DataSetPackage::setColumnComputedType(size_t columnIndex, computedColumnType type)
{
	if(!_dataSet)
		return;

	Column* column = _dataSet->column(columnIndex);
	if (!column)
		return;

	column->setCodeType(type);

	//emit dataChanged(index(0, columnIndex), index(rowCount() - 1, columnIndex));
	//we need to actually send lots of signals from ColumnModel but because of the undo/redo this is a bit convoluted now...

	refresh();
}

void DataSetPackage::setColumnComputedType(const std::string & columnName, computedColumnType type)
{
	setColumnComputedType(getColumnIndex(columnName), type);
}

void DataSetPackage::setColumnHasCustomEmptyValues(size_t columnIndex, bool hasCustomEmptyValue)
{
	if(!_dataSet)
		return;

	Column* column = _dataSet->column(columnIndex);
	if (!column || column->hasCustomEmptyValues() == hasCustomEmptyValue)
		return;
	
	column->setHasCustomEmptyValues(hasCustomEmptyValue);
	
	refresh();
	emit datasetChanged({tq(column->name())}, {}, {}, false, false);
}

void DataSetPackage::setColumnCustomEmptyValues(size_t columnIndex, const stringset& customEmptyValues)
{
	if(!_dataSet)
		return;

	Column* column = _dataSet->column(columnIndex);
	
	if(column && column->setCustomEmptyValues(customEmptyValues))
	{
		refresh();
		
		emit datasetChanged({tq(column->name())}, {}, {}, false, false);
	}
}

void DataSetPackage::columnsReverseValues(intset columnIndexes)
{
	columnsApply(columnIndexes, [&](Column * column) 
	{ 
		column->valuesReverse();
		return true;
	});
}

void DataSetPackage::columnsSetAutoSortForColumns(std::map<int,bool> sortPerColumn)
{
	intset cols;
	for(auto colSort : sortPerColumn)
		cols.insert(colSort.first);
	
	columnsApply(cols, [&](Column * column, int colIdx) 
	{ 
		column->setAutoSortByValue(sortPerColumn[colIdx]);
		return true;
	});
	
	if(cols.size() == 1)
		emit chooseColumn(*cols.begin());
}

void DataSetPackage::columnsApply(intset columnIndexes, std::function<bool(Column * column, int col)> applyThis)
{
	if(!_dataSet)
		return;
	
	QStringList changedCols;

	for(int columnIndex : columnIndexes)
	{
		Column* column = _dataSet->column(columnIndex);
	
		if(column)
		{
			if(applyThis(column, columnIndex))
				changedCols << tq(column->name());
		}	
	}
	
	if(changedCols.size() > 0)
	{
		refresh();
		emit datasetChanged(changedCols, {}, {}, false, false);
	}
}

void DataSetPackage::columnsApply(intset columnIndexes, std::function<bool(Column * column)> applyThis)
{
	columnsApply(columnIndexes, [&](Column * column, int){ return applyThis(column); });
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

columnType DataSetPackage::getColumnType(const QString& name) const
{
	return _dataSet ? getColumnType(_dataSet->getColumnIndex(fq(name))) : columnType::unknown;
}


std::string DataSetPackage::getColumnName(size_t columnIndex) const
{
	return _dataSet && _dataSet->column(columnIndex) ? _dataSet->column(columnIndex)->name() : "";
}

QStringList DataSetPackage::getColumnLabelsAsStringList(size_t columnIndex)	const
{
	return tq(getColumnLabelsAsStrVec(columnIndex));
}


boolvec DataSetPackage::getColumnFilterAllows(size_t columnIndex) const 
{
	boolvec list;
	if(columnIndex < 0 || columnIndex >= dataColumnCount()) 
		return list;
	
	Column * column =_dataSet->columns()[columnIndex];
	
	for (const Label * label : column->labels())
		list.push_back(label->filterAllows());
	
	while(list.size() < column->labelsTempCount())
		list.push_back(true);

	return list;
}

stringvec DataSetPackage::getColumnLabelsAsStrVec(size_t columnIndex) const
{
	stringvec list;
	if(columnIndex < 0 || columnIndex >= dataColumnCount()) 
		return list;

	return _dataSet->columns()[columnIndex]->labelsTemp();
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


void DataSetPackage::labelMoveRows(size_t colIdx, std::vector<qsizetype> rows, bool up)
{
	Column	*	column		= _dataSet->columns()[colIdx];
	sizetset	rowsChanged = column->labelsMoveRows(rows, up);
	
	if(rowsChanged.size())
	{
		QModelIndex p = indexForSubNode(column);
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
	stringvec prefs = fq(PreferencesModel::prefs()->emptyValues());
	setWorkspaceEmptyValues(stringset(prefs.begin(), prefs.end()));
}

void DataSetPackage::setWorkspaceEmptyValues(const stringset &emptyValues, bool reset)
{
	if (!_dataSet) return;
	
	if(reset)	beginResetModel();
	_dataSet->setWorkspaceEmptyValues(emptyValues);
	if(reset)	endResetModel();
	
	emit workspaceEmptyValuesChanged();
}

void DataSetPackage::pasteSpreadsheet(size_t row, size_t col, const std::vector<std::vector<QString>> & values, const std::vector<std::vector<QString>> &  labels, const intvec & coltypes, const QStringList & colNames, const std::vector<boolvec> & selected)
{
	JASPTIMER_SCOPE(DataSetPackage::pasteSpreadsheet);

	int		rowMax			= ( values.size() > 0 ? values[0].size() : 0), 
			colMax			= values.size();
	bool	rowCountChanged = int(row + rowMax) > dataRowCount()	,
			colCountChanged = int(col + colMax) > dataColumnCount()	;
	
	auto isSelected = [&selected](int row, int col)
	{
		return selected.size() == 0 || 	selected[col][row];
	};

	beginSynchingData(false);
	_dataSet->beginBatchedToDB();
	
	if(colCountChanged || rowCountChanged)	
		setDataSetSize(std::max(size_t(dataColumnCount()), colMax + col), std::max(size_t(dataRowCount()), rowMax + row));
	
	stringvec changed;
	strstrmap changeNameColumns;

	for(int c=0; c<colMax; c++)
	{
		Column	*	column		= _dataSet->column(c + col);
		columnType	desiredType	= coltypes.size() > c ? columnType(coltypes[c]) : column->type();
					desiredType = desiredType == columnType::unknown ? columnType::scale : desiredType;
		std::string colName		= (colNames.size() > c && !colNames[c].isEmpty()) ? fq(colNames[c]) : column->name();
		
		column->setType(desiredType);

		bool aChange = false;
		for(int r=0; r<rowMax; r++)
			if(isSelected(r, c))
				aChange = column->setStringValue(r+row, fq(values[c][r]), labels.size() <= c || labels[c].size() <= r ? "" : fq(labels[c][r])) || aChange;
			
		aChange = aChange || colName != column->name() || desiredType != column->type();
		
		if(colName != column->name())
			changeNameColumns[column->name()] = colName;
		
		column->setName(colName);

		if(aChange)
		{
			changed.push_back(colName);
			column->labelsTempReset();
		}
	}

	_dataSet->endBatchedToDB();
	
	stringvec		missingColumns;

	endSynchingData(changed, missingColumns, changeNameColumns, rowCountChanged, colCountChanged, false);
	setManualEdits(true); //set manual edits here so external synching is turned off, endSynchingData also just reset it, so thats why it is way down here
}

QString DataSetPackage::insertColumnSpecial(int columnIndex, const QMap<QString, QVariant>& props)
{
	if(columnIndex < 0)
		columnIndex = 0;

	if(columnIndex > dataColumnCount())
		columnIndex = dataColumnCount(); //the column will be created if necessary but only if it is in a logical place. So the end of the vector

	setManualEdits(true); //Don't synch with external file after editing
#ifdef ROUGH_RESET
	beginResetModel();
#else
	beginInsertColumns(indexForSubNode(_dataSet->dataNode()), columnIndex, columnIndex);
#endif

	_dataSet->insertColumn(columnIndex);
	
	Column * column = _dataSet->column(columnIndex);

	column->setName(			props.contains("name")		? fq(props["name"].toString())					: freeNewColumnName(columnIndex)	);
	column->setDefaultValues(	props.contains("type")		? columnType(props["type"].toInt())				: columnType::scale					);
	column->setCodeType(		props.contains("computed")	? computedColumnType(props["computed"].toInt())	: computedColumnType::notComputed	);

	_dataSet->incRevision();

#ifdef ROUGH_RESET
	endResetModel();
#else
	endInsertColumns();
#endif
	
	emit datasetChanged(tq(stringvec{column->name()}), {}, {}, false, true);

	ColumnEncoder::setCurrentColumnNames(getColumnNames());
	
	if(column->codeType() == computedColumnType::constructorCode || column->codeType() == computedColumnType::rCode)
		emit columnAddedManually(tq(column->name())); //Will trigger setChosenColumn and setVisible(true) on ColumnModel, showing it to the user

	return QString::fromStdString(column->name());
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
		_dataSet->column(c)->setName(name);
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

	ColumnEncoder::setCurrentColumnNames(getColumnNames());

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

	ColumnEncoder::setCurrentColumnNames(getColumnNames());

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
	
	for(Column * column : dataSet()->columns())
	{
		changed.push_back(column->name());
		
		//if(row+count > column->rowCount())
		//	Log::log() << "???" << std::endl;
	
		for(int r=row+count; r>row; r--)
			column->rowDelete(r-1);
	}

	dataSet()->setRowCount(dataSet()->rowCount() - count);
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

void DataSetPackage::columnsReorder(const stringvec &order)
{
	beginResetModel();
	_dataSet->columnsReorder(order);
	endResetModel();
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

bool DataSetPackage::currentFileIsExample() const
{
	return currentFile().startsWith(AppDirs::examples());
}

void DataSetPackage::setDataFilePath(std::string filePath, long timestamp)
{
	if(!_dataSet || _dataSet->dataFilePath() == filePath)
		return;

	if (timestamp == 0 && !filePath.empty())
	{
		QFileInfo fileInfo(tq(filePath));
		timestamp = fileInfo.isFile() ? fileInfo.lastModified().toSecsSinceEpoch() : 0;
	}

	_dataSet->setDataFile(filePath, timestamp);
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

stringset DataSetPackage::columnsCreatedByAnalysis(Analysis * analysis)
{
	if(!_dataSet)
		return {};

	stringset cols;

	for(Column * col : _dataSet->columns())
		if(col->analysisId() == analysis->id())
			cols.insert(col->name());

	return cols;
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

bool DataSetPackage::requestColumnCreation(const std::string & columnName, Analysis * analysis, columnType type)
{
	if(!DataSetPackage::pkg()->isColumnNameFree(columnName))
		return false;
	
	createComputedColumn(columnName, type, computedColumnType::analysisNotComputed, analysis);
	return true;
}


bool DataSetPackage::requestComputedColumnDestruction(const std::string& columnName, Analysis * analysis)
{
	if(columnName.empty())
		return false;

	Column * col = dataSet()->column(columnName);

	if(!col || !col->isComputed() || !col->isComputedByAnalysis(analysis->id()))
		return false;

	removeColumn(columnName);

	emit checkForDependentColumnsToBeSent(tq(columnName));
	
	return true;
}

void DataSetPackage::checkDataSetForUpdates()
{
	if(!_dataSet)
		return;

	stringvec changedCols, missingCols;
	bool newCols = false, rowCountChanged = false;

	if(_dataSet->checkForUpdates(&changedCols, &missingCols, &newCols, &rowCountChanged))
	{
		Log::log()	<< "Updates found for DataSet " << _dataSet->id() 
					<< "| missing cols: '" << tq(missingCols).join(",")
					<< "' | changed cols: '" << tq(changedCols).join(",")
					<< "' | " << (newCols ? " has new cols" : "") << (rowCountChanged ? "| rowcount changed |" : "|") << std::endl;
		refresh();

		emit datasetChanged(tq(changedCols), tq(missingCols), {}, newCols, rowCountChanged);
	}
}

bool DataSetPackage::manualEdits() const
{
	return _manualEdits;
}

void DataSetPackage::setManualEdits(bool newManualEdits)
{
	// During synchronization, even if some data are changed, manualEdits should not be set to true
	if ((_synchingData && newManualEdits) || _manualEdits == newManualEdits)
		return;

	_manualEdits = newManualEdits;

	if(_manualEdits)
		setSynchingExternally(false);

	emit manualEditsChanged();
}

