#include <QMap>
#include "filtermodel.h"
#include "datasetpackage.h"
#include "filter.h"
#include "qutils.h"
#include "undostack.h"

FilterModel::FilterModel(QObject * parent)
	: QObject(parent)
{

	connect(DataSetPackage::pkg(), &DataSetPackage::shownDataSetChanged,	this, &FilterModel::filterChanged);
	connect(DataSetPackage::pkg(), &DataSetPackage::filtersCountChanged,	this, &FilterModel::filterDropDownListChanged,	Qt::QueuedConnection);
	connect(DataSetPackage::pkg(), &DataSetPackage::shownFilterChanged,		this, &FilterModel::filterChanged									);
	connect(DataSetPackage::pkg(), &DataSetPackage::shownFilterChanged,		this, &FilterModel::filterDropDownListChanged,	Qt::QueuedConnection);
}

Filter *FilterModel::filter() const
{
	return DataSetPackage::filter();
}


bool FilterModel::isJustGeneratedFilter() const
{
	return filter() && filter()->rFilter() == Filter::defaultRFilter() && filter()->constructorJson() == DEFAULT_FILTER_JSON;
}

void FilterModel::applyConstructorJson(QString newConstructorJson)
{
	if(!filter())
		return;

	if (newConstructorJson != filter()->constructorJson())
		UndoStack::singleton()->pushCommand(new SetJsonFilterCommand(filter(), newConstructorJson));
}

void FilterModel::applyRFilter(QString newRFilter)
{
	if(!filter())
		return;

	if (newRFilter != filter()->rFilter())
		UndoStack::singleton()->pushCommand(new SetRFilterCommand(filter(), newRFilter));
}

void FilterModel::resetRFilter()
{
	if(!filter())
		return;

	if (filter()->defaultRFilter() != filter()->rFilter())
		UndoStack::singleton()->pushCommand(new SetRFilterCommand(filter(), filter()->defaultRFilter()));
}


void FilterModel::processFilterResult(QString name)
{
	if(!filter()) 
		return; //Cause there probably is no data anyway then
	
	if(filter()->nameQ() ==  name)
	{
		filter()->checkFilterResults();
		return;
	}
	
	Filter * f = DataSetPackage::pkg()->dataSet() ? DataSetPackage::pkg()->dataSet()->filter(fq(name)) : nullptr;
	
	if(f)
		f->checkFilterResults();
	
}

void FilterModel::onFilterChanged()
{
	if(filter())
		setCurrentFilterId(filter()->id());
}

void FilterModel::computeColumnSucceeded(QString columnName, QString, bool dataChanged)
{
	if(!filter())
		return;

	if(dataChanged && filter()->columnUsed(columnName))
		filter()->setInvalidated(true);
}

QVariantList FilterModel::filterDropDownList() const
{
	typedef QMap<QString, QVariant> localMap;
	
	QVariantList out;
	
	if(DataSetPackage::pkg()->workspace())
	{
		//out.append(localMap{std::make_pair("value", tq("---")), std::make_pair("label", "---")});
		
		for(DataSet * dataSet : DataSetPackage::pkg()->workspace()->dataSets())
		{
			out.append(localMap{std::make_pair("value", tq(dataSet == DataSetPackage::pkg()->dataSet() ? "*" : "-")), std::make_pair("label", dataSet->title() + ":")});
			
			if(dataSet->defaultFilter())
				out.append(localMap{std::make_pair("value", tq(std::to_string(dataSet->defaultFilter()->id()))), std::make_pair("label", dataSet->defaultFilter()->title())});
			
			for(const Filter * f : dataSet->filters())
				if(f != dataSet->defaultFilter())
					out.append(localMap{std::make_pair("value", tq(std::to_string(f->id()))), std::make_pair("label", f->title())});
			
			out.append(localMap{std::make_pair("value", tq("---")), std::make_pair("label", tq(std::to_string(dataSet->id())))});
		}
	}
	
	return out;
}

QVariantList FilterModel::filterDropDownAnalysisList() const
{
	typedef QMap<QString, QVariant> localMap;
	
	QVariantList out;
	
	if(DataSetPackage::pkg()->workspace())
	{
		out.append(localMap{std::make_pair("value", tq("---")), std::make_pair("label", "---")});
		
		for(DataSet * dataSet : DataSetPackage::pkg()->workspace()->dataSets())
		{
			out.append(localMap{std::make_pair("value", tq(dataSet == DataSetPackage::pkg()->dataSet() ? "*" : "-")), std::make_pair("label", dataSet->title() + ":")});
			
			if(dataSet->defaultFilter())
				out.append(localMap{std::make_pair("value", tq(std::to_string(dataSet->defaultFilter()->id()))), std::make_pair("label", dataSet->defaultFilter()->title())});
			
			for(const Filter * f : dataSet->filters())
				if(f != dataSet->defaultFilter())
					out.append(localMap{std::make_pair("value", tq(std::to_string(f->id()))), std::make_pair("label", f->title())});
			
			out.append(localMap{std::make_pair("value", tq("---")), std::make_pair("label", "---")});
		}
	}
	
	return out;
}

QVariantList FilterModel::computeFilterDropDownList() const
{
	typedef QMap<QString, QVariant> localMap;
	
	QVariantList out;
	
	if(DataSet * dataSet = DataSetPackage::pkg()->dataSet())
	{
		if(dataSet->defaultFilter())
			out.append(localMap{std::make_pair("value", tq(dataSet->defaultFilter()->name())), std::make_pair("label", dataSet->defaultFilter()->title())});
		
		for(const Filter * f : dataSet->filters())
			if(f != dataSet->defaultFilter())
				out.append(localMap{std::make_pair("value", tq(f->name())), std::make_pair("label", f->title())});
	}
	
	return out;
}

bool FilterModel::filterVisible() const
{
	return _filterVisible;
}

void FilterModel::setFilterVisible(bool newFilterVisible)
{
	if (_filterVisible == newFilterVisible)
		return;
	_filterVisible = newFilterVisible;
		
	emit filterVisibleChanged();
}

bool FilterModel::showEasyFilter() const
{
	return _showEasyFilter;
}

void FilterModel::setShowEasyFilter(bool newShowEasyFilter)
{
	if (_showEasyFilter == newShowEasyFilter)
		return;
	_showEasyFilter = newShowEasyFilter;
	emit showEasyFilterChanged();
}

void FilterModel::reset()
{
	_showEasyFilter = true;
	_filterVisible  = false;
}

QString FilterModel::currentFilter() const
{
	return !filter() ? "" : tq(filter()->name());
}

int FilterModel::currentFilterId() const
{
	return !filter() ? -1 : filter()->id();
}

QString FilterModel::currentFilterTitle() const
{
	return !filter() ? "" : filter()->title();
}

void FilterModel::setCurrentFilterId(int id)
{
	DataSetPackage::pkg()->workspace()->showFilter(id);
	
	emit filterChanged();
	emit filterDropDownListChanged();
	
	DataSetPackage::pkg()->workspace()->refresh();
	
}

void FilterModel::renameCurrentFilter(const QString &newName)
{
	DataSet * ds = DataSetPackage::pkg()->dataSet();
	Filter  * f  = ds ? ds->shownFilter() : nullptr;

	if(!f)
		return;

	const std::string name = fq(newName);

	//Guard against empty names, renaming the (single, unnamed) default filter, and name collisions:
	//duplicate filter names would make filter(name)/filterGetId lookups ambiguous.
	if(name.empty() || name == DEFAULT_FILTER_NAME || (name != f->name() && !Filter::filterNameIsFree(ds, name)))
		return;

	f->setName(name);
	emit filterChanged();
	emit filterDropDownListChanged();
}

void FilterModel::deleteCurrentFilter()
{
	if(DataSetPackage::pkg()->dataSet())
		DataSetPackage::pkg()->dataSet()->deleteShownFilter();
	emit filterChanged();
	emit filterDropDownListChanged();
}

void FilterModel::addFilter(int dataSetId)
{
	DataSet * dataSet = dataSetId == -1 
			? DataSetPackage::pkg()->dataSet() 
			: DataSetPackage::pkg()->workspace() 
			  ? DataSetPackage::pkg()->workspace()->dataSetById(dataSetId) 
			  : nullptr;
	
	if(dataSet)
		dataSet->addFilter();
}
