#include "filtermodel.h"
#include "variablespage/labelfiltergenerator.h"
#include "jsonutilities.h"

void FilterModel::reset()
{
	setGeneratedFilter(QString::fromStdString(labelFilterGenerator(NULL).generateFilter()));
	setConstructedJSON(DEFAULT_FILTER_JSON);
	setRFilter(DEFAULT_FILTER);
}

void FilterModel::setDataSetPackage(DataSetPackage * package)
{
	_package = package;

	if(_package != NULL)
	{
		setGeneratedFilter(QString::fromStdString(labelFilterGenerator(_package).generateFilter()));

		setConstructedJSON(QString::fromStdString(_package->filterConstructorJson()));
		setRFilter(QString::fromStdString(_package->dataFilter()));
	}
	else
		reset();
}

void FilterModel::init()
{
	checkForSendFilter();
}

void FilterModel::setRFilter(QString newRFilter)
{
	if(newRFilter != _rFilter)
	{
		bool oldHasFilter = hasFilter();

		_rFilter = newRFilter;

		if(oldHasFilter != hasFilter())
			emit hasFilterChanged();

		checkForSendFilter();

		emit rFilterChanged();
	}
}

void FilterModel::setConstructedJSON(QString newConstructedJSON)
{
	if(newConstructedJSON != _constructedJSON)
	{
		bool oldHasFilter = hasFilter();

		_constructedJSON = newConstructedJSON;

		if(oldHasFilter != hasFilter())
			emit hasFilterChanged();

		std::set<std::string> columnsUsedInConstructedFilter = JsonUtilities::convertDragNDropFilterJSONToSet(_constructedJSON.toStdString());

		if(columnsUsedInConstructedFilter != _columnsUsedInConstructedFilter)
			emit updateColumnsUsedInConstructedFilter(columnsUsedInConstructedFilter);

		_columnsUsedInConstructedFilter = columnsUsedInConstructedFilter;

		emit constructedJSONChanged();
	}

	if(_package != NULL)
		_package->setFilterConstructorJson(newConstructedJSON.toStdString());
}

void FilterModel::setConstructedR(QString newConstructedR)
{
	if(newConstructedR != _constructedR)
	{
		_constructedR = newConstructedR;

		emit constructedRChanged();
		emit updateGeneratedFilterWithR(_constructedR);
	}
}

void FilterModel::setGeneratedFilter(QString newGeneratedFilter)
{
	if(newGeneratedFilter != _generatedFilter)
	{
		_generatedFilter = newGeneratedFilter;

		checkForSendFilter(true);

		emit generatedFilterChanged();
	}
}


void FilterModel::processFilterResult(std::vector<bool> filterResult, int requestId)
{
	if((requestId > -1 && requestId < _lastSentRequestId) || _package == NULL || _package->dataSet() == NULL)
		return;

	_package->setDataFilter(_rFilter.toStdString()); //store the filter that was last used and actually gave results.
	_package->dataSet()->setFilterVector(filterResult);

	emit filterUpdated();

	updateStatusBar();
}


void FilterModel::processFilterErrorMsg(QString filterErrorMsg, int requestId)
{
	if(requestId == _lastSentRequestId || requestId == -1)
		setFilterErrorMsg(filterErrorMsg);
}

void FilterModel::checkForSendFilter(bool justCameFromGeneratedFilterUpdate)
{
	if(!justCameFromGeneratedFilterUpdate || (_package != NULL && _package->refreshAnalysesAfterFilter()))
	{
		setFilterErrorMsg("");
		emit sendFilter(_generatedFilter, _rFilter, ++_lastSentRequestId);
	}
}

void FilterModel::updateStatusBar()
{
	if(_package == NULL || _package->dataSet() == NULL)
	{
		setStatusBarText("No data loaded!");
		return;
	}

	int		TotalCount			= _package->dataSet()->rowCount(),
			TotalThroughFilter	= _package->dataSet()->filteredRowCount();
	double	PercentageThrough	= 100.0 * ((double)TotalThroughFilter) / ((double)TotalCount);

	std::stringstream ss;
	if(hasFilter())	ss << "Data has " << TotalCount << " rows, " << TotalThroughFilter << " (~" << (int)round(PercentageThrough) << "%)  passed through filter";

	setStatusBarText(QString::fromStdString(ss.str()));

	if(_package->refreshAnalysesAfterFilter()) //After loading a JASP package we do not want to rerun all analyses because it might take very long
		refreshAllAnalyses();

	_package->setRefreshAnalysesAfterFilter(true);
}

void FilterModel::rescanRFilterForColumns()
{
	_columnsUsedInRFilter = ComputedColumn::findUsedColumnNamesStatic(_rFilter.toStdString());
}

void FilterModel::computeColumnSucceeded(std::string columnName, std::string, bool dataChanged)
{
	if(dataChanged && (_columnsUsedInConstructedFilter.count(columnName) > 0 || _columnsUsedInRFilter.count(columnName) > 0))
		checkForSendFilter();
}
