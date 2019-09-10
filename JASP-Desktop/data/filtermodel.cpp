#include "filtermodel.h"
#include "variablespage/labelfiltergenerator.h"
#include "utilities/jsonutilities.h"

void FilterModel::reset()
{
	_setGeneratedFilter(DEFAULT_FILTER_GEN);
	setConstructedJSON(DEFAULT_FILTER_JSON);
	_setRFilter(DEFAULT_FILTER);

	if(_package != nullptr && _package->dataSet() != nullptr && _package->dataSet()->rowCount() > 0)
		sendGeneratedAndRFilter();
}

void FilterModel::setDataSetPackage(DataSetPackage * package)
{
	_package = package;

	if(_package != nullptr)
	{
		_setGeneratedFilter(QString::fromStdString(labelFilterGenerator(_package).generateFilter()));
		setConstructedJSON(QString::fromStdString(_package->filterConstructorJson()));
		_setRFilter(QString::fromStdString(_package->dataFilter()));

		if(!_package->isArchive() || _package->filterShouldRunInit()) //Either this wasn't a JASP file (archive) and we need to run the filter after loading, or it *is* a JASP file but it is old (<0.11) and doesn't have filterVector stored in it yet.
			sendGeneratedAndRFilter();
	}
	else
		reset();
}

void FilterModel::setRFilter(QString newRFilter)
{
	if (_setRFilter(newRFilter))
		sendGeneratedAndRFilter();
}

bool FilterModel::_setRFilter(const QString& newRFilter)
{
	if(newRFilter != _rFilter)
	{
		bool oldHasFilter = hasFilter();

		_rFilter = newRFilter;

		if(oldHasFilter != hasFilter())
			emit hasFilterChanged();

		emit rFilterChanged();

		return true;
	}

	return false;
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

	if(_package != nullptr)
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
	if (_setGeneratedFilter(newGeneratedFilter))
		sendGeneratedAndRFilter();	
}

bool FilterModel::_setGeneratedFilter(const QString& newGeneratedFilter)
{
	if (newGeneratedFilter != _generatedFilter)
	{
		_generatedFilter = newGeneratedFilter;
		emit generatedFilterChanged();
		return true;
	}

	return false;
}


void FilterModel::processFilterResult(std::vector<bool> filterResult, int requestId)
{
	if((requestId > -1 && requestId < _lastSentRequestId) || _package == nullptr || _package->dataSet() == nullptr)
		return;

	_package->dataSet()->setSynchingData(true);
	_package->setDataFilter(_rFilter.toStdString()); //store the filter that was last used and actually gave results.
	if(_package->dataSet()->setFilterVector(filterResult))
	{
		_package->dataSet()->setSynchingData(false);
		refreshAllAnalyses();
		emit filterUpdated();
		updateStatusBar();
	}
	_package->dataSet()->setSynchingData(false);
}


void FilterModel::processFilterErrorMsg(QString filterErrorMsg, int requestId)
{
	if(requestId == _lastSentRequestId || requestId == -1)
		setFilterErrorMsg(filterErrorMsg);
}

void FilterModel::sendGeneratedAndRFilter()
{
	setFilterErrorMsg("");
	_lastSentRequestId = emit sendFilter(_generatedFilter, _rFilter);
}

void FilterModel::updateStatusBar()
{
	if(_package == nullptr || _package->dataSet() == nullptr)
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
}

void FilterModel::rescanRFilterForColumns()
{
	_columnsUsedInRFilter = ComputedColumn::findUsedColumnNamesStatic(_rFilter.toStdString());
}

void FilterModel::computeColumnSucceeded(QString columnName, QString, bool dataChanged)
{
	if(dataChanged && (_columnsUsedInConstructedFilter.count(columnName.toStdString()) > 0 || _columnsUsedInRFilter.count(columnName.toStdString()) > 0))
		sendGeneratedAndRFilter();
}
