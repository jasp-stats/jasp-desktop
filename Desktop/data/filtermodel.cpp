#include "filtermodel.h"
#include "jsonutilities.h"
#include "columnencoder.h"

FilterModel::FilterModel(labelFilterGenerator * labelFilterGenerator)
	: QObject(DataSetPackage::pkg()), _labelFilterGenerator(labelFilterGenerator)
{
	reset();
	connect(this,					&FilterModel::rFilterChanged,	this, &FilterModel::rescanRFilterForColumns	);
	connect(DataSetPackage::pkg(),	&DataSetPackage::modelReset,	this, &FilterModel::dataSetPackageResetDone	);
	connect(DataSetPackage::pkg(),	&DataSetPackage::modelInit,		this, &FilterModel::modelInit				);
}

void FilterModel::reset()
{
	_setGeneratedFilter(DEFAULT_FILTER_GEN	);
	setConstructedJSON(	DEFAULT_FILTER_JSON	);
	_setRFilter(		DEFAULT_FILTER		);

	if(DataSetPackage::pkg()->rowCount() > 0)
		sendGeneratedAndRFilter();
}

void FilterModel::dataSetPackageResetDone()
{
	_setGeneratedFilter(tq(_labelFilterGenerator->generateFilter())			);
	setConstructedJSON(	tq(DataSetPackage::pkg()->filterConstructorJson())	);
	_setRFilter(		tq(DataSetPackage::pkg()->dataFilter())				);
}

void FilterModel::modelInit()
{
	if(!DataSetPackage::pkg()->isArchive() || DataSetPackage::pkg()->filterShouldRunInit()) //Either this wasn't a JASP file (archive) and we need to run the filter after loading, or it *is* a JASP file but it is old (<0.11) and doesn't have filterVector stored in it yet.
		sendGeneratedAndRFilter();

	DataSetPackage::pkg()->setFilterShouldRunInit(true); //Make sure next time we come here (because of computed columns or something) we do actually run the filter
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

	if(DataSetPackage::pkg() != nullptr)
		DataSetPackage::pkg()->setFilterConstructorJson(newConstructedJSON.toStdString());
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
	if((requestId > -1 && requestId < _lastSentRequestId))
		return;

	//store the filter that was last used and actually gave results and those results:
	if(DataSetPackage::pkg()->setFilterData(_rFilter.toStdString(), filterResult))
	{
		refreshAllAnalyses();
		emit filterUpdated();
		updateStatusBar();
	}
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
	if(!DataSetPackage::pkg()->hasDataSet())
	{
		setStatusBarText("No data loaded!");
		return;
	}

	int     TotalCount			= DataSetPackage::pkg()->rowCount(),
	        TotalThroughFilter	= DataSetPackage::pkg()->filteredRowCount();
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

void FilterModel::datasetChanged(	QStringList             changedColumns,
                                    QStringList             missingColumns,
                                    QMap<QString, QString>	changeNameColumns,
                                    bool                    rowCountChanged,
                                    bool                  /*hasNewColumns*/)
{
	bool invalidateMe = rowCountChanged;

	if(!invalidateMe)
		for(const QString & changed : changedColumns)
			if(_columnsUsedInRFilter.count(fq(changed)) > 0 || _columnsUsedInConstructedFilter.count(fq(changed)) > 0)
			{
				invalidateMe = true;
				break;
			}

	auto iUseOneOfTheseColumns = [&](std::vector<std::string> cols) -> bool
	{
		for(const std::string & col : cols)
			if(_columnsUsedInRFilter.count(col) > 0 || _columnsUsedInConstructedFilter.count(col) > 0)
				return true;

		return false;
	};

	if(iUseOneOfTheseColumns(fq(changeNameColumns.keys())))
	{
		std::map<std::string, std::string> stdChangeNameCols(fq(changeNameColumns));

		invalidateMe = true;

		setRFilter(        tq(ColumnEncoder::replaceColumnNamesInRScript(fq(rFilter()),                     stdChangeNameCols)));
		setConstructedJSON( tq(JsonUtilities::replaceColumnNamesInDragNDropFilterJSON(fq(constructedJSON()), stdChangeNameCols)));
	}

	auto missingStd = fq(missingColumns);
	if(iUseOneOfTheseColumns(missingStd))
	{
		setRFilter(tq(ColumnEncoder::removeColumnNamesFromRScript(fq(rFilter()), missingStd)));

		setConstructedJSON( tq(JsonUtilities::removeColumnsFromDragNDropFilterJSON( fq(constructedJSON()), missingStd)));

		invalidateMe = false; //Actually, if stuff is removed from the filter it won't work will it now?

		processFilterResult(std::vector<bool>(DataSetPackage::pkg()->rowCount(), true), -1); //Just reset the filter result to everything true while the user gets the change to fix there now broken filter

		//The following errormsg is overwritten immediately but that is because constructedJson changed triggers qml which triggers (some vents later) a send event. So yeah...
		//Ill leave it here though because it would be nice to show this friendlier msg then "null not found"
		setFilterErrorMsg("Some columns were removed from the data and your filter(s)!");
	}

	if(invalidateMe)
		sendGeneratedAndRFilter();
}
