#include "filtermodel.h"
#include "jsonutilities.h"
#include "columnencoder.h"
#include "timers.h"

FilterModel::FilterModel(labelFilterGenerator * labelFilterGenerator)
	: QObject(DataSetPackage::pkg()), _labelFilterGenerator(labelFilterGenerator)
{
	_undoStack = DataSetPackage::pkg()->undoStack();

	reset();
	connect(this,					&FilterModel::rFilterChanged,	this, &FilterModel::rescanRFilterForColumns	);
	connect(DataSetPackage::pkg(),	&DataSetPackage::modelReset,	this, &FilterModel::dataSetPackageResetDone	);
	connect(DataSetPackage::pkg(),	&DataSetPackage::modelInit,		this, &FilterModel::modelInit				);
}

QString FilterModel::rFilter()			const	{ return !DataSetPackage::filter() ? defaultRFilter()		: tq(DataSetPackage::filter()->rFilter());					}
QString FilterModel::constructorR()		const	{ return !DataSetPackage::filter() ? ""						: tq(DataSetPackage::filter()->constructorR());				}
QString FilterModel::filterErrorMsg()	const	{ return !DataSetPackage::filter() ? ""						: tq(DataSetPackage::filter()->errorMsg());					}
QString FilterModel::generatedFilter()	const	{ return !DataSetPackage::filter() ? DEFAULT_FILTER_GEN		: tq(DataSetPackage::filter()->generatedFilter());			}
QString FilterModel::constructorJson()	const	{ return !DataSetPackage::filter() ? DEFAULT_FILTER_JSON	: tq(DataSetPackage::filter()->constructorJson());			}

const char * FilterModel::defaultRFilter()
{
	static std::string defaultFilter;

	const std::string forceTranslatedStuffToAlwaysBeAComment =
		tr(
			"Above you see the code that JASP generates for both value filtering and the drag&drop filter."					"\n"
			"This default result is stored in 'generatedFilter' and can be replaced or combined with a custom filter."		"\n"
			"To combine you can append clauses using '&': 'generatedFilter & customFilter & perhapsAnotherFilter'"			"\n"
			"Click the (i) icon in the lower right corner for further help."												"\n").toStdString();

	defaultFilter = "# " + stringUtils::replaceBy(forceTranslatedStuffToAlwaysBeAComment, "\n", "\n# ") + "\n\ngeneratedFilter";

	return defaultFilter.c_str();
}

void FilterModel::reset()
{
	_setGeneratedFilter(DEFAULT_FILTER_GEN	);
	setConstructorJson(	DEFAULT_FILTER_JSON	);
	_setRFilter(		defaultRFilter()		);

	if(DataSetPackage::pkg()->dataRowCount() > 0)
		sendGeneratedAndRFilter();
}

void FilterModel::dataSetPackageResetDone()
{
	_setGeneratedFilter(tq(_labelFilterGenerator->generateFilter())		);
	setConstructorJson(	!DataSetPackage::filter() ? "" : tq(DataSetPackage::filter()->constructorJson())	);
	_setRFilter(		!DataSetPackage::filter() ? "" : tq(DataSetPackage::filter()->rFilter())			);
}

void FilterModel::modelInit()
{
	if(!DataSetPackage::pkg()->isJaspFile() || DataSetPackage::pkg()->filterShouldRunInit()) //Either this wasn't a JASP file (archive) and we need to run the filter after loading, or it *is* a JASP file but it is old (<0.11) and doesn't have filterVector stored in it yet.
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
	if(newRFilter != rFilter())
	{
		bool oldHasFilter = hasFilter();

		if(DataSetPackage::filter())
			DataSetPackage::filter()->setRFilter(fq(newRFilter));

		if(oldHasFilter != hasFilter())
			emit hasFilterChanged();

		emit rFilterChanged();

		return true;
	}

	return false;
}

void FilterModel::setFilterErrorMsg(	QString newFilterErrorMsg)
{
	if(newFilterErrorMsg != filterErrorMsg())
	{
		if(DataSetPackage::filter())
			DataSetPackage::filter()->setErrorMsg(fq(newFilterErrorMsg));
		
		emit filterErrorMsgChanged();
	}
}

void FilterModel::applyConstructorJson(QString newConstructorJson)
{
	if (newConstructorJson != constructorJson())
		_undoStack->pushCommand(new SetJsonFilterCommand(DataSetPackage::pkg(), this, newConstructorJson));
}

void FilterModel::applyRFilter(QString newRFilter)
{
	if (newRFilter != rFilter())
		_undoStack->pushCommand(new SetRFilterCommand(DataSetPackage::pkg(), this, newRFilter));
}

void FilterModel::setConstructorJson(QString newconstructorJson)
{
	if(newconstructorJson != constructorJson())
	{
		bool oldHasFilter = hasFilter();

		if(DataSetPackage::filter())
			DataSetPackage::filter()->setConstructorJson(fq(newconstructorJson));

		if(oldHasFilter != hasFilter())
			emit hasFilterChanged();

		std::set<std::string> columnsUsedInConstructedFilter = JsonUtilities::convertDragNDropFilterJSONToSet(constructorJson().toStdString());

		if(columnsUsedInConstructedFilter != _columnsUsedInConstructedFilter)
			emit updateColumnsUsedInConstructedFilter(columnsUsedInConstructedFilter);

		_columnsUsedInConstructedFilter = columnsUsedInConstructedFilter;

		emit constructorJsonChanged();
	}
}

void FilterModel::setConstructorR(QString newConstructorR)
{
	if(newConstructorR != constructorR())
	{
		if(DataSetPackage::filter())
			DataSetPackage::filter()->setConstructorR(fq(newConstructorR));

		emit constructorRChanged();
		setGeneratedFilter(tq(_labelFilterGenerator->generateFilter()));
	}
}
void FilterModel::setGeneratedFilter(QString newGeneratedFilter)
{
	JASPTIMER_SCOPE(FilterModel::setGeneratedFilter);

	_setGeneratedFilter(newGeneratedFilter);
	// After this commit https://github.com/jasp-stats/jasp-desktop/commit/65f007cba2ff8986fc3ad86ac4b1a00fa706769b  the filter was not executed.
	// If a model reset is called just before, this will set the generatedFilter to the right value, and _setGeneratedFilter will returns false
	// But the engine did not compute yet the filter. So send the filter to the engine always.
	sendGeneratedAndRFilter();
}

bool FilterModel::_setGeneratedFilter(const QString& newGeneratedFilter)
{
	JASPTIMER_SCOPE(FilterModel::_setGeneratedFilter);

	if (newGeneratedFilter != generatedFilter())
	{
		if(DataSetPackage::filter())
			DataSetPackage::filter()->setGeneratedFilter(fq(newGeneratedFilter));

		emit generatedFilterChanged(); //does nothing?
		return true;
	}

	return false;
}


void FilterModel::processFilterResult(int requestId)
{
	if((requestId < _lastSentRequestId))
		return;

	if(!(DataSetPackage::pkg()->dataSet() || DataSetPackage::pkg()->dataSet()->filter()))
		return;

	//Load new filter values from database
	if(DataSetPackage::pkg()->dataSet()->filter()->dbLoadResultAndError())
	{
		emit filterErrorMsgChanged();
		emit refreshAllAnalyses();
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
	JASPTIMER_SCOPE(FilterModel::sendGeneratedAndRFilter);

	setFilterErrorMsg("");
	_lastSentRequestId = emit sendFilter(generatedFilter(), rFilter());
}

void FilterModel::updateStatusBar()
{
	if(!DataSetPackage::pkg()->hasDataSet())
	{
		setStatusBarText(tr("No data loaded!"));
		return;
	}

	int     TotalCount			= DataSetPackage::pkg()->dataRowCount(),
	        TotalThroughFilter	= DataSetPackage::pkg()->filteredRowCount();
	int		PercentageThrough	= (int)round(100.0 * ((double)TotalThroughFilter) / ((double)TotalCount));
	bool	Approximate			= PercentageThrough != TotalThroughFilter;

	setStatusBarText(tr("Data has %1 rows, %2 (%3%4%) passed through filter").arg(TotalCount).arg(TotalThroughFilter).arg(Approximate ? "~" : "").arg(PercentageThrough));
}

void FilterModel::rescanRFilterForColumns()
{
	_columnsUsedInRFilter = DataSetPackage::pkg() && DataSetPackage::pkg()->dataSet() ? DataSetPackage::pkg()->dataSet()->findUsedColumnNames(fq(rFilter())) : stringset();
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
		setConstructorJson( tq(JsonUtilities::replaceColumnNamesInDragNDropFilterJSONStr(fq(constructorJson()), stdChangeNameCols)));
	}

	auto missingStd = fq(missingColumns);
	if(iUseOneOfTheseColumns(missingStd))
	{
		setRFilter(tq(ColumnEncoder::removeColumnNamesFromRScript(fq(rFilter()), missingStd)));

		setConstructorJson( tq(JsonUtilities::removeColumnsFromDragNDropFilterJSONStr( fq(constructorJson()), missingStd)));

		invalidateMe = false; //Actually, if stuff is removed from the filter it won't work will it now?

		//Just reset the filter result to everything true while the user gets the change to fix their now broken filter
		if(DataSetPackage::filter())
			DataSetPackage::filter()->reset();

		emit refreshAllAnalyses();
		emit filterUpdated();
		updateStatusBar();

		//The following errormsg is overwritten immediately but that is because constructorJson changed triggers qml which triggers (some vents later) a send event. So yeah...
		//Ill leave it here though because it would be nice to show this friendlier msg then "null not found"
		setFilterErrorMsg("Some columns were removed from the data and your filter(s)!");
	}

	if(invalidateMe)
		sendGeneratedAndRFilter();
}
