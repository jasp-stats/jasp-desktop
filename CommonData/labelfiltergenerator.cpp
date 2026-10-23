#include "labelfiltergenerator.h"
#include "qutils.h"
#include "dataset.h"
#include "filter.h"
#include "column.h"
#include "timers.h"
#include <QThread>

LabelFilterGenerator::LabelFilterGenerator(Filter * filter)
	: QObject(nullptr), _filter(filter)
{
	connect(_filter->data(),	&DataSet::labelFilterChanged,	this,	&LabelFilterGenerator::regenerateGeneratedFilter	);
	connect(_filter->data(),	&DataSet::allFiltersReset,		this,	&LabelFilterGenerator::regenerateGeneratedFilter	);
	connect(_filter,			&Filter::constructorRChanged,	this,	&LabelFilterGenerator::regenerateGeneratedFilter	);
	
	//Filters are built by the importers on the AsyncLoader thread, but the Filter itself was already
	//moved to the GUI thread by DataSetBaseNode's ctor (it is a model QML binds to), so our affinity has to follow it before we can be parented to it.
	QThread * target = filter->thread();

	if(thread() != target)
		moveToThread(target);

	//setParent() sends a ChildAdded event to the parent and QCoreApplication::sendEvent() asserts (in debug builds) when called from a thread other than the one owning the receiver. 
	// if the Filter dies before the GUI thread runs this the queued call is dropped and leak - narrow.
	if(QThread::currentThread() == target)
		setParent(filter);
}

std::string LabelFilterGenerator::generateFilter()
{
	JASPTIMER_SCOPE(LabelFilterGenerator::generateFilter);

	int neededFilters = 0;

	for(Column * c : _filter->data()->columns())
		if(c->hasLabelFilter())
			neededFilters++;

	std::stringstream newGeneratedFilter;

	std::string filterRScript = _filter->constructorR();

	newGeneratedFilter << "generatedFilter <- ";

	if(neededFilters == 0)
	{
		if(filterRScript == "")	return DEFAULT_FILTER_GEN;
		else					newGeneratedFilter << "("<< filterRScript <<")";
	}
	else
	{
		bool	moreThanOne = neededFilters > 1,
				first		= true;

		if(moreThanOne)
			newGeneratedFilter << "(";
		
		for(Column * c : _filter->data()->columns())
			if(c->hasLabelFilter())
			{
				newGeneratedFilter << (first ? "" : " & ") << c->generateLabelFilter();
				first = false;
			}

		if(moreThanOne)
			newGeneratedFilter << ")";

		if(filterRScript != "")
			newGeneratedFilter << " & \n("<<filterRScript<<")";
	}

	return newGeneratedFilter.str();
}

void LabelFilterGenerator::regenerateGeneratedFilter()
{
	JASPTIMER_SCOPE(LabelFilterGenerator::regenerateGeneratedFilter);

	_filter->setGeneratedFilter(generateFilter());
}


