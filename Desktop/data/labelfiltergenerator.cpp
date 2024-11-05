#include "labelfiltergenerator.h"
#include "timers.h"

labelFilterGenerator::labelFilterGenerator(ColumnModel *columnModel, QObject *parent)
	: QObject(parent), _columnModel(columnModel)
{
	connect(_columnModel,	&ColumnModel::labelFilterChanged,	this,	&labelFilterGenerator::labelFilterChanged	);
	connect(_columnModel,	&ColumnModel::allFiltersReset,		this,	&labelFilterGenerator::labelFilterChanged	);
}

std::string labelFilterGenerator::generateFilter()
{
	JASPTIMER_SCOPE(labelFilterGenerator::generateFilter);

	int neededFilters = 0;
	
	DataSetPackage * pkg = DataSetPackage::pkg();
	
	for(size_t col=0; col<pkg->dataColumnCount(); col++)
		if(pkg->labelNeedsFilter(col))
			neededFilters++;

	std::stringstream newGeneratedFilter;
	Filter* filter = DataSetPackage::pkg()->filter();
	std::string filterRScript = filter ? filter->constructorR() : "";

	newGeneratedFilter << "generatedFilter <- ";

	if(neededFilters == 0)
	{
		if(filterRScript == "")	return DEFAULT_FILTER_GEN;
		else					newGeneratedFilter << "("<< filterRScript <<")";
	}
	else
	{
		bool moreThanOne = neededFilters > 1, first = true;

		if(moreThanOne)
			newGeneratedFilter << "(";
		
		
		for(size_t col=0; col<pkg->dataColumnCount(); col++)
			if(pkg->labelNeedsFilter(col))
			{
				newGeneratedFilter << (first ? "" : " & ") << generateLabelFilter(col);
				first = false;
			}

		if(moreThanOne)
			newGeneratedFilter << ")";

		if(filterRScript != "")
				newGeneratedFilter << " & \n("<<filterRScript<<")";
	}

	return newGeneratedFilter.str();
}

void labelFilterGenerator::labelFilterChanged()
{
	JASPTIMER_SCOPE(labelFilterGenerator::labelFilterChanged);
	emit setGeneratedFilter(QString::fromStdString(generateFilter()));
}

std::string	labelFilterGenerator::generateLabelFilter(size_t col)
{
	JASPTIMER_SCOPE(labelFilterGenerator::generateLabelFilter);
	
	DataSetPackage	*	pkg				= DataSetPackage::pkg();
	std::string			columnName		= pkg->getColumnName(col);
	boolvec				filterAllows	= pkg->getColumnFilterAllows(col);
	stringvec			labels			= pkg->getColumnLabelsAsStrVec(col);
	int					pos				= std::count_if(filterAllows.begin(), filterAllows.end(), [](bool f){ return f; }), 
						cnt				= 0;
	bool				bePositive		= pos <= filterAllows.size() - pos;
	std::stringstream	out;
	
	for(size_t row=0; row<filterAllows.size(); row++)
		if(filterAllows[row] == bePositive) //Also make sure we use .nominal because otherwise we might be comparing to the value instead...
			out << (cnt++ > 0 ? (bePositive ? " | " : " & ") : "") << columnName << ".nominal" << (bePositive ? " == \"" : " != \"") << labels[row] << "\"";
	
	return "(" + out.str() + ")";
}
