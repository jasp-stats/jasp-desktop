#include "labelfiltergenerator.h"

labelFilterGenerator::labelFilterGenerator(ColumnModel *columnModel, QObject *parent)
	: QObject(parent), _columnModel(columnModel)
{
	connect(_columnModel,	&ColumnModel::labelFilterChanged,	this,	&labelFilterGenerator::labelFilterChanged	);
	connect(_columnModel,	&ColumnModel::allFiltersReset,		this,	&labelFilterGenerator::labelFilterChanged	);
}

std::string labelFilterGenerator::generateFilter()
{
	int neededFilters = 0;
	
	for(size_t col=0; col<_columnModel->dataColumnCount(); col++)
		if(_columnModel->labelNeedsFilter(col))
			neededFilters++;

	std::stringstream newGeneratedFilter;

	newGeneratedFilter << "generatedFilter <- ";

	if(neededFilters == 0)
	{
		if(easyFilterConstructorRScript == "")	return DEFAULT_FILTER_GEN;
		else									newGeneratedFilter << "("<< easyFilterConstructorRScript <<")";
	}
	else
	{
		bool moreThanOne = neededFilters > 1, first = true;

		if(moreThanOne)
			newGeneratedFilter << "(";
		
		
		for(size_t col=0; col<_columnModel->dataColumnCount(); col++)
			if(_columnModel->labelNeedsFilter(col))
			{
				newGeneratedFilter << (first ? "" : " & ") << generateLabelFilter(col);
				first = false;
			}

		if(moreThanOne)
			newGeneratedFilter << ")";

		if(easyFilterConstructorRScript != "")
				newGeneratedFilter << " & \n("<<easyFilterConstructorRScript<<")";
	}

	return newGeneratedFilter.str();
}

void labelFilterGenerator::labelFilterChanged()
{
	emit setGeneratedFilter(QString::fromStdString(generateFilter()));
}

std::string	labelFilterGenerator::generateLabelFilter(size_t col)
{
	std::string columnName = _columnModel->columnName(col);
	std::stringstream out;
	int pos = 0, neg = 0;
	bool first = true;
	
	std::vector<bool> filterAllows = _columnModel->filterAllows(col);
	for(bool allow : filterAllows)
		(allow ? pos : neg)++;

	bool bePositive = pos <= neg;

	out << "(";
	
	std::vector<std::string> labels = _columnModel->labels(col);
	for(size_t row=0; row<filterAllows.size(); row++)
		if(filterAllows[row] == bePositive)
		{
			out << (!first ? (bePositive ? " | " : " & ") : "") << columnName << (bePositive ? " == \"" : " != \"") << labels[row] << "\"";
			first = false;
		}
	out << ")";

	return out.str();
}

void labelFilterGenerator::easyFilterConstructorRCodeChanged(QString newRScript)
{
	if(easyFilterConstructorRScript != newRScript.toStdString())
	{
		easyFilterConstructorRScript = newRScript.toStdString();
		emit setGeneratedFilter(QString::fromStdString(generateFilter()));
	}
}
