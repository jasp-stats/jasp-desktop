#include "labelfiltergenerator.h"

labelFilterGenerator::labelFilterGenerator(LabelModel *labelModel, QObject *parent)
	: QObject(parent), _labelModel(labelModel)
{
	connect(_labelModel,	&LabelModel::labelFilterChanged,	this,	&labelFilterGenerator::labelFilterChanged	);
	connect(_labelModel,	&LabelModel::allFiltersReset,		this,	&labelFilterGenerator::labelFilterChanged	);
}

std::string labelFilterGenerator::generateFilter()
{
	int neededFilters = 0;

	for(size_t col=0; col<_labelModel->dataColumnCount(); col++)
		if(_labelModel->labelNeedsFilter(col))
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


		for(size_t col=0; col<_labelModel->dataColumnCount(); col++)
			if(_labelModel->labelNeedsFilter(col))
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
	std::string columnName = _labelModel->columnName(col);
	std::stringstream out;
	int pos = 0, neg = 0;
	bool first = true;

	std::vector<bool> filterAllows = _labelModel->filterAllows(col);
	for(bool allow : filterAllows)
		(allow ? pos : neg)++;

	bool bePositive = pos <= neg;

	out << "(";

	std::vector<std::string> labels = _labelModel->labels(col);
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
