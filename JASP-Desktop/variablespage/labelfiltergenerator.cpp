#include "labelfiltergenerator.h"

labelFilterGenerator::labelFilterGenerator(DataSetPackage *package, QObject *parent) : QObject(parent)
{
	_package = package;
}

std::string labelFilterGenerator::generateFilter()
{
	int neededFilters = 0;

	for(Column & col : _package->dataSet->columns())
		if(labelNeedsFilter(col))
			neededFilters++;

	std::stringstream newGeneratedFilter;

	newGeneratedFilter << "generatedFilter <- ";

	if(neededFilters == 0)
	{
		if(easyFilterConstructorRScript == "")
			newGeneratedFilter << "rep(TRUE," << _package->dataSet->rowCount() << ")";
		else
			newGeneratedFilter << "("<<easyFilterConstructorRScript<<")";
	}
	else
	{
		bool moreThanOne = neededFilters > 1, first = true;

		if(moreThanOne)
			newGeneratedFilter << "(";


		for(Column & col : _package->dataSet->columns())
			if(labelNeedsFilter(col))
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

std::string	labelFilterGenerator::generateLabelFilter(Column & column)
{
	std::string columnName = column.name();
	std::stringstream out;
	int pos = 0, neg = 0;
	bool first = true;

	for(const Label & label : column.labels())
		(label.filterAllows() ? pos : neg)++;

	bool bePositive = pos <= neg;

	out << "(";

	for(const Label & label : column.labels())
		if(label.filterAllows() == bePositive)
		{
			out << (!first ? (bePositive ? " | " : " & ") : "") << columnName << (bePositive ? " == \"" : " != \"") << label.text() << "\"";
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
