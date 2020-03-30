//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "log.h"
#include "utilities/qutils.h"
#include "listmodeljagsdatainput.h"
#include "analysis/analysisform.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optionterm.h"
#include "r_functionwhitelist.h"
#include "boundqmltableview.h"

ListModelJAGSDataInput::ListModelJAGSDataInput(BoundQMLTableView *parent, QString tableType) : ListModelTableViewBase(parent, tableType)
{
	_defaultCellVal = "...";
	_colNames.clear();
	_values.clear();
	_colNames.push_back(getDefaultColName(0));
	_values.push_back({});
	_colNames.push_back(getDefaultColName(1));
	_values.push_back({});

	parent->setItemProperty("parseDefaultValue", false);
	parent->setItemProperty("defaultEmptyValue", _defaultCellVal);
}

void ListModelJAGSDataInput::sourceTermsChanged(const Terms *, const Terms *)
{
	beginResetModel();

	Terms sourceTerms = getSourceTerms();
	if (_values.length() > 0)
	{
		QMap<QString, QVariant> mapping;
		const QVector<QVariant>& firstCol = _values[0];
		const QVector<QVariant>& secondCol = _values[1];
		int row = 0;
		for (const QVariant& key : firstCol)
		{
			mapping[key.toString()] = secondCol[row];
			row++;
		}
		_values.clear();
		_rowNames.clear();
		_rowCount = sourceTerms.size();
		for (size_t colNb = 1; colNb <= _rowCount; colNb++)
			_rowNames.push_back(getDefaultRowName(colNb));
		QList<QString> firstColumnValues = sourceTerms.asQList();
		QVector<QVariant> firstColumn;
		QVector<QVariant> secondColumn;
		for (const QString& firstValue : firstColumnValues)
		{
			firstColumn.push_back(firstValue);
			QVariant secondValue = mapping.contains(firstValue) ? mapping[firstValue] : _defaultCellVal;
			secondColumn.push_back(secondValue);
		}
		_values.push_back(firstColumn);
		_values.push_back(secondColumn);
	}

	endResetModel();

	emit columnCountChanged();
	emit rowCountChanged();
	emit modelChanged();
}


QString ListModelJAGSDataInput::getDefaultColName(size_t index) const
{
	if(index == 0)
		return "Parameter";
	return "R Code";
}

Qt::ItemFlags ListModelJAGSDataInput::flags(const QModelIndex &index) const
{
	if (index.column() == 0 && listView()->sourceModels().length() > 0)
		return Qt::ItemIsSelectable | Qt::ItemIsEnabled;

	return Qt::ItemIsSelectable | Qt::ItemIsEnabled | Qt::ItemIsEditable;
}

int ListModelJAGSDataInput::getMaximumColumnWidthInCharacters(size_t columnIndex) const
{
	return columnIndex == 0 ? 6 : 25;
}

OptionsTable *ListModelJAGSDataInput::createOption()
{
	Options* optsTemplate =		new Options();
	optsTemplate->add("name",	new OptionString());
	optsTemplate->add("levels", new OptionVariables());
	optsTemplate->add("values", new OptionTerm());


	OptionsTable * returnThis = new OptionsTable(optsTemplate);

	if(_initialColCnt > 0)
	{
		std::vector<std::string> stdlevels;
		for (int row=0; row<_initialRowCnt; row++)
			stdlevels.push_back(fq(getDefaultRowName(row)));

		std::vector<Options*> allOptions;

		for (int colIndex = 0; colIndex < _initialColCnt; colIndex++)
		{
			Options* options =		new Options();
			options->add("name",	new OptionString(fq(getDefaultColName(colIndex))));
			options->add("levels",	new OptionVariables(stdlevels));

			std::vector<std::string> tempValues;
			for (const auto & level: stdlevels)
				tempValues.push_back(_defaultCellVal.toString().toStdString());
			options->add("values",	new OptionTerm(tempValues));

			allOptions.push_back(options);
		}

		returnThis->setValue(allOptions);
	}

	return returnThis;
}

void ListModelJAGSDataInput::initValues(OptionsTable * bindHere)
{
	_colNames.clear();
	_rowNames.clear();
	_values.clear();

	_boundTo = bindHere;

	std::vector<Options *>	options = bindHere->value();

	OptionVariables		* optionLevels = nullptr;

	for (Options * newRow : options)
	{
		OptionString	*	optionName		= static_cast<OptionString		*>(newRow->get("name"));
							optionLevels	= static_cast<OptionVariables	*>(newRow->get("levels")); // why not store it once?
		OptionTerm		*	optionValues	= static_cast<OptionTerm		*>(newRow->get("values"));

		_colNames.push_back(QString::fromStdString(optionName->value()));
		//levels = optionLevels->variables(); //The old code (in boundqmltableview.cpp) seemed to specify to simply use the *last* OptionVariables called "levels" in the binding option. So I'll just repeat that despite not getting it.
		_values.push_back({});
		for (const std::string & val : optionValues->term())
			_values[_values.size()-1].push_back(tq(val));
	}

	if(optionLevels)
		for(const std::string & level : optionLevels->variables())
			_rowNames.push_back(QString::fromStdString(level));

	//No need to check colnames to cols in values because they are created during the same loop and thus crash if non-matching somehow
	if (_values.size() > 0 && int(_values[0].size()) != _rowNames.size())
		addControlError(tr("Number of rows specifed in Options for ListModelJAGSDataInput does not match number of rows in values!"));


	beginResetModel();

	_columnCount = _colNames.size();

	for(auto & col : _values)
		if(_rowNames.size() < col.size())
		{
			Log::log() << "Too many rows in a column of OptionsTable for ListModelJAGSDataInput! Shrinking column to fit." << std::endl;
			col.resize(_rowNames.size());
		}
		else
			for (int row = col.size(); row < _rowNames.size(); row++)
				col.push_back(1);

	//Ok, going to assume that the following: for (size_t i = values.size(); i < _columnCount; ++i) means we should add columns in case the data wasn't filled correctly (aka colNames did not match with values) but that cannot be now.

	endResetModel();

	emit columnCountChanged();
	emit rowCountChanged();
}

void ListModelJAGSDataInput::modelChangedSlot() // Should move this to listmodeltableviewbase as well probably? And also connect columnCount and colNames etc
{
	if (_boundTo)
	{
		std::vector<std::string> stdlevels;
		for (const QString& rowName : _rowNames)
			stdlevels.push_back(rowName.toStdString());

		std::vector<Options*> allOptions;

		for (int colIndex = 0; colIndex < _colNames.size(); colIndex++)
		{
			Options* options =		new Options();
			options->add("name",	new OptionString(_colNames[colIndex].toStdString()));
			options->add("levels",	new OptionVariables(stdlevels));
			
			std::vector<std::string> tempValues;
			for (QVariant val : _values[colIndex].toStdVector())
				tempValues.push_back(val.toString().toStdString());
			options->add("values",	new OptionTerm(tempValues));

			allOptions.push_back(options);

		}

		_boundTo->setValue(allOptions);
	}
}
