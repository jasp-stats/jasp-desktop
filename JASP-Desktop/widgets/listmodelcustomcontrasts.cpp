//
// Copyright (C) 2013-2020 University of Amsterdam
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
#include "listmodelcustomcontrasts.h"
#include "analysis/analysisform.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optionterm.h"
#include "analysis/options/optionboolean.h"
#include "r_functionwhitelist.h"
#include "boundqmltableview.h"
#include "listmodelrepeatedmeasuresfactors.h"

ListModelCustomContrasts::ListModelCustomContrasts(BoundQMLTableView *parent, QString tableType) : ListModelTableViewBase(parent, tableType)
{
	_defaultCellVal = "0";
	_colNames.clear();
	_values.clear();
	_colNames.push_back(getDefaultColName(0));
	_values.push_back({});

	parent->setItemProperty("parseDefaultValue", false);
	parent->setItemProperty("defaultEmptyValue", _defaultCellVal);

	connect(DataSetPackage::pkg(), &DataSetPackage::labelChanged,		this, &ListModelCustomContrasts::labelChanged);
	connect(DataSetPackage::pkg(), &DataSetPackage::labelsReordered,	this, &ListModelCustomContrasts::labelsReordered);

	connect(this, &ListModelCustomContrasts::variableCountChanged,	[&]() { listView()->setItemProperty("variableCount", _variables.size()); });
	connect(listView()->item(), SIGNAL(scaleFactorChanged()),			this, SLOT(scaleFactorChanged()));
}

void ListModelCustomContrasts::sourceTermsChanged(const Terms *, const Terms *)
{
	_resetValuesEtc();
}

QStringList ListModelCustomContrasts::_getVariables()
{
	if (!_colName.isEmpty())
		return _colName.split(Term::separator);
	else
		return getSourceTerms().asQList();
}

void ListModelCustomContrasts::_getVariablesAndLabels(QStringList& variables, QVector<QVector<QVariant> >& allLabels)
{
	variables = _getVariables();

	// First set all combinations of all labels in values
	for (const QString& newVariable : variables)
	{
		QList<QString> labels;
		if (_factors.contains(newVariable))
			labels = _factors[newVariable];
		else
		{
			columnType colType = DataSetPackage::pkg()->getColumnType(fq(newVariable));
			if (colType == columnType::scale)
			{
				if (_scaleFactor == 0)
					labels = {"0"};
				else
				{
					labels.push_back(QString::number(-_scaleFactor));
					labels.push_back("0");
					labels.push_back(QString::number(_scaleFactor));
				}
			}
			else
				labels = DataSetPackage::pkg()->getColumnLabelsAsStringList(fq(newVariable));
		}

		QVector<QVector<QVariant> > copyAllLabels = allLabels;
		int len = copyAllLabels.length() > 0 ? copyAllLabels[0].length() : 1;
		allLabels.clear();

		for (const auto & copyValue : copyAllLabels)
		{
			QVector<QVariant> oneRow;
			for (int i = 0; i < labels.size(); i++)
				oneRow.append(copyValue);
			allLabels.push_back(oneRow);
		}

		QVector<QVariant> lastRow;
		for (const QString& label : labels)
			lastRow.insert(lastRow.length(), len, label);
		allLabels.push_back(lastRow);
	}

}

void ListModelCustomContrasts::_resetValuesEtc()
{
	QStringList newVariables;
	QVector<QVector<QVariant> > newValues;

	_getVariablesAndLabels(newVariables, newValues);

	beginResetModel();

	int nbContrast = int(_columnCount) - _variables.size();

	// Maps the new variables with the old ones (if they existed)
	QMap<int, int> variablesMap;
	for (int i = 0; i < newVariables.length(); i++)
		variablesMap[i] = _variables.indexOf(newVariables.at(i));

	int newMaxRows = newValues.length() > 0 ? newValues[0].length() : 0;

	// Make a mapping between the new rows and the old ones
	QMap<int, int> rowMapping;

	for (int row = 0; row < newMaxRows; row++)
	{
		// For this, for each row, we first build a boolean matrix that tells where the labels in the new values are found in the old values.
		QVector<QVector<bool> > allBools;

		for (int col = 0; col < newVariables.length(); col++)
		{
			if (variablesMap[col] >= 0 && variablesMap[col] < _values.length())
			{
				QVector<bool> bools;
				QVariant label = newValues[col][row];
				const QVector<QVariant>& oldValues = _values[variablesMap[col]];
				for (int oldRow = 0; oldRow < oldValues.length(); oldRow++)
					bools.push_back(oldValues.at(oldRow) == label);

				allBools.push_back(bools);
			}
		}

		int bestFitRow = -1;
		int bestFit = -1;

		// From the boolean matrix, find the row where the new labels are found the most.
		for (int oldRow = 0; oldRow < int(_rowCount); oldRow++)
		{
			int max = 0;
			for (int oldCol = 0; oldCol < allBools.length(); oldCol++)
			{
				if (allBools[oldCol].length() > oldRow && allBools[oldCol][oldRow])
				{
					max++;
					if (max > bestFit)
					{
						bestFitRow = oldRow;
						bestFit = max;
					}
				}
			}
		}

		rowMapping[row] = bestFitRow;
	}

	if (nbContrast == 0)
	{
		// No contrast yet: fill contrasts with default values.
		QVector<QVariant> contrasts;
		for (size_t i = 0; i < _initialColCnt; i++)
		{
			for (int row = 0; row < newMaxRows; row++)
				contrasts.push_back(_defaultCellVal);
			newValues.push_back(contrasts);
		}
	}
	else
	{
		// For each contrast, set the value corresponding to the rowMapping
		for (int i = 0; i < nbContrast; i++)
		{
			QVector<QVariant> contrasts;
			int oldContrastIndex = _variables.length() + i;

			if (_values.length() <= oldContrastIndex)
			{
				Log::log() << "ListModelCustomContrasts::sourceTermsChanged: Not the same amount of contrasts!!!" << std::endl;
				continue;
			}

			for (int row = 0; row < newMaxRows; row++)
				contrasts.push_back(rowMapping[row] >= 0 ? _values[oldContrastIndex][rowMapping[row]] : _defaultCellVal);

			newValues.push_back(contrasts);
		}
	}


	_variables = newVariables;
	_values = newValues;
	_colNames.clear();
	_columnCount = size_t(_values.length());
	_rowNames.clear();
	_rowCount = _values.length() > 0 ? size_t(_values[0].length()) : 0;

	for (size_t rowNb = 1; rowNb <= _rowCount; rowNb++)
		_rowNames.push_back(getDefaultRowName(rowNb));

	for (size_t colNb = 0; colNb < _columnCount; colNb++)
		_colNames.push_back(getDefaultColName(colNb));

	endResetModel();

	emit columnCountChanged();
	emit rowCountChanged();
	emit variableCountChanged();
	emit modelChanged();
}


QString ListModelCustomContrasts::getDefaultColName(size_t index) const
{
	int indexi = int(index);

	if (indexi < _variables.size())
		return _variables.at(indexi);
	else
		return tr("Contrast %1").arg(indexi - _variables.size() + 1);
}

void ListModelCustomContrasts::reset()
{
	if (_values.length() <= _variables.length() + _initialColCnt)
		return;

	beginResetModel();

	_values.erase(_values.begin() + _variables.length() + _initialColCnt, _values.end());
	_columnCount = size_t(_values.length());
	_colNames.erase(_colNames.begin() + _columnCount, _colNames.end());

	endResetModel();

	emit columnCountChanged();
	emit modelChanged();

}

void ListModelCustomContrasts::setup()
{
	// This cannot be done in the constructor: the form is then not yet known.
	connect(_tableView->form(), &AnalysisForm::dataSetChanged, this, &ListModelCustomContrasts::dataSetChangedHandler,	Qt::QueuedConnection	);
	QString factorsSourceName = _tableView->getItemProperty("factorsSource").toString();
	if (!factorsSourceName.isEmpty())
	{
		ListModelRepeatedMeasuresFactors* factorsSourceModel = dynamic_cast<ListModelRepeatedMeasuresFactors*>(_tableView->form()->getModel(factorsSourceName));
		if (factorsSourceModel)
		{
			_setFactorsSource(factorsSourceModel);
			connect(factorsSourceModel, &ListModelRepeatedMeasuresFactors::modelChanged, this, &ListModelCustomContrasts::factorsSourceChanged);
		}
	}
	_loadColumnInfo();
}

int ListModelCustomContrasts::getMaximumColumnWidthInCharacters(size_t columnIndex) const
{
	return 5;
}

OptionsTable *ListModelCustomContrasts::createOption()
{
	Options* optsTemplate =		new Options();
	optsTemplate->add("name",	new OptionString());
	optsTemplate->add("levels", new OptionVariables());
	optsTemplate->add("values", new OptionTerm());
	optsTemplate->add("isContrast",	new OptionBoolean(true));

	OptionsTable * returnThis = new OptionsTable(optsTemplate);

	QStringList variables;
	QVector<QVector<QVariant> > allLables;

	_getVariablesAndLabels(variables, allLables);

	if (variables.length() > 0)
	{
		std::vector<Options*> allOptions;
		std::vector<std::string> rowNames;

		for (int row = 0; row < allLables[0].length(); row++)
			rowNames.push_back(fq(getDefaultRowName(row)));

		int col = 0;
		for (const QString& variable : variables)
		{
			Options* options =			new Options();
			options->add("name",		new OptionString(fq(variable)));
			options->add("levels",		new OptionVariables(rowNames));
			options->add("isContrast",	new OptionBoolean(false));

			std::vector<std::string> tempValues;
			for (const QVariant & label: allLables[col])
				tempValues.push_back(label.toString().toStdString());
			options->add("values",	new OptionTerm(tempValues));

			allOptions.push_back(options);
			col++;
		}

		if (_initialColCnt > 0)
		{
			std::string defValue = _defaultCellVal.toString().toStdString();

			for (int colIndex = 0; colIndex < _initialColCnt; colIndex++)
			{
				Options* options =			new Options();
				options->add("name",		new OptionString(fq(getDefaultColName(variables.length() + colIndex))));
				options->add("levels",		new OptionVariables(rowNames));
				options->add("isContrast",	new OptionBoolean(true));

				std::vector<std::string> tempValues;
				for (const auto & level: rowNames)
					tempValues.push_back(defValue);
				options->add("values",	new OptionTerm(tempValues));

				allOptions.push_back(options);
			}
		}

		returnThis->setValue(allOptions);
	}

	return returnThis;
}

void ListModelCustomContrasts::initValues(OptionsTable * bindHere)
{
	_colNames.clear();
	_rowNames.clear();
	_values.clear();
	_variables.clear();

	_boundTo = bindHere;

	std::vector<Options *>	options = bindHere->value();

	OptionVariables		* optionLevels = nullptr;

	for (Options * newRow : options)
	{
		OptionBoolean	*	optionContrast	= static_cast<OptionBoolean		*>(newRow->get("isContrast"));
		OptionString	*	optionName		= static_cast<OptionString		*>(newRow->get("name"));
							optionLevels	= static_cast<OptionVariables	*>(newRow->get("levels")); // why not store it once?
		OptionTerm		*	optionValues	= static_cast<OptionTerm		*>(newRow->get("values"));

		QString colName = QString::fromStdString(optionName->value());
		_colNames.push_back(colName);
		if (!optionContrast->value())
			_variables.push_back(colName);

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
		addControlError(tr("Number of rows specifed in Options for ListModelMarginalMeansContrasts does not match number of rows in values!"));


	beginResetModel();

	_columnCount = _colNames.size();
	_rowCount = _rowNames.size();

	for(auto & col : _values)
		if(_rowNames.size() < col.size())
		{
			Log::log() << "Too many rows in a column of OptionsTable for ListModelMarginalMeansContrasts! Shrinking column to fit." << std::endl;
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

void ListModelCustomContrasts::modelChangedSlot() // Should move this to listmodeltableviewbase as well probably? And also connect columnCount and colNames etc
{
	if (_boundTo)
	{
		std::vector<std::string> stdlevels;
		for (const QString& rowName : _rowNames)
			stdlevels.push_back(rowName.toStdString());

		std::vector<Options*> allOptions;

		for (int colIndex = 0; colIndex < _colNames.size(); colIndex++)
		{
			Options* options =			new Options();
			options->add("name",		new OptionString(_colNames[colIndex].toStdString()));
			options->add("levels",		new OptionVariables(stdlevels));
			options->add("isContrast",	new OptionBoolean(colIndex >= _variables.length()));

			std::vector<std::string> tempValues;
			for (QVariant val : _values[colIndex].toStdVector())
				tempValues.push_back(val.toString().toStdString());
			options->add("values",	new OptionTerm(tempValues));

			allOptions.push_back(options);

		}

		_boundTo->setValue(allOptions);
	}
}

void ListModelCustomContrasts::labelChanged(QString columnName, QString originalLabel, QString newLabel)
{
	bool isChanged = _labelChanged(columnName, originalLabel, newLabel);

	if (isChanged)
		emit modelChanged();
}

bool ListModelCustomContrasts::_labelChanged(const QString& columnName, const QString& originalLabel, const QString& newLabel)
{
	bool isChanged = false;
	int col = _variables.indexOf(columnName);

	if (col >= 0 && col < _values.length())
	{
		for (int row = 0; row < _values[col].length(); row++)
		{
			if (_values[col][row].toString() == originalLabel)
			{
				_values[col][row] = newLabel;
				isChanged = true;
			}
		}
	}

	return isChanged;
}

void ListModelCustomContrasts::_setFactorsSource(ListModelRepeatedMeasuresFactors *factorsSourceModel)
{
	_factorsSourceModel = factorsSourceModel;

	_setFactors();
}

void ListModelCustomContrasts::_setFactors()
{
	_factors.clear();

	if (_factorsSourceModel)
	{
		std::vector<std::pair<std::string, std::vector<std::string> > > factors = _factorsSourceModel->getFactors();
		for (const auto& factor : factors)
		{
			QList<QString> levels;
			for (const std::string& level : factor.second)
				levels.push_back(QString::fromStdString(level));
			_factors[QString::fromStdString(factor.first)] = levels;
		}
	}

}

void ListModelCustomContrasts::_loadColumnInfo()
{
	setColName(	_tableView->getItemProperty("colName").toString());
}

void ListModelCustomContrasts::labelsReordered(QString )
{
	_resetValuesEtc();
}

void ListModelCustomContrasts::scaleFactorChanged()
{
	double oldScaleFactor = _scaleFactor;
	_scaleFactor = listView()->getItemProperty("scaleFactor").toDouble();

	QVector<QString> scaleVariables;
	for (const QString& variable : _variables)
	{
		if (DataSetPackage::pkg()->getColumnType(fq(variable)) == columnType::scale)
			scaleVariables.push_back(variable);
	}

	if (scaleVariables.length() > 0)
	{
		if (oldScaleFactor == 0 || _scaleFactor == 0) // this will decrease or increase the number of rows
			_resetValuesEtc();
		else
		{
			beginResetModel();
			for (const QString& scaleVariable : scaleVariables)
			{
				_labelChanged(scaleVariable, QString::number(-oldScaleFactor), QString::number(-_scaleFactor));
				_labelChanged(scaleVariable, QString::number(oldScaleFactor), QString::number(_scaleFactor));
			}
			endResetModel();

			emit modelChanged();
		}
	}

}

void ListModelCustomContrasts::setColName(QString colName)
{
	if (_colName == colName)
		return;

	_colName = colName;
	emit colNameChanged(_colName);

	_resetValuesEtc();
}

void ListModelCustomContrasts::dataSetChangedHandler()
{
	_resetValuesEtc();
}

void ListModelCustomContrasts::factorsSourceChanged()
{
	_setFactors();
	_resetValuesEtc();
}
