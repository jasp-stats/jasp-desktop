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
#include "r_functionwhitelist.h"
#include "tableviewbase.h"
#include "listmodelrepeatedmeasuresfactors.h"
#include "data/columnsmodel.h"

ListModelCustomContrasts::ListModelCustomContrasts(TableViewBase *parent) : ListModelTableViewBase(parent)
{
	_keepRowsOnReset = false;

	_tableTerms.colNames.push_back(getDefaultColName(0));
	_tableTerms.values.push_back({});
	_loadColumnInfo();

	_needsSource = _colName.isEmpty();

	parent->setProperty("parseDefaultValue", false);

	connect(this, &ListModelCustomContrasts::variableCountChanged,		_tableView, &TableViewBase::variableCountChanged);
	connect(listView(), SIGNAL(scaleFactorChanged()),					this,		SLOT(scaleFactorChanged()));
	connect(ColumnsModel::singleton(), &ColumnsModel::labelsChanged,	this,		&ListModelCustomContrasts::sourceLabelsChanged);
	connect(ColumnsModel::singleton(), &ColumnsModel::labelsReordered,	this,		&ListModelCustomContrasts::sourceLabelsReordered);
	connect(ColumnsModel::singleton(), &ColumnsModel::columnsChanged,	this,		&ListModelCustomContrasts::sourceColumnsChanged);
}

void ListModelCustomContrasts::sourceTermsReset()
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

void ListModelCustomContrasts::getVariablesAndLabels(QStringList& variables, QVector<QVector<QVariant> >& allLabels)
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
			columnType colType = columnType(requestInfo(newVariable, VariableInfo::VariableType).toInt());
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
				labels = requestInfo(newVariable, VariableInfo::Labels).toStringList();
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

	getVariablesAndLabels(newVariables, newValues);

	beginResetModel();

	int nbContrast = int(columnCount()) - _tableTerms.variables.size();

	// Maps the new variables with the old ones (if they existed)
	QMap<int, int> variablesMap;
	for (int i = 0; i < newVariables.length(); i++)
		variablesMap[i] = _tableTerms.variables.indexOf(newVariables.at(i));

	int newMaxRows = newValues.length() > 0 ? newValues[0].length() : 0;

	// Make a mapping between the new rows and the old ones
	QMap<int, int> rowMapping;

	for (int row = 0; row < newMaxRows; row++)
	{
		// For this, for each row, we first build a boolean matrix that tells where the labels in the new values are found in the old values.
		QVector<QVector<bool> > allBools;

		for (int col = 0; col < newVariables.length(); col++)
		{
			if (variablesMap[col] >= 0 && variablesMap[col] < _tableTerms.values.length())
			{
				QVector<bool> bools;
				QVariant label = newValues[col][row];
				const QVector<QVariant>& oldValues = _tableTerms.values[variablesMap[col]];
				for (int oldRow = 0; oldRow < oldValues.length(); oldRow++)
					bools.push_back(oldValues.at(oldRow) == label);

				allBools.push_back(bools);
			}
		}

		int bestFitRow = -1;
		int bestFit = -1;

		// From the boolean matrix, find the row where the new labels are found the most.
		for (int oldRow = 0; oldRow < rowCount(); oldRow++)
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
		for (int i = 0; i < _tableView->initialColumnCount(); i++)
		{
			for (int row = 0; row < newMaxRows; row++)
				contrasts.push_back(_tableView->defaultValue());
			newValues.push_back(contrasts);
		}
	}
	else
	{
		// For each contrast, set the value corresponding to the rowMapping
		for (int i = 0; i < nbContrast; i++)
		{
			QVector<QVariant> contrasts;
			int oldContrastIndex = _tableTerms.variables.length() + i;

			if (_tableTerms.values.length() <= oldContrastIndex)
			{
				Log::log() << "ListModelCustomContrasts::sourceTermsChanged: Not the same amount of contrasts!!!" << std::endl;
				continue;
			}

			for (int row = 0; row < newMaxRows; row++)
				contrasts.push_back(rowMapping[row] >= 0 ? _tableTerms.values[oldContrastIndex][rowMapping[row]] : _tableView->defaultValue());

			newValues.push_back(contrasts);
		}
	}

	_tableTerms.clear();

	_tableTerms.variables = newVariables;
	_tableTerms.values = newValues;
	size_t colCount = size_t(_tableTerms.values.length());
	size_t rowCount = _tableTerms.values.length() > 0 ? size_t(_tableTerms.values[0].length()) : 0;

	for (size_t rowNb = 0; rowNb < rowCount; rowNb++)
		_tableTerms.rowNames.push_back(getDefaultRowName(rowNb));

	for (size_t colNb = 0; colNb < colCount; colNb++)
		_tableTerms.colNames.push_back(getDefaultColName(colNb));

	endResetModel();

	emit columnCountChanged();
	emit rowCountChanged();
	emit variableCountChanged();
}


QString ListModelCustomContrasts::getDefaultColName(size_t index) const
{
	int indexi = int(index);

	if (indexi < _tableTerms.variables.size())
		return _tableTerms.variables.at(indexi);
	else
		return tr("Contrast %1").arg(indexi - _tableTerms.variables.size() + 1);
}

void ListModelCustomContrasts::reset()
{
	if (_tableTerms.values.length() <= _tableTerms.variables.length() + _tableView->initialColumnCount())
		return;

	beginResetModel();

	_tableTerms.values.erase(_tableTerms.values.begin() + _tableTerms.variables.length() + _tableView->initialColumnCount(), _tableTerms.values.end());
	_tableTerms.colNames.erase(_tableTerms.colNames.begin() + _tableTerms.values.length(), _tableTerms.colNames.end());

	endResetModel();

	emit columnCountChanged();
}

void ListModelCustomContrasts::setup()
{
	QString factorsSourceName = _tableView->property("factorsSource").toString();
	if (!factorsSourceName.isEmpty())
	{
		ListModelRepeatedMeasuresFactors* factorsSourceModel = dynamic_cast<ListModelRepeatedMeasuresFactors*>(_tableView->form()->getModel(factorsSourceName));
		if (factorsSourceModel)
		{
			_setFactorsSource(factorsSourceModel);
			connect(factorsSourceModel, &ListModelRepeatedMeasuresFactors::termsChanged, this, &ListModelCustomContrasts::factorsSourceChanged);
		}
	}
}

QString ListModelCustomContrasts::getItemInputType(const QModelIndex &index) const
{
	if (index.column() >= _tableTerms.variables.length())
	{
		if (_tableView->itemType() == JASPControl::ItemType::Double)	return "double";
		else															return "formula";
	}
	else																return "string";
}

int ListModelCustomContrasts::getMaximumColumnWidthInCharacters(size_t) const
{
	return 5;
}

bool ListModelCustomContrasts::sourceLabelsChanged(QString columnName, QMap<QString, QString> changedLabels)
{
	bool doRefresh = false;

	if (changedLabels.size() == 0)	_resetValuesEtc();
	else
	{
		QMapIterator<QString, QString> it(changedLabels);
		while (it.hasNext())
		{
			it.next();
			if (_labelChanged(columnName, it.key(), it.value())) doRefresh = true;
		}
	}
	if (doRefresh)	refresh();

	return true;
}

bool ListModelCustomContrasts::_labelChanged(const QString& columnName, const QString& originalLabel, const QString& newLabel)
{
	bool isChanged = false;
	int col = _tableTerms.variables.indexOf(columnName);

	if (col >= 0 && col < _tableTerms.values.length())
	{
		for (int row = 0; row < _tableTerms.values[col].length(); row++)
		{
			if (_tableTerms.values[col][row].toString() == originalLabel)
			{
				_tableTerms.values[col][row] = newLabel;
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
	setColName(	_tableView->property("colName").toString());
}

bool ListModelCustomContrasts::sourceLabelsReordered(QString )
{
	_resetValuesEtc();
	return true;
}

void ListModelCustomContrasts::sourceColumnsChanged(QStringList columns)
{
	bool doReset = false;
	for (const QString& col : columns)
		if (_tableTerms.variables.contains(col)) doReset = true;

	if (doReset) _resetValuesEtc();
}

void ListModelCustomContrasts::scaleFactorChanged()
{
	double oldScaleFactor = _scaleFactor;
	_scaleFactor = listView()->property("scaleFactor").toDouble();

	QVector<QString> scaleVariables;
	for (const QString& variable : _tableTerms.variables)
	{
		if (requestInfo(variable, VariableInfo::VariableType).toInt() == int(columnType::scale))
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

void ListModelCustomContrasts::factorsSourceChanged()
{
	_setFactors();
	_resetValuesEtc();
}
