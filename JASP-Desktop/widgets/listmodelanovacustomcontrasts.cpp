//
// Copyright (C) 2013-2019 University of Amsterdam
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
#include "boundqmltableview.h"
#include "analysis/analysisform.h"
#include "listmodelanovacustomcontrasts.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optionvariables.h"
#include "analysis/options/optiondoublearray.h"
#include "analysis/jaspcontrolbase.h"
#include "listmodelrepeatedmeasuresfactors.h"

ListModelANOVACustomContrasts::ListModelANOVACustomContrasts(BoundQMLTableView * parent) : ListModelTableViewBase(parent)
{
	_defaultCellVal		= 0;
	_initialRowCnt		= 1;
	_keepColsOnReset	= true;
	QQuickItem::connect(_tableView->item(), SIGNAL(colNameSignal(QString)),	this, SLOT(setColName(QString))	);
}

void ListModelANOVACustomContrasts::loadColumnInfo()
{
	setColName(	_tableView->getItemProperty("colName").toString());
}

void ListModelANOVACustomContrasts::factorsSourceChanged()
{
	setFactors();
	setColLabels();
}

void ListModelANOVACustomContrasts::reset()
{
	QString colName = _colName;

	setColName("");
	setColName(colName);
}

void ListModelANOVACustomContrasts::setup()
{
	connect(_tableView->form(), &AnalysisForm::dataSetChanged, this, &ListModelANOVACustomContrasts::dataSetChangedHandler,	Qt::QueuedConnection	);
	QString factorsSourceName = _tableView->getItemProperty("factorsSource").toString();
	if (!factorsSourceName.isEmpty())
	{
		ListModelRepeatedMeasuresFactors* factorsSourceModel = dynamic_cast<ListModelRepeatedMeasuresFactors*>(_tableView->form()->getModel(factorsSourceName));
		if (factorsSourceModel)
		{
			setFactorsSource(factorsSourceModel);
			connect(factorsSourceModel, &ListModelRepeatedMeasuresFactors::modelChanged, this, &ListModelANOVACustomContrasts::factorsSourceChanged);
		}
	}
	loadColumnInfo();
}

void ListModelANOVACustomContrasts::setFactorsSource(ListModelRepeatedMeasuresFactors *factorsSourceModel)
{
	_factorsSourceModel = factorsSourceModel;

	setFactors();
}

QString ListModelANOVACustomContrasts::getDefaultColName(size_t index) const
{
	if(index >= _colNames.size()) return "?";

	return _colNames[index];
}

void ListModelANOVACustomContrasts::setColName(QString colName)
{
	if (_colName == colName)
		return;

	_colName = colName;
	emit colNameChanged(_colName);

	setColLabels();

	emit modelChanged();
}

void ListModelANOVACustomContrasts::setColLabels()
{
	QVector<QString> colLabels;

	if (!_colName.isEmpty())
	{
		if (_factors.contains(_colName))	colLabels = _factors[_colName].toVector();
		else								colLabels = (_colName == "" ? QStringList() : DataSetPackage::pkg()->getColumnLabelsAsStringList(_colName.toStdString())).toVector();
	}


	_colNames	= colLabels;
	_rowCount	= _initialRowCnt;

	modifyValuesNamesEtcetera(); //Make sure we have all _colNames, _rowNames and that _values is the right size
}

int ListModelANOVACustomContrasts::getMaximumColumnWidthInCharacters(size_t columnIndex) const
{
	return std::max(ListModelTableViewBase::getMaximumColumnWidthInCharacters(columnIndex), getDefaultColName(columnIndex).size());
}


void ListModelANOVACustomContrasts::modelChangedSlot()
{
	if (_boundTo)
	{
		std::vector<Options*> allOptions;

		for (int colIndex = 0; colIndex < _colNames.size(); colIndex++)
		{
			Options* options =			new Options();
			options->add("colName",		new OptionString(_colName.toStdString()));
			options->add("colLabel",	new OptionString(_colNames[colIndex].toStdString()));

			std::vector<double> tempValues;
			for (QVariant val : _values[colIndex].toStdVector())
				tempValues.push_back(val.toDouble());
			options->add("values",	new OptionDoubleArray(tempValues));

			allOptions.push_back(options);
		}

		_boundTo->setValue(allOptions);
	}
}

OptionsTable *ListModelANOVACustomContrasts::createOption()
{
	Options* optsTemplate =			new Options();
	optsTemplate->add("colName",	new OptionString());
	optsTemplate->add("colLabel",	new OptionString());
	optsTemplate->add("values",		new OptionDoubleArray());

	return new OptionsTable(optsTemplate);
}

void ListModelANOVACustomContrasts::modifyValuesNamesEtcetera()
{
	_rowNames.clear();

	for(int i=0; i<_rowCount; i++)
		_rowNames.push_back(getDefaultRowName(i));

	beginResetModel();

	_columnCount = _colNames.size();

	if(_values.size() != _columnCount)
		_values.resize(_columnCount);

	for(auto & col : _values)
		if(_rowCount < col.size())
			col.resize(_rowCount);
		else
			for (int row = col.size(); row < _rowCount; row++)
				col.push_back(_defaultCellVal.toDouble());

	endResetModel();

	emit columnCountChanged();
	emit rowCountChanged();
}

void ListModelANOVACustomContrasts::setFactors()
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

void ListModelANOVACustomContrasts::initValues(OptionsTable * bindHere)
{
	setColName(""); //Also resets _columnCount, _colNames and _colLabels

	_rowNames.clear();
	_values.clear();

	_boundTo = bindHere;

	std::vector<Options *>	options = bindHere->value();

	_rowCount = 0;

	for (Options * newRow : options)
	{
		OptionString		*	optionColName	= static_cast<OptionString		*>(newRow->get("colName"));
		OptionString		*	optionColLabel	= static_cast<OptionString		*>(newRow->get("colLabel")); //Doesn't need to be read, because it is connected to the column name anyway. It's mostly in the options to show the R programmer what's what
		OptionDoubleArray	*	optionValues	= static_cast<OptionDoubleArray	*>(newRow->get("values"));

		if(optionColName->value() != "")
			_colName = tq(optionColName->value());

		_colNames.push_back(tq(optionColLabel->value()));


		_values.push_back({});
		for (double val : optionValues->value())
			_values[_values.size()-1].push_back(_itemType == "integer" ? round(val) : val);

		_rowCount = std::max(_rowCount, size_t(_values[_values.size()-1].size()));
	}

	emit colNameChanged(_colName);
	modifyValuesNamesEtcetera();
	loadColumnInfo(); //Maybe the user has set colName?
}


void ListModelANOVACustomContrasts::dataSetChangedHandler()
{
	setColLabels();
}
