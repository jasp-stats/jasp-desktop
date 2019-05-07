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

#include "boundqmltableview.h"

#include "../analysis/analysisform.h"
#include "analysis/options/optiondoublearray.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optionvariables.h"
#include <QQmlProperty>
#include <QQuickItem>
#include <QTimer>
#include "log.h"

BoundQMLTableView::BoundQMLTableView(QQuickItem* item, AnalysisForm* form)
	: QMLItem(item, form)
	, QMLListView(item, form)
	, BoundQMLItem()
{
	_boundTo = nullptr;
	QString modelType = _item->property("modelType").toString();
	QString tableType = _item->property("tableType").toString();

	if (modelType == "MultinomialChi2Model")
		_tableModel	= _multinomialChi2TestModel = new ListModelMultinomialChi2Test(this, tableType);

	QQuickItem::connect(item, SIGNAL(addColumn()), this, SLOT(addColumnSlot()));
	QQuickItem::connect(item, SIGNAL(removeColumn(int)), this, SLOT(removeColumnSlot(int)));
	QQuickItem::connect(item, SIGNAL(reset()), this, SLOT(resetSlot()));
	QQuickItem::connect(item, SIGNAL(itemChanged(int, int, QString)), this, SLOT(itemChangedSlot(int, int, QString)));

	connect(_tableModel, &ListModel::modelChanged, this, &BoundQMLTableView::modelChangedSlot);
}

void BoundQMLTableView::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable *>(option);

	if (_boundTo != nullptr)
	{
		std::vector<Options *> _groups = _boundTo->value();
		std::vector<std::vector<double> > values;
		std::vector<std::string> levels;
		std::vector<std::string> colNames;

		for (std::vector<Options *>::iterator it = _groups.begin(); it != _groups.end(); ++it) {

			Options *newRow = static_cast<Options *>(*it);
			OptionString *optionName = static_cast<OptionString *>(newRow->get("name"));
			OptionVariables *optionLevels = static_cast<OptionVariables *>(newRow->get("levels"));
			OptionDoubleArray *optionValues = static_cast<OptionDoubleArray *>(newRow->get("values"));
			colNames.push_back(optionName->value());
			levels = optionLevels->variables();
			values.push_back(optionValues->value());
		}

		if (_multinomialChi2TestModel)
			_multinomialChi2TestModel->initValues(colNames, levels, values);

		_item->setProperty("columnCount", (int)(colNames.size()));
		_item->setProperty("rowCount", (int)(levels.size()));

	}
	else
		Log::log()  << "could not bind to OptionBoolean in BoundQuickCheckBox.cpp" << std::endl;
}


Option *BoundQMLTableView::createOption()
{
	Options* templote = new Options();
	templote->add("name", new OptionString());
	templote->add("levels", new OptionVariables());
	templote->add("values", new OptionDoubleArray());
	return new OptionsTable(templote);
}

bool BoundQMLTableView::isOptionValid(Option *option)
{
	return dynamic_cast<OptionsTable*>(option) != nullptr;
}

bool BoundQMLTableView::isJsonValid(const Json::Value &optionValue)
{
	return optionValue.type() == Json::arrayValue;
}

void BoundQMLTableView::setUp()
{
	QMLListView::setUp();
}

void BoundQMLTableView::addColumnSlot()
{
	if (_multinomialChi2TestModel)
		_multinomialChi2TestModel->addColumn();
}

void BoundQMLTableView::removeColumnSlot(int col)
{
	if (_multinomialChi2TestModel)
		_multinomialChi2TestModel->removeColumn(col);
}

void BoundQMLTableView::resetSlot()
{
	if (_multinomialChi2TestModel)
		_multinomialChi2TestModel->reset();
}

void BoundQMLTableView::itemChangedSlot(int col, int row, QString value)
{
	if (_multinomialChi2TestModel)
	{
		bool ok = false;
		double val = value.toDouble(&ok);
		if (ok)
			_multinomialChi2TestModel->itemChanged(col, row, val);
		else
			QTimer::singleShot(0, _multinomialChi2TestModel, SLOT(refreshModel()));
	}
}

void BoundQMLTableView::modelChangedSlot()
{
	if (_multinomialChi2TestModel)
	{
		const QVector<QVector<double> >& allValues = _multinomialChi2TestModel->values();
		const QVector<QString>& colNames = _multinomialChi2TestModel->colNames();
		const QVector<QString>& rowNames = _multinomialChi2TestModel->rowNames();

		std::vector<std::string> stdlevels;
		for (const QString& rowName : rowNames)
			stdlevels.push_back(rowName.toStdString());

		std::vector<Options*> allOptions;
		int colIndex = 0;
		for (const QString& colName : colNames)
		{
			Options* options = new Options();
			options->add("name", new OptionString(colName.toStdString()));
			OptionVariables* levels = new OptionVariables();
			levels->setValue(stdlevels);
			options->add("levels", levels);
			OptionDoubleArray* values = new OptionDoubleArray();
			values->setValue(allValues[colIndex].toStdVector());
			options->add("values", values);

			allOptions.push_back(options);
			colIndex++;
		}

		_boundTo->setValue(allOptions);

		_item->setProperty("columnCount", colNames.length());
		_item->setProperty("rowCount", rowNames.length());
	}
}
