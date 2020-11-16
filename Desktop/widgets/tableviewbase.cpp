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
#include <QTimer>
#include "analysis/jaspcontrol.h"
#include "tableviewbase.h"
#include "listmodeljagsdatainput.h"
#include "../analysis/analysisform.h"
#include "listmodelfiltereddataentry.h"
#include "listmodelmultinomialchi2test.h"
#include "listmodelrepeatedmeasuresfactors.h"
#include "listmodelcustomcontrasts.h"
#include "listmodeltableviewsimple.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optionvariables.h"
#include "analysis/options/optiondoublearray.h"


TableViewBase::TableViewBase(QQuickItem* parent)
	: JASPListControl(parent)
{
	_controlType = ControlType::TableView;
}

void TableViewBase::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable *>(option);

	if (_boundTo != nullptr && _tableModel)		_tableModel->initValues(_boundTo);
	else										Log::log()  << "could not bind to OptionTable in boundqmltableview.cpp" << std::endl;
}

void TableViewBase::setUpModel()
{
	QString modelType	= property("modelType").toString(),
			tableType	= property("tableType").toString(),
			itemType	= property("itemType").toString();

	if (modelType == "MultinomialChi2Model")	_tableModel	= new ListModelMultinomialChi2Test(	this, tableType	);
	if (modelType == "JAGSDataInputModel")		_tableModel	= new ListModelJAGSDataInput(		this, tableType	);
	if (modelType == "FilteredDataEntryModel")	_tableModel = new ListModelFilteredDataEntry(	this, tableType	);
	if (modelType == "CustomContrasts")			_tableModel = new ListModelCustomContrasts(		this, tableType	);
	if (!_tableModel)                            _tableModel = new ListModelTableViewSimple(     this, tableType );
	JASPListControl::setUpModel();

	_tableModel->setItemType(itemType);

	QQuickItem::connect(this, SIGNAL(addColumn()),						this, SLOT(addColumnSlot()));
	QQuickItem::connect(this, SIGNAL(removeColumn(int)),				this, SLOT(removeColumnSlot(int)));
	QQuickItem::connect(this, SIGNAL(addRow()),							this, SLOT(addRowSlot()));
	QQuickItem::connect(this, SIGNAL(removeRow(int)),					this, SLOT(removeRowSlot(int)));
	QQuickItem::connect(this, SIGNAL(reset()),							this, SLOT(resetSlot()));
	QQuickItem::connect(this, SIGNAL(itemChanged(int, int, QString, QString)),	this, SLOT(itemChangedSlot(int, int, QString, QString)));

	connect(_tableModel, &ListModelTableViewBase::columnCountChanged,	[&](){ setProperty("columnCount",	_tableModel->colNames().size()); }); //Possibly the best way to connect the signals of the listmodel to the slots of the qml item?
	connect(_tableModel, &ListModelTableViewBase::rowCountChanged,		[&](){ setProperty("rowCount",		_tableModel->rowNames().size()); });

	int		initialColumnCount	= property("initialColumnCount").toInt(),
			initialRowCount		= property("initialRowCount").toInt();

	if(initialColumnCount > 0 && _tableModel)
		_tableModel->setInitialColumnCount(initialColumnCount);

	if(initialRowCount > 0 && _tableModel)
		_tableModel->setInitialRowCount(initialRowCount);
}


Option *TableViewBase::createOption()
{
	return !_tableModel ? nullptr : _tableModel->createOption();
}

bool TableViewBase::isOptionValid(Option *option)
{
	return dynamic_cast<OptionsTable*>(option) != nullptr;
}

bool TableViewBase::isJsonValid(const Json::Value &optionValue)
{
	return optionValue.type() == Json::arrayValue;
}

void TableViewBase::setUp()
{
	JASPListControl::setUp();

	// form is not always known in the constructor, so all references to form (and dataset) must be done here
	connect(form(),		&AnalysisForm::refreshTableViewModels,			this, &TableViewBase::refreshMe	);
	_tableModel->setup();
}

void TableViewBase::addColumnSlot()
{
	if (_tableModel)
		_tableModel->addColumn();
}

void TableViewBase::removeColumnSlot(int col)
{
	if (_tableModel)
		_tableModel->removeColumn(col);
}

void TableViewBase::addRowSlot()
{
	if (_tableModel)
		_tableModel->addRow();
}

void TableViewBase::removeRowSlot(int row)
{
	if (_tableModel)
		_tableModel->removeRow(row);
}

void TableViewBase::resetSlot()
{
	if (_tableModel)
		_tableModel->reset();
}

void TableViewBase::itemChangedSlot(int col, int row, QString value, QString type)
{
	if (_tableModel)
	{
		if (_tableModel->valueOk(value))	_tableModel->itemChanged(col, row, value, type);
		else								QTimer::singleShot(0, _tableModel, &ListModelTableViewBase::refreshModel);
	}
}

void TableViewBase::rScriptDoneHandler(const QString & result)
{
	if(_tableModel)
		_tableModel->rScriptDoneHandler(result);
}

void TableViewBase::refreshMe()
{
	if(_tableModel)
		_tableModel->refreshModel();
}
