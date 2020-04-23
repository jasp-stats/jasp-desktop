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
#include "analysis/jaspcontrolbase.h"
#include "boundqmltableview.h"
#include "listmodeljagsdatainput.h"
#include "../analysis/analysisform.h"
#include "listmodelfiltereddataentry.h"
#include "listmodelmultinomialchi2test.h"
#include "listmodelanovacustomcontrasts.h"
#include "listmodelrepeatedmeasuresfactors.h"
#include "listmodelmarginalmeanscontrasts.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optionvariables.h"
#include "analysis/options/optiondoublearray.h"


BoundQMLTableView::BoundQMLTableView(JASPControlBase* item)
	: JASPControlWrapper(item)
	, QMLListView(item)
	, BoundQMLItem()
{
	QString modelType	= getItemProperty("modelType").toString(),
			tableType	= getItemProperty("tableType").toString(),
			itemType	= getItemProperty("itemType").toString();

	if (modelType == "MultinomialChi2Model")	_tableModel	= new ListModelMultinomialChi2Test(	this, tableType	);
	if (modelType == "JAGSDataInputModel")		_tableModel	= new ListModelJAGSDataInput(		this, tableType	);
	if (modelType == "FilteredDataEntryModel")	_tableModel = new ListModelFilteredDataEntry(	this, tableType	);
	if (modelType == "CustomContrasts")			_tableModel = new ListModelANOVACustomContrasts(this			);
	if (modelType == "MarginalMeansContrasts")	_tableModel = new ListModelMarginalMeansContrasts(this, tableType);

	if(!_tableModel) addControlError(tr("No model specified for TableView!"));
	else			_tableModel->setItemType(itemType);

	QQuickItem::connect(item, SIGNAL(addColumn()),						this, SLOT(addColumnSlot()));
	QQuickItem::connect(item, SIGNAL(removeColumn(int)),				this, SLOT(removeColumnSlot(int)));
	QQuickItem::connect(item, SIGNAL(addRow()),							this, SLOT(addRowSlot()));
	QQuickItem::connect(item, SIGNAL(removeRow(int)),					this, SLOT(removeRowSlot(int)));
	QQuickItem::connect(item, SIGNAL(reset()),							this, SLOT(resetSlot()));
	QQuickItem::connect(item, SIGNAL(itemChanged(int, int, QString, QString)),	this, SLOT(itemChangedSlot(int, int, QString, QString)));

	connect(_tableModel, &ListModelTableViewBase::columnCountChanged,	[&](){ setItemProperty("columnCount",	_tableModel->colNames().size()); }); //Possibly the best way to connect the signals of the listmodel to the slots of the qml item?
	connect(_tableModel, &ListModelTableViewBase::rowCountChanged,		[&](){ setItemProperty("rowCount",		_tableModel->rowNames().size()); });

	int		initialColumnCount	= getItemProperty("initialColumnCount").toInt(),
			initialRowCount		= getItemProperty("initialRowCount").toInt();

	if(initialColumnCount > 0 && _tableModel)
		_tableModel->setInitialColumnCount(initialColumnCount);

	if(initialRowCount > 0 && _tableModel)
		_tableModel->setInitialRowCount(initialRowCount);

}

void BoundQMLTableView::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable *>(option);

	if (_boundTo != nullptr && _tableModel)		_tableModel->initValues(_boundTo);
	else										Log::log()  << "could not bind to OptionTable in boundqmltableview.cpp" << std::endl;
}


Option *BoundQMLTableView::createOption()
{
	return !_tableModel ? nullptr : _tableModel->createOption();
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

	// form is not always known in the constructor, so all references to form (and dataset) must be done here
	connect(form(),		&AnalysisForm::refreshTableViewModels,			this, &BoundQMLTableView::refreshMe	);
	QString modelType	= getItemProperty("modelType").toString();
	if(modelType == "CustomContrasts")
	{
		ListModelANOVACustomContrasts* customContrastsModel = dynamic_cast<ListModelANOVACustomContrasts*>(_tableModel);
		connect(form(), &AnalysisForm::dataSetChanged, customContrastsModel, &ListModelANOVACustomContrasts::dataSetChangedHandler,	Qt::QueuedConnection	);
		QString factorsSourceName = getItemProperty("factorsSource").toString();
		if (!factorsSourceName.isEmpty())
		{
			ListModelRepeatedMeasuresFactors* factorsSourceModel = dynamic_cast<ListModelRepeatedMeasuresFactors*>(form()->getModel(factorsSourceName));
			customContrastsModel->setFactorsSource(factorsSourceModel);
			connect(factorsSourceModel, &ListModelRepeatedMeasuresFactors::modelChanged, customContrastsModel, &ListModelANOVACustomContrasts::factorsSourceChanged);
		}
		customContrastsModel->loadColumnInfo();
	}
}

void BoundQMLTableView::addColumnSlot()
{
	if (_tableModel)
		_tableModel->addColumn();
}

void BoundQMLTableView::removeColumnSlot(int col)
{
	if (_tableModel)
		_tableModel->removeColumn(col);
}

void BoundQMLTableView::addRowSlot()
{
	if (_tableModel)
		_tableModel->addRow();
}

void BoundQMLTableView::removeRowSlot(int row)
{
	if (_tableModel)
		_tableModel->removeRow(row);
}

void BoundQMLTableView::resetSlot()
{
	if (_tableModel)
		_tableModel->reset();
}

void BoundQMLTableView::itemChangedSlot(int col, int row, QString value, QString type)
{
	if (_tableModel)
	{
		if (_tableModel->valueOk(value))	_tableModel->itemChanged(col, row, value, type);
		else								QTimer::singleShot(0, _tableModel, &ListModelTableViewBase::refreshModel);
	}
}

void BoundQMLTableView::rScriptDoneHandler(const QString & result)
{
	if(_tableModel)
		_tableModel->rScriptDoneHandler(result);
}

void BoundQMLTableView::refreshMe()
{
	if(_tableModel)
		_tableModel->refreshModel();
}
