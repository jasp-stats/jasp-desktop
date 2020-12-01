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

#include "jasplistcontrol.h"
#include "../analysis/analysisform.h"
#include "../analysis/jaspcontrol.h"
#include "listmodel.h"
#include "listmodellabelvalueterms.h"
#include "log.h"
#include "rowcontrols.h"
#include "sourceitem.h"

#include <QQmlContext>

const QString JASPListControl::_defaultKey = "_JASPDefaultKey";


JASPListControl::JASPListControl(QQuickItem *parent)
	: JASPControl(parent)
{
}

void JASPListControl::setUpModel()
{
	emit modelChanged();
}

void JASPListControl::setupSources()
{
	for (SourceItem* sourceItem : _sourceItems)
		delete sourceItem;
	_sourceItems.clear();

	_sourceItems = SourceItem::readAllSources(this);

	ListModel* listModel = model();
	bool termsAreVariables = true;
	bool termsAreInteractions = false;

	for (SourceItem* sourceItem : _sourceItems)
	{
		if (sourceItem->model())
		{
			if (!sourceItem->model()->areTermsVariables() || !sourceItem->controlName().isEmpty() || sourceItem->modelUse() == "levels")
				termsAreVariables = false;
			if (sourceItem->model()->areTermsInteractions() || sourceItem->combineWithOtherModels())
				termsAreInteractions = true;
		}
	}

	if (!termsAreVariables)
		listModel->setTermsAreVariables(false); // set it only when it is false
	if (termsAreInteractions)
		listModel->setTermsAreInteractions(true); // set it only when it is true
}

void JASPListControl::addRowComponentsDefaultOptions(Options *options)
{
	if (!hasRowComponent())
		return;

	if (_defaultRowControls)
		delete _defaultRowControls;

	// Create a dummy QML control, so that we can create the right kind of options.
	_defaultRowControls = new RowControls(this->model(), rowComponent(), QMap<QString, Option*>(), true);
	_defaultRowControls->init(0, Term(_defaultKey), true);

	const QMap<QString, JASPControl*>& map = _defaultRowControls->getJASPControlsMap();
	QMapIterator<QString, JASPControl*> it(map);

	while (it.hasNext())
	{
		it.next();
		JASPControl* control = it.value();
		BoundControl* boundItem = dynamic_cast<BoundControl*>(control);
		if (control->isBound() && boundItem)
		{
			// The options might depend on properties set by the setup
			// e.g. setup of BoundQMLListViewTerms sets whether the terms have interactions, which influences the kind of options that will be used.
			control->setUp();
			Option* option = boundItem->createOption();
			std::string optionName = control->name().toStdString();

			if (form() && (optionName == _optionKey.toStdString()))
				form()->addFormError(tr("The list %1 has a rowComponent with the same name (%2) as its optionKey. Change the optionKey property of the list or the control name.").arg(name()).arg(tq(optionName)));
			options->add(optionName, option);
		}
	}
}

void JASPListControl::setUp()
{
	if (!model())	setUpModel();
	JASPControl::setUp();
	_setAllowedVariables();

	ListModel* listModel = model();
	if (!listModel)	return;

	listModel->setRowComponent(rowComponent());
	setupSources();

	connect(this,		&JASPListControl::sourceChanged,	this,	&JASPListControl::sourceChangedHandler);
	connect(listModel,	&ListModel::termsChanged,			this,	&JASPListControl::termsChangedHandler);
	connect(listModel,	&ListModel::modelReset,			[this]() { emit countChanged(); });
}

void JASPListControl::cleanUp()
{
	try
	{
		ListModel* _model = model();
		if (_model)
			_model->disconnect();
		for (SourceItem* sourceItem : _sourceItems)
			delete sourceItem;

		for (RowControls* rowControls : _model->getRowControls().values())
			for (JASPControl* control : rowControls->getJASPControlsMap().values())
				control->cleanUp();

		if (_defaultRowControls)
			for (JASPControl* control : _defaultRowControls->getJASPControlsMap().values())
				control->cleanUp();

		_sourceItems.clear();

		JASPControl::cleanUp();
	}
	catch (...) {}
}

Terms JASPListControl::_getCombinedTerms(SourceItem* sourceToCombine)
{
	Terms result = sourceToCombine->getTerms();
	Terms termsToBeCombinedWith;
	for (SourceItem* sourceItem : _sourceItems)
		if (sourceItem != sourceToCombine)
			termsToBeCombinedWith.add(sourceItem->getTerms());

	Terms termsToCombine = sourceToCombine->getTerms();
	for (const Term& termToCombine : termsToCombine)
	{
		for (const Term& termToBeCombined : termsToBeCombinedWith)
		{
			QStringList components = termToCombine.components();
			components.append(termToBeCombined.components());
			result.add(Term(components));
		}
	}

	return result;
}

void JASPListControl::applyToAllSources(std::function<void(SourceItem *sourceItem, const Terms& terms)> applyThis)
{
	for (SourceItem* sourceItem : _sourceItems)
		applyThis(sourceItem, sourceItem->combineWithOtherModels() ? _getCombinedTerms(sourceItem) : sourceItem->getTerms());
}

bool JASPListControl::addRowControl(const QString &key, JASPControl *control)
{
	bool success = false;

	if (key == _defaultKey)
	{
		if (_defaultRowControls)
			success = _defaultRowControls->addJASPControl(control);
	}
	else if (model())
		success = model()->addRowControl(key, control);

	return success;
}

bool JASPListControl::hasRowComponent() const
{
	return rowComponent() != nullptr;
}

JASPControl *JASPListControl::getChildControl(QString key, QString name)
{
	return getRowControl(key, name);
}

JASPControl *JASPListControl::getRowControl(const QString &key, const QString &name) const
{
	return model() ? model()->getRowControl(key, name) : nullptr;
}

QString JASPListControl::getSourceType(QString name)
{
	return model() ? model()->getItemType(name) : "";
}

int JASPListControl::count()
{
	return model() ? model()->rowCount() : 0;
}

void JASPListControl::sourceChangedHandler()
{
	if (!model())	return;

	setupSources();
	model()->sourceTermsChanged();
}

int JASPListControl::_getAllowedColumnsTypes()
{
	int allowedColumnsTypes = -1;
	
	QStringList allowedColumns = property("allowedColumns").toStringList();
	if (allowedColumns.isEmpty())
	{
		QString allowedColumn = property("allowedColumns").toString();
		if (!allowedColumn.isEmpty())
			allowedColumns.append(allowedColumn);
	}
	if (!allowedColumns.isEmpty())
	{
		allowedColumnsTypes = 0;
		for (QString& allowedColumn: allowedColumns)
		{
			if (allowedColumn == "ordinal")				allowedColumnsTypes |= int(columnType::ordinal);
			else if (allowedColumn == "nominal")		allowedColumnsTypes |= int(columnType::nominal);
			else if (allowedColumn == "nominalText")	allowedColumnsTypes |= int(columnType::nominalText);
			else if (allowedColumn == "scale")			allowedColumnsTypes |= int(columnType::scale);
			else
				addControlError(tr("Wrong column type: %1 for ListView %2").arg(allowedColumn).arg(name()));
		}
	}
	
	return allowedColumnsTypes;
}

void JASPListControl::_setAllowedVariables()
{
	_variableTypesAllowed = 0xff;
	
	int allowedColumnsTypes = _getAllowedColumnsTypes();
	
	if (allowedColumnsTypes >= 0)
		_variableTypesAllowed = allowedColumnsTypes;
}



