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
#include "analysisform.h"
#include "jaspcontrol.h"
#include "models/listmodel.h"
#include "models/listmodelassignedinterface.h"
#include "models/columntypesmodel.h"
#include "log.h"
#include "rowcontrols.h"
#include "sourceitem.h"
#include "jasptheme.h"
#include "utilities/desktopcommunicator.h"
#include "preferencesmodelbase.h"

#include <QQmlContext>


JASPListControl::JASPListControl(QQuickItem *parent)
	: JASPControl(parent)
{
	_hasUserInteractiveValue = false;
	_allowedTypesModel		= new ColumnTypesModel(this);

	connect(VariableInfo::info(),	&VariableInfo::dataSetChanged,		this,	&JASPListControl::levelsChanged);

}

void JASPListControl::setUpModel()
{
	if (model() && form())	form()->addModel(model());

	emit modelChanged();
}

void JASPListControl::_setupSources()
{
	for (SourceItem* sourceItem : _sourceItems)
	{
		if (sourceItem->sourceListModel())
		{
			JASPListControl* sourceControl = sourceItem->sourceListModel()->listView();
			disconnect(sourceControl, &JASPListControl::containsVariablesChanged,		this, &JASPListControl::setContainsVariables);
			disconnect(sourceControl, &JASPListControl::containsInteractionsChanged,	this, &JASPListControl::setContainsInteractions);
		}
		delete sourceItem;
	}
	_sourceItems.clear();

	_sourceItems = SourceItem::readAllSources(this);

	for (SourceItem* sourceItem : _sourceItems)
	{
		if (sourceItem->sourceListModel())
		{
			JASPListControl* sourceControl = sourceItem->sourceListModel()->listView();
			connect(sourceControl, &JASPListControl::containsVariablesChanged,		this, &JASPListControl::setContainsVariables);
			connect(sourceControl, &JASPListControl::containsInteractionsChanged,	this, &JASPListControl::setContainsInteractions);
		}
	}

	setContainsVariables();
	setContainsInteractions();
}

void JASPListControl::setContainsVariables()
{
	bool containsVariables = _containsVariables;

	ListModelAssignedInterface* assignedModel = qobject_cast<ListModelAssignedInterface*>(model());
	if (assignedModel && assignedModel->availableModel())
		containsVariables = assignedModel->availableModel()->listView()->containsVariables();

	if (!containsVariables)
	{
		for (SourceItem* sourceItem : _sourceItems)
		{
			if (sourceItem->isAnalysisDataSet())	containsVariables = true;
			else if (sourceItem->sourceListModel())
			{
				if (sourceItem->sourceListModel()->listView()->containsVariables() && sourceItem->rowControlName().isEmpty() && !sourceItem->sourceFilter().contains("levels"))
					containsVariables = true;
			}
		}
	}

	if (_containsVariables != containsVariables)
	{
		_containsVariables = containsVariables;
		emit containsVariablesChanged();
	}
}

void JASPListControl::setContainsInteractions()
{
	bool containsInteractions = false;

	if (_termsAreInteractions)
		containsInteractions = true;

	if (!containsInteractions)
	{
		ListModelAssignedInterface* assignedModel = qobject_cast<ListModelAssignedInterface*>(model());
		if (assignedModel && assignedModel->availableModel())
			containsInteractions = assignedModel->availableModel()->listView()->containsInteractions();
	}

	if (!containsInteractions)
	{
		for (SourceItem* sourceItem : _sourceItems)
		{
			if (sourceItem->sourceListModel())
			{
				JASPListControl* sourceControl = sourceItem->sourceListModel()->listView();
				if (sourceControl->containsInteractions() || sourceItem->generateInteractions())
					containsInteractions = true;
			}
		}
	}

	if (_containsInteractions != containsInteractions)
	{
		_containsInteractions = containsInteractions;
		emit containsInteractionsChanged();
	}
}

void JASPListControl::termsChangedHandler()
{
	if (checkLevelsConstraints())
	{
		setColumnsTypes(model()->getUsedTypes());
		setColumnsNames(model()->terms().asQList());
	}
}

void JASPListControl::_termsChangedHandler() 
{
	termsChangedHandler();
	
	if (containsVariables() && isBound() && model())
		emit usedVariablesChanged();
}

void JASPListControl::setUp()
{
	if (!model())	setUpModel();
	JASPControl::setUp();

	ListModel* listModel = model();
	if (!listModel)	return;

	listModel->setRowComponent(rowComponent());
	_setupSources();

	_setAllowedVariables();

	connect(this,								&JASPListControl::sourceChanged,				this,	&JASPListControl::sourceChangedHandler		);
	connect(listModel,							&ListModel::termsChanged,						this,	&JASPListControl::_termsChangedHandler		);
	connect(listModel,							&ListModel::termsChanged,						this,	[this]() { emit countChanged(); }			);
	connect(listModel,							&ListModel::termsChanged,						this,	&JASPListControl::maxTermsWidthChanged		);
	connect(DesktopCommunicator::singleton(),	&DesktopCommunicator::uiScaleChanged,			this,	&JASPListControl::maxTermsWidthChanged		);
	connect(DesktopCommunicator::singleton(),	&DesktopCommunicator::interfaceFontChanged,		this,	&JASPListControl::maxTermsWidthChanged		);
	connect(this,								&JASPListControl::maxLevelsChanged,				this,	&JASPListControl::checkLevelsConstraints	);
	connect(this,								&JASPListControl::minLevelsChanged,				this,	&JASPListControl::checkLevelsConstraints	);
	connect(this,								&JASPListControl::maxNumericLevelsChanged,		this,	&JASPListControl::checkLevelsConstraints	);
	connect(this,								&JASPListControl::minNumericLevelsChanged,		this,	&JASPListControl::checkLevelsConstraints	);
	connect(DesktopCommunicator::singleton(),	&DesktopCommunicator::currentJaspThemeChanged,	this,	&JASPListControl::_setAllowedVariables		);
	connect(this,								&JASPListControl::allowedColumnsChanged,		this,	&JASPListControl::_setAllowedVariables		);
	connect(listModel,							&ListModelDraggable::termsChanged,				this,	&JASPListControl::levelsChanged				);
	connect(listModel,							&ListModelDraggable::filterChanged,				this,	&JASPListControl::levelsChanged				);
	connect(listModel,							&ListModelDraggable::filterChanged,				this,	&JASPListControl::checkLevelsConstraints, Qt::QueuedConnection	);
}

void JASPListControl::cleanUp()
{
	try
	{
		ListModel* _model = model();

		if (_model)
		{
			_model->disconnect();
			for (RowControls* rowControls : _model->getAllRowControls().values())
				for (JASPControl* control : rowControls->getJASPControlsMap().values())
					control->cleanUp();
		}

		for (auto source : _sourceItems)
			source->disconnectModels();

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

bool JASPListControl::hasNativeSource() const
{
	return _sourceItems.size() == 1 && _sourceItems[0]->isNativeModel();
}

bool JASPListControl::addRowControl(const QString &key, JASPControl *control)
{
	return model() ? model()->addRowControl(key, control) : false;
}

bool JASPListControl::hasRowComponent() const
{
	return rowComponent() != nullptr;
}

JASPControl *JASPListControl::getChildControl(const QString & key, const QString & name)
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

columnType JASPListControl::getVariableType(const QString &name)
{
	return model()->getVariableType(name);
}

int JASPListControl::count()
{
	return model() ? model()->rowCount() : 0;
}

double JASPListControl::maxTermsWidth()
{
	if (!model()) return 0;
	double maxWidth = 0;

	QFontMetricsF& metrics = JaspTheme::fontMetrics();
	for (const Term& term : model()->terms())
		maxWidth = std::max(maxWidth, metrics.horizontalAdvance(term.asQString()));

	return maxWidth;
}

std::vector<std::string> JASPListControl::usedVariables() const
{
	if (containsVariables() && isBound() && model())	return model()->terms().asVector();
	else												return {};
}

void JASPListControl::sourceChangedHandler()
{
	if (!model())	return;

	_setupSources();
	model()->sourceTermsReset();
}

void JASPListControl::_setInitialized(const Json::Value &value)
{
	if (model() && hasSource()) model()->sourceTermsReset();

	JASPControl::_setInitialized(value);
}

QAbstractListModel *JASPListControl::allowedTypesModel()
{
	return _allowedTypesModel;
}

bool JASPListControl::isTypeAllowed(columnType type) const
{
	return _allowedTypesModel->hasType(type);
}

columnType JASPListControl::defaultType() const
{
	return _allowedTypesModel->defaultType();
}

bool JASPListControl::hasMandatoryType() const
{
	return _allowedTypesModel->hasMandatoryType();
}


bool JASPListControl::_checkLevelsConstraintsForVariable(const QString& variable)
{
	if (variable.isEmpty())
		return true;

	columnType	type	= (columnType)model()->requestInfo(VariableInfo::VariableType, variable).toInt();
	if (type == columnType::unknown)
		return true;

	int nbLevels			= model()->requestInfo(VariableInfo::TotalLevels, variable).toInt(),
		nbNumValues			= model()->requestInfo(VariableInfo::TotalNumericValues, variable).toInt(),
		maxScaleLevels		= PreferencesModelBase::preferences()->maxScaleLevels();
	bool noScaleAllowed		= !_allowedTypesModel->hasType(columnType::scale);

	if (_minLevels >= 0 && nbLevels < _minLevels)
	{
		addControlErrorPermanent(tr("Minimum number of levels is %1. Variable %2 has only %3 levels").arg(_minLevels).arg(variable).arg(nbLevels));
		return false;
	}
	else if (_maxLevels >= 0 && nbLevels > _maxLevels)
	{
		addControlErrorPermanent(tr("Maximum number of levels is %1. Variable %2 has %3 levels.").arg(_maxLevels).arg(variable).arg(nbLevels));
		return false;
	}
	else if (_maxLevels < 0 && noScaleAllowed && type == columnType::scale && nbLevels > maxScaleLevels)
	{
		// This is the case when a scale variable is transformed into a nominal or ordinal, and the variable has more than the default maximum number of levels
		// This should not be checked if maxLevels is explicitly set (that is if _maxLevels >= 0)
		addControlErrorPermanent(tr("Attempt to transform scale variable %1 into a %2 variable, but its number of levels %3 exceeds the maximum %4. If you still want to use this variable, either change its type, or change 'Maximum allowed levels for scale' in Preferences / Data menu")
								 .arg(variable).arg(columnTypeToQString(_allowedTypesModel->defaultType())).arg(nbLevels).arg(maxScaleLevels));
		return false;
	}
	else if (_minNumericLevels >= 0 && nbNumValues < _minNumericLevels)
	{
		addControlErrorPermanent(tr("Minimum number of numeric values is %1. Variable %2 has only %3 different numeric values").arg(_minNumericLevels).arg(variable).arg(nbNumValues));
		return false;
	}
	else if (_maxNumericLevels >= 0 && nbNumValues > _maxNumericLevels)
	{
		addControlErrorPermanent(tr("Maximum number of numeric values is %1. Variable %2 has %3 different numeric values").arg(_maxNumericLevels).arg(variable).arg(nbNumValues));
		return false;
	}

	return true;
}

bool JASPListControl::_checkLevelsConstraints()
{
	bool checked = true;

	for (const Term& term : model()->terms())
	{
		if (!_checkLevelsConstraintsForVariable(term.asQString()))
		{
			checked = false;
			break;
		}
	}

	return checked;
}

bool JASPListControl::checkLevelsConstraints()
{
	bool checked			= true,
		 noScaleAllowed		= !_allowedTypesModel->hasType(columnType::scale);

	if (_minLevels >= 0 || _maxLevels >= 0 || _minNumericLevels >= 0 || _maxNumericLevels >= 0 || noScaleAllowed)
		checked = _checkLevelsConstraints();

	if (checked)
		clearControlError();

	return checked;
}

QStringList JASPListControl::levels() const
{
	return model() ? model()->allLevels(model()->terms()) : QStringList();
}

QStringList JASPListControl::allowedColumnsIcons() const
{
	return _allowedTypesModel->iconList();
}

void JASPListControl::_setAllowedVariables()
{
	columnTypeVec allowedTypes;

	for (const QString& typeStr: allowedColumns())
	{
		columnType typeCol = columnTypeFromString(fq(typeStr), columnType::unknown);

		if (typeCol != columnType::unknown)
			allowedTypes.push_back(typeCol);
	}

	_allowedTypesModel->setTypes(allowedTypes);

	emit allowedColumnsIconsChanged();

	if (form() && form()->initialized())
		// If the allowed columns have changed, then refresh the model so that columns that are not allowed anymore are removed.
		model()->refresh();
}


