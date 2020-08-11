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

#include "jaspcontrolwrapper.h"
#include "../analysis/analysisform.h"
#include <QQmlProperty>
#include "../analysis/jaspcontrolbase.h"
#include "widgets/boundqmlcheckbox.h"
#include "widgets/boundqmlcombobox.h"
#include "widgets/boundqmlslider.h"
#include "widgets/boundqmltextinput.h"
#include "widgets/boundqmltextarea.h"
#include "widgets/boundqmllavaantextarea.h"
#include "widgets/boundqmljagstextarea.h"
#include "widgets/boundqmlradiobuttons.h"
#include "widgets/boundqmllistviewterms.h"
#include "widgets/boundqmllistviewmeasurescells.h"
#include "widgets/boundqmllistviewlayers.h"
#include "widgets/boundqmlinputlist.h"
#include "widgets/boundqmlcomponentslist.h"
#include "widgets/boundqmlrepeatedmeasuresfactors.h"
#include "widgets/boundqmlfactorsform.h"
#include "widgets/boundqmltableview.h"
#include "widgets/qmllistviewtermsavailable.h"
#include "widgets/qmlexpander.h"
#include "log.h"


JASPControlWrapper::JASPControlWrapper(JASPControlBase *item)
	: _item(item)
{
}

JASPControlWrapper* JASPControlWrapper::buildJASPControlWrapper(JASPControlBase* control)
{
	JASPControlWrapper* controlWrapper = nullptr;

	switch(control->controlType())
	{
	case JASPControlBase::ControlType::Switch:			//fallthrough:
	case JASPControlBase::ControlType::CheckBox:					controlWrapper		= new BoundQMLCheckBox(control);					break;
	case JASPControlBase::ControlType::Slider:						controlWrapper		= new BoundQMLSlider(control);						break;
	case JASPControlBase::ControlType::ComboBox:					controlWrapper		= new BoundQMLComboBox(control);					break;
	case JASPControlBase::ControlType::Expander:					controlWrapper		= new QMLExpander(control);							break;
	case JASPControlBase::ControlType::TableView:					controlWrapper		= new BoundQMLTableView(control);					break;
	case JASPControlBase::ControlType::TextField:					controlWrapper		= new BoundQMLTextInput(control);					break;
	case JASPControlBase::ControlType::FactorsForm:					controlWrapper		= new BoundQMLFactorsForm(control);					break;
	case JASPControlBase::ControlType::InputListView:				controlWrapper		= new BoundQMLInputList(control);					break;
	case JASPControlBase::ControlType::TabView:						controlWrapper		= new BoundQMLComponentsList(control);				break;
	case JASPControlBase::ControlType::ComponentsList:				controlWrapper		= new BoundQMLComponentsList(control);				break;
	case JASPControlBase::ControlType::RadioButtonGroup:			controlWrapper		= new BoundQMLRadioButtons(control);				break;
	case JASPControlBase::ControlType::RepeatedMeasuresFactorsList:	controlWrapper		= new BoundQMLRepeatedMeasuresFactors(control);		break;
	case JASPControlBase::ControlType::TextArea:
	{
		QString textType = control->property("textType").toString();
		if		(textType == "lavaan")								controlWrapper		= new BoundQMLLavaanTextArea(control);
		else if (textType == "JAGSmodel")							controlWrapper		= new BoundQMLJAGSTextArea(control);
		else														controlWrapper		= new BoundQMLTextArea(control);
		break;
	}
	case JASPControlBase::ControlType::VariablesListView:
	{
		JASPControlBase::ListViewType	listViewType = JASPControlBase::ListViewType(control->property("listViewType").toInt());

		switch(listViewType)
		{
		case JASPControlBase::ListViewType::AssignedVariables:		controlWrapper = new BoundQMLListViewTerms(control, false);		break;
		case JASPControlBase::ListViewType::Interaction:			controlWrapper = new BoundQMLListViewTerms(control, true);		break;
		case JASPControlBase::ListViewType::RepeatedMeasures:		controlWrapper = new BoundQMLListViewMeasuresCells(control);	break;
		case JASPControlBase::ListViewType::Layers:					controlWrapper = new BoundQMLListViewLayers(control);			break;
		case JASPControlBase::ListViewType::AvailableVariables:		controlWrapper = new QMLListViewTermsAvailable(control, false);	break;
		case JASPControlBase::ListViewType::AvailableInteraction:	controlWrapper = new QMLListViewTermsAvailable(control, true);	break;
		}
		break;
	}
	case JASPControlBase::ControlType::GroupBox:
	case JASPControlBase::ControlType::JASPControl:
	default:
		controlWrapper = new JASPControlWrapper(control);
	}

	return controlWrapper;
}

QString JASPControlWrapper::friendlyName() const
{
	return JASPControlBase::ControlTypeToFriendlyString(item()->controlType());
}

void JASPControlWrapper::cleanUp()
{
	if (_item)
		_item->disconnect();
}

void JASPControlWrapper::resetQMLItem(JASPControlBase *item)
{
	_item = item;
}

const QString& JASPControlWrapper::name() const
{
	return _item->name();
}

bool JASPControlWrapper::isBound() const
{
	return _item->isBound();
}

AnalysisForm *JASPControlWrapper::form() const
{
	return _item->form();
}

void JASPControlWrapper::addControlError(const QString &error)
{
	Log::log() << "JASPControlWrapper::addControlError: " << error << std::endl;
	if (form())
		form()->addFormError(error);
}

bool JASPControlWrapper::addDependency(JASPControlWrapper *item)
{
	if (_depends.count(item) > 0 || this == item)
		return false;
	
	_depends.insert(item);
	return true;
}

void JASPControlWrapper::removeDependency(JASPControlWrapper *item)
{
	_depends.erase(item);
}

void JASPControlWrapper::setItemProperty(const QString& name, const QVariant& value)
{
	if (_item)
		_item->setProperty(name.toStdString().c_str(), value);
}

JASPControlWrapper* JASPControlWrapper::parentListControl()
{
	JASPControlBase* listView = qobject_cast<JASPControlBase*>(_item->parentListView());

	if (listView)	return listView->getWrapper();
	else			return nullptr;
}

QString JASPControlWrapper::parentListControlKey()
{
	return _item->parentListViewKey();
}

QVariant JASPControlWrapper::getItemProperty(const QString &name)
{
	if (_item)
		return _item->property(name.toStdString().c_str());
	else
		return QVariant();
}
