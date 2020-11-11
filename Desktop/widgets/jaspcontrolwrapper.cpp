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
#include "../analysis/jaspcontrol.h"
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


JASPControlWrapper::JASPControlWrapper(JASPControl *item)
	: _item(item)
{
}

JASPControlWrapper* JASPControlWrapper::buildJASPControlWrapper(JASPControl* control)
{
	JASPControlWrapper* controlWrapper = nullptr;

	switch(control->controlType())
	{
	case JASPControl::ControlType::Switch:			//fallthrough:
	case JASPControl::ControlType::CheckBox:					controlWrapper		= new BoundQMLCheckBox(control);					break;
	case JASPControl::ControlType::Slider:						controlWrapper		= new BoundQMLSlider(control);						break;
	case JASPControl::ControlType::ComboBox:					controlWrapper		= new BoundQMLComboBox(control);					break;
	case JASPControl::ControlType::Expander:					controlWrapper		= new QMLExpander(control);							break;
	case JASPControl::ControlType::TableView:					controlWrapper		= new BoundQMLTableView(control);					break;
	case JASPControl::ControlType::TextField:					controlWrapper		= new BoundQMLTextInput(control);					break;
	case JASPControl::ControlType::FactorsForm:					controlWrapper		= new BoundQMLFactorsForm(control);					break;
	case JASPControl::ControlType::InputListView:				controlWrapper		= new BoundQMLInputList(control);					break;
	case JASPControl::ControlType::TabView:						controlWrapper		= new BoundQMLComponentsList(control);				break;
	case JASPControl::ControlType::ComponentsList:				controlWrapper		= new BoundQMLComponentsList(control);				break;
	case JASPControl::ControlType::RadioButtonGroup:			controlWrapper		= new BoundQMLRadioButtons(control);				break;
	case JASPControl::ControlType::RepeatedMeasuresFactorsList:	controlWrapper		= new BoundQMLRepeatedMeasuresFactors(control);		break;
	case JASPControl::ControlType::TextArea:
	{
		QString textType = control->property("textType").toString();
		if		(textType == "lavaan")								controlWrapper		= new BoundQMLLavaanTextArea(control);
		else if (textType == "JAGSmodel")							controlWrapper		= new BoundQMLJAGSTextArea(control);
		else														controlWrapper		= new BoundQMLTextArea(control);
		break;
	}
	case JASPControl::ControlType::VariablesListView:
	{
		JASPControl::ListViewType	listViewType = JASPControl::ListViewType(control->property("listViewType").toInt());

		switch(listViewType)
		{
		case JASPControl::ListViewType::AssignedVariables:		controlWrapper = new BoundQMLListViewTerms(control, false);		break;
		case JASPControl::ListViewType::Interaction:			controlWrapper = new BoundQMLListViewTerms(control, true);		break;
		case JASPControl::ListViewType::RepeatedMeasures:		controlWrapper = new BoundQMLListViewMeasuresCells(control);	break;
		case JASPControl::ListViewType::Layers:					controlWrapper = new BoundQMLListViewLayers(control);			break;
		case JASPControl::ListViewType::AvailableVariables:		controlWrapper = new QMLListViewTermsAvailable(control, false);	break;
		case JASPControl::ListViewType::AvailableInteraction:	controlWrapper = new QMLListViewTermsAvailable(control, true);	break;
		}
		break;
	}
	case JASPControl::ControlType::GroupBox:
	case JASPControl::ControlType::DefaultControl:
	default:
		controlWrapper = new JASPControlWrapper(control);
	}

	return controlWrapper;
}

QString JASPControlWrapper::friendlyName() const
{
	return JASPControl::ControlTypeToFriendlyString(item()->controlType());
}

void JASPControlWrapper::cleanUp()
{
	if (_item)
		_item->disconnect();
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
	JASPControl* listView = qobject_cast<JASPControl*>(_item->parentListView());

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
