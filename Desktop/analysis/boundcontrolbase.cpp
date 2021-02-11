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

#include "boundcontrolbase.h"
#include "jaspcontrol.h"
#include "widgets/jasplistcontrol.h"
#include "analysisform.h"
#include "log.h"
#include "widgets/listmodel.h"
#include "widgets/rowcontrols.h"

BoundControlBase::BoundControlBase(JASPControl* control) : _control(control)
{
}

void BoundControlBase::setBoundValue(const Json::Value &value, bool emitChange)
{
	AnalysisForm* form = _control->form();

	if (form && _control->isBound())
	{
		if (_isColumn && value.isString())
		{
			const Json::Value& orgValue = boundValue();
			std::string newName = value.asString();
			std::string orgName = orgValue.asString();
			if (newName.empty() && !orgName.empty())
				emit _control->requestComputedColumnDestruction(orgName);
			else if (newName != orgName)
			{
				if (_isComputedColumn)
					emit _control->requestComputedColumnCreation(newName);
				else
					emit _control->requestColumnCreation(newName, _columnType);

				if (!orgName.empty())
					emit _control->requestComputedColumnDestruction(orgName);
			}
		}

		if (_isColumn || _control->encodeValue())	_meta["shouldEncode"] = true;

		form->setBoundValue(getName(), value, _meta, _control->getParentKeys());
	}
	if (emitChange)	emit _control->boundValueChanged(_control);
}

std::vector<std::string> BoundControlBase::usedVariables()
{
	if (_isColumn || _control->encodeValue())
	{
		JASPListControl* listControl = qobject_cast<JASPListControl*>(_control);
		if (listControl)
			return listControl->model()->terms().asVector();
		else
		{
			const Json::Value value = boundValue();
			if (value.isString())
				return { value.asString() };
		}
	}

	return {};
}

void BoundControlBase::setIsRCode(std::string key)
{
	if (key.empty())	_meta["isRCode"] = true;
	else				_meta[key]["isRCode"] = true;
}

const Json::Value &BoundControlBase::boundValue()
{
	AnalysisForm* form = _control->form();

	if (form && form->analysisObj())	return form->analysisObj()->boundValue(getName(), _control->getParentKeys());
	else								return Json::Value::null;
}

void BoundControlBase::setIsColumn(bool isComputed, columnType type)
{
	_isColumn = true;
	_isComputedColumn = isComputed;
	_columnType = type;

	AnalysisForm* form = _control->form();
	if (form)	form->addColumnControl(_control, isComputed);
}


const std::string &BoundControlBase::getName()
{
	if (_name.empty())
		_name = _control->name().toStdString();

	return _name;
}

void BoundControlBase::_readTableValue(const Json::Value &value, const std::string& key, bool hasMultipleTerms, Terms& terms, ListModel::RowControlsValues& allControlValues)
{
	for (const Json::Value& row : value)
	{
		const Json::Value& keyValue = row[key];
		if (hasMultipleTerms)
		{
			if (keyValue.isArray())
			{
				std::vector<std::string> term;
				for (const Json::Value& component : keyValue)
					term.push_back(component.asString());
				terms.add(Term(term));
			}
			else
				Log::log() << "Key (" << key << ") bind value is not an array in " << _name << ": " << value.toStyledString() << std::endl;
		}
		else
		{
			if (keyValue.isString())
				terms.add(Term(keyValue.asString()));
			else
				Log::log() << "Key (" << key << ") bind value is not a string in " << _name << ": " << value.toStyledString() << std::endl;
		}

		QMap<QString, Json::Value> controlMap;
		for (auto itr = row.begin(); itr != row.end(); ++itr)
		{
			const std::string& name = itr.key().asString();
			if (name != key)
				controlMap[tq(name)] = *itr;
		}

		allControlValues[tq(key)] = controlMap;
	}
}

void BoundControlBase::_setTableValue(const Terms& terms, const QMap<QString, RowControls*>& allControls, const std::string& key, bool hasMultipleTerms)
{
	Json::Value boundValue(Json::arrayValue);
	for (const Term& term : terms)
	{
		Json::Value rowValues(Json::objectValue);
		if (hasMultipleTerms)
		{
			Json::Value keyValue(Json::arrayValue);
			for (const std::string& comp : term.scomponents())
				keyValue.append(comp);
			rowValues[key] = keyValue;
		}
		else
		{
			Json::Value keyValue(term.asString());
			rowValues[key] = keyValue;
		}

		RowControls* rowControls = allControls[term.asQString()];
		if (rowControls)
		{
			const QMap<QString, JASPControl*>& controlsMap = rowControls->getJASPControlsMap();
			QMapIterator<QString, JASPControl*> it(controlsMap);
			while (it.hasNext())
			{
				it.next();
				JASPControl* control = it.value();
				BoundControl* boundControl = control->boundControl();
				if (boundControl)
				{
					const QString& name = it.key();
					const Json::Value& value = boundControl->boundValue();
					rowValues[fq(name)] = value;
				}
			}
		}
		boundValue.append(rowValues);
	}

	setBoundValue(boundValue);
}

