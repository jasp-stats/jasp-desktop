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

#include "boundcontrolterms.h"
#include "variableslistbase.h"
#include "log.h"
#include "../analysis/analysisform.h"
#include "listmodeltermsavailable.h"
#include "listmodeltermsassigned.h"
#include "listmodelinteractionassigned.h"
#include "rowcontrols.h"

BoundControlTerms::BoundControlTerms(ListModelAssignedInterface* listModel, bool isSingleRow) : BoundControlBase(listModel->listView())
{
	_termsModel = listModel;
	_listView = qobject_cast<JASPListControl*>(_control);
	_isSingleRow = isSingleRow;
	_optionKey = _listView->optionKey().toStdString();
}

void BoundControlTerms::bindTo(const Json::Value &value)
{
	BoundControlBase::bindTo(value);

	Terms terms;
	ListModel::RowControlsValues allControlValues;

	if (_listView->hasRowComponent() || _listView->containsInteractions())
	{
		if (_optionKey != _optionKeyFromFile)
		{
			// Backward compatibility: the key in the JASP file does not correspond to the current key. This must be replaced
			// TODO: value[_optionKey] = value[_optionKeyFromFile];
		}
		_readTableValue(value, _optionKey, _listView->containsInteractions(), terms, allControlValues);
	}
	else
	{
		if (value.isArray())
		{
			for (const Json::Value& variable : value)
				terms.add(Term(variable.asString()));
		}
		else if (value.isString())
		{
			std::string str = value.asString();
			if (!str.empty())
				terms.add(Term(str));
		}
		else
			Log::log() << "Control " << _control->name() << " is bound with a value that is neither an array, an object bor a string :" << value.toStyledString() << std::endl;
	}

	_termsModel->initTerms(terms, allControlValues);
}

Json::Value BoundControlTerms::createJson()
{
	if (_listView->hasRowComponent() || _listView->containsInteractions())
		return Json::Value(Json::arrayValue);
	else if (_isSingleRow)
		return Json::Value("");
	else
		return Json::Value(Json::arrayValue);
}

bool BoundControlTerms::isJsonValid(const Json::Value &optionValue)
{
	bool valid = true;
	if (_listView->hasRowComponent() || _listView->containsInteractions())
	{
		valid = optionValue.type() == Json::arrayValue;
		if (valid)
		{
			for (uint i = 0; i < optionValue.size(); i++)
			{
				const Json::Value& value = optionValue[i];
				valid = value.type() == Json::objectValue;
				if (valid)
				{
					_optionKeyFromFile = _optionKey;
					if (value[_optionKey].isNull() && value.size() > 0)
					{
						_optionKeyFromFile = value.begin().memberName();
						Log::log() << "JASP file has options for " << _listView->name() << " without '" << _optionKey << "' key. Per default, first key '" << _optionKeyFromFile << "' is used. Probably the file comes from an older version of JASP." << std::endl;
					}
					const Json::Value& components = value[_optionKeyFromFile];
					if (_listView->containsInteractions())
					{
						valid = components.type() == Json::arrayValue;
						if (components.type() == Json::stringValue)
						{
							valid = true;
							Log::log() << "JASP file has a VariableList with interaction but the elements are strings in place of arrays. Probably an old JASP file." << std::endl;
						}
					}
					else
						valid = components.type() == Json::stringValue;
				}
				if (!valid)
					break;
			}
		}
	}
	else if (_isSingleRow)
		valid = optionValue.type() == Json::stringValue;
	else
	{
		valid = optionValue.type() == Json::arrayValue;
		if (valid)
		{
			for (uint i = 0; i < optionValue.size(); i++)
			{
				const Json::Value& value = optionValue[i];
				valid = value.type() == Json::stringValue;
				if (!valid)
					break;
			}
		}
	}

	return valid;
}

void BoundControlTerms::resetBoundValue()
{
	const Terms& terms = _termsModel->terms();
	const QMap<QString, RowControls*>& allControls = _termsModel->getAllRowControls();

	if (_listView->hasRowComponent() || _listView->containsInteractions())
		_setTableValue(terms, allControls, _optionKey, _listView->containsInteractions());
	else if (_isSingleRow)
	{
		std::string str = terms.size() > 0 ? terms[0].asString() : "";
		Json::Value boundValue(str);
		setBoundValue(boundValue);
	}
	else
	{
		Json::Value boundValue(Json::arrayValue);
		for (const Term& term : terms)
			boundValue.append(term.asString());
		setBoundValue(boundValue);
	}
}


