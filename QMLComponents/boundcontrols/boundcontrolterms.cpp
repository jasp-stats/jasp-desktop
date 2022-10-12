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
#include "controls/variableslistbase.h"
#include "log.h"
#include "analysisform.h"
#include "models/listmodeltermsavailable.h"
#include "models/listmodeltermsassigned.h"
#include "models/listmodelinteractionassigned.h"
#include "controls/rowcontrols.h"

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
		_readTableValue(value, _optionKey, _listView->containsInteractions(), terms, allControlValues);
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

Json::Value BoundControlTerms::createJson() const
{
	const Terms& terms = _termsModel->terms();

	if (_listView->containsInteractions() || _listView->hasRowComponent())
	{
		Json::Value jsonValue(Json::arrayValue);
		for (const Term& term : terms)
		{
			Json::Value row(Json::objectValue);
			if (_listView->containsInteractions())
			{
				Json::Value keyValue(Json::arrayValue);
				for (const std::string& comp : term.scomponents())
					keyValue.append(comp);
				row[_optionKey] = keyValue;
			}
			else
			{
				Json::Value keyValue(term.asString());
				row[_optionKey] = keyValue;
			}

			if (_listView->hasRowComponent())
			{
				auto allRowControls = _termsModel->getAllRowControls();
				if (allRowControls.contains(term.asQString()))
				{
					RowControls* rowControls = allRowControls[term.asQString()];
					const QMap<QString, JASPControl*>&	controlsMap = rowControls->getJASPControlsMap();
					for (const QString& controlName : controlsMap.keys())
					{
						JASPControl* control = controlsMap[controlName];
						BoundControl* boundControl = control->boundControl();
						if (boundControl)
							row[fq(controlName)] = boundControl->createJson();
					}
				}
			}
			jsonValue.append(row);
		}

		return jsonValue;
	}
	else if (_isSingleRow)
	{
		if (terms.size() > 0)	return Json::Value(terms.at(0).asString());
		else					return Json::Value("");
	}
	else
	{
		Json::Value jsonValue(Json::arrayValue);
		for (const Term& term : terms)
			jsonValue.append(term.asString());
		return jsonValue;
	}
}

bool BoundControlTerms::isJsonValid(const Json::Value &optionValue) const
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
					const Json::Value& components = value[_optionKey];
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

	if (_listView->hasRowComponent() || _listView->containsInteractions())
		_setTableValue(_termsModel->getTermsWithComponentValues(), _optionKey, _listView->containsInteractions());
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

Json::Value BoundControlTerms::addTermsToOption(const Json::Value &option, const Terms &terms, const ListModel::RowControlsValues &extraTermsMap) const
{
	Json::Value result = option;
	Terms termsAlreadyInOptions = _getValuesFromOptions(option);

	if (_listView->hasRowComponent() || _listView->containsInteractions())
	{
		Terms termsToAdd;
		for (const Term& term : terms)
		{
			if (!termsAlreadyInOptions.contains(term)) continue; // Don't add term that is already in option.
				termsToAdd.add(term);
		}

		for (const Term& term : terms)
		{
			if (termsAlreadyInOptions.contains(term)) continue; // Don't add term that is already in option.

			Json::Value rowValues(Json::objectValue);
			if (_listView->containsInteractions())
			{
				Json::Value keyValue(Json::arrayValue);
				for (const std::string& comp : term.scomponents())
					keyValue.append(comp);
				rowValues[_optionKey] = keyValue;
			}
			else
			{
				Json::Value keyValue(term.asString());
				rowValues[_optionKey] = keyValue;
			}

			QString termStr = term.asQString();
			if (extraTermsMap.contains(termStr))
			{
				const QMap<QString, Json::Value>& controlsMap = extraTermsMap[termStr];
				QMapIterator<QString, Json::Value> it(controlsMap);
				while (it.hasNext())
				{
					it.next();
					rowValues[fq(it.key())] = it.value();
				}
			}
			result.append(rowValues);
		}
	}
	else if (_isSingleRow)
	{
		std::string str = terms.size() > 0 ? terms[0].asString() : "";
		result = Json::Value(str);
	}
	else
	{
		for (const Term& term : terms)
			if (!termsAlreadyInOptions.contains(term))
				result.append(term.asString());
	}

	return result;
}

bool BoundControlTerms::areTermsInOption(const Json::Value &option, Terms &terms) const
{
	if (terms.size() == 0) return false;

	bool result = true;
	Terms termsInOptions = _getValuesFromOptions(option);
	Terms termsToSearch = terms;

	for (const Term& term : termsToSearch)
	{
		if (termsInOptions.contains(term))	terms.remove(term);
		else								result = false;
	}

	return result;
}

Terms BoundControlTerms::_getValuesFromOptions(const Json::Value& option) const
{
	Terms result;

	if (_listView->hasRowComponent() || _listView->containsInteractions())
	{
		if (!option.isArray()) return result; // Just to be sure

		for (const Json::Value& row : option)
		{
			if (_listView->containsInteractions())
			{
				if (row.isArray())
				{
					std::vector<std::string> term;
					for (const Json::Value& val : row)
					{
						if (val.isString())
							term.push_back(val.asString());
					}
					result.add(Term(term));
				}
			}
			else
			{
				if (row.isString())
					result.add(row.asString());
			}
		}
	}
	else if (_isSingleRow)
	{
		if (!option.isString()) return result; // Just to be sure
		result.add(option.asString());
	}
	else
	{
		if (!option.isArray()) return result;

		for (const Json::Value& row : option)
		{
			if (row.isString())
				result.add(row.asString());
		}
	}

	return result;
}


