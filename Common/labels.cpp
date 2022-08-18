//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "labels.h"
#include "iostream"
#include "utils.h"
#include "log.h"

using namespace std;

const char * labelNotFound::what() const noexcept
{
	//Just here to have an out-of-line virtual method so that clang and gcc don't complain so much
	return std::runtime_error::what();
}

map<int, map<int, string> > Labels::_orgStringValues;
int Labels::_counter = 0;

typedef unsigned int uint;

Labels::Labels(boost::interprocess::managed_shared_memory *mem)
	: _labels(mem->get_segment_manager())
{
	 _id = ++Labels::_counter;
	_mem = mem;
}

Labels::~Labels()
{
}

void Labels::clear()
{
	_labels.clear();
}

int Labels::add(int display)
{
	Label label(display);
	_labels.push_back(label);

	return display;
}

int Labels::add(const std::string &display)
{
	return add(_labels.size(), display, true);
}

int Labels::add(int key, const std::string &display, bool filterAllows, bool isText)
{
	Label label(display, key, filterAllows, isText);
	_labels.push_back(label);

	return key;
}

void Labels::removeValues(std::set<int> valuesToRemove)
{
	_labels.erase(
		std::remove_if(
			_labels.begin(),
			_labels.end(),
			[&valuesToRemove](const Label& label) {
				return std::find(valuesToRemove.begin(), valuesToRemove.end(), label.value()) != valuesToRemove.end();
			}),
				_labels.end());
}

std::map<string, int> Labels::_resetLabelValues(int& maxValue)
{
	std::map<string, int> result;
	map<int, string> &orgStringValues = getOrgStringValues();
	map<int, string> newOrgStringValues;
	int labelValue = 1;
	for (Label& label : _labels)
	{
		int oldLabelValue = label.value();
		std::string labelText = label.text();

		if (orgStringValues.find(oldLabelValue) != orgStringValues.end())
			labelText = newOrgStringValues[labelValue] = orgStringValues[oldLabelValue];

		if (oldLabelValue != labelValue)
			label.setValue(labelValue, false);

		result[labelText] = labelValue;
		labelValue++;
	}

	orgStringValues.clear();
	orgStringValues.insert(newOrgStringValues.begin(), newOrgStringValues.end());
	maxValue = labelValue - 1;

	return result;
}

bool Labels::syncInts(const map<int, string> &values)
{
	std::set<int> keys;
	for (const auto &value : values)
		keys.insert(value.first);

	bool changed = syncInts(keys);

	for (Label& label : _labels)
	{
		int					value				= label.value();
		const string	&	new_string_label	= values.at(value),
							old_string_label	= label.text();

		if (new_string_label != old_string_label)
		{
			_setNewStringForLabel(label, new_string_label);
			changed = true;
		}
	}

	return changed;
}

bool Labels::syncInts(const std::set<int> &values)
{
	std::set<int>	valuesToAdd		= values,
					valuesToRemove;
	bool			isChanged			= false;

	for (Label & label : _labels)
	{
		int value = label.value();

		if (!label.hasIntValue())
		{
			label.setValue(value);
			isChanged = true;
		}

		if(values.count(value) > 0)	valuesToAdd.erase(value);
		else						valuesToRemove.insert(value);
	}

	removeValues(valuesToRemove);	

	for (int value : valuesToAdd)
		add(value);

	return isChanged || (valuesToAdd.size() + valuesToRemove.size() > 0);
}

std::map<std::string, int> Labels::syncStrings(const std::vector<std::string> &new_values, const std::map<std::string, std::string> &new_labels, bool *changedSomething)
{
	std::vector<std::pair<std::string,std::string> > valuesToAdd;
	std::map<std::string, std::vector<unsigned int> > mapValuesToAdd;
	unsigned int valuesToAddIndex = 0;

	for (const std::string& newValue : new_values)
	{
		std::string shortValue	= newValue.length() > Label::MAX_LABEL_LENGTH ? newValue.substr(0, Label::MAX_LABEL_LENGTH) : newValue;
		valuesToAdd.push_back(make_pair(newValue, shortValue));
		auto elt = mapValuesToAdd.find(shortValue);
		if (elt != mapValuesToAdd.end())
			elt->second.push_back(valuesToAddIndex);
		else
			mapValuesToAdd[shortValue] = { valuesToAddIndex };
		valuesToAddIndex++;
	}
	
	std::set<int>				valuesToRemove;
	std::map<std::string, int>	result;
	int							maxLabelKey = 0;

	for (const Label &label : _labels)
	{
		std::string labelText	= _getOrgValueFromLabel(label);
		int labelValue			= label.value();
		
		if (labelValue > maxLabelKey)
			maxLabelKey = labelValue;

		auto elt = mapValuesToAdd.find(labelText);
		if (elt != mapValuesToAdd.end())
		{
			for (uint i : elt->second)
				result[valuesToAdd[i].first] = labelValue;
			mapValuesToAdd.erase(elt);
		}
		else
			valuesToRemove.insert(labelValue);
	}

	if(changedSomething != nullptr && (valuesToRemove.size() > 0 || mapValuesToAdd.size() > 0))
		*changedSomething = true;

	if (valuesToRemove.size() > 0)
	{
		removeValues(valuesToRemove);
		result = _resetLabelValues(maxLabelKey);
	}
	
	for (auto elt : valuesToAdd)
	{
		const std::string& newLabel = elt.first;
		const std::string& shortLabel = elt.second;
		if (mapValuesToAdd.find(shortLabel) != mapValuesToAdd.end())
		{
			maxLabelKey++;
			add(maxLabelKey, shortLabel, true);
			result[newLabel] = maxLabelKey;
		}
	}

	for (Label& label : _labels)
	{
		std::string labelText = _getOrgValueFromLabel(label);
		auto newLabelIt = new_labels.find(labelText);
		if (newLabelIt != new_labels.end())
		{
			std::string newStringLabel = newLabelIt->second;
			if (labelText != newStringLabel)
			{
				_setNewStringForLabel(label, newStringLabel);

				if(changedSomething != nullptr)
					*changedSomething = true;
			}
		}
	}
	return result;
}

std::set<int> Labels::getIntValues()
{
	std::set<int> result;
	for (const Label& label : _labels)
		result.insert(label.value());
	
	return result;
}

map<int, string> &Labels::getOrgStringValues() const
{
	return Labels::_orgStringValues[_id];
}

void Labels::setOrgStringValues(int key, std::string value)
{
	getOrgStringValues()[key] = value;
}

const Label &Labels::getLabelObjectFromKey(int index) const

{
	for (const Label &label: _labels)
	{
		if (label.value() == index)
			return label;
	}

	Log::log() << "Cannot find entry " << index << std::endl;
	for(const Label &label: _labels)
	{
		Log::log() << "Label Value: " << label.value() << ", Text: " << label.text() << std::endl;
	}

	throw labelNotFound("Cannot find this entry");
}

bool Labels::setLabelFromRow(int row, const string &display)
{
	if (row >= (int)_labels.size() || row < 0)
	{
		Log::log() << "Set label with wrong row: " << row << ", size: " << _labels.size() << std::endl;
		return false;
	}

	try
	{
		Label &label = _labels.at(row);
		if (label.text() == display)
			return false;

		_setNewStringForLabel(label, display);
	}
	catch(...)
	{
		return false;
	}
	return true;
}

void Labels::log()
{
	Log::log() << "Labels: " << std::endl;
	for (Label& label : _labels)
		Log::log() << (label.hasIntValue() ? "int: " : "text: ") << label.value() << ": " << label.text() << std::endl;
	Log::log() << std::flush;
}

void Labels::_setNewStringForLabel(Label &label, const string &display)
{
	int label_value = label.value();
	string label_string = label.text();

	if (!label.hasIntValue())
	{
		map<int, string> &orgStringValues = getOrgStringValues();

		if (orgStringValues.find(label_value) == orgStringValues.end())
			orgStringValues[label_value] = label_string;
	}

	label.setLabel(display);
}

string Labels::_getValueFromLabel(const Label &label) const
{
	if (label.hasIntValue())	return std::to_string(label.value());
	else						return _getOrgValueFromLabel(label);
}

string Labels::_getOrgValueFromLabel(const Label &label) const
{
	map<int, string> &orgStringValues	= getOrgStringValues();
	map<int, string>::const_iterator it = orgStringValues.find(label.value());

	if (it == orgStringValues.end())	return label.text();
	else								return it->second;
}

string Labels::getValueFromKey(int key) const
{
	try
	{
		const Label &label = getLabelObjectFromKey(key);
		return _getValueFromLabel(label);
	}
	catch (const labelNotFound & e)
	{
		Log::log() << "Label not found, msg: " << e.what() << ", returning emptyValue\n";
		return Utils::emptyValue;
	}
}

string Labels::getValueFromRow(int row) const
{
	if (row >= (int)_labels.size())
	{
		Log::log() << "Get value with wrong row: " << row << ", size: " << _labels.size() << std::endl;
		return "";
	}
	const Label &label = _labels.at(row);
	return _getValueFromLabel(label);
}

Label& Labels::operator[](size_t index)
{
	return _labels.at(index);
}

std::string Labels::getLabelFromRow(int row) const
{
	if (row >= (int)_labels.size())
	{
		Log::log() << "Get label with wrong row: " << row << ", size: " << _labels.size() << std::endl;
		return "";
	}
	const Label & label = _labels.at(row);

	return label.text();
}

void Labels::set(vector<Label> &labels)
{
	clear();
	for (const Label &label : labels)
	{
		_labels.push_back(label);
	}
}

size_t Labels::size() const
{
	return _labels.size();
}

Labels &Labels::operator=(const Labels &labels)
{
	if (&labels != this)
	{
		this->_mem = labels._mem;
		this->_labels = labels._labels;
	}

	return *this;
}

void Labels::setSharedMemory(boost::interprocess::managed_shared_memory *mem)
{
	_mem = mem;
}

Labels::const_iterator Labels::begin() const
{
	return _labels.begin();
}

Labels::const_iterator Labels::end() const
{
	return _labels.end();
}
