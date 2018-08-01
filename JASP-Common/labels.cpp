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


using namespace std;

map<int, map<int, string> > Labels::_orgStringValues;
int Labels::_counter = 0;

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

int Labels::add(int key, const std::string &display, bool filterAllows)
{
	Label label(display, key, filterAllows);
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

void Labels::syncInts(map<int, string> &values)
{
	std::set<int> keys;
	for (const auto &value : values)
		keys.insert(value.first);

	syncInts(keys);

	for (Label& label : _labels)
	{
		int value = label.value();
		string &new_string_label = values[value];
		string old_string_label = label.text();
		if (new_string_label != old_string_label)
			_setNewStringForLabel(label, new_string_label);
	}

}

void Labels::syncInts(const std::set<int> &values)
{
	std::set<int> valuesToAdd = values;
	std::set<int> valuesToRemove;

	for (const Label& label : _labels)
	{
		int value = label.value();
		if (std::find(values.begin(), values.end(), value) != values.end())
		{
			std::set<int>::iterator value_it = std::find(valuesToAdd.begin(), valuesToAdd.end(), value);
			if (value_it != valuesToAdd.end())
				valuesToAdd.erase(value_it);
		}
		else
		{
			std::cout << "Remove label " << label.text() << std::endl;
			std::cout.flush();
			valuesToRemove.insert(value);
		}
	}

	removeValues(valuesToRemove);	

	for (int value : valuesToAdd)
		add(value);
}

map<string, int> Labels::syncStrings(const vector<string> &new_values, const map<string, string> &new_labels)
{
	map<string,string> valuesToAdd;
	for (const string& newValue : new_values)
	{
		string shortValue = (newValue.length() > Label::MAX_LABEL_LENGTH ? newValue.substr(0, Label::MAX_LABEL_LENGTH) : newValue);
		valuesToAdd[shortValue] = newValue;
	}
	
	std::set<int> valuesToRemove;
	map<string, int> result;

	int maxLabelKey = 0;
	for (const Label &label : _labels)
	{
		string labelText = _getOrgValueFromLabel(label);
		int labelValue = label.value();
		
		if (labelValue > maxLabelKey)
			maxLabelKey = labelValue;

		map<string, string>::const_iterator elt = valuesToAdd.find(labelText);
		if (elt != valuesToAdd.end())
		{
			result[elt->second] = labelValue;
			valuesToAdd.erase(elt);
		}
		else
			valuesToRemove.insert(labelValue);
	}

	removeValues(valuesToRemove);
	
	for (auto elt : valuesToAdd)
	{
		maxLabelKey++;
		add(maxLabelKey, elt.first, true);
		result[elt.second] = maxLabelKey;
	}

	for (Label& label : _labels)
	{
		string labelText = _getOrgValueFromLabel(label);
		map<string, string>::const_iterator newLabelIt = new_labels.find(labelText);
		if (newLabelIt != new_labels.end())
		{
			string newStringLabel = newLabelIt->second;
			if (labelText != newStringLabel)
				_setNewStringForLabel(label, newStringLabel);
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
	map<int, string> &orgStringValues = getOrgStringValues();
	orgStringValues[key] = value;
}

const Label &Labels::getLabelObjectFromKey(int index) const

{
	for (const Label &label: _labels)
	{
		if (label.value() == index)
			return label;
	}

	std::cout << "Cannot find entry " << index << std::endl;
	for(const Label &label: _labels)
	{
		std::cout << "Label Value: " << label.value() << ", Text: " << label.text() << std::endl;
	}
	std::cout.flush();
	throw runtime_error("Cannot find this entry");
}

bool Labels::setLabelFromRow(int row, const string &display)
{
	if (row >= (int)_labels.size() || row < 0)
	{
		std::cout << "Set label with wrong row: " << row << ", size: " << _labels.size() << std::endl;
		std::cout.flush();
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

void Labels::_setNewStringForLabel(Label &label, const string &display)
{
	int label_value = label.value();
	string label_string = label.text();
	map<int, string> &orgStringValues = getOrgStringValues();
	if (orgStringValues.find(label_value) == orgStringValues.end())
		orgStringValues[label_value] = label_string;
	label.setLabel(display);
}

string Labels::_getValueFromLabel(const Label &label) const
{
	if (label.hasIntValue())
	{
		std::ostringstream ss;
		ss << label.value();
		return ss.str();
	}
	else
	{
		return _getOrgValueFromLabel(label);
	}
}

string Labels::_getOrgValueFromLabel(const Label &label) const
{
	map<int, string> &orgStringValues = getOrgStringValues();
	map<int, string>::const_iterator it = orgStringValues.find(label.value());
	if (it == orgStringValues.end())
		return label.text();
	else
		return it->second;	
}

string Labels::getValueFromKey(int key) const
{
	const Label &label = getLabelObjectFromKey(key);
	return _getValueFromLabel(label);
}

string Labels::getValueFromRow(int row)
{
	if (row >= (int)_labels.size())
	{
		std::cout << "Get value with wrong row: " << row << ", size: " << _labels.size() << std::endl;
		std::cout.flush();
		return "";
	}
	const Label &label = _labels.at(row);
	return _getValueFromLabel(label);
}

Label& Labels::operator[](size_t index)
{
	return _labels.at(index);
}

std::string Labels::getLabelFromRow(int row)
{
	if (row >= (int)_labels.size())
	{
		std::cout << "Get label with wrong row: " << row << ", size: " << _labels.size() << std::endl;
		std::cout.flush();
		return "";
	}
	Label &label = _labels.at(row);
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
