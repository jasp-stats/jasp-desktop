//
// Copyright (C) 2013-2017 University of Amsterdam
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
#include <boost/foreach.hpp>

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
	return add(_labels.size(), display);
}

int Labels::add(int index, const std::string &display)
{
	Label label(display, index);
	_labels.push_back(label);

	return index;
}

void Labels::syncInts(map<int, string> &values)
{
	std::set<int> keys;
	for (map<int, string>::const_iterator it = values.begin(); it != values.end(); ++it)
		keys.insert(it->first);

	syncInts(keys);

	for (LabelVector::iterator it = _labels.begin(); it != _labels.end(); ++it)
	{
		Label &label = *it;
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

	for (LabelVector::const_iterator it = _labels.begin(); it != _labels.end(); /*++it*/)
	{
		const Label &label = *it;
		int value = label.value();
		if (std::find(values.begin(), values.end(), value) != values.end())
		{
			std::set<int>::iterator value_it = std::find(valuesToAdd.begin(), valuesToAdd.end(), value);
			if (value_it != valuesToAdd.end())
				valuesToAdd.erase(value_it);
			++it;
		}
		else
		{
			std::cout << "Remove label " << label.text() << std::endl;
			std::cout.flush();
			_labels.erase(it);
		}
	}

	for (std::set<int>::iterator it = valuesToAdd.begin(); it != valuesToAdd.end(); ++it)
	{
		int value = *it;
		add(value);
	}
}

map<string, int> Labels::syncStrings(const vector<string> &new_values, const map<string, string> &new_labels)
{
	const map<int, string> &orgStringValues = getOrgStringValues();
	vector<string> valuesToAdd = new_values;
	map<string, int> result;


	int maxLabelIndex = 0;
	for (Labels::const_iterator it = begin(); it != end(); /*++it*/)
	{
		const Label &label = *it;
		if (label.value() > maxLabelIndex)
			maxLabelIndex = label.value();

		string value = label.text();

		map<int, string>::const_iterator orgStringValuesIt = orgStringValues.find(label.value());
		if (orgStringValuesIt != orgStringValues.end()) value = orgStringValuesIt->second;

		if (std::find(new_values.begin(), new_values.end(), value) != new_values.end())
		{
			result[value] = label.value();
			vector<string>::iterator value_it = std::find(valuesToAdd.begin(), valuesToAdd.end(), value);
			if (value_it != valuesToAdd.end())
				valuesToAdd.erase(value_it);
			++it;
		}
		else
		{
			_labels.erase(it);
		}
	}

	for (vector<string>::iterator it = valuesToAdd.begin(); it != valuesToAdd.end(); ++it)
	{
		string value = *it;
		maxLabelIndex++;
		add(maxLabelIndex,value);
		result[value] = maxLabelIndex;
	}

	for (LabelVector::iterator it = _labels.begin(); it != _labels.end(); ++it)
	{
		Label &label = *it;
		string string_label = label.text();
		map<string, string>::const_iterator new_labels_it = new_labels.find(string_label);
		if (new_labels_it != new_labels.end())
		{
			string new_string_label = new_labels_it->second;
			if (string_label != new_string_label)
				_setNewStringForLabel(label, new_string_label);
		}
	}

	return result;
}

map<int, string> &Labels::getOrgStringValues()
{
	return Labels::_orgStringValues[_id];
}

void Labels::setOrgStringValues(int key, std::string value)
{
	map<int, string> &orgStringValues = getOrgStringValues();
	orgStringValues[key] = value;
}

const Label &Labels::labelFor(int index) const

{
	BOOST_FOREACH(const Label &label, _labels)
	{
		if (label.value() == index)
			return label;
	}

	std::cout << "Cannot find entry " << index << std::endl;
	BOOST_FOREACH(const Label &label, _labels)
	{
		std::cout << "Label Value: " << label.value() << ", Text: " << label.text() << std::endl;
	}
	std::cout.flush();
	throw runtime_error("Cannot find this entry");
}

bool Labels::setLabelFromRow(int row, const string &display)
{
	if (row >= (int)_labels.size())
	{
		std::cout << "Set label with wrong row: " << row << ", size: " << _labels.size() << std::endl;
		std::cout.flush();
		return false;
	}
	Label &label = _labels.at(row);
	if (label.text() == display)
		return false;

	_setNewStringForLabel(label, display);
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

string Labels::_getValueFromLabel(const Label &label)
{
	if (label.hasIntValue())
	{
		std::ostringstream ss;
		ss << label.value();
		return ss.str();
	}
	else
	{
		map<int, string> &orgStringValues = getOrgStringValues();
		map<int, string>::const_iterator it = orgStringValues.find(label.value());
		if (it == orgStringValues.end())
			return label.text();
		else
			return it->second;
	}
}

string Labels::getValueFromIndex(int index)
{
	const Label &label = labelFor(index);
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
	BOOST_FOREACH(Label &label, labels)
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
