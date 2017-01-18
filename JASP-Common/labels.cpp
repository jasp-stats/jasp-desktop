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

Labels::Labels(boost::interprocess::managed_shared_memory *mem)
	: _labels(mem->get_segment_manager())
{
	_mem = mem;
}

void Labels::clear()
{
	_labels.clear();
}

int Labels::add(int display)
{
	return add(display, display);
}

int Labels::add(const std::string &display)
{
	return add(_labels.size(), display);
}

int Labels::add(int raw, int display)
{
	LabelEntry entry(raw, Label(display));
	_labels.push_back(entry);

	return display;
}

int Labels::add(int raw, const std::string &display)
{
	LabelEntry entry(raw, Label(display, raw));
	_labels.push_back(entry);

	return raw;
}

void Labels::sync(const std::set<int> &values)
{
	std::set<Labels::const_iterator> labelsToRemove;
	std::set<int> valuesToAdd = values;

	for (Labels::const_iterator it = begin(); it != end(); ++it)
	{
		const LabelEntry &entry = *it;
		int value = entry.second.value();
		if (std::find(values.begin(), values.end(), value) != values.end())
		{
			std::set<int>::iterator value_it = std::find(valuesToAdd.begin(), valuesToAdd.end(), value);
			if (value_it != valuesToAdd.end())
				valuesToAdd.erase(value_it);
		}
		else
		{
			labelsToRemove.insert(it);
		}
	}

	for (std::set<Labels::const_iterator>::iterator it = labelsToRemove.begin(); it != labelsToRemove.end(); ++it)
	{
		Labels::const_iterator label_it = *it;
		_labels.erase(label_it);
	}

	for (std::set<int>::iterator it = valuesToAdd.begin(); it != valuesToAdd.end(); ++it)
	{
		int value = *it;
		add(value);
	}
}

std::map<std::string, int> Labels::syncStrings(const std::vector<std::string> &values)
{
	std::set<Labels::const_iterator> labelsToRemove;
	std::vector<std::string> valuesToAdd = values;
	std::map<std::string, int> result;


	int maxLabelIndex = 0;
	for (Labels::const_iterator it = begin(); it != end(); ++it)
	{
		const LabelEntry &entry = *it;
		if (entry.first > maxLabelIndex)
			maxLabelIndex = entry.first;

		std::map<int, std::string>::iterator orgValueIt = _orgValues.find(entry.first);
		std::string value = orgValueIt == _orgValues.end() ? entry.second.text() : orgValueIt->second;
		if (std::find(values.begin(), values.end(), value) != values.end())
		{
			result[value] = entry.first;
			std::vector<std::string>::iterator value_it = std::find(valuesToAdd.begin(), valuesToAdd.end(), value);
			if (value_it != valuesToAdd.end())
				valuesToAdd.erase(value_it);
		}
		else
		{
			labelsToRemove.insert(it);
		}
	}

	for (std::set<Labels::const_iterator>::iterator it = labelsToRemove.begin(); it != labelsToRemove.end(); ++it)
	{
		Labels::const_iterator label_it = *it;
		const LabelEntry &entry = *label_it;
		std::cout << "Remove Label " << entry.first << ", Value:" << entry.second.value() << ", text: " << entry.second.text() << std::endl;
		std::cout.flush();
		_labels.erase(label_it);
	}

	for (std::vector<std::string>::iterator it = valuesToAdd.begin(); it != valuesToAdd.end(); ++it)
	{
		std::string value = *it;
		std::cout << "Add value " << value << std::endl;
		std::cout.flush();
		maxLabelIndex++;
		add(maxLabelIndex,value);
		result[value] = maxLabelIndex;
	}

	return result;
}

const std::map<int, std::string> &Labels::getOrgValues()
{
	return _orgValues;
}

void Labels::setOrgValue(int key, std::string value)
{
	_orgValues[key] = value;
}

const Label &Labels::labelFor(int raw) const
{
	BOOST_FOREACH(const LabelEntry &entry, _labels)
	{
		if (entry.first == raw)
			return entry.second;
	}

	std::cout << "Cannot find entry " << raw << std::endl;
	BOOST_FOREACH(const LabelEntry &entry, _labels)
	{
		std::cout << "Label " << entry.first << ", " << "Value: " << entry.second.value() << ", Text: " << entry.second.text() << std::endl;
		if (entry.first == raw)
			return entry.second;
	}
	std::cout.flush();
	throw runtime_error("Cannot find this entry");
}

void Labels::setLabel(int index, const std::string &display)
{
    LabelEntry &label_entry = _labels.at(index);
	if (_orgValues.find(label_entry.first) == _orgValues.end())
		_orgValues[label_entry.first] = label_entry.second.text();

    label_entry.second.setLabel(display);
}

void Labels::set(vector<LabelEntry> &labels)
{
	clear();
	BOOST_FOREACH(LabelEntry &label, labels)
	{
		add(label.first, label.second.text());
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
