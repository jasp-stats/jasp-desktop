//
// Copyright (C) 2013-2016 University of Amsterdam
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

const Label &Labels::labelFor(int raw) const
{
	BOOST_FOREACH(const LabelEntry &entry, _labels)
	{
		if (entry.first == raw)
			return entry.second;
	}

	throw runtime_error("Cannot find this entry");
}

const LabelEntry &Labels::at(int index) const
{
	return _labels.at(index);
}

void Labels::setLabel(int index, const std::string &display) {
    LabelEntry &label_entry = _labels.at(index);
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
