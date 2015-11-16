//
// Copyright (C) 2013-2015 University of Amsterdam
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

using namespace std;

Labels::Labels(boost::interprocess::managed_shared_memory *mem)
	: _labels(std::less<int>(), mem->get_segment_manager())
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
	_labels[raw] = Label(display);

	return display;
}

int Labels::add(int raw, const std::string &display)
{
	_labels[raw] = Label(display, raw);

	return raw;
}

const Label &Labels::at(int raw) const
{
	return _labels.at(raw);
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
