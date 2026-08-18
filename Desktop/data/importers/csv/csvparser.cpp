//
// Copyright (C) 2013-2026 University of Amsterdam
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
#include "csvparser.h"

#include <boost/algorithm/string.hpp>

using namespace std;

CSVParser::CSVParser(char delimiter, bool replaceLineEndings)
	: QObject()
	, _delimiter(delimiter)
	, _replaceLineEndings(replaceLineEndings)
	, _skipNextLF(false)
{
	reset();
}

CSVParser::Grid CSVParser::parse(const string& data)
{
	reset();
	size_t i = 0;
	while (i < data.size()) {
		char ch = data[i];
		if (processChar(ch)) {
			// Re-process the same character
			continue;
		}
		i++;
	}
	finishRow();
	emit parsingComplete();
	
	return getGrid();
}

CSVParser::Grid CSVParser::parse(const QString& data)
{
	return parse(data.toStdString());
}

bool CSVParser::processChar(char ch)
{
	switch (_state)
	{
	case Normal:
		switch (ch)
		{
		case '"':
			_state = Quoted;
			return false;
		case '\r':
			finishField();
			_gridQueue.push(_currentRow);
			_currentRow.clear();
			_rowFinished = true;
			_skipNextLF = true;
			emit rowParsed(_gridQueue.back());
			return false;
		case '\n':
			if (_skipNextLF)
			{
				_skipNextLF = false;
				return false;
			}
			finishField();
			_gridQueue.push(_currentRow);
			_currentRow.clear();
			_rowFinished = true;
			emit rowParsed(_gridQueue.back());
			return false;
		default:
			_skipNextLF = false;
			if (ch == _delimiter)
			{
				finishField();
			}
			else
			{
				_currentField.push_back(ch);
			}
			return false;
		}

	case Quoted:
		switch (ch)
		{
		case '"':
			_state = QuotedQuote;
			return false;
		default:
			_currentField.push_back(ch);
			return false;
		}

	case QuotedQuote:
		switch (ch)
		{
		case '"':
			_currentField.push_back('"');
			_state = Quoted;
			return false;
		default:
			_skipNextLF = false;
			_state = Normal;
			return true;
		}
	}
	return false;
}

bool CSVParser::hasRow() const
{
	return !_gridQueue.empty() || (_rowFinished && (!_currentRow.empty() || !_currentField.empty()));
}

vector<string> CSVParser::extractRow()
{
	if (!_currentRow.empty() || !_currentField.empty())
	{
		finishRow();
	}

	if (_gridQueue.empty())
	{
		return {};
	}

	auto row = _gridQueue.front();
	_gridQueue.pop();
	return row;
}

CSVParser::Grid CSVParser::getGrid()
{
	Grid grid;
	
	while(!_gridQueue.empty())
	{
		grid.push_back(_gridQueue.front());
		_gridQueue.pop();
	}

	return grid;
}

size_t CSVParser::getRowCount() const
{
	return _gridQueue.size();
}

bool CSVParser::hasPendingData() const
{
	return !_currentField.empty() || !_currentRow.empty() || !_gridQueue.empty();
}

void CSVParser::reset()
{
	_state = Normal;
	_currentField.clear();
	_currentRow.clear();
	while(!_gridQueue.empty())
		_gridQueue.pop();
	_rowFinished = false;
}

void CSVParser::setDelimiter(char delimiter)
{
	_delimiter = delimiter;
	_skipNextLF = false;
	reset();
}

void CSVParser::finishField()
{
	if (_replaceLineEndings)
	{
		replaceLineEndings(_currentField);
	}
	_currentRow.push_back(_currentField);
	_currentField.clear();
}

void CSVParser::finishRow()
{
	if (!_currentField.empty() || !_currentRow.empty())
	{
		finishField();
	}

	if (!_currentRow.empty())
	{
		_gridQueue.push(_currentRow);
		_currentRow.clear();
	}
	_rowFinished = false;
}

void CSVParser::replaceLineEndings(string& field) const
{
	boost::algorithm::replace_all(field, "\r\n", " ");
	boost::algorithm::replace_all(field, "\r", " ");
	boost::algorithm::replace_all(field, "\n", " ");
}
