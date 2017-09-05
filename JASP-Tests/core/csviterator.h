//
// Copyright (C) 2017 University of Amsterdam
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

#ifndef CSVITERATOR_H
#define CSVITERATOR_H

#include <iterator>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>

static std::string trim(const std::string& str, const std::string& whitespace = " \t")
{
  const auto strBegin = str.find_first_not_of(whitespace);

  if (strBegin == std::string::npos)
  {
    return "";
  }

  const auto strEnd = str.find_last_not_of(whitespace);
  const auto strRange = strEnd - strBegin + 1;

  return str.substr(strBegin, strRange);
}

class CSVRow
{
    public:
        std::string const& operator[](std::size_t index) const
        {
            return currentWord[index];
        }
        std::size_t size() const
        {
            return currentWord.size();
        }
        void readNextRow(std::istream& str)
        {
            std::string line;
            std::getline(str,line);

            std::stringstream lineStream(line);
            std::string cell;

            currentWord.clear();
            while(std::getline(lineStream,cell,','))
            {
                currentWord.push_back(trim(cell));
            }
        }
    private:
        std::vector<std::string> currentWord;
};

static std::istream& operator>>(std::istream& str,CSVRow& data)
{
    data.readNextRow(str);
    return str;
} 

class CSVIterator
{   
    public:
        typedef std::input_iterator_tag iterator_category;
        typedef CSVRow value_type;
        typedef std::size_t difference_type;
        typedef CSVRow* pointer;
        typedef CSVRow& reference;

        CSVIterator(std::istream& str):currentString(str.good()?&str:NULL) 
        { 
            ++(*this); 
        }
        CSVIterator():currentString(NULL) {}

        // Pre Increment
        CSVIterator& operator++() 
        {
            if (currentString) 
            { 
                (*currentString) >> m_row;
                currentString = currentString->good()?currentString:NULL;
            }
            return *this;
        }
        // Post increment
        CSVIterator operator++(int)
        {
            CSVIterator temp(*this);
            ++(*this);
            return temp;
        }
        CSVRow const& operator*() const 
        {
            return m_row;
        }
        CSVRow const* operator->() const 
        {
            return &m_row;
        }

        bool operator==(CSVIterator const& rightHandSide) 
        {
            return ((this == &rightHandSide) || ((this->currentString == NULL) && (rightHandSide.currentString == NULL)));
        }
        bool operator!=(CSVIterator const& rightHandSide) 
        {
            return !((*this) == rightHandSide);
        }
    private:
        std::istream* currentString;
        CSVRow m_row;
};

#endif // CSVITERATOR_H
