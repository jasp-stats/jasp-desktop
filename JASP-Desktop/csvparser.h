#ifndef CSVPARSER_H
#define CSVPARSER_H

#include <iterator>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>

class CSVParser
{
    public:
        CSVParser();
        std::string const& operator[](std::size_t index) const;
        std::size_t size() const;
        void readNextRow(std::istream& str);

    private:
        std::vector<std::string>    m_data;
};
#endif // CSVPARSER_H
