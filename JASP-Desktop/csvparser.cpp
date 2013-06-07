#include "csvparser.h"

#include "boost/tokenizer.hpp"

#include <QDebug>

using namespace std;
using namespace boost;

CSVParser::CSVParser()
{
}

string const& CSVParser::operator[](size_t index) const
{
    return m_data[index];
}

size_t CSVParser::size() const
{
    return m_data.size();
}

typedef tokenizer<escaped_list_separator<char> >::iterator charItr;

void CSVParser::readNextRow(istream& stream)
{
    string line;

    m_data.clear();

    getline(stream, line);

    if ( ! stream.eof()) {

        tokenizer<escaped_list_separator<char> > tk(line, escaped_list_separator<char>('\\', ',', '\"'));

        for (charItr itr(tk.begin()); itr != tk.end(); ++itr)
        {
            //qDebug() << ((string)*itr).c_str();
            m_data.push_back(*itr);
        }



    }

}

istream& operator>>(istream& str, CSVParser& data)
{
    data.readNextRow(str);
    return str;
}

/*int main()
{
    std::ifstream       file("plop.csv");

    CSVParser              row;
    while(file >> row)
    {
        std::cout << "4th Element(" << row[3] << ")\n";
    }
}*/
