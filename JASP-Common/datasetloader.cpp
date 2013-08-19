#include "datasetloader.h"

#include "dataset.h"
#include "csvparser.h"

#include <QDebug>

#include <boost/foreach.hpp>

#include "boost/lexical_cast.hpp"

using boost::lexical_cast;
using namespace boost::interprocess;

DataSetLoader::DataSetLoader()
{
}

DataSet* DataSetLoader::loadFile(istream &is) {

	CSVParser parser;
	std::vector<string> columns = std::vector<string>();
	std::vector< std::vector<string> > cells = std::vector<std::vector<string> >();

    parser.readNextRow(is);

	/*int bufferSize = 2048;
	char buffer[bufferSize];

	int count = is.readsome(buffer, bufferSize);

	if (count < 3 || (buffer[0] == 0xEF && buffer[1] == 0xBB && buffer[2] == 0xBF))
	{
		// UTF8
	}
	else if (buffer[0] == 0xFF && buffer[1] == 0xFE)
	{
		// UTF16LE
	}
	else if (buffer[0] == 0xFE && buffer[1] == 0xFF)
	{
		// UTF16BE
	}
	else if (buffer[0] == 0xFF && buffer[1] == 0xFE && buffer[2] == 0 && buffer[3] == 0)
	{
		// UTF32LE
	}
	else if (buffer[0] == 0 && buffer[1] == 0 && buffer[2] == 0xFE && buffer[3] == 0xFF)
	{
		// UTF32BE
	}*/


    int columnCount = parser.size();

    for (int i = 0; i < columnCount; i++)
    {
		columns.push_back(parser[i]);
		cells.push_back(std::vector<string>());
    }

    parser.readNextRow(is);

    while (parser.size() > 0)
    {
        int i = 0;
        for (; i < parser.size() && i < columnCount; i++)
			cells[i].push_back(parser[i]);
        for (; i < columnCount; i++)
			cells[i].push_back(string());

        parser.readNextRow(is);
    }

	managed_shared_memory* mem = SharedMemory::create();

	DataSet *dataSet = mem->construct<DataSet>(boost::interprocess::unique_instance)(mem);

	dataSet->setColumnCount(columnCount);
	if (cells.size() > 0)
		dataSet->setRowCount(cells.at(0).size());

	int colNo = 0;
	BOOST_FOREACH(Column &column, dataSet->columns())
	{
		column.setName(columns.at(colNo));

		vector<string> &columnRows = cells.at(colNo);

		colNo++;

		Column::Ints::iterator intInputItr = column.AsInts.begin();
		bool success = true;

		BOOST_FOREACH(string &value, columnRows)
		{
			if (value != "NaN" && value != "")
			{
				try
				{
					*intInputItr = lexical_cast<int>(value);
				}
				catch (...)
				{
					success = false;
					break;
				}
			}
			else
			{
				*intInputItr = INT_MIN;
			}

			intInputItr++;
		}

		if (success)
			continue;

		Column::Doubles::iterator doubleInputItr = column.AsDoubles.begin();
		success = true;

		BOOST_FOREACH(string &value, columnRows)
		{
			try
			{
				*doubleInputItr = lexical_cast<double>(value);
				doubleInputItr++;
			}
			catch (...)
			{
				success = false;
				break;
			}
		}

		if (success)
		{
			column._columnType = Column::DoubleColumnType;
			continue;
		}

		vector<string> inColumn = columnRows;
		sort(inColumn.begin(), inColumn.end());
		vector<string> cases;
		std::unique_copy(inColumn.begin(), inColumn.end(), back_inserter(cases));

		std::map<int, string> casesMap;
		int i = 0;

		BOOST_FOREACH (string &value, cases)
		{
			pair<int, string> p(i, value);
			casesMap.insert(p);
			i++;
		}

		column.setLabels(casesMap);

		intInputItr = column.AsInts.begin();

		BOOST_FOREACH (string &value, columnRows)
		{
			*intInputItr = distance(cases.begin(), find(cases.begin(), cases.end(), value));
			intInputItr++;
		}
	}

	return dataSet;

	//return mem->construct<DataSet>(boost::interprocess::unique_instance)(mem, &columns, &cells);
}
