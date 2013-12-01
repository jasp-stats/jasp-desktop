#include "datasetloader.h"

#include "dataset.h"
#include "csvparser.h"

#include <QDebug>

#include <boost/foreach.hpp>

#include "boost/lexical_cast.hpp"
#include "sys/stat.h"

using boost::lexical_cast;
using namespace boost::interprocess;
using namespace std;

DataSetLoader::DataSetLoader()
{
}

DataSet* DataSetLoader::loadDataSet(const string &locator)
{
	struct stat fileInfo;
	stat(locator.c_str(), &fileInfo);

	int fileSize = fileInfo.st_size;

	ifstream is;
	is.open(locator.c_str(), ios::in);

	CSVParser parser;
	vector<string> columns = vector<string>();
	vector<vector<string> > cells = vector<vector<string> >();

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


	unsigned long long progress;
	unsigned long long lastProgress = -1;

	int columnCount = parser.size();

    for (int i = 0; i < columnCount; i++)
    {
		columns.push_back(parser[i]);
		cells.push_back(vector<string>());
    }

	parser.readNextRow(is);

    while (parser.size() > 0)
    {
		progress = 50 * is.tellg() / fileSize;
		if (progress != lastProgress)
		{
			this->progress("Loading Data Set", progress);
			lastProgress = progress;
		}

        int i = 0;
        for (; i < parser.size() && i < columnCount; i++)
			cells[i].push_back(parser[i]);
        for (; i < columnCount; i++)
			cells[i].push_back(string());

		parser.readNextRow(is);
    }	

	if (SharedMemory::isCreatedRW() == false)
		SharedMemory::createRW();

	managed_shared_memory* mem = SharedMemory::get();

	DataSet *dataSet = mem->construct<DataSet>(boost::interprocess::unique_instance)();

	bool success;

	do
	{
		try {

			success = true;

			dataSet->setColumnCount(columnCount);
			if (cells.size() > 0)
				dataSet->setRowCount(cells.at(0).size());

		}
		catch (boost::interprocess::bad_alloc &e)
		{
			cout << "growing shared memory\n";
			cout.flush();

			try {

				cout << mem->get_size() << "\n";
				mem = SharedMemory::grow(mem->get_size());
				cout << mem->get_size() << "\n";
				cout.flush();

				dataSet = mem->find<DataSet>(boost::interprocess::unique_instance).first;

			}
			catch (exception &e)
			{
				cout << e.what();
				cout.flush();
			}

			cout << "memory grown\n";
			cout.flush();

			success = false;
		}
		catch (exception e)
		{
			cout << "n " << e.what();
			cout.flush();
		}
		catch (...)
		{
			cout << "something else\n ";
			cout.flush();
		}
	}
	while ( ! success);

	cout << "success!\n";
	cout.flush();

	int colNo = 0;
	BOOST_FOREACH(Column &column, dataSet->columns())
	{
		this->progress("Loading Data Set", 50 + 50 * colNo / dataSet->columnCount());

		column.setName(columns.at(colNo));

		vector<string> &columnRows = cells.at(colNo);

		colNo++;

		Column::Ints::iterator intInputItr = column.AsInts.begin();
		bool success = true;

		BOOST_FOREACH(string &value, columnRows)
		{
			if (value != "NaN" && value != "nan" && value != "")
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
		{
			column._dataType = Column::DataTypeInt;
			column._columnType = Column::ColumnTypeOrdinal;
			column._columnTypesAllowed = (Column::ColumnType)(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
			continue;
		}

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
			column._dataType = Column::DataTypeDouble;
			column._columnType = Column::ColumnTypeScale;
			column._columnTypesAllowed = Column::ColumnTypeScale;
			continue;
		}

		vector<string> inColumn = columnRows;
		sort(inColumn.begin(), inColumn.end());
		vector<string> cases;
		unique_copy(inColumn.begin(), inColumn.end(), back_inserter(cases));

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

		column._dataType = Column::DataTypeInt;
		column._columnType = Column::ColumnTypeNominal;
		column._columnTypesAllowed = Column::ColumnTypeNominal;
	}

	return dataSet;
}

void DataSetLoader::freeDataSet(DataSet *dataSet)
{
	SharedMemory::get()->destroy_ptr(dataSet);
}
