
#include "datasetloader.h"

#include <boost/foreach.hpp>

#include "boost/lexical_cast.hpp"
#include "sys/stat.h"

#include "dataset.h"
#include "csv.h"

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

	CSV csv(locator);
	csv.open();

	vector<string> columns = vector<string>();
	vector<vector<string> > cells = vector<vector<string> >();

	csv.readLine(columns);

	unsigned long long progress;
	unsigned long long lastProgress = -1;

	int columnCount = columns.size();

	for (int i = 0; i < columnCount; i++)  // columns
		cells.push_back(vector<string>());

	vector<string> line;
	bool success = csv.readLine(line);

	while (success)
    {
		progress = 50 * csv.pos() / csv.size();
		if (progress != lastProgress)
		{
			this->progress("Loading Data Set", progress);
			lastProgress = progress;
		}

        int i = 0;
		for (; i < line.size() && i < columnCount; i++)
			cells[i].push_back(line[i]);
        for (; i < columnCount; i++)
			cells[i].push_back(string());

		line.clear();
		success = csv.readLine(line);
    }	

	managed_shared_memory* mem = SharedMemory::get(true);

	DataSet *dataSet = mem->construct<DataSet>(boost::interprocess::unique_instance)();

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
			//cout << "growing shared memory\n";
			//cout.flush();

			try {

				//cout << mem->get_size() << "\n";
				mem = SharedMemory::grow(mem->get_size());
				//cout << mem->get_size() << "\n";
				//cout.flush();

				dataSet = mem->find<DataSet>(boost::interprocess::unique_instance).first;

			}
			catch (exception &e)
			{
				cout << e.what();
				cout.flush();
			}

			//cout << "memory grown\n";
			//cout.flush();

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


	int colNo = 0;
	BOOST_FOREACH(Column &column, dataSet->columns())
	{
		this->progress("Loading Data Set", 50 + 50 * colNo / dataSet->columnCount());

		column.setName(columns.at(colNo));

		vector<string> &columnRows = cells.at(colNo);

		colNo++;

		// try to make the column nominal

		bool success = true;
		set<int> uniqueValues;
		Column::Ints::iterator intInputItr = column.AsInts.begin();

		BOOST_FOREACH(string &value, columnRows)
		{
			if (value != "NaN" && value != "nan" && value != "" && value != " ")
			{
				try
				{
					int v = lexical_cast<int>(value);
					uniqueValues.insert(v);
					*intInputItr = v;
				}
				catch (...)
				{
					// column can't be made nominal numeric

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

		if (success && uniqueValues.size() <= 24)
		{
			map<int, int> rawToActual;
			map<int, int> actualToRaw;

			int index = 0;
			BOOST_FOREACH(int value, uniqueValues)
			{
				(void)uniqueValues;
				rawToActual[index] = value;
				actualToRaw[value] = index;
				index++;
			}

			Column::Ints::iterator intInputItr = column.AsInts.begin();
			for (; intInputItr != column.AsInts.end(); intInputItr++)
			{
				int actual = *intInputItr;
				if (actual != INT_MIN)
					*intInputItr = actualToRaw.at(actual);
			}

			column._columnType = Column::ColumnTypeNominal;
			column.setLabels(rawToActual);

			continue;
		}

		// try to make the column scale

		Column::Doubles::iterator doubleInputItr = column.AsDoubles.begin();
		success = true;

		BOOST_FOREACH(string &value, columnRows)
		{
			if (value != "" && value != " ")
			{
				try
				{
					*doubleInputItr = lexical_cast<double>(value);
				}
				catch (...)
				{
					// column can't be made scale

					success = false;
					break;
				}
			}
			else
			{
				*doubleInputItr = NAN;
			}

			doubleInputItr++;
		}

		if (success)
		{
			column._columnType = Column::ColumnTypeScale;
			continue;
		}

		// if it can't be made nominal numeric or scale, try and make it nominal-text

		vector<string> inColumn = columnRows;
		sort(inColumn.begin(), inColumn.end());
		vector<string> cases;
		unique_copy(inColumn.begin(), inColumn.end(), back_inserter(cases));
		sort(cases.begin(), cases.end());

		for (vector<string>::iterator itr = cases.begin(); itr != cases.end(); itr++)
		{
			if (*itr == "") // remove empty string
			{
				cases.erase(itr);
				break;
			}
		}

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
			if (value == "")
				*intInputItr = INT_MIN;
			else
				*intInputItr = distance(cases.begin(), find(cases.begin(), cases.end(), value));

			intInputItr++;
		}

		column._columnType = Column::ColumnTypeNominalText;
	}

	return dataSet;
}

void DataSetLoader::freeDataSet(DataSet *dataSet)
{
	SharedMemory::get()->destroy_ptr(dataSet);
}
