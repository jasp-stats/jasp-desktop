
#include "csvimporter.h"

#include <boost/foreach.hpp>
#include <boost/lexical_cast.hpp>

#include "sharedmemory.h"
#include "dataset.h"
#include "csv.h"

using boost::lexical_cast;
using namespace boost::interprocess;
using namespace std;

DataSet* CSVImporter::loadDataSet(const string &locator, boost::function<void(const string &, int)> progressCallback)
{
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
			progressCallback("Loading Data Set", progress);
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

	DataSet *dataSet = SharedMemory::createDataSet();

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
			dataSet = SharedMemory::enlargeDataSet(dataSet);
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


	for (int colNo = 0; colNo < dataSet->columnCount(); colNo++)
	{
		bool success = true;

		do {

			try {

				progressCallback("Loading Data Set", 50 + 50 * colNo / dataSet->columnCount());

				string columnName = columns.at(colNo);

				if (columnName == "")
				{
					stringstream ss;
					ss << "V";
					ss << (colNo + 1);
					columnName = ss.str();
				}

				Column &column = dataSet->column(colNo);
				initColumn(column, columnName, cells.at(colNo));

			}
			catch (boost::interprocess::bad_alloc &e)
			{
				dataSet = SharedMemory::enlargeDataSet(dataSet);
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

		} while (success == false);
	}

	return dataSet;
}


void CSVImporter::initColumn(Column &column, const string &name, const vector<string> &cells)
{
	// we treat single spaces as missing values, because SPSS saves missing values as a single space in CSV files

	column.setName(name);

	// try to make the column nominal

	bool success = true;
	set<int> uniqueValues;
	Column::Ints::iterator intInputItr = column.AsInts.begin();
	Labels &labels = column.labels();
	labels.clear();

	BOOST_FOREACH(const string &value, cells)
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
		labels.clear();

		BOOST_FOREACH(int value, uniqueValues)
		{
			(void)uniqueValues;
			labels.add(value);
		}

		column._columnType = Column::ColumnTypeNominal;

		return;
	}

	// try to make the column scale

	Column::Doubles::iterator doubleInputItr = column.AsDoubles.begin();
	success = true;

	BOOST_FOREACH(const string &value, cells)
	{
		string v = deEuropeanise(value);

		if (v != "" && v != " ")
		{
			try
			{
				*doubleInputItr = lexical_cast<double>(v);
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
		return;
	}

	// if it can't be made nominal numeric or scale, make it nominal-text

	vector<string> sorted = cells;
	sort(sorted.begin(), sorted.end());
	vector<string> cases;
	unique_copy(sorted.begin(), sorted.end(), back_inserter(cases));
	sort(cases.begin(), cases.end());

	for (vector<string>::iterator itr = cases.begin(); itr != cases.end(); itr++)
	{
		if (*itr == "") // remove empty string
		{
			cases.erase(itr);
			break;
		}
	}

	for (vector<string>::iterator itr = cases.begin(); itr != cases.end(); itr++)
	{
		if (*itr == " ") // remove empty string
		{
			cases.erase(itr);
			break;
		}
	}

	labels.clear();

	BOOST_FOREACH (string &value, cases)
		labels.add(value);

	intInputItr = column.AsInts.begin();

	BOOST_FOREACH (const string &value, cells)
	{
		if (value == "" || value == " ")
			*intInputItr = INT_MIN;
		else
			*intInputItr = distance(cases.begin(), find(cases.begin(), cases.end(), value));

		intInputItr++;
	}

	column._columnType = Column::ColumnTypeNominalText;
}


string CSVImporter::deEuropeanise(const string &value)
{
	int dots = 0;
	int commas = 0;

	for (int i = 0; i < value.length(); i++)
	{
		if (value[i] == '.')
			dots++;
		else if (value[i] == ',')
			commas++;
	}

	if (commas > 0)
	{
		string uneurope = value;

		if (dots > 0)
		{
			int i = 0;
			int j = 0;

			for (;i < value.size(); i++)
			{
				if (value[i] == '.')
					continue;
				uneurope[j] = value[i];

				j++;
			}

			uneurope.resize(j);
		}

		for (int i = 0; i < uneurope.length(); i++)
		{
			if (uneurope[i] == ',')
			{
				uneurope[i] = '.';
				break;
			}
		}

		return uneurope;
	}

	return value;
}

