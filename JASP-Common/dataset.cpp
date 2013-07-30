
#include "dataset.h"


#include <QDebug>

#include <vector>
#include <string>
#include <algorithm>
#include <climits>

#include "boost/foreach.hpp"
#include "boost/lexical_cast.hpp"

#include "csvparser.h"
#include "column.h"

using boost::lexical_cast;
using boost::interprocess::anonymous_instance;

/*
 * DataSet is implemented as a set of columns
 */

DataSet::DataSet(boost::interprocess::managed_shared_memory *mem) :
	_columns(mem)
{
	_rowCount = 0;
	_columnCount = 0;
}

DataSet::DataSet(boost::interprocess::managed_shared_memory *mem, std::vector<string> *columnInfo, std::vector<std::vector<string> > *data) :
	_columns(mem)
{
	_mem = mem;

	if (columnInfo->size() == 0)
	{
		_rowCount = 0;
		_columnCount = 0;

		return;
	}

	_columnCount = columnInfo->size();
	_rowCount = data->at(0).size();

	BOOST_FOREACH(string &columnName, *columnInfo)
	{
		Column column(mem);
		column.setName(columnName);

		_columns._columnStore.push_back(column);
	}

	int colNo = 0;

	BOOST_FOREACH(Column &column, _columns)
	{
		column.append(_rowCount);

		vector<string> &columnRows = data->at(colNo++);

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
}

Columns &DataSet::columns()
{
	return _columns;
}

void DataSet::setRowCount(int rowCount)
{
	_columns.setRowCount(rowCount);
}

int DataSet::rowCount()
{
	return _rowCount;
}

int DataSet::columnCount()
{
	return _columnCount;
}

