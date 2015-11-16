//
// Copyright (C) 2015 University of Amsterdam
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

#include "spssimporter.h"

#include "sharedmemory.h"
#include "dataset.h"

#include <boost/nowide/fstream.hpp>

using namespace std;
using namespace boost;

void SPSSImporter::loadDataSet(DataSetPackage *packageData, const string &locator, boost::function<void (const string &, int)> progress)
{
	DataSet *dataSet = SharedMemory::createDataSet();

	(void)progress;

	nowide::ifstream stream(locator.c_str(), ios::in);

	int rowCount;
	stream.seekg(80);
	stream.read((char*)&rowCount, sizeof(int));

	stream.seekg(176);

	vector<SPSSColumn> columns;

	readHeaders(stream, columns);
	readData(stream, columns);

	dataSet = setDataSetSize(dataSet, rowCount, columns.size());

	for (int i = 0; i < columns.size(); i++)
	{
		SPSSColumn &spss = columns[i];
		Column &column = dataSet->column(i);

		column.setName(spss.name);

		if (spss.isStrings == false)
		{
			column.setColumnType(Column::ColumnTypeScale);

			Column::Doubles::iterator itr = column.AsDoubles.begin();

			for (int j = 0; j < rowCount; j++)
				*itr++ = spss.numeric.at(j);
		}
	}

	packageData->isArchive = false;
	packageData->dataSet = dataSet;
}

typedef enum { VariableRecord = 2, LabelRecord = 3, DataRecord = 7, EndHeader = 999 } RecordType;

void SPSSImporter::readHeaders(istream &stream, vector<SPSSColumn> &columns)
{
	while (true)
	{
		int recordType;
		stream.read((char*)&recordType, sizeof(recordType));

		if ( ! stream.good())
			throw runtime_error("unexpected EOF");

		std::cout << "recordType: " << recordType << "@" << stream.tellg() << "\n";
		std::cout.flush();

		switch (recordType)
		{
		case VariableRecord:
			readColumnInfoRecord(stream, columns);
			break;
		case LabelRecord:
			readLabelRecord(stream);
			break;
		case DataRecord:
			readDataRecord(stream);
			break;
		case EndHeader:
			stream.seekg(sizeof(int), ios_base::cur); // skip padding
			return;
		default:
			std::cout << "unknown record type: " << recordType << "@" << stream.tellg() << "\n";
			std::cout.flush();
			return;
		}
	}
}

void SPSSImporter::readColumnInfoRecord(istream &stream, std::vector<SPSSColumn> &columns)
{
	// record type already skipped by readNextRecord

	char buffer[28];

	stream.read(buffer, sizeof(buffer));

	int type = *(int*)&buffer;

	if (type == -1)
	{
		if (columns.size() == 0)
			throw runtime_error("-1 subtype not expected at beginning");

		columns[columns.size() - 1].columnSpan++;

		return;
	}

	int hasVarLabel = *(int*)&buffer[4];
	int missingCount = *(int*)&buffer[8];
	char *name = &buffer[20];
	string columnName = string(name, 8);

	if (hasVarLabel)
	{
		int labelLength;
		stream.read((char*)&labelLength, sizeof(int));
		char name[labelLength + 2];

		stream.read(name, labelLength + 2); // consume an additional 2 spaces, not sure why
		columnName = string(name, labelLength);
	}

	SPSSColumn c;
	c.name = columnName;
	c.columnSpan = 1;
	c.isStrings = type != 0;

	columns.push_back(c);

	if (missingCount > 0)
		stream.seekg(8 * missingCount, ios_base::cur);
}

void SPSSImporter::readDataRecord(istream &stream)
{
	char header[12];

	stream.read(header, sizeof(header));
	if (stream.gcount() != 12)
		throw std::runtime_error("unexpected EOF");

	int subType = *(int*)&header[0];
	int siz = *(int*)&header[4];
	int count = *(int*)&header[8];

	stream.seekg(siz * count, ios_base::cur);
}

void SPSSImporter::readLabelRecord(istream &stream)
{
	int labelCount;
	stream.read((char*)&labelCount, sizeof(int));

	for (int i = 0; i < labelCount; i++)
	{
		unsigned char labelSize;
		stream.seekg(8, ios_base::cur);
		stream.read((char*)&labelSize, sizeof(char));

		int padding = 8 - ((labelSize + 1) % 8);

		stream.seekg(labelSize + padding, ios_base::cur);
	}

	stream.seekg(4, ios_base::cur);
	int varCount;
	stream.read((char*)&varCount, sizeof(int));

	stream.seekg(sizeof(int) * varCount, ios_base::cur);
}

void SPSSImporter::readData(istream &stream, vector<SPSSColumn> &columns)
{
	unsigned char buffer[8];

	int index = 0;
	int subIndex = 0;

	stream.read((char*)buffer, sizeof(buffer));

	while (stream.eof() == false)
	{
		for (int i = 0; i < sizeof(buffer); i++)
		{
			SPSSColumn &column = columns[index];
			int v = buffer[i];

			if (v >= 1 && v <= 251)
			{
				columns[index].numeric.push_back(v - 100);
			}
			else if (v == 253)
			{
				if (column.isStrings)
				{
					char str[8];
					stream.read(str, sizeof(str));
					column.strings.push_back(string(str, sizeof(str)));
				}
				else
				{
					double value;
					stream.read((char*)&value, sizeof(double));
					column.numeric.push_back(value);
				}
			}
			else
			{
				column.numeric.push_back(numeric_limits<double>::signaling_NaN());
				column.strings.push_back("");
			}

			subIndex++;
			if (subIndex >= column.columnSpan)
			{
				subIndex = 0;
				index = (index + 1) % columns.size();
			}
		}

		stream.read((char*)buffer, sizeof(buffer));
	}
}

DataSet *SPSSImporter::setDataSetSize(DataSet *dataSet, int rowCount, int columnCount)
{
	bool success;

	do
	{
		try {

			success = true;

			dataSet->setColumnCount(columnCount);
			dataSet->setRowCount(rowCount);

		}
		catch (boost::interprocess::bad_alloc &e)
		{
			dataSet = SharedMemory::enlargeDataSet(dataSet);
			success = false;
		}
		catch (std::exception e)
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

	return dataSet;
}
