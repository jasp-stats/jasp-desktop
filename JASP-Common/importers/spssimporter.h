#ifndef SPSSIMPORTER_H
#define SPSSIMPORTER_H

#include "datasetpackage.h"
#include <boost/function.hpp>
#include <string>

#include <boost/nowide/fstream.hpp>
#include <iostream>

class SPSSImporter
{
private:
	typedef struct
	{
		std::string name;
		bool isStrings;
		int columnSpan;
		std::vector<double> numeric;
		std::vector<std::string> strings;

	} SPSSColumn;

public:

	static void loadDataSet(DataSetPackage *packageData, const std::string &locator, boost::function<void (const std::string &, int)> progress);
	static void readHeaders(std::istream &stream, std::vector<SPSSColumn> &columns);
	static void readColumnInfoRecord(std::istream &stream, std::vector<SPSSColumn> &columns);
	static void readDataRecord(std::istream &stream);
	static void readLabelRecord(std::istream &stream);
	static void readData(std::istream &stream, std::vector<SPSSColumn> &columns);
	static DataSet *setDataSetSize(DataSet *dataSet, int rowCount, int columnCount);

};

#endif // SPSSIMPORTER_H
