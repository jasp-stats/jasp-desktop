//
// Copyright (C) 2013-2016 University of Amsterdam
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

#ifndef SPSSIMPORTER_H
#define SPSSIMPORTER_H

//#include "datasetpackage.h"
#include "./spss/fileheaderrecord.h"
#include "./spss/integerinforecord.h"
#include <boost/function.hpp>
#include <boost/nowide/fstream.hpp>
#include <string>

#include "spss/systemfileformat.h"
#include "column.h"


/*
 * built with information from
 *
 * http://www.gnu.org/software/pspp/pspp-dev/html_node/System-File-Format.html
 */


class SPSSImporter
{
public:

    static void loadDataSet(DataSetPackage *packageData, const std::string &locator, boost::function<void (const std::string &, int)> progress);

    /**
	* @brief ReportProgress Reports progress for stream.
	* @param position Position to report.
	* @param progress report to here.
	*/
    static void reportFileProgress(SPSSStream::pos_type position, boost::function<void (const std::string &, int)> progress);

protected:

    /**
	* @brief setDataSetSize Sets the data set size in the passed
	* @param dataSetPg The Data Set Package to manipluate.
	* @param rowCount The (real) number of rows we have.
	* @param colCount The number of columns.
	*/
    static void setDataSetSize(DataSetPackage &dataSetPg, size_t rowCount, size_t colCount);

	/**
	 * @brief setColumnStringData Sets String data into the column, after doing a code page convert.
	 * @param column The columns to insert into.
	 * @param strConvertor The string converter to use.
	 * @param spssCol The Source of the data.
	 */
	static void setColumnConvrtStringData(Column &column, CodePageConvert &strConvertor, spss::SPSSColumn &spssCol);

	/**
	 * @brief setColumnLabeledData Sets numeric data into the column, with labels.
	 * @param column The columns to insert into.
	 * @param numCases The number of cases.
	 * @param spssCol The Source of the data.
	 */
	static void setColumnLabeledData(Column &column, size_t numCases, const spss::SPSSColumn &spssCol);

	/**
	 * @brief setColumnScaleData Sets floating point / scalar data into the column.
	 * @param column The columns to insert into.
	 * @param numCases The number of cases
	 * @param spssCol The Source of the data.
	 */
	static void setColumnScaleData(Column &column, size_t numCases, const spss::SPSSColumn &spssCol);

private:

    /**
	* @brief killFhr Kills the FileHeaderRecord instance.
	*/
    static void killFhr();

    /**
	* @brief m_pFhr A (pointer to a) File header.
	*/
	static spss::FileHeaderRecord *_pFileHeaderRecord;

    /*
	* Last found Integer info record, or default values.
	*/
	static spss::IntegerInfoRecord _integerInfo;

    /*
	* Last found Float info, or default.
	*/
    static spss::FloatInfoRecord _floatInfo;

    /*
	* Size of the file imported.
	*/
    static double _fileSize;
};



#if 0
#include <boost/nowide/fstream.hpp>
#include <iostream>

/*
 * built with information from
 *
 * http://www.gnu.org/software/spss/pspp-dev/html_node/System-File-Format.html
 */

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
#endif // defined(0)

#endif // SPSSIMPORTER_H
