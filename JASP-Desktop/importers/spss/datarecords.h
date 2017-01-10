//
// Copyright (C) 2015-2017 University of Amsterdam
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

#ifndef DATARECORDS_H
#define DATARECORDS_H

#include "systemfileformat.h"
#include "fileheaderrecord.h"
#include "../spssimporter.h"
#include "spssimportcolumn.h"

namespace spss {

/**
 * @brief The DataRecords class
 *  Decodes both compressed and plain variables.
 */
class DataRecords {

public:
	/**
	 * @brief DataRecords ctor
	 * @param importer
	 * @param fixer - Fixes byte order for data.
	 * @param fileHeader File header record.
	 * @param columns The columns data we collected readling the headers.
	 * @param fromStream The stream to read.
	 * @param progress Report progress call back.
	 */
	DataRecords(SPSSImporter* importer, SPSSImportDataSet *dataset, const NumericConverter &fixer, const FileHeaderRecord &fileHeader, SPSSStream &fromStream,
				boost::function<void (const std::string &, int)> &progress);


	/**
	 * @brief read Reads the values to the dataset.
	 */
	void read();

	size_t numDbls() const { return _numDbls; }
	size_t numStrs() const { return _numStrs; }

protected:
	/*
	 * From ctor()
	 */
	SPSSImporter			*_importer;
	SPSSImportDataSet 		*_dataset;
	const FileHeaderRecord 	&_fileHeader;
	SPSSStream 				&_from;
	boost::function<void (const std::string &, int)> &_progress;

	enum e_knownCodes
	{
		code_ignore = 0,		   // Ignored / padding.
		code_eof = 252,			 // End of file.
		code_notCompressed = 253,   // A not compressed value.
		code_allSpaces = 254,	   // All spaces.
		code_systmMissing = 255 	// Syatem missing value
	};

	/**
	 * @brief readCompressed - Reads compressed data
	 */
	void readCompressed();

	/**
	 * @brief readUncompressed - Reads uncompressed data
	 */
	void readUncompressed();

private:
	/**
	 * Hold a copy of the fixer.
	 */
	const NumericConverter &_fixer;

	/**
	 * @brief _numDbls Number doubles read to date.
	 */
	size_t  _numDbls;

	/**
	 * @brief _numDbls Number string (cells) read to date.
	 */
	size_t  _numStrs;

	/**
	 * @brief insertToCol Insrts a string into the (next) column.
	 * @param col The colum to insert into.
	 * @param str The teing value to insert / append.
	 */
	void insertToCol(SPSSImportColumn &col, const std::string &str);

	/**
	 * @brief insertToCol Inserts a string into the (next) column.
	 * @param col The colum to insert into.
	 * @param value The value to insert
	 */
	void insertToCol(SPSSImportColumn &col, double value);

	/**
	 * @brief readUnCompVal Reads in and stores a single data value
	 * @param col the cilum to insert into.
	 */
	void readUnCompVal(SPSSImportColumn &col);

};


}

#endif // DATARECORDS_H
