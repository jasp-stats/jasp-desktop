#ifndef DATARECORDS_H
#define DATARECORDS_H

#include "systemfileformat.h"
#include "fileheaderrecord.h"

namespace spss {

/**
 * @brief The DataRecords class
 *  Decodes both compressed and plain variables.
 */
class DataRecords {

public:
	/**
	 * @brief DataRecords ctor
	 * @param fileHeader File header record.
	 * @param columns The columns data we collected readling the headers.
	 * @param fromStream The stream to read.
	 * @param progress Report progress call back.
	 */
	DataRecords(const FileHeaderRecord &fileHeader, SPSSColumns &columns, SPSSStream &fromStream,
				boost::function<void (const std::string &, int)> &progress);


	/**
	 * @brief read Reads the values to the dataset.
	 * @param dataSet The data set to write.
	 */
	void read(/* OUT */ DataSetPackage *dataSet);

	size_t numDbls() const { return _numDbls; }
	 size_t numStrs() const { return _numStrs; }

protected:
	/*
	 * From ctor()
	 */
	const FileHeaderRecord 	&_fileHeader;
	SPSSColumns 			   &_cols;
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
	 * @brief readCompressed - Reads compressed data,
	 * @param dataSet The data set to write.
	 */
	void readCompressed(/* OUT */ DataSetPackage *dataSet);

	/**
	 * @brief readUncompressed - Reads uncompressed data,
	 * @param dataSet The data set to write.
	 */
	void readUncompressed(/* OUT */ DataSetPackage *dataSet);

private:


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
	void insertToCol(SPSSColumn &col, const std::string &str);

	/**
	 * @brief insertToCol Inserts a string into the (next) column.
	 * @param col The colum to insert into.
	 * @param value The value to insert
	 */
	void insertToCol(SPSSColumn &col, double value);

	/**
	 * @brief readUnCompVal Reads in and stores a single data value
	 * @param col the cilum to insert into.
	 */
	void readUnCompVal(SPSSColumn &col);

};


}

#endif // DATARECORDS_H
