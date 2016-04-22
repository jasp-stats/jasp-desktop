#ifndef FILEHEADERRECORD_H
#define FILEHEADERRECORD_H

#include "readablerecord.h"

namespace spss
{

/**
 * @brief The FileHeader class: decodes file header records.
 *
 * Associated with record type rectype_file_header = 0x324C4624/"$FL2"
 */
class FileHeaderRecord : public ReadableRecord< rectype_file_header >
{
public:

	/**
	 * @brief FileHeader Read from file.
	 * @param fileType The record type value, as found in the file.
	 * @param fromStream file to read.
	 * @param double expectedBias The bias value expected.
	 *
	 * An exception is thrown if the bias value read is not the
	 * same as passed - This is method used to check that the
	 * file uses the same floating point format.
	 */
	FileHeaderRecord(RecordTypes fileType, SPSSStream &fromStream, double expectedBias = 100.0);

	virtual ~FileHeaderRecord();

	enum e_compressed
	{
		compression_none = 0,
		compression_bytecode = 1,
		compression_zlib = 3
	};

	/*
	 * Data elements...
	 */
	SPSSIMPORTER_READ_ATTRIB(Char_60, prod_name)
	SPSSIMPORTER_READ_ATTRIB(int32_t,layout_code)
	SPSSIMPORTER_READ_ATTRIB(int32_t, nominal_case_size)
	SPSSIMPORTER_READ_ATTRIB(int32_t, compressed)
	SPSSIMPORTER_READ_ATTRIB(int32_t, weight_index)
	SPSSIMPORTER_READ_ATTRIB(int32_t, ncases)
	SPSSIMPORTER_READ_ATTRIB(double, bias)
	SPSSIMPORTER_READ_ATTRIB(Char_9, creation_date)
	SPSSIMPORTER_READ_ATTRIB(Char_8, creation_time)
	SPSSIMPORTER_READ_ATTRIB(Char_64, file_label)
	SPSSIMPORTER_READ_ATTRIB(Char_3, padding)

	/**
	 * @brief SPSSIMPORTER_READ_ATTRIB The number of variable records found to date.
	 */
	SPSSIMPORTER_READ_ATTRIB(size_t, varRecordCount)

	/**
	 * @brief incVarRecordCount Found another one!
	 * @return new value
	 */
	int32_t incVarRecordCount() { return _varRecordCount++; }

	/**
	 * @brief biasIsAcceptable Checks for an expected bias value.
	 * @param value The expected value.
	 * @return True if the \code bias \endcode == \code value \endcode
	 */
	bool biasIsAcceptable(double value = 100.0) const { return (bias() == value); }

	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
	virtual void process(SPSSColumns & columns);
};

}

#endif // FILEHEADERRECORD_H
