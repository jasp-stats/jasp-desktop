#ifndef VARIBABLERECORD_H
#define VARIBABLERECORD_H

#include "readablerecord.h"
#include "fileheaderrecord.h"

namespace spss
{

/**
 * @brief The VariableRecord class : decodes variable records
 *
 * Associated with record type rectype_variable = 2
 */
class VariableRecord: public ReadableRecord<rectype_variable>
{
public:

	/**
	 * @brief VariableRecord Ctor
	 * @param fileType The record type value, as found in the file.
	 * @param fileHeader The file ehadewr we are associated with.
	 * @param fromStream The file to read from.
	 *
	 */
	VariableRecord(RecordTypes fileType, FileHeaderRecord * fileHeader, SPSSStream &fromStream);

	virtual ~VariableRecord();


	/*
	 * The data,
	 */
	SPSSIMPORTER_READ_ATTRIB(int32_t, rec_type)
	SPSSIMPORTER_READ_ATTRIB(int32_t, type)
	SPSSIMPORTER_READ_ATTRIB(int32_t, has_var_label)
	SPSSIMPORTER_READ_ATTRIB(int32_t, n_missing_values)
	SPSSIMPORTER_READ_ATTRIB(int32_t, print)
	SPSSIMPORTER_READ_ATTRIB(int32_t, write)
	SPSSIMPORTER_READ_ATTRIB(Char_8, name_file)
	SPSSIMPORTER_READ_ATTRIB(int32_t, label_len)
	SPSSIMPORTER_READ_ATTRIB(std::string, label) // label().length() == label_len
	SPSSIMPORTER_READ_ATTRIB(VecDbls, missing_values)

	// Not from the file, but from counting number times we are created,
	SPSSIMPORTER_READ_ATTRIB(size_t, dictionary_index)


	/*
	 * Write and Print fields are in fact unions:
	 * Inlines to break up extarct the fields;
	 */
	static unsigned char num_places(int32_t prField) { return ((prField >> 0) & 0xFF); }
	static unsigned char field_width(int32_t prField) { return ((prField >> 8) & 0xFF); }
	static unsigned char format_type(int32_t prField) { return ((prField >> 16) & 0xFF); }
	static unsigned char zero_filler(int32_t prField)  { return ((prField >> 24) & 0xFF); }

	/*
	 * Name of the variable, cleaned-up.
	 */
	const std::string &name() const { return _name; }

	/*
	 * Data checks,
	 */
	bool isNumeric() const { return (_type == 0); }
	bool isString() const { return ((_type != 0) && (_type != -1)); }
	bool isStringContinuation() const { return (_type == -1) && !hasVarLabel() && !hasMissingValues(); }
	bool hasVarLabel() const { return (_has_var_label == 1); }
	bool hasMissingValues() const { return (_n_missing_values != 0); }

	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
	virtual void process(SPSSColumns & columns);

private:
	const FileHeaderRecord *_pFHR;
	std::string  _name;
};

}

#endif // VARIBABLERECORD_H
