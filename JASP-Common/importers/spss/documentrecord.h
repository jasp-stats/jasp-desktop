#ifndef DOCUMENTRECORD_H
#define DOCUMENTRECORD_H

#include "systemfileformat.h"
#include "readablerecord.h"
namespace spss
{

/**
 * @brief The DocumentRecord class : decodes documents
 *
 * Associated with record type rectype_document = 6
 */
class DocumentRecord : public ReadableRecord<rectype_document>
{
public:

	/**
	 * @brief DocumentRecord Ctor
	 * @param fileType The record type value, as found in the file.
	 * @param fromStream The file to read from.
	 *
	 */
	DocumentRecord(RecordTypes fileType, SPSSStream &fromStream);

	virtual ~DocumentRecord();

	SPSSIMPORTER_READ_ATTRIB(int32_t,					n_lines)
	SPSSIMPORTER_READ_ATTRIB(std::vector< std::string >, lines)

	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
	virtual void process(SPSSColumns & columns);

	static const size_t LINE_LENGTH = 80;
};

}

#endif // DOCUMENTRECORD_H
