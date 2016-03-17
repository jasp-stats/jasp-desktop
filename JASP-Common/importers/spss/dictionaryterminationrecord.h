#ifndef DICTIONARYTERMINATIONRECORD_H
#define DICTIONARYTERMINATIONRECORD_H



#include "readablerecord.h"

namespace spss
{

/**
 * @brief The DictionaryTermination class Deacode diationery termination (place holder)
 *
 * Assocateed with record type  rectype_dict_term = 999
 *
 */
class DictionaryTermination : public ReadableRecord<rectype_dict_term>
{
public:

	/**
	 * @brief DictionaryTermination Ctor
	 * @param fileType The record type value, as found in the file.
	 * @param fromStream File to read from
	 */
    DictionaryTermination(RecordTypes fileType, SPSSStream &fromStream);

    virtual ~DictionaryTermination();

	/*
	 * Dummy data.
	 */
    SPSSIMPORTER_READ_ATTRIB(int32_t, filler);

	/**
	 * @brief process Manipulates columns by adding the contents of thie record.
	 * @param columns
	 *
	 * Implematations should examine columns to determine the record history.
	 */
    virtual void process(SPSSColumns & columns);
};

}
#endif // DICTIONARYTERMINATIONRECORD_H
