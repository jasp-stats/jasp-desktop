#ifndef VALUELABELVARSRECORD_H
#define VALUELABELVARSRECORD_H

#include "systemfileformat.h"
#include "readablerecord.h"

namespace spss
{

/**
 * @brief The ValueLabelRecord class : decodes label records and following variable
 *
 * Associated with record type rectype_value_labels = 3
 */
class ValueLabelVarsRecord: public ReadableRecord<rectype_value_labels>
{
public:

	/**
	 * @brief ValueLabelRecord Ctor
	 * @param const Converters &fixer - Fixexes endianness.
	 * @param fileType The record type value, as found in the file.
	 * @param fromStream The file to read from.
	 *
	 */
	ValueLabelVarsRecord(const NumericConverter &fixer, RecordTypes fileType, SPSSStream &fromStream);

	virtual ~ValueLabelVarsRecord();

	/*
	 * Data !
	 */
   struct LabelMeta
   {
	   SpssDataCell value;
	   char	label_len;
	   std::string label;
   };

   typedef std::vector<LabelMeta> LabelMetas;


   SPSSIMPORTER_READ_ATTRIB(int32_t, label_count)
   SPSSIMPORTER_READ_ATTRIB(LabelMetas,  Labels)
   SPSSIMPORTER_READ_ATTRIB(int32_t, var_rec_type) // must be == rectype_value_labels_var
   SPSSIMPORTER_READ_ATTRIB(int32_t, var_count)
   SPSSIMPORTER_READ_ATTRIB(std::vector<int32_t>, vars)

   /**
	* @brief process Manipulates columns by adding the contents of thie record.
	* @param columns
	*
	* Implematations should examine columns to determine the record history.
	*/
   virtual void process(SPSSImporter* importer, SPSSImportDataSet* dataset);

};

}

#endif // VALUELABELVARSRECORD_H
