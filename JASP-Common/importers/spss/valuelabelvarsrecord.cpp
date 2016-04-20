


#include "valuelabelvarsrecord.h"


using namespace std;
using namespace boost;
using namespace spss;

/**
 * @brief ValueLabelRecord Ctor
 * @param fileType The record type value, as found in the file.
 * @param from The file to read from.
 *
 */
ValueLabelVarsRecord::ValueLabelVarsRecord(RecordTypes fileType, SPSSStream &from)
	: ReadableRecord(fileType, from)
{
	SPSSIMPORTER_READ_MEMBER(label_count, from);
	for (int32_t i = 0; i < label_count(); i++ )
	{
		// Read a single meta..
		LabelMeta meta;
		_SPSSIMPORTER_READ_VAR(meta.value, from);
		_SPSSIMPORTER_READ_VAR(meta.label_len, from);
		{
			char * buffer = new char [meta.label_len + 2];
			from.read(buffer, meta.label_len);
			meta.label.append(buffer, meta.label_len);
			delete buffer;
		}
		// insert
		_labels.push_back(meta);

		// find the following padding, by rounding up to 8 byte blocks,
		// and taking modulo
		size_t padLen = roundUpTo(sizeof(meta.label_len)+meta.label_len, 8 * 8) - (sizeof(meta.label_len)+meta.label_len);
		for (size_t j = 0; j < padLen; j++)
		{
			char padding;
			_SPSSIMPORTER_READ_VAR(padding, from);
		}
	}

	// now start in the value label record.
	SPSSIMPORTER_READ_MEMBER(var_rec_type, from);

	if (var_rec_type() != rectype_value_labels_var)
	{
		DEBUG_COUT3("ValueLabelVarsRecord::ctor Next record not ", rectype_value_labels_var, "- File unreadable.");
		throw runtime_error("Incorrect record following a value labels record. SAV file corrupt.");
	}

	SPSSIMPORTER_READ_MEMBER(var_count, from);
	for (int i = 0; i < var_count(); i++)
	{
		int32_t indx;
		_SPSSIMPORTER_READ_VAR(indx, from);
		_vars.push_back(indx);
	}
}

ValueLabelVarsRecord::~ValueLabelVarsRecord()
{

}

/**
 * @brief Does nothing
 *
 */
void ValueLabelVarsRecord::process(SPSSColumns & /* columns */)
{
}
