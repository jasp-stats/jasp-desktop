

#include "variablerecord.h"
#include "debug_cout.h"
#include "stringutils.h"

using namespace std;
using namespace boost;
using namespace spss;

/**
 * @brief VariableRecord Ctor
 * @param fileType The record type value, as found in the file.
 * @param fileHeader The file ehadewr we are associated with.
 * @param fromStream The file to read from.
 *
 */
VariableRecord::VariableRecord(RecordTypes fileType, FileHeaderRecord * fileHeader, SPSSStream &from)
	: ReadableRecord(fileType, from)
	, _pFHR(fileHeader)
{
	/*
	 * The data,
	 */
	SPSSIMPORTER_READ_MEMBER(type, from);
	SPSSIMPORTER_READ_MEMBER(has_var_label, from);
	SPSSIMPORTER_READ_MEMBER(n_missing_values, from);
	SPSSIMPORTER_READ_MEMBER(print, from);
	SPSSIMPORTER_READ_MEMBER(write, from);
	SPSSIMPORTER_READ_MEMBER(name_file, from);

	{
		const size_t numChars = sizeof(_name_file) / sizeof(char);
		char buffer[numChars + 1];
		memcpy(buffer, _name_file, numChars);
		buffer[numChars] = '\0';
		StrUtils::rTrimWSIP(buffer, numChars - 1);
		_name = buffer;
	}


	if (has_var_label() == 1)
	{
		SPSSIMPORTER_READ_MEMBER(label_len, from);
		if (label_len() > 0)
		{
			// Find the buffer size rounded up to 32 bit increments,
			size_t buffSize = roundUpTo(label_len(), 32);
			char * buffer = new char[ buffSize ];
			from.read(buffer, buffSize);
			_label.append(buffer, label_len());
			delete buffer;
		}
	}

	for (int32_t i = 0; i != abs(n_missing_values()); i++)
	{
		double val;
		from.read((char *) &val, sizeof(val)/sizeof(char));
		_missing_values.push_back(val);
	}

	_dictionary_index = fileHeader->incVarRecordCount();

}


VariableRecord::~VariableRecord()
{

}

/**
 * @brief createCol Appends a colum to the vector.
 *
 */
void VariableRecord::process(SPSSColumns &columns)
{

	// check for string continuation.
	if (isStringContinuation())
	{
		if ((columns.size() != 0) && (columns[columns.size()-1].isString()))
			columns[columns.size()-1].columnSpan++;

		DEBUG_COUT5("Existing column ", columns[columns.size()-1].spssName, " spans ", columns[columns.size()-1].columnSpan, " cols.");

		return;
	}

	int32_t strLen = 0;
	int32_t measure = spss::Measures::measure_undefined;

	if (type() == 0)
		measure = spss::Measures::measure_continuous;

	else if (type() == -1)
		measure = spss::Measures::string_type;

	else
	{
		measure = spss::Measures::string_type;
		strLen = type();
	}

	{
		SPSSColumn col(name(), hasVarLabel()? label() : name(),
					  MissingValueChecker(n_missing_values(), missing_values()),
					  strLen, measure);
		columns.push_back(col);
	}

	DEBUG_COUT4("VariableRecord::process() - Added column ", columns.back().spssName, "/", columns.back().spssLabel);
}
