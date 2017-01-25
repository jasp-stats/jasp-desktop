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

#include "valuelabelvarsrecord.h"
#include "spssimportdataset.h"


using namespace std;
using namespace boost;
using namespace spss;

/**
 * @brief ValueLabelRecord Ctor
 * @param const Converters &fixer - Fixes endianness.
 * @param fileType The record type value, as found in the file.
 * @param from The file to read from.
 *
 */
ValueLabelVarsRecord::ValueLabelVarsRecord(const NumericConverter &fixer, RecordTypes fileType, SPSSStream &from)
	: ReadableRecord(fixer, fileType, from)
{
	SPSSIMPORTER_READ_MEMBER(label_count, from, fixer);
	for (int32_t i = 0; i < label_count(); i++ )
	{
		// Read a single meta..
		LabelMeta meta;
		_SPSSIMPORTER_READ_VAR(meta.value, from);
		_SPSSIMPORTER_READ_VAR(meta.label_len, from);
		{
			char * buffer = new char [meta.label_len + 2];
			from.read(buffer, meta.label_len);
			fixer.fixup(buffer, meta.label_len);
			meta.label.append(buffer, meta.label_len);
			delete[] buffer;
		}
		// insert
		_Labels.push_back(meta);

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
	SPSSIMPORTER_READ_MEMBER(var_rec_type, from, fixer);

	if (var_rec_type() != rectype_value_labels_var)
	{
//		DEBUG_COUT3("ValueLabelVarsRecord::ctor Next record not ", rectype_value_labels_var, "- File unreadable.");
		throw runtime_error("Incorrect record following a value labels record. SAV file corrupt.");
	}

	SPSSIMPORTER_READ_MEMBER(var_count, from, fixer);
	for (int i = 0; i < var_count(); i++)
	{
		int32_t indx;
		_SPSSIMPORTER_READ_VAR(indx, from);
		fixer.fixup(&indx);
		_vars.push_back(indx - 1);
	}
}

ValueLabelVarsRecord::~ValueLabelVarsRecord()
{

}

/**
 * @brief Add labels
 *
 */
void ValueLabelVarsRecord::process(SPSSImporter* importer, SPSSImportDataSet *dataset)
{
	for (size_t i = 0; i < _vars.size(); ++i)
	{
		// Get the next applicable column.
		SPSSImportColumn* column = dataset->getColumn(_vars[i]);
		// Iterate over all the found labels meta.
		for (size_t j = 0; j < _Labels.size(); ++j)
		{
			LabelMeta &meta = _Labels[j];
			SPSSImportColumn::LabelByValueDictEntry entry(meta.value, meta.label);
			column->spssLables.insert( entry );
		}
	}

}

