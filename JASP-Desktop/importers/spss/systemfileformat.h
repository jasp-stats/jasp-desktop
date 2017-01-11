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

#ifndef SYSTEM_FILE_FORMAT_H
#define SYSTEM_FILE_FORMAT_H


#include <stdint.h>
#include <stdexcept>
#include <vector>
#include <map>
#include <memory>
#include "spssstream.h"

#include "datasetpackage.h"

/*
 *
 * Data structure(s) as used by the SPSS importer.
 * Information taken from PSPP:
 * https://www.gnu.org/software/pspp/pspp-dev/pspp-dev.pdf
 * - As sampled May 2015.
 */


/*
 * Macro that ease creation of raw data
 * as read-only attributes - also creates
 * constants for the size.
 */
#define _SPSSIMPORTER_ATTRIB(type, variable) private: type _##variable;

#define SPSSIMPORTER_READ_ATTRIB(type, variable)					\
	_SPSSIMPORTER_ATTRIB(type, variable)							\
	public: const type & variable () const { return _##variable; }	\


/*
 * We use this fragment in constuctors, so, make a macro out of it.
 * (This macro assumes that file is an instance of an ifstream like object,
 * in that it has a member function of signature read(char *, size_t) ).
 */
#define _SPSSIMPORTER_READ_VAR(variable, file)			\
	(file.read((char *) &variable, sizeof(variable)))	\

#define SPSSIMPORTER_READ_MEMBER(variable, file, fixer) \
	_SPSSIMPORTER_READ_VAR(_##variable, file);			\
	fixer.fixup( &_##variable )							\


namespace spss {



/*
 * useful typedefs.
 */
typedef char SPSSChar;
typedef SPSSChar Char_3[3];
typedef SPSSChar Char_4[4];
typedef SPSSChar Char_8[8];
typedef SPSSChar Char_9[9];
typedef SPSSChar Char_60[60];
typedef SPSSChar Char_64[64];
typedef SPSSChar Char_80[80];
typedef std::vector<double> VecDbls;

/**
 * Data cells are all of this type:
 */
union SpssDataCell
{
	Char_8 chars;
	double	dbl;

	// Allow use in ordered collections.
	bool operator <(const SpssDataCell &that)
	const
	{
		return forOrdering < that.forOrdering;
	}

private:
	// This type does not exist in the file, but
	// we use it here to allow the cells to be
	// used in ordered containers.
	uint64_t forOrdering;
};

/*
  * Values we could expect to see in a rec_type field.
  */
typedef enum e_recordTypes
{
	rectype_unknown = 0,
	rectype_file_header = 0x324c4624, // == "$FL2" on little endin machines. (i.e. PC's and Intel Macs)
	rectype_variable = 2,
	rectype_value_labels = 3,
	rectype_value_labels_var = 4,
	rectype_document = 6,
	rectype_meta_data = 7, // see subtypes for integer / float / display  / long var / long string / etc.
	rectype_dict_term = 999,
} RecordTypes;

/*
  * Values we would expect to see in a subtype field, where rec_type == type_data (7).
  * In addtion, misc. infmation records can be 'any value'
  * The sub type can have any value, of which only a number are defined.
  * We define the type as an int, but the known values as an enumeration.
  */
typedef enum e_subRecordTypes
{
	recsubtype_unknown = 0,
	recsubtype_integer = 3,
	recsubtype_float = 4,
	recsubtype_display = 11,
	recsubtype_longvar = 13,
	recsubtype_verylongstr = 14,
	recsubtype_extnumcases = 16,	// New 13th July 2014.
	recsubtype_charEncoding = 20	// New May 2016
} RecordSubTypes;

}; // end namespace!


// undefine these to stop them getting in the way elsewhere
#undef _SIZES




#endif // SYSTEM_FILE_FORMAT_H
