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

#ifndef SPSSIMPORTDATASET_H
#define SPSSIMPORTDATASET_H

#include "../importerutils.h"
#include "numericconverter.h"
#include "spssimportcolumn.h"
#include "importers/importdataset.h"
#include "fileheaderrecord.h"
#include "integerinforecord.h"
#include "floatinforecord.h"

#include <vector>
#include <string>
#include <set>
#include <map>

namespace spss
{

class SPSSImportDataSet : public ImportDataSet
{
public:
	typedef std::map<std::string, size_t> LongColsData;

	SPSSImportDataSet();
	virtual ~SPSSImportDataSet();

	void add(size_t dictIndex, SPSSImportColumn *column);

	// Set the right name for each column and build the name to col map
	void setColumnMap();

	SPSSImportColumn *getColumn(size_t entry);

	RW_ATTR(size_t, charsRemaining)		// The numer of chars (in this case!) remaind to read-in.

	int rowCount() const;

	/**
	 * @brief numCases Set the number of cases.
	 * @param num Number of cases to set.
	 */
	void numCases(int32_t num);
	void numCases(int64_t num);

	/**
	 * @brief numCases
	 * @return The numer of cases found (-1 if notknown.)
	 */
	size_t numCases() const { return (size_t) _numCases; }

	/**
	 * @brief hasNoCases Checks for number of cases > 0
	 * @return true if no cases held/found.
	 */
	bool hasNoCases() const { return _numCases < 1L; }

	/**
	 * @brief veryLongColsDat Sets the very long strings data.
	 * @param vlcd The value to set.
	 */
	void veryLongColsDat(const LongColsData &vlcd)
	{ _longColsDta.insert(vlcd.begin(), vlcd.end()); }

	/**
	 * @brief veryLongColsDat Gets the very long strings data.
	 * @return The value found.
	 */
	const LongColsData &veryLongColsDat() const { return _longColsDta; }

	/**
	 * @brief numericsConv Access the numeric convertor.
	 * @return
	 */
	NumericConverter &numericsConv() { return _numConvert; }

	/**
	 * @brief setStrCnvrtr Sets the string convertor to use.
	 * @param convtr
	 *
	 * N.B. This class takes over ownership of the instance:
	 * @code
		...
		inst.setStrCnvrtr( new SpssCPConvert(ICUConnnector::dos437) );
		...
	   @endcode
	 */
	void setStrCnvrtr(CodePageConvert *convtr) { _stringConvert.reset(convtr); }

	/**
	 * @brief stringsConv Gets the string convertor.
	 * @return String convertoer instance.
	 *
	 * N.B. Will "blow-up" (null pointer exception) if not set!
	 */
	CodePageConvert &stringsConv() { return *_stringConvert.get(); }


	void setHeaderInfo(IntegerInfoRecord &integerInfo, FloatInfoRecord &floatInfo)
	{
		_floatInfo = floatInfo;
		_integerInfo = integerInfo;
	}

	const IntegerInfoRecord& getIntegerInfo() const {return _integerInfo;}
	const FloatInfoRecord& getFloatInfo() const {return _floatInfo;}

private:
	std::map<size_t, SPSSImportColumn*>	_indexToColumnMap;
	NumericConverter					_numConvert; /** < Numeric Endain fixer. */
	std::auto_ptr<CodePageConvert>		_stringConvert; /** < Code Page convertor. */
	LongColsData						_longColsDta; /** < Very long strings data. */
	int64_t								_numCases; /** < Number of cases found to date. */
	IntegerInfoRecord					_integerInfo;
	FloatInfoRecord						_floatInfo;
};

} // end namespace


#endif // SPSSIMPORTDATASET_H
