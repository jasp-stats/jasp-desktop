//
// Copyright (C) 2013-2017 University of Amsterdam
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

#ifndef COLUMN_H
#define COLUMN_H

#include <boost/iterator/iterator_facade.hpp>
#include <boost/range.hpp>

#include <boost/container/map.hpp>
#include <boost/container/string.hpp>
#include <boost/container/vector.hpp>

#include "datablock.h"
#include "labels.h"

class Column
{
	friend class DataSet;
	friend class Columns;
	friend class DataSetLoader;
	friend class boost::iterator_core_access;

	typedef unsigned long long ull;
	typedef boost::interprocess::allocator<boost::interprocess::offset_ptr<DataBlock>, boost::interprocess::managed_shared_memory::segment_manager> BlockAllocator;
	typedef boost::container::map<ull, boost::interprocess::offset_ptr<DataBlock>, BlockAllocator>::value_type BlockEntry;
	typedef boost::interprocess::allocator<BlockEntry, boost::interprocess::managed_shared_memory::segment_manager> BlockEntryAllocator;
	typedef boost::container::map<ull, boost::interprocess::offset_ptr<DataBlock>, std::less<ull>, BlockEntryAllocator> BlockMap;

	typedef boost::interprocess::allocator<char, boost::interprocess::managed_shared_memory::segment_manager> CharAllocator;
	typedef boost::container::basic_string<char, std::char_traits<char>, CharAllocator> String;
	typedef boost::interprocess::allocator<String, boost::interprocess::managed_shared_memory::segment_manager> StringAllocator;

public:
	static bool isEmptyValue(const std::string& val);
	static bool isEmptyValue(const double& val);

	bool resetEmptyValues(std::map<int, std::string>& emptyValuesMap);


	typedef struct IntsStruct
	{
		friend class Column;

		class iterator : public boost::iterator_facade<
				iterator, int, boost::forward_traversal_tag>
		{
			friend class boost::iterator_core_access;

		public:

			explicit iterator(BlockMap::iterator blockItr, int currentPos);

		private:

			void increment();
			bool equal(iterator const& other) const;
			int& dereference() const;

			BlockMap::iterator _blockItr;
			int _currentPos;
		};

		int& operator[](int index);

		iterator begin();
		iterator end();

		IntsStruct();

	private:

		Column *getParent() const;

	} Ints;

	typedef struct DoublesStruct
	{
		friend class Column;

		class iterator : public boost::iterator_facade<
				iterator, double, boost::forward_traversal_tag>
		{

			friend class boost::iterator_core_access;

		public:

			explicit iterator(BlockMap::iterator blockItr, int currentPos);

		private:

			void increment();
			bool equal(iterator const& other) const;
			double& dereference() const;

			BlockMap::iterator _blockItr;
			int _currentPos;

		};

		double& operator[](int index);

		iterator begin();
		iterator end();

	private:
		DoublesStruct();

		Column *getParent() const;

	} Doubles;

	Column(boost::interprocess::managed_shared_memory *mem);
	Column(const Column& col);
	~Column();

	std::string name() const;
	void setName(std::string name);

	void setValue(int row, int value);
	void setValue(int row, double value);

	bool isValueEqual(int row, int value);
	bool isValueEqual(int row, double value);
	bool isValueEqual(int row, const std::string &value);

	std::string operator[](int row);
	std::string getOriginalValue(int row);

	void append(int rows);
	void truncate(int rows);

	// If the column is a scale, it uses the AsDoubles which is a mapping between the row numbers and the double values.
	// Scale columns do not have labels.
	// It the column is Nominal. NominalText or Ordinal, it uses the AsInts structure
	// For Nominal & Ordinal, the values of the column are integers, so the AsInts get directly these values:
	// it is a mapping between the row number and the value. The Labels is then a mapping of the unique integer values
	// and the label strings (this string represents first the integer value, but can be later on modified)
	// For NominalText, the values are strings, so the values are stored in the labels with some keys (this time the keys have no meaning).
	// The AsInts is then a mapping between the row numbers and these keys. In this case, if the label of one value
	// is modified, the new value is in the label object, and the original string value is kept in another mapping
	// structure (cf. labels.h).
	// Both AsDoubles & AsInts get their space from the BlockMap _blocks which is a mapping of DataBlock.
	Doubles AsDoubles;
	Ints AsInts;

	enum ColumnType { ColumnTypeUnknown = 0, ColumnTypeNominal = 1, ColumnTypeNominalText = 2, ColumnTypeOrdinal = 4, ColumnTypeScale = 8 };
	static std::string getColumnTypeAsString(ColumnType type);

	void setColumnType(ColumnType columnType);
	ColumnType columnType() const;

	bool changeColumnType(ColumnType newColumnType);

	int rowCount() const;

	Labels& labels();

	Column &operator=(const Column &columns);

	void setSharedMemory(boost::interprocess::managed_shared_memory *mem);

	std::map<int, std::string> setColumnAsNominalText(const std::vector<std::string> &values);
	std::map<int, std::string> setColumnAsNominalText(const std::vector<std::string> &values, const std::map<std::string, std::string> &labels);
	void setColumnAsNominalOrOrdinal(const std::vector<int> &values, const std::set<int> &uniqueValues, bool is_ordinal = false);
	void setColumnAsNominalOrOrdinal(const std::vector<int> &values, std::map<int, std::string> &uniqueValues, bool is_ordinal = false);
	void setColumnAsScale(const std::vector<double> &values);

private:
	void _setColumnAsNominalOrOrdinal(const std::vector<int> &values, bool is_ordinal = false);

	boost::interprocess::managed_shared_memory *_mem;

	String _name;
	ColumnType _columnType;
	int _rowCount;

	BlockMap _blocks;
	Labels _labels;

	int id;
	static int count;

	void _setRowCount(int rowCount);
	std::string _getLabelFromKey(int key) const;
	std::string _getScaleValue(int row);

	void _convertVectorIntToDouble(std::vector<int> &intValues, std::vector<double> &doubleValues);

	bool _resetEmptyValuesForNominal(std::map<int, std::string> &emptyValuesMap);
	bool _resetEmptyValuesForScale(std::map<int, std::string> &emptyValuesMap);
	bool _resetEmptyValuesForNominalText(std::map<int, std::string> &emptyValuesMap, bool tryToConvert = true);

	bool _changeColumnToNominalOrOrdinal(ColumnType newColumnType);
	bool _changeColumnToScale();

};

namespace boost
{
	template <>
	struct range_const_iterator< Column::Ints >
	{
		typedef Column::Ints::iterator type;
	};

	template <>
	struct range_const_iterator< Column::Doubles >
	{
		typedef Column::Doubles::iterator type;
	};
}


#endif // COLUMN_H
