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

		Column *getParent();

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

		Column *getParent();

	} Doubles;

	Column(boost::interprocess::managed_shared_memory *mem);
	Column(const Column& col);
	~Column();

	std::string name() const;
	void setName(std::string name);

	void setValue(int rowIndex, int value);
	void setValue(int rowIndex, double value);
	void setValue(int rowIndex, std::string value);

	bool isValueEqual(int rowIndex, int value);
	bool isValueEqual(int rowIndex, double value);
	bool isValueEqual(int rowIndex, const std::string &value);

	std::string operator[](int row);
	std::string getOriginalValue(int row);

	void append(int rows);
	void truncate(int rows);

	Doubles AsDoubles;
	Ints AsInts;

	enum ColumnType { ColumnTypeUnknown = 0, ColumnTypeNominal = 1, ColumnTypeNominalText = 2, ColumnTypeOrdinal = 4, ColumnTypeScale = 8 };
	void setColumnType(ColumnType columnType);
	ColumnType columnType() const;

	void changeColumnType(ColumnType newColumnType);

	int rowCount() const;

	Labels& labels();

	Column &operator=(const Column &columns);

	void setSharedMemory(boost::interprocess::managed_shared_memory *mem);

	void setColumnAsNominalString(const std::vector<std::string> &values);
	void setColumnAsNominalString(const std::vector<std::string> &values, const std::map<std::string, std::string> &labels);
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

	static const std::string _emptyValue;
	static const std::string _emptyValues[];
	static const int _emptyValuesCount;

	void _setRowCount(int rowCount);
	std::string _labelFromIndex(int index) const;
	std::string _getScaleValue(int row);
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
