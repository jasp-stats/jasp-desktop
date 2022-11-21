//
// Copyright (C) 2013-2018 University of Amsterdam
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

#include "columntype.h"


///
/// This class contains the actual data for a column, stored as either as int (IntsStruct) or double (DoublesStruct)
/// It allocates the memory for that through the use of DataBlock (see datablock.h) and Column::append()/Column::truncate()
/// If there are stringlabels these are stored in Labels
/// It is part of Columns, which is part of DataSet.
class Column
{
	friend class DataSet;
	friend class Columns;
	friend class ComputedColumn;
	friend class ComputedColumns;
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
	///ColumnType is set up to be used as bitflags in places such as assignedVariablesModel and such
	//enum ColumnType { unknown = 0, nominal = 1, nominalText = 2, ordinal = 4, scale = 8 };

	bool resetEmptyValues(std::map<int, std::string>& emptyValuesMap);


	bool overwriteDataWithScale(std::vector<double> scalarData);
	bool overwriteDataWithOrdinal(std::vector<int> ordinalData, std::map<int, std::string> levels);
	bool overwriteDataWithNominal(std::vector<int> nominalData, std::map<int, std::string> levels);
	bool overwriteDataWithOrdinal(std::vector<int> ordinalData);
	bool overwriteDataWithNominal(std::vector<int> nominalData);
	bool overwriteDataWithNominal(std::vector<std::string> nominalData);
	void setDefaultValues(columnType columnType = columnType::unknown);

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
		DoublesStruct() {}

		Column *getParent() const;

	} Doubles;

	Column(boost::interprocess::managed_shared_memory *mem)  : _mem(mem), _name(mem->get_segment_manager()), _columnType(columnType::nominal), _rowCount(0), _blocks(std::less<ull>(), mem->get_segment_manager()), _labels(mem)
	{
		_id = ++count;
	}

	Column(const Column& col) : _mem(col._mem), _name(col._name), _columnType(col._columnType), _rowCount(col._rowCount), _blocks(col._blocks), _labels(col._labels)
	{
		_id = ++count;
	}

	~Column() {}

	std::string name() const;
	int id() const;
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

	void setColumnType(enum columnType columnType);
	enum columnType getColumnType() const;

	columnTypeChangeResult changeColumnType(enum columnType newColumnType);

	size_t rowCount() const { return _rowCount; }

			Labels & labels();
	const	Labels & labels() const;

	Column &operator=(const Column &columns);

	void setSharedMemory(boost::interprocess::managed_shared_memory *mem);

	bool						setColumnAsScale(const std::vector<double> &values);

	std::map<int, std::string>	setColumnAsNominalText(const std::vector<std::string> &values,	const std::map<std::string, std::string> &labels, bool * changedSomething = NULL);
	std::map<int, std::string>	setColumnAsNominalText(const std::vector<std::string> &values, bool * changedSomething = NULL);

	bool						setColumnAsNominalOrOrdinal(const std::vector<int> &values,		std::map<int, std::string> uniqueValues,	bool is_ordinal = false);
	bool						setColumnAsNominalOrOrdinal(const std::vector<int> &values,													bool is_ordinal = false);

	bool allLabelsPassFilter() const;

	bool hasFilter() const;

	void resetFilter();

	bool isColumnDifferentFromStringValues(std::vector<std::string> strVals);

private:	

	bool		_setColumnAsNominalOrOrdinal(const std::vector<int> &values, bool is_ordinal = false);

	void		_setRowCount(int rowCount);
	std::string	_getLabelFromKey(int key) const;
	std::string	_getScaleValue(int row, bool forDisplay);

	void		_convertVectorIntToDouble(std::vector<int> &intValues, std::vector<double> &doubleValues);

	bool		_resetEmptyValuesForNominal(std::map<int, std::string> &emptyValuesMap);
	bool		_resetEmptyValuesForScale(std::map<int, std::string> &emptyValuesMap);
	bool		_resetEmptyValuesForNominalText(std::map<int, std::string> &emptyValuesMap, bool tryToConvert = true);

	columnTypeChangeResult	_changeColumnToNominalOrOrdinal(enum columnType newColumnType);
	columnTypeChangeResult	_changeColumnToScale();

private:
	boost::interprocess::managed_shared_memory * _mem = nullptr;

	String			_name;
	enum columnType _columnType;
	size_t			_rowCount;

	BlockMap		_blocks;
	Labels			_labels;

	int				_id;
	static int		count;
};

namespace boost
{
	template <> struct range_const_iterator< Column::Ints >		{ typedef Column::Ints::iterator type;		};
	template <> struct range_const_iterator< Column::Doubles >	{ typedef Column::Doubles::iterator type;	};
}


#endif // COLUMN_H
