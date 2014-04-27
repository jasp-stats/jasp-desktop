#ifndef COLUMN_H
#define COLUMN_H

#include <string>
#include <map>

#include <boost/iterator/iterator_facade.hpp>
#include <boost/range.hpp>

#include <boost/interprocess/segment_manager.hpp>
#include <boost/container/map.hpp>
#include <boost/container/string.hpp>

#include "sharedmemory.h"
#include "datablock.h"

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

	typedef boost::container::map<int, String, StringAllocator>::value_type LabelEntry;
	typedef boost::interprocess::allocator<LabelEntry, boost::interprocess::managed_shared_memory::segment_manager> LabelEntryAllocator;
	typedef boost::container::map<int, String, std::less<int>, LabelEntryAllocator> Labels;

	typedef boost::container::map<int, int>::value_type NumericEntry;
	typedef boost::interprocess::allocator<NumericEntry, boost::interprocess::managed_shared_memory::segment_manager> NumericEntryAllocator;
	typedef boost::container::map<int, int, std::less<int>, NumericEntryAllocator> NumericLabels;

public:

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

			//iterator(BlockMap *blocks);
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

	Column();

	std::string name() const;
	void setName(std::string name);

	void setValue(int rowIndex, int value);
	void setValue(int rowIndex, double value);
	void setValue(int rowIndex, std::string value);

	std::string operator[](int index);

	void append(int rows);

	Doubles AsDoubles;
	Ints AsInts;

	enum ColumnType { ColumnTypeUnknown = 0, ColumnTypeNominal = 1, ColumnTypeNominalText = 2, ColumnTypeOrdinal = 4, ColumnTypeScale = 8 };
	ColumnType columnType() const;

	void changeColumnType(ColumnType newColumnType);

	int rowCount() const;

	bool hasLabels() const;
	std::map<int, std::string> labels() const;
	void setLabels(std::map<int, std::string> labels);
	std::string stringFromRaw(int value) const;

	bool hasNumericLabels() const;
	void setLabels(std::map<int, int> labels);
	std::map<int, int> numericLabels() const;
	int actualFromRaw(int value) const;

private:

	String _name;
	boost::interprocess::offset_ptr<Labels> _labels;
	boost::interprocess::offset_ptr<NumericLabels> _numericLabels;
	int _rowCount;
	ColumnType _columnType;

	BlockMap _blocks;

	void setRowCount(int rowCount);
	void insert(int rowCount, int index);

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
