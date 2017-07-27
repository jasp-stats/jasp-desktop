#include "jaspsharedmem_impl.h"
#include "../JASP-Common/sharedmemory.h"
#include <iostream>
#include <fstream>

using namespace std;

extern "C" JASPChannelInterface* CreateJASPChannel(const char* name, int slaveNo)
{
	return new JASPChannelImpl(name, slaveNo);
}

JASPChannelImpl::JASPChannelImpl(const char *name, int slaveNo)
{
	_channel = new IPCChannel(name, slaveNo, true);
}

JASPChannelImpl::~JASPChannelImpl()
{

}

void JASPChannelImpl::destroy()
{

}

void JASPChannelImpl::send(const char *data)
{
	std::string strData(data);
	_channel->send(strData);
}

bool JASPChannelImpl::receive(const char** data, int timeout)
{
	timeout = 1000;
	bool result = _channel->receive(_received, timeout);
	if (result)
	{
		*data = _received.c_str();
	}

	return result;
}

DataSetImpl* DataSetImpl::SingleDataSetWrapper = NULL;

extern "C" DataSetInterface* retrieveDataSet()
{
	DataSet* dataSet = SharedMemory::retrieveDataSet();
	if (DataSetImpl::SingleDataSetWrapper == NULL)
		DataSetImpl::SingleDataSetWrapper = new DataSetImpl(dataSet);
	else
		DataSetImpl::SingleDataSetWrapper->reset(dataSet);

	return DataSetImpl::SingleDataSetWrapper;
}

DataSetImpl::DataSetImpl(DataSet *dataset)
{
	_dataSet = dataset;
	_singleColumnIteratorWrapper = NULL;
}

ColumnIteratorInterface *DataSetImpl::columnBegin()
{
	Columns& columns = _dataSet->columns();
	if (_singleColumnIteratorWrapper != NULL)
		_singleColumnIteratorWrapper->reset(&columns);
	else
		_singleColumnIteratorWrapper = new ColumnIteratorImpl(this, &columns);
	return _singleColumnIteratorWrapper;
}

ColumnInterface *DataSetImpl::getColumnFromMap(Column& column)
{
	ColumnImpl* result = NULL;
	int colId = column.id();
	std::map<int, ColumnImpl*>::iterator it = _columnsWrapper.find(colId);
	if (it == _columnsWrapper.end())
	{
		result = new ColumnImpl(&column);
		_columnsWrapper.insert(std::pair<int, ColumnImpl*>(colId, result));
	}
	else
	{
		result = it->second;
		result->reset(&column);
	}
	return result;
}

ColumnInterface *DataSetImpl::getColumn(char *name)
{
	Column& column = _dataSet->columns().get(name);
	return getColumnFromMap(column);
}

void DataSetImpl::reset(DataSet *dataSet)
{
	_dataSet = dataSet;
}

ColumnIteratorImpl::ColumnIteratorImpl(DataSetImpl* parent, Columns *columns) :
	_parent(parent),
	_currentColumnIterator(columns->begin()),
	_endColumnIterator(columns->end())

{
	_isEnd = (_currentColumnIterator == _endColumnIterator);
}

void ColumnIteratorImpl::reset(Columns *columns)
{
	_currentColumnIterator = columns->begin();
	_endColumnIterator = columns->end();
	_isEnd = (_currentColumnIterator == _endColumnIterator);
}

bool ColumnIteratorImpl::isEnd()
{
	return _isEnd;
}

void ColumnIteratorImpl::increment()
{
	if (!_isEnd)
	{
		++_currentColumnIterator;
		_isEnd = (_currentColumnIterator == _endColumnIterator);
	}
}

ColumnInterface *ColumnIteratorImpl::getColumn()
{
	Column &column = *_currentColumnIterator;
	return _parent->getColumnFromMap(column);
}

ColumnImpl::ColumnImpl(Column *column)
{
	_column = column;
	_singleDoubleIteratorWrapper = NULL;
	_singleIntIteratorWrapper = NULL;
	_singleLabelsWrapper = NULL;
}

void ColumnImpl::reset(Column *column)
{
	if (column->id() != _column->id())
	{
		_column = column;
		_singleDoubleIteratorWrapper = NULL;
		_singleIntIteratorWrapper = NULL;
		_singleLabelsWrapper = NULL;
	}
}

Column::ColumnType ColumnImpl::columnType()
{
	return _column->columnType();
}

int ColumnImpl::rowCount()
{
	return _column->rowCount();
}

const char* ColumnImpl::name()
{
	_name = _column->name();
	return _name.c_str();
}

LabelsInterface *ColumnImpl::labels()
{
	Labels& labels = _column->labels();
	if (_singleLabelsWrapper == NULL)
		_singleLabelsWrapper = new LabelsImpl(&labels);
	else
		_singleLabelsWrapper->reset(&labels);
	return _singleLabelsWrapper;
}

DoubleIteratorInterface *ColumnImpl::doubleBegin()
{
	Column::Doubles &doubles = _column->AsDoubles;
	if (_singleDoubleIteratorWrapper == NULL)
		_singleDoubleIteratorWrapper = new DoubleIteratorImpl(&doubles);
	else
		_singleDoubleIteratorWrapper->reset(&doubles);
	return _singleDoubleIteratorWrapper;
}

IntIteratorInterface *ColumnImpl::intBegin()
{
	Column::Ints &ints = _column->AsInts;
	if (_singleIntIteratorWrapper == NULL)
		_singleIntIteratorWrapper = new IntIteratorImpl(&ints);
	else
		_singleIntIteratorWrapper->reset(&ints);
	return _singleIntIteratorWrapper;
}

IntIteratorImpl::IntIteratorImpl(Column::Ints *colInts) :
	_currentIterator(colInts->begin()),
	_endIterator(colInts->end())
{
	_isEnd = (_currentIterator == _endIterator);
}

void IntIteratorImpl::reset(Column::Ints *colInts)
{
	_currentIterator = colInts->begin();
	_endIterator = colInts->end();
	_isEnd = (_currentIterator == _endIterator);
}

bool IntIteratorImpl::isEnd()
{
	return _isEnd;
}

void IntIteratorImpl::increment()
{
	if (!_isEnd)
	{
		++_currentIterator;
		_isEnd = (_currentIterator == _endIterator);
	}
}

int IntIteratorImpl::getValue()
{
	return *_currentIterator;
}

DoubleIteratorImpl::DoubleIteratorImpl(Column::Doubles *colDoubles) :
	_currentIterator(colDoubles->begin()),
	_endIterator(colDoubles->end())
{
	_isEnd = (_currentIterator == _endIterator);
}

void DoubleIteratorImpl::reset(Column::Doubles *colDoubles)
{
	_currentIterator = colDoubles->begin();
	_endIterator = colDoubles->end();
	_isEnd = (_currentIterator == _endIterator);
}

bool DoubleIteratorImpl::isEnd()
{
	return _isEnd;
}

void DoubleIteratorImpl::increment()
{
	if (!_isEnd)
	{
		++_currentIterator;
		_isEnd = (_currentIterator == _endIterator);
	}
}

double DoubleIteratorImpl::getValue()
{
	return *_currentIterator;
}

LabelsImpl::LabelsImpl(Labels *labels)
{
	_labels = labels;
}

void LabelsImpl::reset(Labels *labels)
{
	_labels = labels;
}

size_t LabelsImpl::size()
{
	return _labels->size();
}

LabelIteratorInterface *LabelsImpl::labelBegin()
{
	if (_singleLabelIteratorWrapper == NULL)
		_singleLabelIteratorWrapper = new LabelIteratorImpl(_labels);
	else
		_singleLabelIteratorWrapper->reset(_labels);
	return _singleLabelIteratorWrapper;
}

LabelIteratorImpl::LabelIteratorImpl(Labels *labels) :
	_currentIterator(labels->begin()),
	_endIterator(labels->end())
{
	_isEnd = (_currentIterator == _endIterator);
}

void LabelIteratorImpl::reset(Labels *labels)
{
	_currentIterator = labels->begin();
	_endIterator = labels->end();
	_isEnd = (_currentIterator == _endIterator);
}

bool LabelIteratorImpl::isEnd()
{
	return _isEnd;
}

void LabelIteratorImpl::increment()
{
	if (!_isEnd)
	{
		++_currentIterator;
		_isEnd = (_currentIterator == _endIterator);
	}
}

int LabelIteratorImpl::getInt()
{
	return _currentIterator->value();
}

const char *LabelIteratorImpl::getText()
{
	_text = _currentIterator->text();
	return _text.c_str();
}
