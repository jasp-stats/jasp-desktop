#ifndef JASPSHAREDMEM_IMPL_H
#define JASPSHAREDMEM_IMPL_H

#include "jaspsharedmem_interface.h"
#include "../JASP-Common/ipcchannel.h"
#include "../JASP-Common/dataset.h"

#include <boost/mpl/size.hpp>

class JASPChannelImpl : public JASPChannelInterface {
public:
	JASPChannelImpl(const char* name, int slaveNo);
	~JASPChannelImpl();

	virtual void STDCALL destroy();
	virtual void STDCALL send(const char* data);
	virtual bool STDCALL receive(const char** data, int timeout);

private:
	IPCChannel *_channel;
	std::string _received;

};

class LabelIteratorImpl: public LabelIteratorInterface {

public:
	LabelIteratorImpl(Labels *labels);
	~LabelIteratorImpl() {}
	void reset(Labels *labels);

	virtual bool STDCALL isEnd();
	virtual void STDCALL increment();
	virtual int STDCALL getInt();
	virtual const char* STDCALL getText();

private:
	Labels::const_iterator _currentIterator;
	Labels::const_iterator _endIterator;
	bool _isEnd;

	std::string _text;
};

class IntIteratorImpl: public IntIteratorInterface {

public:
	IntIteratorImpl(Column::Ints *colInts);
	~IntIteratorImpl() {}
	void reset(Column::Ints *colInts);

	virtual bool STDCALL isEnd();
	virtual void STDCALL increment();
	virtual int STDCALL getValue();

private:
	Column::Ints::iterator _currentIterator;
	Column::Ints::iterator _endIterator;
	bool _isEnd;
};

class DoubleIteratorImpl: public DoubleIteratorInterface {

public:
	DoubleIteratorImpl(Column::Doubles* colDoubles);
	~DoubleIteratorImpl() {}
	void reset(Column::Doubles* colDoubles);

	virtual bool STDCALL isEnd();
	virtual void STDCALL increment();
	virtual double STDCALL getValue();

private:
	Column::Doubles::iterator _currentIterator;
	Column::Doubles::iterator _endIterator;
	bool _isEnd;
};

class LabelsImpl: public LabelsInterface {

public:
	LabelsImpl(Labels *labels);
	~LabelsImpl() {}
	void reset(Labels *labels);

	virtual size_t STDCALL size();
	virtual LabelIteratorInterface* STDCALL labelBegin();

private:
	Labels* _labels;
	LabelIteratorImpl* _singleLabelIteratorWrapper;
};

class DataSetImpl;

class ColumnIteratorImpl : public ColumnIteratorInterface {

public:
	ColumnIteratorImpl(DataSetImpl* parent, Columns* columns);
	~ColumnIteratorImpl() {}
	void reset(Columns* columns);

	virtual bool STDCALL isEnd();
	virtual void STDCALL increment();
	virtual ColumnInterface* STDCALL getColumn();

private:
	DataSetImpl* _parent;
	Columns::iterator _currentColumnIterator;
	Columns::iterator _endColumnIterator;
	bool _isEnd;

};


class ColumnImpl: public ColumnInterface {

public:
	ColumnImpl(Column* column);
	~ColumnImpl() {}
	void reset(Column* column);

	virtual Column::ColumnType STDCALL columnType();
	virtual int STDCALL rowCount();
	virtual const char* STDCALL name();
	virtual LabelsInterface* STDCALL labels();
	virtual DoubleIteratorInterface* STDCALL doubleBegin();
	virtual IntIteratorInterface* STDCALL intBegin();

private:
	Column *_column;
	std::string _name;

	LabelsImpl* _singleLabelsWrapper;
	DoubleIteratorImpl* _singleDoubleIteratorWrapper;
	IntIteratorImpl* _singleIntIteratorWrapper;
};


class DataSetImpl : public DataSetInterface {
public:
	static DataSetImpl* SingleDataSetWrapper;

	DataSetImpl(DataSet* dataset);
	~DataSetImpl() {}
	void reset(DataSet* dataSet);

	virtual ColumnIteratorInterface* STDCALL columnBegin();
	virtual ColumnInterface* STDCALL getColumn(char* name);

	ColumnInterface* getColumnFromMap(Column &column);

private:
	DataSet *_dataSet;
	std::map<int, ColumnImpl *> _columnsWrapper;
	ColumnIteratorImpl *_singleColumnIteratorWrapper;

};



#endif // JASPSHAREDMEM_IMPL_H
