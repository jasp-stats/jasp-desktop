#ifndef JASPSHAREDMEM_INTERFACE_H
#define JASPSHAREDMEM_INTERFACE_H

#include "jaspsharedmem_global.h"
#include "../JASP-Common/column.h"

class JASPChannelInterface {
protected:
	~JASPChannelInterface() { }  // use destroy()

public:
	virtual void STDCALL destroy() = 0;
	virtual void STDCALL send(const char* data) = 0;
	virtual bool STDCALL receive(char** data, int timeout) = 0;
};

class LabelIteratorInterface {
protected:
	~LabelIteratorInterface() {}

public:
	virtual bool STDCALL isEnd() = 0;
	virtual void STDCALL increment() = 0;
	virtual int STDCALL getInt() = 0;
	virtual const char* STDCALL getText() = 0;
};

class LabelsInterface {
protected:
	~LabelsInterface() {}

public:
	virtual size_t STDCALL size() = 0;
	virtual LabelIteratorInterface* STDCALL labelBegin() = 0;

};

class DoubleIteratorInterface {

protected:
	~DoubleIteratorInterface() {}

public:
	virtual bool STDCALL isEnd() = 0;
	virtual void STDCALL increment() = 0;
	virtual double STDCALL getValue() = 0;
};

class IntIteratorInterface {

protected:
	~IntIteratorInterface() {}

public:
	virtual bool STDCALL isEnd() = 0;
	virtual void STDCALL increment() = 0;
	virtual int STDCALL getValue() = 0;
};

class ColumnInterface {
protected:
	~ColumnInterface() {}

public:
	virtual Column::ColumnType STDCALL columnType() = 0;
	virtual int STDCALL rowCount() = 0;
	virtual const char* STDCALL name() = 0;
	virtual LabelsInterface* STDCALL labels() = 0;
	virtual DoubleIteratorInterface* STDCALL doubleBegin() = 0;
	virtual IntIteratorInterface* STDCALL intBegin() = 0;
};

class ColumnIteratorInterface {

protected:
	~ColumnIteratorInterface() {}

public:
	virtual bool STDCALL isEnd() = 0;
	virtual void STDCALL increment() = 0;
	virtual ColumnInterface* STDCALL getColumn() = 0;
};

class DataSetInterface {
protected:
	~DataSetInterface() {}

public:
	virtual ColumnIteratorInterface* STDCALL columnBegin() = 0;
	virtual ColumnInterface* STDCALL getColumn(char* name) = 0;
};




extern "C" JASPSHAREDMEMSHARED_EXPORT JASPChannelInterface* CreateJASPChannel(const char* name, int slaveNo);
extern "C" JASPSHAREDMEMSHARED_EXPORT DataSetInterface* retrieveDataSet();

#endif // JASPSHAREDMEM_INTERFACE_H
