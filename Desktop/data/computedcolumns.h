#ifndef COMPUTEDCOLUMNS_H
#define COMPUTEDCOLUMNS_H

#include "columntype.h"
#include "computedcolumn.h"

///
/// Collects all ComputedColumn objects and makes them accessible and interoperable
/// Also the main interface for creating and destroying them (through some help of DataSetPackage ofc)
class ComputedColumns
{
public:
	static ComputedColumns * singleton() { return _singleton; }
	static ComputedColumns * _singleton;

	typedef std::vector<ComputedColumn*>::iterator iterator;


						ComputedColumns() { if(_singleton) throw std::runtime_error("ComputedColumns can be instantiated only once!"); _singleton = this; }
						~ComputedColumns() { _singleton = nullptr; }

	void				reset();
	ComputedColumn *	createComputedColumn(std::string name, columnType type, ComputedColumn::computedType desiredType);
	void				createColumn(std::string name, columnType type);
	void				removeComputedColumn(std::string name);
	void				refreshColumnPointers();
	void				findAllColumnNames();

	bool				setConstructorJson(std::string name, std::string json);
	bool				setRCode(std::string name, std::string rCode);
	bool				setError(std::string name, std::string error);

	bool				usesRCode(std::string name);

	std::string			getConstructorJson(std::string name);
	std::string			getRCode(std::string name);
	std::string			getRCodeCommentStripped(std::string name);
	std::string			getError(std::string name);

	size_t				findIndexByName(std::string name)			const;

	iterator			begin()			{ return _computedColumns.begin();	}
	iterator			end()			{ return _computedColumns.end();	}
	size_t				columnCount()	{ return _computedColumns.size();	}

	void setPackageModified();

			ComputedColumn & operator[](size_t i)					{ return *_computedColumns[i]; }
	const	ComputedColumn & operator[](size_t i)			const	{ return *_computedColumns[i]; }
			ComputedColumn & operator[](std::string name)			{ return *_computedColumns[findIndexByName(name)]; }
	const	ComputedColumn & operator[](std::string name)	const	{ return *_computedColumns[findIndexByName(name)]; }

	Json::Value			convertToJson();
	void				convertFromJson(Json::Value json);

private:

	std::vector<ComputedColumn*>	_computedColumns;
};

#endif // COMPUTEDCOLUMNS_H
