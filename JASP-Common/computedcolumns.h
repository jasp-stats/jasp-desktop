#ifndef COMPUTEDCOLUMNS_H
#define COMPUTEDCOLUMNS_H

#include "columns.h"
#include "computedcolumn.h"

class DataSetPackage;
class Columns;

class ComputedColumns
{
public:
	typedef std::vector<ComputedColumn*>::iterator iterator;

						ComputedColumns() {}
	ComputedColumn *	createComputedColumn(std::string name, Column::ColumnType type, ComputedColumn::computedType desiredType);
	void				removeComputedColumn(std::string name);
	void				refreshColumnPointers();
	void				setPackage(DataSetPackage * package) { _package = package; }
	void				findAllColumnNames();

	bool				setConstructorJson(std::string name, std::string json);
	bool				setRCode(std::string name, std::string rCode);
	bool				setError(std::string name, std::string error);

	bool				usesRCode(std::string name);

	std::string			getConstructorJson(std::string name);
	std::string			getRCode(std::string name);
	std::string			getError(std::string name);

	size_t				findIndexByName(std::string name)			const;

	iterator			begin()			{ return _computedColumns.begin();	}
	iterator			end()			{ return _computedColumns.end();	}
	size_t				columnCount()	{ return _computedColumns.size();	}

			ComputedColumn & operator[](size_t i)					{ return *_computedColumns[i]; }
	const	ComputedColumn & operator[](size_t i)			const	{ return *_computedColumns[i]; }
			ComputedColumn & operator[](std::string name)			{ return *_computedColumns[findIndexByName(name)]; }
	const	ComputedColumn & operator[](std::string name)	const	{ return *_computedColumns[findIndexByName(name)]; }

	Json::Value			convertToJson();
	void				convertFromJson(Json::Value json);

	Columns&			columns();

private:

	std::vector<ComputedColumn*>	_computedColumns;
	DataSetPackage*					_package = NULL;

};

#endif // COMPUTEDCOLUMNS_H
