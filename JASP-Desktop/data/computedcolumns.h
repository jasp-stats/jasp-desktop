#ifndef COMPUTEDCOLUMNS_H
#define COMPUTEDCOLUMNS_H

#include "columntype.h"
#include "computedcolumn.h"

class DataSetPackage;

class ComputedColumns
{
public:
	typedef std::vector<ComputedColumn*>::iterator iterator;

						ComputedColumns(DataSetPackage * package = NULL) { setPackage(package); }
	ComputedColumn *	createComputedColumn(std::string name, columnType type, ComputedColumn::computedType desiredType);
	void				createColumn(std::string name, columnType type);
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
	DataSetPackage*					_package = nullptr;

};

#endif // COMPUTEDCOLUMNS_H
