#ifndef COMPUTEDCOLUMN_H
#define COMPUTEDCOLUMN_H

#include "columns.h"
#include "jsonredirect.h"
#include <list>

// #include "analysis.h" // for use later on

class ComputedColumn
{
public:
	enum class computedType { rCode, constructorCode};

	ComputedColumn(std::vector<ComputedColumn*> * allComputedColumns, Column * col, computedType kindOfCode = computedType::constructorCode) : _computedColumns(allComputedColumns), _codeType(kindOfCode), _name(col->name()), _constructorCode(Json::objectValue), _outputColumn(col)
	{
		_constructorCode["formulas"] = Json::arrayValue;
	}
	ComputedColumn(std::vector<ComputedColumn*> * allComputedColumns, Columns * columns, Json::Value json); //Conversion from JSON!

			std::string				name()							const			{ return _name;					}
			void					setColumn(Column * newPointer)					{ _outputColumn = newPointer;	}
			Column					*column()										{ return _outputColumn;			}

			Column::ColumnType		columnType()					const			{ return _outputColumn->columnType();	}

			bool					setRCode(std::string rCode);
			bool					setError(std::string error);
			bool					setConstructorJson(std::string jsonStr);

			std::string				rCode()											{ return _rCode; }
			std::string				error()											{ return _error; }
			computedType			codeType()										{ return _codeType; }
			std::string				constructorJson()								{ return _constructorCode.toStyledString(); }

			bool					isInvalidated()					const			{ return _invalidated; }
			void					invalidate();
			void					validate()										{ _invalidated = false; }
			bool					hasError()						const			{ return _error.size() > 0; }

			std::set<std::string>	findUsedColumnNames()							{ return findUsedColumnNames(_rCode); }
			std::set<std::string>	findUsedColumnNames(std::string searchThis);
	static	void					setAllColumnNames(std::set<std::string> names);
			bool					dependsOn(std::string columnName)				{ return _dependsOnColumns.count(columnName) > 0; }
			std::set<std::string>	findThoseDependingOnMe();
			bool					iShouldBeSentAgain();
			void					checkForLoopInDepenedencies(std::string code);

			Json::Value				convertToJson();

	static	std::string				computedTypeToString(computedType type);
	static	computedType			computedTypeFromString(std::string type);

private:
			void					_checkForLoopInDepenedencies(std::set<std::string> foundNames, std::list<std::string> loopList);

			std::vector<ComputedColumn*>	*_computedColumns;
			bool							_invalidated = false;
			computedType					_codeType = computedType::rCode;
			std::string						_name,
											_rCode = "#Enter your R code here :)",
											_error = "";
			Json::Value						_constructorCode;

	static	std::vector<std::string>		_allColumnNames;
			std::set<std::string>			_dependsOnColumns;

			Column							*_outputColumn;

	//Analysis*		_sourceAnalysis = NULL; //How do we know what to do with it?
	//dependencies ought to be added and taken into account

};

#endif // COMPUTEDCOLUMN_H
