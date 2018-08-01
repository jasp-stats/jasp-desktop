#ifndef COMPUTEDCOLUMN_H
#define COMPUTEDCOLUMN_H

#include "columns.h"
#include "jsonredirect.h"
#include <list>

class Analysis;

class ComputedColumn
{
public:
	enum class computedType { rCode, constructorCode, analysis};

			ComputedColumn(std::vector<ComputedColumn*> * allComputedColumns, Column * col, computedType kindOfCode = computedType::constructorCode)
			: _computedColumns(allComputedColumns), _codeType(kindOfCode), _name(col->name()), _outputColumn(col)
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
			void					setAnalysis(Analysis *analysis);


			int						analysisId()					const			{ return _analysisId;						}
			std::string				rCode()							const			{ return _rCode;							}
			std::string				error()							const			{ return _error;							}
			computedType			codeType()						const			{ return _codeType;							}
			std::string				constructorJson()				const			{ return _constructorCode.toStyledString(); }
			Analysis *				analysis()										{ return _analysis;							}

			bool					isInvalidated()					const			{ return _invalidated;						}
			void					invalidate();
			void					validate()										{ _invalidated = false;						}
			bool					hasError()						const			{ return _error.size() > 0;					}

			std::set<std::string>	findUsedColumnNames()							{ return findUsedColumnNames(_rCode);		}
			std::set<std::string>	findUsedColumnNames(std::string searchThis);
			void					findDependencies();
	static	void					setAllColumnNames(std::set<std::string> names);
			bool					dependsOn(std::string columnName)				{ return dependsOnColumns().count(columnName) > 0; }
			std::set<std::string>	findThoseDependingOnMe();
			bool					iShouldBeSentAgain();
			void					checkForLoopInDepenedencies(std::string code);

	const	std::set<std::string>&	dependsOnColumns();

			Json::Value				convertToJson();

	static	std::string				computedTypeToString(computedType type);
	static	computedType			computedTypeFromString(std::string type);

			void					replaceChangedColumnNamesInRCode(std::map<std::string, std::string> changedNames);
private:
			void					_checkForLoopInDepenedencies(std::set<std::string> foundNames, std::list<std::string> loopList);

			std::vector<ComputedColumn*>	*_computedColumns;
			bool							_invalidated		= false;
			computedType					_codeType;
			std::string						_name,
											_rCode				= "#Enter your R code here :)",
											_error				= "";
			Json::Value						_constructorCode	= Json::objectValue;
			Analysis						*_analysis			= NULL;

	static	std::vector<std::string>		_allColumnNames;
			std::set<std::string>			_dependsOnColumns;

			Column							*_outputColumn;

			int								_analysisId			= -1;
};

#endif // COMPUTEDCOLUMN_H
