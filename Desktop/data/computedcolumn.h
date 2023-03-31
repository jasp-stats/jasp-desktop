#ifndef COMPUTEDCOLUMN_H
#define COMPUTEDCOLUMN_H

#include <json/json.h>
#include <list>
#include <set>

class Analysis;

///
/// Handles everything related to a single computed column
/// Checks whether it has dependencies on other computed columns, on analyses (if they generate a computed column for instance)
/// Also used to make sure the computed column are reran if any of their sources of information change (like a normal column)
/// Furthermore can convert itself to and fro json for easy storage inside a jasp-file.
class ComputedColumn
{
public:
	enum class computedType { rCode, constructorCode, analysis, analysisNotComputed};

			ComputedColumn(std::string name, std::vector<ComputedColumn*> * allComputedColumns, computedType kindOfCode = computedType::constructorCode)
			: _computedColumns(allComputedColumns), _codeType(kindOfCode), _name(name)
			{
				_constructorCode["formulas"] = Json::arrayValue;
			}

			ComputedColumn(std::vector<ComputedColumn*> * allComputedColumns, Json::Value json); //Conversion from JSON!


			std::string				name()							const			{ return _name;								}

			bool					setRCode(std::string rCode);
			bool					setError(std::string error);
			bool					setConstructorJson(std::string jsonStr);
			void					setAnalysis(Analysis *analysis);


			int						analysisId()					const			{ return _analysisId;						}
			std::string				rCode()							const			{ return _rCode;							}
			std::string				rCodeCommentStripped()			const			{ return _rCodeStripped;					}
			std::string				error()							const			{ return _error;							}
			computedType			codeType()						const			{ return _codeType;							}
			std::string				constructorJson()				const			{ return _constructorCode.toStyledString(); }
			Analysis *				analysis()										{ return _analysis;							}

			bool					isInvalidated()					const			{ return _invalidated;						}
			void					invalidate();
			void					invalidateDependents();
			void					validate()										{ _invalidated = false;						}
			bool					hasError()						const			{ return _error.size() > 0;					}

			std::set<std::string>	findUsedColumnNames()							{ return findUsedColumnNames(_rCode);		}
			std::set<std::string>	findUsedColumnNames(std::string searchThis);
	static	std::set<std::string>	findUsedColumnNamesStatic(std::string searchThis);

			void					findDependencies();
	static	void					setAllColumnNames(std::vector<std::string> names);
			bool					dependsOn(std::string columnName, bool refresh = true);
			std::set<std::string>	findThoseDependingOnMe();
			bool					iShouldBeSentAgain();
			void					checkForLoopInDepenedencies(std::string code);

	const	std::set<std::string>&	dependsOnColumns(bool refresh = true);

			Json::Value				convertToJson();

	static	std::string				computedTypeToString(computedType type);
	static	computedType			computedTypeFromString(std::string type);			

private:
			void					_checkForLoopInDepenedencies(std::set<std::string> foundNames, std::list<std::string> loopList);

			std::vector<ComputedColumn*>	*_computedColumns;
			bool							_invalidated		= false;
			computedType					_codeType;
			std::string						_name,
											_rCode				= "#Enter your R code here :)",
											_error				= "",
											_rCodeStripped		= "";
			Json::Value						_constructorCode	= Json::objectValue;
			Analysis						*_analysis			= nullptr;

	static	std::vector<std::string>		_allColumnNames;
			std::set<std::string>			_dependsOnColumns;

			int								_analysisId			= -1;
};

#endif // COMPUTEDCOLUMN_H
