#ifndef EMPTYVALUES_H
#define EMPTYVALUES_H

#include "json/json.h"
#include "utils.h"
#include "stringutils.h"

class EmptyValues
{
protected:
	typedef std::map<std::string, std::map<int, std::string>>	missingDataMap;

public:
							EmptyValues();
							~EmptyValues();

	static	EmptyValues *	singleton() { return _singleton; }

			void			fromJson(				const Json::Value	& json);
			Json::Value		toJson() const;

	const	stringset &		emptyValues(			const std::string	& colName)								const;
	const	doubleset &		doubleEmptyValues(		const std::string	& colName)								const;
	const	stringset &		workspaceEmptyValues()																const;
	const	doubleset &		workspaceDoubleEmptyValues()														const;
	const	intstrmap &		missingData(			const std::string	& colName)								const;
			bool			hasCutomEmptyValues(	const std::string	& colName)								const;
			void			setWorkspaceEmptyValues(const stringset		& values);
			void			setCustomEmptyValues(	const std::string	& colName, const stringset	& values);
			void			setMissingData(			const std::string	& colName, const intstrmap	& data);
			void			setHasCustomEmptyValues(const std::string	& colName, bool hasCustom);

private:
	void					resetEmptyValues();

	Json::Value				stringSetToJson(const stringset& vec)		const;
	stringset				jsonToStringSet(const Json::Value& json)	const;
	doubleset				getDoubleValues(const stringset& values)	const;


	missingDataMap						_missingData;
	stringset							_workspaceEmptyValues;
	doubleset							_workspaceDoubleEmptyValues;
	std::map<std::string, stringset>	_customEmptyValuesPerColumn;
	std::map<std::string, doubleset>	_customDoubleEmptyValuesPerColumn;

	const intstrmap						_emptyMissingData;

	static EmptyValues *				_singleton; //I hope this really is a singleton, but I think there is never more than 1 DataSet active at the same time?
};

#endif // EMPTYVALUES_H
