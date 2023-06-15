#ifndef EMPTYVALUES_H
#define EMPTYVALUES_H

#include "json/json.h"
#include "utils.h"

class EmptyValues
{
protected:
	typedef std::map<std::string, std::map<int, std::string>>	emptyValsType;

public:
				EmptyValues();

	void		fromJson(const Json::Value & json);
	Json::Value toJson() const;

private:
	void				storeInEmptyValues(const std::string & columnName, const intstrmap & emptyValues)	{ _map[columnName] = emptyValues;		}
	void				resetEmptyValues()																	{ _map.clear();							}


	emptyValsType		_map;

	friend class DataSet;
};

#endif // EMPTYVALUES_H
