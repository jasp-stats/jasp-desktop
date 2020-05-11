#ifndef ENUMUTILITIES_H
#define ENUMUTILITIES_H

//See https://stackoverflow.com/a/48820063
//I expanded it a bit and added from string -> enum and QString stuff
//Also made sure the methods and map and all are defined only in the cpp file (when defining ENUM_DECLARATION_CPP first that is)

#include "stringutils.h"
#include <algorithm>
#include <sstream>
#include <string>
#include <vector>
#include <map>

#ifdef JASP_USES_QT_HERE
#include <QString>
#endif

#define STRING_REMOVE_CHAR(str, ch) str.erase(std::remove(str.begin(), str.end(), ch), str.end())

#define DECLARE_ENUM_WITH_TYPE_BASE(E, T, ...)																	\
	enum class E : T																							\
	{																											\
		__VA_ARGS__																								\
	};																											\
	std::ostream	&operator<<(std::ostream &os, E enumTmp);													\
	size_t			operator*(E enumTmp);																		\
	std::string		operator~(E enumTmp);																		\
	std::string		operator+(std::string &&str, E enumTmp);													\
	std::string		operator+(E enumTmp, std::string &&str);													\
	std::string		&operator+=(std::string &str, E enumTmp);													\
	E				operator++(E &enumTmp);																		\
	E				E##FromString(std::string enumName);														\
	E				E##FromString(std::string enumName, E devaultValue);										\
	std::string		E##ToString(E enumVal);																		\
	bool			valid##E(T value);

#define DECLARE_ENUM_METHODS_WITH_TYPE_BASE(E, T, ...)															\
	std::map<T, std::string>	E##MapName(generateEnumMap<T>(#__VA_ARGS__));									\
	std::map<std::string, T>	E##FromNameMap(generateEnumNameMap<T>(#__VA_ARGS__));							\
	std::ostream				&operator<<(std::ostream &os, E enumTmp)										\
	{																											\
		os << E##MapName[static_cast<T>(enumTmp)];																\
		return os;																								\
	}																											\
	size_t		operator*(E enumTmp) { (void) enumTmp; return E##MapName.size(); }								\
	std::string operator~(E enumTmp) { return E##MapName[static_cast<T>(enumTmp)]; }							\
	std::string operator+(std::string &&str, E enumTmp) { return str + E##MapName[static_cast<T>(enumTmp)]; }	\
	std::string operator+(E enumTmp, std::string &&str) { return E##MapName[static_cast<T>(enumTmp)] + str;	}	\
	std::string &operator+=(std::string &str, E enumTmp)														\
	{																											\
		str += E##MapName[static_cast<T>(enumTmp)];																\
		return str;																								\
	}																											\
	E operator++(E &enumTmp)																					\
	{																											\
		auto iter = E##MapName.find(static_cast<T>(enumTmp));													\
		if (iter == E##MapName.end() || std::next(iter) == E##MapName.end())									\
			iter = E##MapName.begin();																			\
		else																									\
		{																										\
			++iter;																								\
		}																										\
		enumTmp = static_cast<E>(iter->first);																	\
		return enumTmp;																							\
	}																											\
	E E##FromString(std::string enumName)																		\
	{																											\
		if(E##FromNameMap.count(enumName) == 0) 																\
			throw std::runtime_error(#E" enum from string misses requested value \""+enumName+"\"");			\
		return (E)E##FromNameMap.at(enumName); 																	\
	}																											\
	E E##FromString(std::string enumName, E defaultValue)														\
	{																											\
		if(E##FromNameMap.count(enumName) == 0) 																\
			return defaultValue;																				\
		return (E)E##FromNameMap.at(enumName); 																	\
	}																											\
	std::string E##ToString(E enumVal)		{ return ~enumVal; }												\
	bool		valid##E(T value) { return (E##MapName.find(value) != E##MapName.end()); }

#ifdef JASP_USES_QT_HERE
	#define DECLARE_ENUM_WITH_TYPE_HEADER(E, T, ...)																				\
	DECLARE_ENUM_WITH_TYPE_BASE(E, T, __VA_ARGS__)																					\
	inline E		E##FromQString(QString enumName)				{ return (E)E##FromString(enumName.toStdString()); }			\
	inline E		E##FromQString(QString enumName, E defaultVal)	{ return (E)E##FromString(enumName.toStdString(), defaultVal);}	\
	inline QString	E##ToQString(E enumVal)							{ return QString::fromStdString(E##ToString(enumVal)); }		\
	inline QString	operator+(QString &&str, E enumTmp)				{ return str + E##ToQString(enumTmp); }							\
	inline QString	operator+(E enumTmp, QString &&str)				{ return E##ToQString(enumTmp) + str;	}						\
	inline QString	&operator+=(QString &str, E enumTmp)																			\
	{																																\
		str += E##ToQString(enumTmp);																								\
		return str;																													\
	}

#define DECLARE_ENUM_WITH_TYPE_IMPLEMENTATION(E, T, ...)		DECLARE_ENUM_METHODS_WITH_TYPE_BASE(E, T, __VA_ARGS__)
#else
	#define DECLARE_ENUM_WITH_TYPE_HEADER(E, T, ...)			DECLARE_ENUM_WITH_TYPE_BASE(E, T, __VA_ARGS__)
	#define DECLARE_ENUM_WITH_TYPE_IMPLEMENTATION(E, T, ...)	DECLARE_ENUM_METHODS_WITH_TYPE_BASE(E, T, __VA_ARGS__)
#endif

#ifdef ENUM_DECLARATION_CPP
#define DECLARE_ENUM(E, ...)											\
	DECLARE_ENUM_WITH_TYPE_HEADER(			E, int32_t, __VA_ARGS__)	\
	DECLARE_ENUM_WITH_TYPE_IMPLEMENTATION(	E, int32_t, __VA_ARGS__)

#define DECLARE_ENUM_WITH_TYPE(E, T, ...)								\
	DECLARE_ENUM_WITH_TYPE_HEADER(			E, T,		__VA_ARGS__)	\
	DECLARE_ENUM_WITH_TYPE_IMPLEMENTATION(	E, T,		__VA_ARGS__)
#else
#define DECLARE_ENUM(E, ...)				DECLARE_ENUM_WITH_TYPE_HEADER(E, int32_t, __VA_ARGS__)
#define DECLARE_ENUM_WITH_TYPE(E, T, ...)	DECLARE_ENUM_WITH_TYPE_HEADER(E, T, __VA_ARGS__)
#endif

template <typename T> std::map<T, std::string> generateEnumMap(std::string strMap)
{
	STRING_REMOVE_CHAR(strMap, ' ');
	STRING_REMOVE_CHAR(strMap, '(');

	std::vector<std::string> enumTokens(stringUtils::splitString(strMap));
	std::map<T, std::string> retMap;
	T inxMap;

	inxMap = 0;
	for (auto & tokenString : enumTokens)
	{
		// Token: [EnumName | EnumName=EnumValue]
		std::string enumName;
		T enumValue;
		if (tokenString.find('=') == std::string::npos)
			enumName = tokenString;
		else
		{
			std::vector<std::string> enumNameValue(stringUtils::splitString(tokenString, '='));
			enumName = enumNameValue[0];
			//inxMap = static_cast<T>(enumNameValue[1]);
			if (std::is_unsigned<T>::value)		inxMap = static_cast<T>(std::stoull(enumNameValue[1], 0, 0));
			else								inxMap = static_cast<T>(std::stoll(enumNameValue[1], 0, 0));
		}
		retMap[inxMap++] = enumName;
	}

	return retMap;
}

template <typename T> std::map<std::string, T> generateEnumNameMap(std::string strMap)
{
	std::map<T, std::string> opposite = generateEnumMap<T>(strMap);
	std::map<std::string, T> retMap;

	for(auto keyval : opposite)
		retMap[keyval.second] = keyval.first;

	return retMap;
}

#endif //ENUMUTILITIES_H, because the rest *should* be allowed to be defined double if the specific enum class does not exist yet:
