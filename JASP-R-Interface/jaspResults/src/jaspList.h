#pragma once
#include "jaspObject.h"

template<typename T>
class jaspList : public jaspObject
{
public:
	jaspList(std::string title = "") : jaspObject(jaspObjectType::list, title), _dummyVal()
	{
		allocatedObjects->erase(this); // lists are never newed!
	}

	void insert(Rcpp::RObject field, T value)
	{
		if(Rcpp::is<Rcpp::NumericVector>(field) || Rcpp::is<Rcpp::IntegerVector>(field))
		{
			int row = Rcpp::as<int>(field) - 1;

			if(_rows.size() <= row)
				_rows.resize(row+1);

			_rows[row] = value;
		}
		else if(Rcpp::is<Rcpp::CharacterVector>(field) || Rcpp::is<Rcpp::StringVector>(field))
		{
			std::string fieldName = Rcpp::as<std::string>(field);

			_field_to_val[fieldName] = value;
		}
		else
			Rf_error("Did not get a number, integer or string to index on.");

		notifyParentOfChanges();
	}

	void add(T value)
	{
		_rows.push_back(value);
		notifyParentOfChanges();
	}

	///using [] (in c++) will give you normal zero-based array but also grows the vector if your request lies outside of it, at() ([[]] in R) however gives you 1-based access and just returns a dummy value if you request something out of range.
	T at(Rcpp::RObject field) const
	{
		if(Rcpp::is<Rcpp::NumericVector>(field) || Rcpp::is<Rcpp::IntegerVector>(field))
		{
			int row = Rcpp::as<int>(field) - 1;

			if(row > _rows.size())
				return T();

			return _rows[row];
		}
		else if(Rcpp::is<Rcpp::CharacterVector>(field) || Rcpp::is<Rcpp::StringVector>(field))
		{
			std::string fieldName = Rcpp::as<std::string>(field);

			return _field_to_val.at(fieldName);
		}
		else
			Rf_error("Did not get a number, integer or string to index on.");

		return T();
	}

	std::string dataToString(std::string prefix) const override
	{
		std::stringstream out;
		out << "{ ";

		if(_rows.size() > 0)
		{
			if(_field_to_val.size() > 0)
				out << "\n" ;

			out << "vec: [";

			int count = 0;
			for(auto row : _rows)
				out << (count++ > 0 ? ", " : "") << '"' << row << '"';

			out << "]";
		}

		if(_field_to_val.size() > 0)
		{
			std::string newPrefix = "\t" + prefix;

			if(_rows.size() > 0)
				out << "\n" << newPrefix;

			out << "map: {";

			int count = 0;
			for(auto key : _field_to_val)
				out << (count++ > 0 ? ",\n": "\n") << newPrefix << "\t\"" << key.first << "\": " << '"' << key.second<< '"';

			out << "\n" << newPrefix << "}";
		}

		out << prefix << "}";

		return out.str();
	}

	void setRows(Rcpp::List vec)
	{
		_rows.clear();
		for(auto v : vec)
			_rows.push_back(Rcpp::as<T>(v));

		Rcpp::RObject namesListRObject = vec.names();

		if(!namesListRObject.isNULL())
		{
			Rcpp::CharacterVector namesList;
			namesList = namesListRObject;

			for(int row=0; row<namesList.size(); row++)
				if(namesList[row] != "")
					_field_to_val[Rcpp::as<std::string>(namesList[row])] = Rcpp::as<T>(vec[row]);
		}
	}

	size_t rowCount()	const { return _rows.size(); }
	size_t fieldCount()	const { return _field_to_val.size(); }

	///using [] (in c++) will give you normal zero-based array but also grows the vector if your request lies outside of it, at() ([[]] in R) however gives you 1-based access and just returns a dummy value if you request something out of range.
	T & operator[](size_t index)
	{
		if(_rows.size() <= index)
			_rows.resize(index + 1); //Yes we create new entries like this but that avoids a whole lot of errors
		return _rows[index];
	}

	const T _dummyVal;

	const T & operator[](size_t index) const
	{
		if(_rows.size() <= index)
			return _dummyVal;
		return _rows.at(index);
	}

			T & operator[](std::string field)				{ return _field_to_val[field];		}
	const	T & operator[](std::string field)		const	{ return _field_to_val.count(field) > 0 ? _field_to_val.at(field) : _dummyVal; }

	bool containsField(std::string field)	const	{ return _field_to_val.count(field) > 0; }


	Json::Value convertToJSON() const override
	{
		Json::Value obj		= jaspObject::convertToJSON();
		obj["rows"]			= Json::arrayValue;
		for(auto r : _rows)
			obj["rows"].append((T)r);

		obj["fields"]		= Json::objectValue;

		for(auto k : _field_to_val)
			obj["fields"][k.first] = k.second;

		if(std::is_same<T, std::string>::value)	obj["listType"] = "string";
		else if(std::is_same<T, double>::value)	obj["listType"] = "double";
		else if(std::is_same<T, int>::value)	obj["listType"] = "int";
		else if(std::is_same<T, bool>::value)	obj["listType"] = "bool";
		else									obj["listType"] = "unknown";

		return obj;
	}

	//Specialized in cpp
	inline T convertStringFromJson(	Json::Value value)		{ return T(); }
	inline T convertDoubleFromJson(	Json::Value value)		{ return T(); }
	inline T convertIntFromJson(	Json::Value value)		{ return T(); }
	inline T convertBoolFromJson(	Json::Value value)		{ return T(); }

	void		convertFromJSON_SetFields(Json::Value in) override
	{
		jaspObject::convertFromJSON_SetFields(in);

		std::string listType = in.get("listType", "unknown").asString();

		if(		listType == "unknown"												||
				(std::is_same<T, std::string>::value	&& listType != "string")	||
				(std::is_same<T, double>::value			&& listType != "double")	||
				(std::is_same<T, bool>::value			&& listType != "bool")		||
				(std::is_same<T, int>::value			&& listType != "int")		)
			throw std::logic_error("Wrong listtype for conversion from JSON to jaspList!");

		_rows.clear();
		for(auto & row : in.get("rows", Json::arrayValue))
			if(std::is_same<T, std::string>::value)	_rows.push_back(convertStringFromJson(row));
			else if(std::is_same<T, double>::value)	_rows.push_back(convertDoubleFromJson(row));
			else if(std::is_same<T, int>::value)	_rows.push_back(convertIntFromJson(row));
			else if(std::is_same<T, bool>::value)	_rows.push_back(convertBoolFromJson(row));

		Json::Value fields(in.get("fields", Json::objectValue));
		_field_to_val.clear();
		for(auto & memberName : fields.getMemberNames())
		{
			Json::Value fieldVal = fields[memberName];

			if(std::is_same<T, std::string>::value)	_field_to_val[memberName] = convertStringFromJson(fieldVal);
			else if(std::is_same<T, double>::value)	_field_to_val[memberName] = convertDoubleFromJson(fieldVal);
			else if(std::is_same<T, int>::value)	_field_to_val[memberName] = convertIntFromJson(fieldVal);
			else if(std::is_same<T, bool>::value)	_field_to_val[memberName] = convertBoolFromJson(fieldVal);
		}
	}

private:
	std::map<std::string, T> _field_to_val;
	std::vector<T> _rows;

};

template <> inline std::string	jaspList<std::string>::	convertStringFromJson(Json::Value	value)	{ return value.asString();	}
template <>	inline double		jaspList<double>::		convertDoubleFromJson(Json::Value	value)	{ return value.asDouble();	}
template <>	inline int			jaspList<int>::			convertIntFromJson(Json::Value		value)	{ return value.asInt();		}
template <>	inline bool			jaspList<bool>::		convertBoolFromJson(Json::Value		value)	{ return value.asBool();	}

typedef jaspList<std::string>	jaspStringlist;
typedef jaspList<double>		jaspDoublelist;
typedef jaspList<int>			jaspIntlist;
typedef jaspList<bool>			jaspBoollist;

template<typename T>
class jaspList_Interface : public jaspObject_Interface
{
public:
	jaspList_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

	void insert(Rcpp::RObject field, T value)	{			static_cast<jaspList<T>*>(myJaspObject)->insert(field, value);	}
	T at(Rcpp::RObject field)					{ return	static_cast<jaspList<T>*>(myJaspObject)->at(field);				}
	void add(T value)							{			static_cast<jaspList<T>*>(myJaspObject)->add(value);			}
};

typedef jaspList_Interface<std::string>	jaspStringlist_Interface;
typedef jaspList_Interface<double>		jaspDoublelist_Interface;
typedef jaspList_Interface<int>			jaspIntlist_Interface;
typedef jaspList_Interface<bool>		jaspBoollist_Interface;

RCPP_EXPOSED_CLASS_NODECL(jaspStringlist_Interface)
RCPP_EXPOSED_CLASS_NODECL(jaspDoublelist_Interface)
RCPP_EXPOSED_CLASS_NODECL(jaspIntlist_Interface)
RCPP_EXPOSED_CLASS_NODECL(jaspBoollist_Interface)

//.constructor(									"Default constructor without setting the title explicitly")
//.constructor<std::string>(						"Constructor that sets the title explicitly")

#define JASPLIST_MODULE_EXPORT(CLASS_NAME_CPP, CLASS_NAME_R)														\
Rcpp::class_<CLASS_NAME_CPP>(CLASS_NAME_R)																			\
	.derives<jaspObject_Interface>("jaspObject")																	\
	.method( "[[",		&CLASS_NAME_CPP::at,		"Access element by fieldname (string) or index (int) ")			\
	.method( "[[<-",	&CLASS_NAME_CPP::insert,	"Insert an element under index (int) or fieldname (string)")	\
	.method( "insert",	&CLASS_NAME_CPP::insert,	"Insert an element under index (int) or fieldname (string)")	\
	.method( "add",		&CLASS_NAME_CPP::add,		"Add an element at the end of the indexable list")				\
	JASP_OBJECT_FINALIZER_LAMBDA(CLASS_NAME_CPP)																	\
;
