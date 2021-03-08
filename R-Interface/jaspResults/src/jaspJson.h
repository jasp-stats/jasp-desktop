#pragma once
#include "jaspObject.h"
#include <limits>
#include "stringutils.h"

class jaspJson : public jaspObject
{
public:
	jaspJson(Json::Value json = Json::nullValue)	: jaspObject(jaspObjectType::json, ""), _json(json) {}
	jaspJson(Rcpp::RObject Robj)					: jaspObject(jaspObjectType::json, ""), _json(RObject_to_JsonValue(Robj)) {}

	void		setValue(Rcpp::RObject Robj)	{ _json = RObject_to_JsonValue(Robj);	}
	std::string	getValue()						{ return _json.toStyledString();		}

	static Json::Value RObject_to_JsonValue(Rcpp::RObject		obj);
	static Json::Value RObject_to_JsonValue(Rcpp::List 			obj);


	template<int RTYPE>	static Json::Value RObject_to_JsonValue(Rcpp::Matrix<RTYPE>	obj)
	{
		Json::Value val(Json::arrayValue);

		for(int col=0; col<obj.ncol(); col++)
		{
			Json::Value valCol(Json::arrayValue);

			for(int row=0; row<obj.column(col).size(); row++)
				valCol.append(RMatrixColumnEntry_to_JsonValue(obj.column(col), row));

			val.append(valCol);
		}

		return val;
	}

	template<int RTYPE>	static Json::Value RObject_to_JsonValue(Rcpp::Vector<RTYPE>	obj)
	{
		Json::Value val("");

		if(obj.size() == 1)
			val = RVectorEntry_to_JsonValue(obj, 0);
		else if(obj.size() > 1)
		{
			val = Json::Value(Json::arrayValue);

			for(int row=0; row<obj.size(); row++)
				val.append(RVectorEntry_to_JsonValue(obj, row));
		}

		return val;
	}

	template<int RTYPE> static inline Json::Value RVectorEntry_to_JsonValue(Rcpp::Vector<RTYPE> obj, int row)				{ return ""; }
	template<int RTYPE> static inline Json::Value RMatrixColumnEntry_to_JsonValue(Rcpp::MatrixColumn<RTYPE> obj, int row)	{ return ""; }

	std::string dataToString(std::string prefix) const override { return jsonToPrefixedStrings(prefix + "\t"); }

			std::string jsonToPrefixedStrings(std::string prefix = "") const { return jsonToPrefixedStrings(_json, prefix); }
	static	std::string jsonToPrefixedStrings(Json::Value val, std::string prefix);

	static Json::Value RcppVector_to_ArrayJson(Rcpp::RObject obj, bool throwError=true) { return VectorJson_to_ArrayJson(RcppVector_to_VectorJson(obj, throwError)); }
	static Json::Value VectorJson_to_ArrayJson(std::vector<Json::Value> vec);
	static Json::Value SetJson_to_ArrayJson(std::set<Json::Value> set);
	static std::set<Json::Value> ArrayJson_to_SetJson(Json::Value arr);
	static std::vector<Json::Value> RList_to_VectorJson(Rcpp::List obj);

	static std::vector<Json::Value> RcppVector_to_VectorJson(Rcpp::RObject obj, bool throwError=false)
	{
		if(Rcpp::is<Rcpp::NumericVector>(obj))			return RcppVector_to_VectorJson<REALSXP>((Rcpp::NumericVector)		obj);
		else if(Rcpp::is<Rcpp::LogicalVector>(obj))		return RcppVector_to_VectorJson<LGLSXP>((Rcpp::LogicalVector)		obj);
		else if(Rcpp::is<Rcpp::IntegerVector>(obj))		return RcppVector_to_VectorJson<INTSXP>((Rcpp::IntegerVector)		obj);
		else if(Rcpp::is<Rcpp::StringVector>(obj))		return RcppVector_to_VectorJson<STRSXP>((Rcpp::StringVector)		obj);
		else if(Rcpp::is<Rcpp::CharacterVector>(obj))	return RcppVector_to_VectorJson<STRSXP>((Rcpp::CharacterVector)		obj);
		else if(Rcpp::is<Rcpp::List>(obj))				return RList_to_VectorJson((Rcpp::List)								obj);
		else if(throwError) Rf_error("JASPjson::RcppVector_to_VectorJson received an SEXP that is not a Vector of some kind.");

		return std::vector<Json::Value>({""});
	}


	template<int RTYPE>	static std::vector<Json::Value> RcppVector_to_VectorJson(Rcpp::Vector<RTYPE> obj)
	{
		std::vector<Json::Value> vec;

		for(int row=0; row<obj.size(); row++)
			vec.push_back(RVectorEntry_to_JsonValue(obj, row));

		return vec;
	}

	template<int RTYPE>	static std::vector<std::vector<Json::Value>> RcppMatrix_to_Vector2Json(Rcpp::Matrix<RTYPE>	obj)
	{
		std::vector<std::vector<Json::Value>> vecvec;

		for(int col=0; col<obj.ncol(); col++)
		{
			std::vector<Json::Value> vec;

			for(int row=0; row<obj.column(col).size(); row++)
				vec.push_back(RMatrixColumnEntry_to_JsonValue(obj.column(col), row));

			vecvec.push_back(vec);
		}

		return vecvec;
	}

	Json::Value	metaEntry()									const	override { return constructMetaEntry("json"); }
	Json::Value	dataEntry(std::string & errorMessage)		const	override;

	Json::Value convertToJSON()								const	override;
	void		convertFromJSON_SetFields(Json::Value in)			override;

protected:
	Json::Value _json;
};

template<> inline Json::Value jaspJson::RVectorEntry_to_JsonValue<INTSXP>(Rcpp::Vector<INTSXP> obj, int row)					{ return obj[row] == NA_INTEGER	? "" : Json::Value((int)(obj[row]));			}
template<> inline Json::Value jaspJson::RMatrixColumnEntry_to_JsonValue<INTSXP>(Rcpp::MatrixColumn<INTSXP> obj, int row)		{ return obj[row] == NA_INTEGER	? "" : Json::Value((int)(obj[row]));			}

template<> inline Json::Value jaspJson::RVectorEntry_to_JsonValue<LGLSXP>(Rcpp::Vector<LGLSXP> obj, int row)					{ return obj[row] == NA_LOGICAL	? "" : Json::Value((bool)(obj[row]));			}
template<> inline Json::Value jaspJson::RMatrixColumnEntry_to_JsonValue<LGLSXP>(Rcpp::MatrixColumn<LGLSXP> obj, int row)		{ return obj[row] == NA_LOGICAL	? "" : Json::Value((bool)(obj[row]));			}

template<> inline Json::Value jaspJson::RVectorEntry_to_JsonValue<STRSXP>(Rcpp::Vector<STRSXP> obj, int row)					{ return obj[row] == NA_STRING	? "" : Json::Value(stringUtils::escapeHtmlStuff((std::string)(obj[row])));	}
template<> inline Json::Value jaspJson::RMatrixColumnEntry_to_JsonValue<STRSXP>(Rcpp::MatrixColumn<STRSXP> obj, int row)		{ return obj[row] == NA_STRING	? "" : Json::Value(stringUtils::escapeHtmlStuff((std::string)(obj[row])));	}

#define TO_INFINITY_AND_BEYOND																					\
{																												\
	double val = static_cast<double>(obj[row]);																	\
	return	R_IsNA(val) ? "" :																					\
				R_IsNaN(val) ? "NaN" :																			\
					val == std::numeric_limits<double>::infinity() ? "\u221E" :									\
						val == -1 * std::numeric_limits<double>::infinity() ? "-\u221E"  :						\
							Json::Value((double)(obj[row]));													\
}

template<> inline Json::Value jaspJson::RVectorEntry_to_JsonValue<REALSXP>(Rcpp::Vector<REALSXP> obj, int row)				TO_INFINITY_AND_BEYOND
template<> inline Json::Value jaspJson::RMatrixColumnEntry_to_JsonValue<REALSXP>(Rcpp::MatrixColumn<REALSXP> obj, int row)	TO_INFINITY_AND_BEYOND


class jaspJson_Interface : public jaspObject_Interface
{
public:
	jaspJson_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

	void 		setValue(Rcpp::RObject Robj)	{ 			static_cast<jaspJson *>(myJaspObject)->setValue(Robj); }
	std::string geValue()						{ return 	static_cast<jaspJson *>(myJaspObject)->getValue(); }
};

RCPP_EXPOSED_CLASS_NODECL(jaspJson)
