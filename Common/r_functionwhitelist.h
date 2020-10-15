#ifndef R_FUNCTIONWHITELIST_H
#define R_FUNCTIONWHITELIST_H

#include <set>
#include <regex>
#include <sstream>

///New exception to give feedback about possibly failing filters and such
class filterException : public std::logic_error
{
public:
	filterException(const std::string & what_arg)	: std::logic_error(what_arg) {}
	filterException(const char * what_arg)			: std::logic_error(what_arg) {}
};


class R_FunctionWhiteList
{
private:
	///The following functions (and keywords that can be followed by a '(') will be allowed in user-entered R-code, such as filters or computed columns. This is for security because otherwise JASP-files could become a attack-vector (which doesn't refer to an R-datatype).
	static const std::set<std::string> functionWhiteList;
	static const std::string functionPrecedingCharacter, functionStartDelimit, functionNameStart, functionNameBody, operatorsR;
	static const std::regex functionNameMatcher, functionNamePrecederMatcher, assignmentWhiteListedRightMatcher, assignmentWhiteListedLeftMatcher, assignmentOperatorRightMatcher, assignmentOperatorLeftMatcher;

public:
	///throws a filterexception if the script is not legal for some reason
	static void scriptIsSafe(std::string const & script);

	///Checks script for unsafe function-calls (all functions that aren't in R_FunctionWhiteList) and returns the set of unsafe calls. If it is empty then the script is deemed safe.
	static std::set<std::string> findIllegalFunctions(std::string const & script);

	///Checks if someone is trying to overwrite whitelisted functions by other functions (like: "mean <- system")
	static std::set<std::string> findIllegalFunctionsAliases(std::string const & script);

	///returns the whitelisted functions in a string, each function on its own line.
	static std::string returnOrderedWhiteList();
};

#endif // R_FUNCTIONWHITELIST_H
