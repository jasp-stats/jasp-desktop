#ifndef R_FUNCTIONWHITELIST_H
#define R_FUNCTIONWHITELIST_H

#include <set>
#include <regex>
#include <sstream>
#include <iostream>

class R_FunctionWhiteList
{
private:
	///The following functions (and keywords that can be followed by a '(') will be allowed in user-entered R-code, such as filters or computed columns. This is for security because otherwise JASP-files could become a vector of attack and that doesn't refer to an R-datatype.
	static const std::set<std::string> functionWhiteList;
	static const std::string functionPrecedingCharacter, functionStartDelimit, functionNameStart, functionNameBody;
	static const std::regex functionNameMatcher, functionNamePrecederMatcher;

public:
	///Checks script for unsafe function-calls (all functions that aren't in R_FunctionWhiteList) and returns the set of unsafe calls. If it is empty then the script is deemed safe.
	static std::set<std::string> scriptIsSafe(std::string const & script);

	///returns the whitelisted functions in a string, each function on its own line.
	static std::string returnOrderedWhiteList();
};

#endif // R_FUNCTIONWHITELIST_H
