#include "icuconnector.h"



using namespace std;
using namespace icu;


map<string, ICUConnector *>	ICUConnector::_instances;
ICUConnector* ICUConnector::_lastGot = 0;


ICUConnector::ICUConnector(const char * charSet)
	:_charSet(charSet)
{
	_lastGot = this;
	_instances.insert(pair<string, ICUConnector *>(charSet, _lastGot));
}


ICUConnector::~ICUConnector()
{
	map<string, ICUConnector *>::iterator iter = _instances.find(_charSet);
	if (iter != _instances.end())
		_instances.erase(iter);
}



/**
 * @brief getCharSets Finds all the suported char sets.
 * @return All the known char sets.
 */
vector<string> ICUConnector::getCharSets()
{
	vector<string> result;
	result.push_back("utf-8");

	return result;
}

/**
 * @brief guessCharSet Attemps to guess a char set from a code page.
 * @param codePage The Code page to work with.
 * @return std::string The guessed char set.
 *
 */
string ICUConnector::guessCharSet(int codePage)
{
	switch(codePage)
	{
	case 65001:	// UTF-8
	case 0:		// ASCII
		return string("utf-8");

	default:
		return string();
	}
}


/**
 * @brief Gets an instance of the class.
 * @param charSet The name of the char set to use.
 *
 * Charsets might be "Windows-1267", "utf-8" or "codepage-437"
 *
 */
ICUConnector * ICUConnector::get(const char * charSet)
{
	// Already built?
	map<string, ICUConnector *>::iterator iter = _instances.find(charSet);
	if (iter != _instances.end())
		_lastGot = iter->second;
	else
		// Build a new one.
		new ICUConnector(charSet);

	return get();
}

/**
 * @brief Convert the passed string to utf-8.
 *
 * The character set from the getter is assumed.
 */

string ICUConnector::toUTF8(const string &str)
{
	return str;
}

/**
 * @brief from7ASCII Converts from ASCII to the selected char set.
 * @return A short string (usually but not always one byte) with the converted char.
 *
 */
string ICUConnector::from7ASCII(char c)
{
	string result;
	result.push_back(c);
	return result;
}
