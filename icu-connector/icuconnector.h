#ifndef ICUCONNECTOR_H
#define ICUCONNECTOR_H


#include <string>
#include <map>
#include <vector>


namespace icu
{


/**
 * @brief The Icuconnector class
 *
 * A Singlton class that deals with Code page converstion.
 * Most of the rela work is done by the IBM Library ICU.
 */
class ICUConnector
{

public:

	/**
	 * @brief getCharSets Finds all the suported char sets.
	 * @return All the known char sets.
	 */
	static std::vector<std::string> getCharSets();

	/**
	 * @brief guessCharSet Attemps to guess a char set from a code page.
	 * @param codePage The Code page to work with.
	 * @return std::string The guessed char set.
	 *
	 */
	static std::string guessCharSet(int codePage);

	/**
	 * @brief Gets an instance of the class.
	 * @param charSet The name of the char set to use.
	 *
	 * Charsets might be "Windows-1267", "utf-8" or "codepage-437"
	 *
	 */
	static ICUConnector * get(const char * charSet);
	static ICUConnector * get(const std::string &charSet) { return get(charSet.c_str()); }

	/**
	 * @brief destroy Destory one char set.
	 * @param charSet The chars set to destroy an instance of.
	 */
	static void destroy(const char *charSet);

	/**
	 * @brief destroy Destroy all instances.
	 */
	static void destroyAll();

	/**
	 * @brief get Gets the last instance got by get(const char *)
	 * @return
	 */
	static ICUConnector * get() { return _lastGot; }

	/**
	 * @brief Convert the passed string to utf-8.
	 *
	 * The character set from the getter is assumed.
	 */

	std::string toUTF8(const std::basic_string<char> &str);
	std::string toUTF8(const std::basic_string<wchar_t> &str);

	/**
	 * @brief from7ASCII Converts from ASCII to the selected char set.
	 * @return A short string (usually but not always one byte) with the converted char.
	 *
	 * Thows a std::runtime_exception if mapping not possible.
	 */
	std::string from7ASCII(char);


protected:

	/**
	 * @brief ICUConnector Ctor
	 * @param const char *charSet The chars set to convert from.
	 *
	 * Proctected for Singleton behaviour.
	 */
	ICUConnector(const char *charSet);

	~ICUConnector();

private:

	// Instaces.
	static std::map<std::string, ICUConnector *>	_instances;
	static ICUConnector* 							_lastGot;

	// Instance data.
	std::string		_charSet;

};

} // namespace icu.

#endif // ICUCONNECTOR_H
