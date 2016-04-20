#include "icuconnector.h"

#include <unicode/ucnv.h>
#include <sstream>

#include <boost/lexical_cast.hpp>

using namespace std;
using namespace boost;
using namespace icu;


map< ICUConnector::CodePage, ICUConnector *>	ICUConnector::_instances;

void checkICUErrorCode(enum UErrorCode code)
{
	if (U_FAILURE(code))
		throw runtime_error("ICU failure number" + lexical_cast<string>((int) code) + ".");
}

/**
 * @brief ICUConnector Ctor
 * @param CodePage codepage - The code page we are interested in.
 *
 * Proctected for Singleton behaviour.
 */
ICUConnector::ICUConnector(CodePage codepage)
	:_converter(0)
	,_codepage(codepage)
	,_charset(guessCharSet(codepage))
{
	// Get the ICU convertor (to unicode).
	UErrorCode ec = U_ZERO_ERROR;
	_converter = ucnv_open(_charset.c_str(), &ec);
	checkICUErrorCode(ec);

	_instances.insert(Instance(codepage, this));
}


ICUConnector::~ICUConnector()
{
	Instances::iterator iter;
	if ((iter = _findInstance(this)) != _endInstance())
		_instances.erase(iter);

	if (_converter != 0)
		ucnv_close(_converter);
}



/**
 * @brief getCharSets Finds all the supported char sets.
 * @return All the known char sets.
 */
vector<string> ICUConnector::getCharSets()
{
	vector<string> result;

	UErrorCode errorCode = U_ZERO_ERROR;

	{
		UEnumeration *codeEnum = ucnv_openAllNames( &errorCode );
		if ((U_FAILURE(errorCode)) || (codeEnum == 0))
			return result;	// Allways possible.

		int32_t strLen= 0;
		const char * str = uenum_next(codeEnum, &strLen, &errorCode);
		if (U_FAILURE(errorCode)) return result;
		while (str != 0)
		{
			// Grab results,
			result.push_back(string(str));

			// Next string.
			str = uenum_next(codeEnum, &strLen, &errorCode);
			if (U_FAILURE(errorCode)) return result;
		}
		uenum_close(codeEnum);
	}

	// This got all possible names,

	return result;
}

/**
 * @brief guessCharSet Attemps to guess a char set from a code page.
 * @param codePage The Code page to work with.
 * @return std::string The guessed char set.
 *
 */
string ICUConnector::guessCharSet(ICUConnector::CodePage codePage)
{
	stringstream buffer;

	const static string _utf("UTF-");
	const static string _ibm("ibm-");
	const static string _ISO_8859("ISO-8859-");
	const static string _iso_8859("iso-8859_");
	const static string _macos("macos-");

	switch(codePage)
	{
	case us_ascii:
		buffer << "US-ASCII";
		break;
	case utf_7:			// Unicode
		buffer << _utf << 7;
		break;
	case utf_8:			// Unicode
		buffer << _utf << 8;
		break;
	case win1250:	case win1251:	case win1252:	case win1253:
	case win1254:	case win1255:	case win1256:	case win1257:
	case win1258:	case dos437:	case dos720:	case dos737:
	case dos755:	case dos850:	case dos852:	case dos855:
	case dos857:	case dos858:	case dos860:	case dos861:
	case dos862:	case dos863:	case dos865:	case dos866:
	case dos869:
		buffer << _ibm << (int) codePage;
		break;
	case IS08859_1:
	buffer << _ISO_8859 << "1";
		break;
	case ISO8859_2:	case ISO8859_3:	case ISO8859_4:	case ISO8859_5:
	case ISO8859_6:	case ISO8859_7:	case ISO8859_8:	case ISO8859_9:
		break;	// No name avilable.
	case ISO8859_10:
		buffer << _iso_8859 << "10";
		break;
	case ISO8859_11:
		buffer << _iso_8859 << "11";
		break;
	case ISO8859_13:
		break;	// No name avilable.
	case ISO8859_14:
		buffer << _iso_8859 << "14";
		break;
	case ISO8859_15:	case ISO8859_16:
		break;	// No name avilable.
	case EBCDIC_US:
		buffer << _ibm << "37";
		break;
	case macintosh:	// mac (general)
		buffer << _macos << "0_2";
		break;
	case mac_greek:	// mac (greek)
		buffer << _macos << "6_2";
		break;
	case mac_cyrillic:	// mac (cyrillic)
		buffer << _macos << "7_3";
		break;
	case mac_thai:	// Thai
		buffer << _macos << "21-10";
		break;
	case mac_c_europe:
		buffer << _macos << "29-10";
		break;
	case mac_symbol:
		buffer << _macos << "33-10";
		break;
	case mac_dingbat:
		buffer << _macos << "34-10";
		break;
	case mac_turkish:
		buffer << _macos << "35-10";
		break;
	case mac_croation:
		buffer << _macos << "36_2";
		break;
	case mac_iceland:
		buffer << _macos << "37_5";
		break;
	case mac_romania:
		buffer << _macos << "38_2";
		break;
	case mac_arabic:
		buffer << _macos << "518-10";
		break;
	case mac_hebrew:
		buffer << _macos << "1285-10";
		break;
	case _unknown_codePage:
		buffer << _ibm << 437;
	default:
		return string();
	}

	return buffer.str();
}




/**
 * @brief Gets an instance of the class.
 * @param CodePage source - The source code page.
 * @param CodePage destination - The destination code page.
 *
 */
ICUConnector * ICUConnector::get(CodePage codepage)
{
	ICUConnector * result = 0;
	// Already built?
	Instances::iterator iter;
	if ((iter = _findInstance(codepage)) != _endInstance())
		result = iter->second;
	else
		// Build a new one.
		result = new ICUConnector(codepage);

	return result;
}

/**
 * @brief destroy Destory one char set.
 * @param CodePage srcCp Source Code page.
 * @param CodePage dstCP Desination code page.
 */
void ICUConnector::destroy(CodePage codepage)
{
	Instances::iterator iter;
	if ((iter = _findInstance(codepage)) != _endInstance())
	{
		delete iter->second;
		_instances.erase(iter);
	}
}

/**
 * @brief destroy Destroy all instances.
 */
void ICUConnector::destroyAll()
{
	while (_instances.size() > 0)
	{
		Instances::iterator i = _instances.begin();
		delete i->second;
	}
	_instances.clear();
}

/**
 * @brief convertToUnicode Converts instring to unicode.
 * @param instring Value to convert.
 * @return Unicode Codepage conversion of instring.
 */
string ICUConnector::convert(const UCharBuffer &instring)
{
	// Start with dest buffer same size as source.
	int32_t destBufferSize = instring.size();
	char *destBuffer = new char[destBufferSize];
	for (UErrorCode ec = U_BUFFER_OVERFLOW_ERROR;
		 ec == U_BUFFER_OVERFLOW_ERROR; )
	{
		memset(destBuffer, 0, destBufferSize);
		ec = U_ZERO_ERROR;
		int32_t res =  ucnv_fromUChars(_converter, destBuffer, destBufferSize,
									 instring.get(), instring.size(), &ec);
		if (ec == U_BUFFER_OVERFLOW_ERROR)
		{
			// Resize dest buffer
			delete destBuffer;
			destBufferSize = res;
			destBuffer = new char[destBufferSize];
		}
		else
			checkICUErrorCode(ec);
	}

	string result(destBuffer, destBufferSize);
	delete destBuffer;
	return result;
}

ICUConnector::UCharBuffer ICUConnector::convert(const string &instring)
{
	// Start with dest buffer same size as source.
	int32_t destBufferSize = instring.size();
	UChar *destBuffer = new UChar[destBufferSize];
	for (UErrorCode ec = U_BUFFER_OVERFLOW_ERROR;
		 ec == U_BUFFER_OVERFLOW_ERROR; )
	{
		memset(destBuffer, 0, destBufferSize);
		ec = U_ZERO_ERROR;
		int32_t res =  ucnv_toUChars(_converter, destBuffer, destBufferSize,
									 instring.data(), instring.size(), &ec);
		if (ec == U_BUFFER_OVERFLOW_ERROR)
		{
			// Resize dest buffer
			delete destBuffer;
			destBufferSize = res;
			destBuffer = new UChar[destBufferSize];
		}
		else
			checkICUErrorCode(ec);
	}

	UCharBuffer result;
	result.set(destBuffer, destBufferSize);
	delete destBuffer;
	return result;
}

/**
 * @brief from7ASCII Converts from ASCII to the selected char set.
 * @return A short string (usually but not always one byte) with the converted char.
 *
 */
string ICUConnector::from7ASCII(char c)
{
	// We really mean 7 bit ASCII
	c &= 0x7f;

	string result;
	result.push_back(c);
	return result;
}

/**
 * @brief _findInstance Finds an instance of this class.
 * @param codepage - Search for...
 * @param inst - Instance to find.
 * @return
 */
ICUConnector::Instances::iterator ICUConnector::_findInstance(CodePage codepage)
{
	return _instances.find(codepage);
}

ICUConnector::Instances::iterator ICUConnector::_findInstance(const ICUConnector *inst)
{
	return _findInstance(inst->_codepage);
}

/*****************************************************************
 *
 * Subclass UCharBuffer
 *
 *****************************************************************/


ICUConnector::UCharBuffer::UCharBuffer()
	: _size(0)
{
}

// Set Zero terminated value.
void ICUConnector::UCharBuffer::set(const UChar *value)
{
	size_t newsize = 0;
	const UChar * ptr = value;
	while (*ptr++ != 0)
		newsize++;
	set(value, newsize);
}

// Set length.
void ICUConnector::UCharBuffer::set(const UChar *value, int32_t length)
{
	resize(length);
	memcpy(_buffer.get(), value, sizeof(UChar) * length);
}

void ICUConnector::UCharBuffer::resize(int32_t size)
{
	if (_size != size)
	{
		_size = size;
		_buffer.reset(new UChar[size]);
	}
}
