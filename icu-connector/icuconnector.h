#ifndef ICUCONNECTOR_H
#define ICUCONNECTOR_H


#include <string>
#include <map>
#include <vector>
#include <memory>
#include <stdint.h>

#include <unicode/ucnv.h>

/*
 * Disbale depricated warnings: CLang (on Mac)
 * doesn't yet have std:unqiue_ptr available.
 */
#pragma GCC diagnostic push

/**
 * @brief The Icuconnector class
 *
 * A Singlton class that deals with Code page converstion.
 * Most of the rela work is done by the IBM Library ICU.
 */
class ICUConnector
{

public:

	class UCharBuffer
	{
	public:
		UCharBuffer();

		const UChar *get() const { return _buffer.get(); }
		void set(const UChar *value);					// Set Zero terminated value.
		void set(const UChar *value, int32_t length);	// Set length.
		void resize(int32_t size);

		int32_t size() const { return _size; }

	private:
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
        std::auto_ptr<UChar>	_buffer;
#pragma GCC diagnostic pop
		int32_t					_size;
	};

	/*
	 * Known code pages, as understood by IBM/Windows (> 0)
	 * Non IBM/Windows char sets (< 0)
	 */
	typedef enum e_codepages
	{
		us_ascii	= 20127,	// 7 bit ASCII
		utf_7		= 650000,	// Unicode 7 -bit coded.
		utf_8		= 650001,	// Unicode 8 -bit coded.
		// No 16 bit!
//		utf_16le	= 1200,		// Unicode 16 Little Endain
//		utf_16be	= 1201,		// Unicode 16 Big Endain
		win1250		= 1250,		// Windows Latin 2 Central Europe.
		win1251 	= 1251,		// Windows Cyrillic
		win1252 	= 1252,		// Windows Latin 1 / Western European
		win1253		= 1253,		// Windows Greek
		win1254		= 1254,		// Windows Turkish
		win1255		= 1255,		// Windows-Hebrew
		win1256		= 1256,		// Windows-Arabic
		win1257 	= 1257,		// Windows Baltic
		win1258 	= 1258,		// Windows-Vietnamese
		dos437		= 437,		// IBM/DOS 437 (US)
		dos720		= 720,		// IBM/DOS 720 (Arabic)
		dos737		= 737,		// IBM/DOS 737 (Greek)
		dos755		= 755,		// IBM/DOS 755 (Baltic rim)
		dos850		= 850,		// IBM/DOS 850 (western Europe aka Multilingual Latin I)
		dos852		= 852,		// IBM/DOS 852 (central Europe aka Multilingual Latin II)
		dos855		= 855,		// IBM/DOS 855 (Cyrillic)
		dos857		= 857,		// IBM/DOS 857 (Turkish)
		dos858		= 858,		// IBM/DOS 858 (Central Europe/Multilingual Latin I + With €)
		dos860		= 860,		// IBM/DOS 860 (Portuguese)
		dos861		= 861,		// IBM/DOS 861 (Icelandic)
		dos862		= 862,		// IBM/DOS 862 (Hebrew)
		dos863		= 863,		// IBM/DOS 863 (French Canadian)
		dos865		= 865,		// IBM/DOS 865 (Nordic)
		dos866		= 866,		// IBM/DOS 866 (Cyrillic)
		dos869		= 869,		// IBM 869 (Greek)
		IS08859_1	= 28591,	// ISO-8559-1 aka Windows 28591
		ISO8859_2	= 28592,	// ISO-8559-2 aka Windows 28592
		ISO8859_3	= 28593,	// ISO-8559-3 aka Windows 28593
		ISO8859_4	= 28594,	// ISO-8559-4 aka Windows 28594
		ISO8859_5	= 28595,	// ISO-8559-5 aka Windows 28595
		ISO8859_6	= 28596,	// ISO-8559-6 aka Windows 28596
		ISO8859_7	= 28597,	// ISO-8559-7 aka Windows 28597
		ISO8859_8	= 28598,	// ISO-8559-8 aka Windows 28598
		ISO8859_9	= 28599,	// ISO-8559-9 aka Windows 28599
		ISO8859_10	= -2,		// ISO-8559-10
		ISO8859_11	= -3,
//		TIS_620		= 874,		// Thai Latin/Thai aka IS08859-11
		ISO8859_13	= 28603,	// ISO-8559-13 aka Windows 28603
		ISO8859_14	= -4,		// ISO-8559-14
		ISO8859_15	= 28605,	// ISO-8559-15 aka Windows 28605
		ISO8859_16	= -6,		// ISO-8559-16
		macintosh	= 10000,	// Mac (US) macos-0_2-10.2
		mac_greek	= 10006,	// Mac (Greek) macos-6_2-10.2
		mac_cyrillic= 10007,	// Mac (Cyrillic/Ukrian) macos-7_3-10.2
		mac_thai	= -7,		// Mac (Thai) 21-10.5
		mac_c_europe= 10029,	// Mac (Central Europe) macos-29-10.2
		mac_symbol	= -8,		// Mac (Symbols) macos-33-10.5
		mac_dingbat	= -9,		// Mac Dingbats macos-34-10.2
		mac_turkish = 10081,	// Mac (Turkish) macos-35-10.2
		mac_croation= -10,		// Mac (Croation) macos-36_2-10.2
		mac_iceland = -11,		// Mac (Icelandic) macos-37_5-10.2
		mac_romania	= -12,		// Mac (Romania) macos-38_2-10.2
		mac_arabic	= -13,		// Mac (Arabic) macos-518_2-10.2
		mac_hebrew	= -14,		// Mac (Hebrew) macos-1285_2-10.2
		EBCDIC_US	= 37,		// EBCDIC (US-CANADA)
		_unknown_codePage = 0
	} CodePage;

	/**
	 * @brief getCharSets Finds all the supported char sets.
	 * @return All the known char sets.
	 */
	static std::vector<std::string> getCharSets();

	/**
	 * @brief guessCharSet Attemps to guess a char set from a code page.
	 * @param codePage The Code page to work with.
	 * @return std::string The guessed char set.
	 *
	 */
	static std::string guessCharSet(CodePage codePage);


	/**
	 * @brief Gets an instance of the class.
	 * @param CodePage codepage The codepage to find an instance for.
	 */
	static ICUConnector * get(CodePage codepage);

	/**
	 * @brief destroy Destory one char set.
	 * @param CodePage codepage Which instance should we destory?
	 */
	static void destroy(CodePage codepage);

	/**
	 * @brief destroy Destroy all instances.
	 */
	static void destroyAll();

	/**
	 * @brief codepage Gets the codepage.
	 * @return Codepage value.
	 */
	CodePage codepage() const { return _codepage; }

	/**
	 * @brief convertToUnicode Converts instring to unicode.
	 * @param instring Value to convert.
	 * @return Unicode (UTF_8) convertion of instring.
	 */
	std::string convert(const UCharBuffer &instring);

	/**
	 * @brief convertToUnicode Converts instring to unicode.
	 * @param instring String to convert (codepage!)
	 * @return Unicode Convertion of instring.
	 */
	UCharBuffer convert(const std::string &instring);

	/**
	 * @brief from7ASCII Converts from ASCII to the selected char set.
	 * @return A short string (usually but not always one byte) with the converted char.
	 *
	 * Thows a std::runtime_exception if mapping not possible.
	 */
	static std::string from7ASCII(char);


protected:

	// Instances by code page.
	typedef std::map< CodePage, ICUConnector * > Instances;
	typedef std::pair< CodePage, ICUConnector * > Instance;

	/**
	 * @brief ICUConnector Ctor
	 * @param CodePage codepage - The code page we are interested in.
	 *
	 * Proctected for Singleton behaviour.
	 */
	ICUConnector(CodePage codepage);

	virtual ~ICUConnector();

	/**
	 * @brief _findInstance Finds an instance of this class.
	 * @param codepage - The codepage to find.
	 * @param inst - Instance to find.
	 * @return
	 */
	static Instances::iterator _findInstance(CodePage codepage);
	static Instances::iterator _findInstance(const ICUConnector *inst);

	static Instances::iterator _endInstance() { return _instances.end(); }

protected:

	// Instaces.
	static Instances		_instances;

	UConverter*				_converter;

	// Instance data.
	CodePage		_codepage;	// Code Page from
	std::string		_charset;	// Char set to convert from.
};


#endif // ICUCONNECTOR_H
