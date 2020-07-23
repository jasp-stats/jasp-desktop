#ifndef STRINGUTILS_H
#define STRINGUTILS_H

#include <set>
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>
#include <locale>
#include <cctype>

typedef std::set<std::string>		stringset;
typedef std::vector<std::string>	stringvec;

///All functions are inline and here to avoid problems through the mixing of MSVC and GCC on Windows. (Because this code can be used from JASP-R-Interface which can only be compiled with RTools and JASP-Common with MSVC...)
class stringUtils
{
public:    
    inline static std::string stripRComments(const std::string & rCode)
    {
        std::stringstream out;

        //Fixes https://github.com/jasp-stats/INTERNAL-jasp/issues/72
        //Gotta do some rudimentary parsing here... A comment starts with # and ends with newline, but if a # is inside a string then it doesn't start a comment...
        //String are started with ' or "

        enum class status { R, Comment, SingleStr, DoubleStr };

        status curStatus = status::R;

        for(size_t r=0; r<rCode.size(); r++)
        {
            bool pushMe = true;

            char kar = rCode[r];

            switch(curStatus)
            {
            case status::R:
                switch(kar)
                {
                case '\'':	curStatus = status::SingleStr;	break;
                case '"':	curStatus = status::DoubleStr;	break;
                case '#':
                    curStatus	= status::Comment;
                    pushMe		= false;
                    break;
                }
                break;

            case status::Comment:
                if(kar == '\n')	curStatus	= status::R;
                else			pushMe		= false;
                break;

            case status::SingleStr:
                if(kar == '\'' && rCode[r - 1] != '\\')
                    curStatus = status::R;
                break;

            case status::DoubleStr:
                if(kar == '"' && rCode[r - 1] != '\\')
                    curStatus = status::R;
                break;
            }

            if(pushMe)
                out << kar;
        }

        return out.str();
    }

    inline static std::vector<std::string> splitString(const std::string & str, const char sep = ',')
    {
        std::vector<std::string>	vecString;
        std::string					item;
        std::stringstream			stringStream(str);

        while (std::getline(stringStream, item, sep))
            vecString.push_back(item);

        return vecString;
    }

	inline static std::string toLower(std::string input)
	{
		std::transform(input.begin(), input.end(), input.begin(), ::tolower);
		return input;
	}

	inline static std::string replaceBy(std::string input, const std::string & replaceThis, const std::string & withThis)
	{
		size_t	oldLen = replaceThis.size(),
				newLen = withThis.size();

		for(std::string::size_type curPos = input.find_first_of(replaceThis); curPos + oldLen < input.size() && curPos != std::string::npos; curPos = input.find_first_of(replaceThis, curPos))
		{
			input.replace(curPos, oldLen, withThis);
			curPos += newLen;
		}

		return input;
	}

	inline static std::string escapeHtmlStuff(std::string input)
	{
		input = replaceBy(input, "&", "&amp;");
		input = replaceBy(input, "<", "&lt;");
		input = replaceBy(input, ">", "&gt;");

		return input;
	}

	inline static std::string stripNonAlphaNum(std::string input)
	{
		//std::remove_if makes sure all non-ascii chars are removed from your vector, but it does not change the length of the vector. That's why we erase the remaining part of the vector afterwards.
		input.erase(std::remove_if(input.begin(), input.end(), [](unsigned char x)
		{
	#ifdef _WIN32
			return !std::isalnum(x, std::locale());
	#else
			return !std::isalnum(x);
	#endif

		}), input.end());

		return input;
	}

	// Blatantly taken from https://stackoverflow.com/a/217605

	// trim from start (in place)
	static inline void ltrim(std::string &s) {
		s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) {
			return !std::isspace(ch);
		}));
	}

	// trim from end (in place)
	static inline void rtrim(std::string &s) {
		s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
			return !std::isspace(ch);
		}).base(), s.end());
	}

	// trim from both ends (in place)
	static inline void trim(std::string &s) {
		ltrim(s);
		rtrim(s);
	}

	// trim from start (copying)
	static inline std::string ltrim_copy(std::string s) {
		ltrim(s);
		return s;
	}

	// trim from end (copying)
	static inline std::string rtrim_copy(std::string s) {
		rtrim(s);
		return s;
	}

	static inline bool startsWith(const std::string & line, const std::string & startsWithThis)
	{
		return line.size() >= startsWithThis.size() && line.substr(0, startsWithThis.size()) == startsWithThis;
	}

	static inline bool escapeValue(std::string &value)
	{
		bool useQuotes = false;
		std::size_t found = value.find(",");
		if (found != std::string::npos)
			useQuotes = true;

		if (value.find_first_of(" \n\r\t\v\f") == 0)
			useQuotes = true;


		if (value.find_last_of(" \n\r\t\v\f") == value.length() - 1)
			useQuotes = true;

		size_t p = value.find("\"");
		while (p != std::string::npos)
		{
			value.insert(p, "\"");
			p = value.find("\"", p + 2);
			useQuotes = true;
		}

		return useQuotes;
	}

private:
    stringUtils();
};

#endif // STRINGUTILS_H
