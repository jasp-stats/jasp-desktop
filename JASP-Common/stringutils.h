#ifndef STRINGUTILS_H
#define STRINGUTILS_H

#include <string>
#include <vector>
#include <sstream>

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

private:
    stringUtils();
};

#endif // STRINGUTILS_H
