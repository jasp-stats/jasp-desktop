#ifndef OPTIONENCODABLESTRING_H
#define OPTIONENCODABLESTRING_H

#include "optionstring.h"

class OptionEncodableString : public OptionString
{
public:
	OptionEncodableString(std::string value = "", std::string regexp = "", int max = -1) : OptionString(value, regexp, max) 
	{
		setShouldEncode(true);
	}

	Json::Value	asMetaJSON()	const	override;
	Option		*clone()		const	override;

};

#endif // OPTIONENCODABLESTRING_H
