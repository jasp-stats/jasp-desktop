//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#ifndef OPTIONTERM_H
#define OPTIONTERM_H

#include "optionterms.h"

class OptionTerm : public OptionTerms
{
public:
	OptionTerm()										: OptionTerms(false, true) {}
	OptionTerm(const std::vector<std::string> &value)	: OptionTerms(false, true) { setValue(value); }

	Json::Value		asJSON()										const	override;
	void			set(const Json::Value& value)							override;
	Option			*clone()										const	override;
	void			setValue(const std::vector<std::string> &value)			override;

	std::vector<std::string> term() const;

};

#endif // OPTIONTERM_H
