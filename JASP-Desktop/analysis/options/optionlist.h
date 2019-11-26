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

#ifndef OPTIONLIST_H
#define OPTIONLIST_H

#include "optioni.h"
#include "common.h"

class OptionList : public OptionI<std::string>
{
public:
	OptionList(const std::vector<std::string> &options, std::string selected = "")	: OptionI(selected == "" && options.size() > 0 ? options.at(0) : selected), _options(options)	{}
	OptionList()																	: OptionI()																	{}

			void						init(const Json::Value &data)			override;
			void						set(const Json::Value& value)			override;
			Option*						clone()							const	override;
			Json::Value					asJSON()						const	override;
			Json::Value					asMetaJSON()					const	override;
	const	std::vector<std::string>	options()						const;
			void						resetOptions(const std::vector<std::string> &options, int selected) { _options = options; _selected = selected; }
			void						set(size_t index);

private:
	std::vector<std::string>	_options;
	std::string					_selected;
};

#endif // OPTIONLIST_H
