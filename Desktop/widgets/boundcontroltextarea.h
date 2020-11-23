//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#ifndef BOUNDCONTROLTEXTAREA_H
#define BOUNDCONTROLTEXTAREA_H

#include "analysis/options/boundcontrol.h"

class TextAreaBase;
class OptionString;

class BoundControlTextArea : public BoundControl
{
public:
	BoundControlTextArea(TextAreaBase* textArea);

	Option*		boundTo()									override { return _boundTo; }
	void		bindTo(Option *option)						override;
	Option*		createOption()								override;
	bool		isOptionValid(Option* option)				override;
	bool		isJsonValid(const Json::Value& optionValue) override;

	virtual	void checkSyntax();

protected:
	Option*						_boundTo	= nullptr;
	TextAreaBase*				_textArea	= nullptr;
};

#endif // BOUNDCONTROLTEXTAREA_H
