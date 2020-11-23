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

#ifndef BOUNDCONTROLJAGSTEXTAREA_H
#define BOUNDCONTROLJAGSTEXTAREA_H

#include "boundcontroltextarea.h"
#include "analysis/options/options.h"
#include <QSet>

class BoundControlJAGSTextArea : public BoundControlTextArea
{
public:
	BoundControlJAGSTextArea(TextAreaBase* textArea) : BoundControlTextArea(textArea) {}

	void	bindTo(Option *option)						override;
	Option* createOption()								override;
	bool	isOptionValid(Option* option)				override;
	bool	isJsonValid(const Json::Value& optionValue) override;
	Option* boundTo()									override { return _options; }

	void	checkSyntax()								override;

private:
	Options*					_options = nullptr;
	std::set<std::string>		_usedColumnNames;
	QSet<QString>				_usedParameters;
	QString						_textEncoded;

};

#endif // BOUNDCONTROLJAGSTEXTAREA_H
