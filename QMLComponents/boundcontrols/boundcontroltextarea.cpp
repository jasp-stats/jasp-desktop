//
// Copyright (C) 2013-2020 University of Amsterdam
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

#include "boundcontroltextarea.h"
#include "controls/textareabase.h"
#include "r_functionwhitelist.h"

BoundControlTextArea::BoundControlTextArea(TextAreaBase* textArea)
	: BoundControlBase(textArea), _textArea(textArea)
{
}

void BoundControlTextArea::bindTo(const Json::Value &value)
{
	BoundControlBase::bindTo(value);
	_textArea->setText(tq(value.asString()));
}

bool BoundControlTextArea::isJsonValid(const Json::Value &optionValue) const
{
	return optionValue.type() == Json::stringValue;
}

Json::Value BoundControlTextArea::createJson() const
{
	return Json::Value(_textArea->text().toStdString());
}

void BoundControlTextArea::checkSyntax()
{
	QString text = _textArea->text();
	JASPControl::TextType textType = _textArea->textType();

	if (textType == JASPControl::TextType::TextTypeRcode)
	{
		setIsRCode();
		try
		{
			R_FunctionWhiteList::scriptIsSafe(text.toStdString());
			_textArea->setHasScriptError(false);
			_textArea->setProperty("infoText", QObject::tr("valid R code"));
		}
		catch(filterException & e)
		{
			_textArea->setHasScriptError(true);
			std::string errorMessage(e.what());
			_textArea->setProperty("infoText", errorMessage.c_str());
		}
	}

	setBoundValue(fq(text));
}
