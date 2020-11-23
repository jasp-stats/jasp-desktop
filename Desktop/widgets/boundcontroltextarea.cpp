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
#include "analysis/options/optionstring.h"
#include "textareabase.h"
#include "log.h"
#include "r_functionwhitelist.h"

BoundControlTextArea::BoundControlTextArea(TextAreaBase* textArea)
	: _textArea(textArea)
{
}

void BoundControlTextArea::bindTo(Option *option)
{
	_boundTo = option;
	OptionString* optionString = dynamic_cast<OptionString *>(option);

	if (optionString != nullptr)
		_textArea->setText(QString::fromStdString(optionString->value()));
	else
		Log::log()  << "could not bind to OptionBoolean in BoundQuickCheckBox.cpp" << std::endl;
}

Option *BoundControlTextArea::createOption()
{
	std::string text = _textArea->text().toStdString();
	return new OptionString(text);
}

bool BoundControlTextArea::isOptionValid(Option *option)
{
	return dynamic_cast<OptionString*>(option) != nullptr;
}

bool BoundControlTextArea::isJsonValid(const Json::Value &optionValue)
{
	return optionValue.type() == Json::stringValue;
}

void BoundControlTextArea::checkSyntax()
{
	QString text = _textArea->text();
	JASPControl::TextType textType = _textArea->textType();

	if (textType == JASPControl::TextType::TextTypeRcode)
	{
		try
		{
			R_FunctionWhiteList::scriptIsSafe(text.toStdString());
			_textArea->setProperty("hasScriptError", false);
			_textArea->setProperty("infoText", QObject::tr("valid R code"));
		}
		catch(filterException & e)
		{
			_textArea->setProperty("hasScriptError", true);
			std::string errorMessage(e.what());
			_textArea->setProperty("infoText", errorMessage.c_str());
		}
	}

	OptionString* optionString = dynamic_cast<OptionString*>(_boundTo);
	if (optionString != nullptr)
		optionString->setValue(text.toStdString());
}
