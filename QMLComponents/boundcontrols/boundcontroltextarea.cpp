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
#include "columnencoder.h"
#include "controls/textareabase.h"
#include "log.h"
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

//mds-plan D3.1: the applied/bound value is the TextArea family's component-owned option state -
//the widget text is a draft (commit-on-apply UX, and re-init restores the draft on purpose).
//BoundControlBase::bindTo funnels through this very virtual, so binds and applies land here alike;
//rlang/JAGS companions' _setBoundValues/checkSyntax writes dispatch here too (non-virtual calls
//would be the only way to miss it, and there are none).
void BoundControlTextArea::setBoundValue(const Json::Value &value, bool emitChange)
{
	if (!value.isNull())
		_appliedValue = value;

	BoundControlBase::setBoundValue(value, emitChange);
}

Json::Value BoundControlTextArea::createEngineValue() const
{
	//before the first bind/apply there is no committed value: fall back to today's createJson
	//shape (the plain text), which is also exactly what the first bindTo will commit.
	return _appliedValue.isNull() ? createJson() : _appliedValue;
}

void BoundControlTextArea::resetBoundValue()
{
	// checkSyntax takes care that the right boundValue is set.
	checkSyntax();
}

ColumnEncoder *BoundControlTextArea::_encoder() const
{
	if (VariableInfo * vi = _textArea->effectiveVarInfo())
		if (VariableInfoProvider * provider = vi->provider())
			if (ColumnEncoder * encoder = provider->columnEncoder())
				return encoder;

	return ColumnEncoder::fallbackEncoder();
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
