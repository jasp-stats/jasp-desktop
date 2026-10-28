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

#ifndef BOUNDCONTROLRLANGTEXTAREA_H
#define BOUNDCONTROLRLANGTEXTAREA_H

#include "boundcontroltextarea.h"
#include "controls/rsyntaxhighlighter.h"

class ColumnEncoder;

class BoundControlRlangTextArea : public BoundControlTextArea
{
public:
	enum class RLangType {Lavaan, CSem, MetaSem, RCode};

	BoundControlRlangTextArea(TextAreaBase* textArea, RLangType type = RLangType::Lavaan);

	bool		isJsonValid(const Json::Value& optionValue)		const	override;
	Json::Value	createJson()									const	override;
	void		bindTo(const Json::Value &value)						override;
	void		resetBoundValue()										override;

	void		checkSyntax()											override;
	QString		rScriptDoneHandler(const QString &result)				override;

protected:
	RLangType								_langType				= RLangType::Lavaan;
	const char *							_checkSyntaxRFunctionName();
	void									_setBoundValues(bool setModel = true);
	///< single live-state builder behind _setBoundValues (termsOut: the terms
	/// derived from the used columns, for the model update). The GENERATORS do not call this:
	/// the TextArea family commits on apply, so BoundControlTextArea::_appliedValue (tracked in
	/// setBoundValue, which _setBoundValues funnels through) is the option state they emit.
	Json::Value								liveBoundValue(Terms* termsOut = nullptr)				const;

	RSyntaxHighlighter*						_rLangHighlighter		= nullptr;

private:

protected:
	stringset								_noPrefixUsedColumnNames;
	std::map<std::string, stringset>		_prefixedUsedColumnNames;
	QString									_textEncoded;
	const stringset							_allowedVarPrefixes = {"data."};
	
	QString									_previouslyUsedTextEncoded;


};

#endif // BOUNDCONTROLRLANGTEXTAREA_H
