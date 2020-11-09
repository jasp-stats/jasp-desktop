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

#ifndef BOUNDQMLTEXTINPUT_H
#define BOUNDQMLTEXTINPUT_H

#include "analysis/boundqmlitem.h"
#include "analysis/options/optioninteger.h"
#include "analysis/options/optionnumber.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optioncomputedcolumn.h"
#include "analysis/options/optionintegerarray.h"
#include "analysis/options/optiondoublearray.h"
#include "analysis/options/optionterm.h"
#include <QObject>
#include "enumutilities.h"


class BoundQMLTextInput : public QObject, public BoundQMLItem
{
	Q_OBJECT

public:
	enum TextInputType { IntegerInputType = 0, StringInputType, NumberInputType, PercentIntputType, IntegerArrayInputType, DoubleArrayInputType, ComputedColumnType, AddColumnType, FormulaType, FormulaArrayType};

	BoundQMLTextInput(JASPControl* item);
	void initTextInput();

	void		bindTo(Option *option)						override;
	Option*		createOption()								override;
	bool		isOptionValid(Option* option)				override;
	bool		isJsonValid(const Json::Value& optionValue) override;
	Option*		boundTo()									override { return _option; }
	void		setUp()										override;
	void		resetQMLItem(JASPControl *item)			override;
	void		rScriptDoneHandler(const QString& result)	override;

	TextInputType	inputType()	{ return _inputType; }
	QString			friendlyName() const override;

signals:
	void		formulaCheckSucceeded();

private slots:
	void		textChangedSlot();
	void		resetValue();

private:
	void		_setOptionValue(Option* option, QString& text);
	bool		_formulaResultInBounds(double result);

	QString		_getPercentValue();
	QString		_getIntegerArrayValue();
	QString		_getDoubleArrayValue();

	TextInputType			  _inputType;
	OptionInteger			* _integer			= nullptr;
	OptionIntegerArray		* _integerArray		= nullptr;
	OptionDoubleArray		* _doubleArray		= nullptr;
	OptionNumber			* _number			= nullptr;
	OptionString			* _string			= nullptr;
	OptionString			* _formula			= nullptr;
	OptionComputedColumn	* _computedColumn	= nullptr;
	Option					* _option			= nullptr;
	QString					  _value;

	bool					_parseDefaultValue	= true;
	QString					_defaultValue		= "";

};

#endif // BOUNDQMLTEXTINPUT_H
