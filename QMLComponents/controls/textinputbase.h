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

#ifndef TEXTINPUTBASE_H
#define TEXTINPUTBASE_H

#include "jaspcontrol.h"
#include "boundcontrols/boundcontrolbase.h"

class TextInputBase : public JASPControl, public BoundControlBase
{
	Q_OBJECT

	Q_PROPERTY( bool		hasScriptError		READ hasScriptError			WRITE setHasScriptError		NOTIFY hasScriptErrorChanged		)
	Q_PROPERTY( QVariant	defaultValue		READ defaultValue			WRITE setDefaultValue		NOTIFY defaultValueChanged			)


public:
	enum TextInputType { IntegerInputType = 0, StringInputType, NumberInputType, PercentIntputType, IntegerArrayInputType, DoubleArrayInputType, ComputedColumnType, AddColumnType, FormulaType, FormulaArrayType};

	TextInputBase(QQuickItem* parent = nullptr);

	bool		isJsonValid(const Json::Value& value)		override;
	Json::Value createJson()								override;
	void		bindTo(const Json::Value& value)			override;
	void		setUp()										override;
	void		rScriptDoneHandler(const QString& result)	override;

	TextInputType	inputType()	{ return _inputType; }
	QString			friendlyName() const override;
	bool			hasScriptError()						const	{ return _hasScriptError;		}
	QVariant		defaultValue()							const	{ return _defaultValue;			}

signals:
	void		formulaCheckSucceeded();
	void		hasScriptErrorChanged();
	void		defaultValueChanged();

public slots:
	GENERIC_SET_FUNCTION(HasScriptError,	_hasScriptError,	hasScriptErrorChanged,	bool		)
	GENERIC_SET_FUNCTION(DefaultValue,		_defaultValue,		defaultValueChanged,	QVariant	)

private slots:
	void		textChangedSlot();
	void		resetValue();

private:
	Json::Value	_getJsonValue(const QVariant& value);
	bool		_formulaResultInBounds(double result);

	QString		_getPercentValue(double val);
	QString		_getIntegerArrayValue(const std::vector<int>& intValues);
	QString		_getDoubleArrayValue(const std::vector<double>& dblValues);

	TextInputType			_inputType;
	QString					_value;

	bool					_parseDefaultValue	= true;
	QVariant				_defaultValue;
	bool					_hasScriptError		= false;

};

#endif // TEXTINPUTBASE_H
