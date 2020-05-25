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

#include "boundqmltextinput.h"
#include "../analysis/analysisform.h"
#include "../analysis/jaspcontrolbase.h"
#include "../analysis/options/optiondoublearray.h"
#include <QQmlProperty>
#include <QQuickItem>
#include "utils.h"

using namespace std;

BoundQMLTextInput::BoundQMLTextInput(JASPControlBase* item)
	: JASPControlWrapper(item)
	, QObject(item)
	, BoundQMLItem()
{
	initTextInput();
}

void BoundQMLTextInput::initTextInput()
{
	QString type = getItemProperty("inputType").toString();

		 if (type == "integer")			_inputType = TextInputType::IntegerInputType;
	else if (type == "number")			_inputType = TextInputType::NumberInputType;
	else if (type == "percent")			_inputType = TextInputType::PercentIntputType;
	else if (type == "integerArray")	_inputType = TextInputType::IntegerArrayInputType;
	else if (type == "doubleArray")		_inputType = TextInputType::DoubleArrayInputType;
	else if (type == "computedColumn")	_inputType = TextInputType::ComputedColumnType;
	else if (type == "addColumn")		_inputType = TextInputType::AddColumnType;
	else if (type == "formula")			_inputType = TextInputType::FormulaType;
	else								_inputType = TextInputType::StringInputType;

	_parseDefaultValue = getItemProperty("parseDefaultValue").toBool();
	_defaultValue = getItemProperty("defaultEmptyValue").toString();

	if (_item)
		 QQuickItem::connect(_item, SIGNAL(editingFinished()), this, SLOT(textChangedSlot()));
}


QString BoundQMLTextInput::_getPercentValue()
{
	double doubleValue = _number->value() * 100; // The value is stored as a double from 0...1, but is displayed as a percent number
	doubleValue = std::max(0., std::min(100., doubleValue));

	int decimals = getItemProperty("decimals").toInt();
	return QString::number(doubleValue, 'f', decimals);
}

QString BoundQMLTextInput::_getIntegerArrayValue()
{
	QString value;
	std::vector<int> intValues = _integerArray->value();
	bool first  = true;
	for (int intValue : intValues)
	{
		if (!first)
			value += ",";
		first = false;
		value += QString::number(intValue);
	}

	return value;
}

QString BoundQMLTextInput::_getDoubleArrayValue()
{
	QString value;
	std::vector<double> doubleValues = _doubleArray->value();
	bool first  = true;
	for (double doubleValue : doubleValues)
	{
		if (!first)
			value += ",";
		first = false;
		value += QString::number(doubleValue);
	}

	return value;
}

void BoundQMLTextInput::bindTo(Option *option)
{
	switch (_inputType)
	{
	case TextInputType::IntegerInputType:
		_integer = dynamic_cast<OptionInteger *>(option);
		if (!_integer)
			_integer = new OptionInteger();
		_option = _integer;
		_value = QString::number(_integer->value());
		break;

	case TextInputType::NumberInputType:
	{
		_number = dynamic_cast<OptionNumber *>(option);
		if (!_number)
			_number = new OptionNumber();
		_option = _number;

		// Ensure the the option does not have more decimals than authorized for backwards compatibility
		int decimals = getItemProperty("decimals").toInt();
		double val = _number->value();
		int pow = 1;
		for (int i = 0; i < decimals; i++) pow = pow * 10;
		val = (round(val * pow))/pow;

		_value = QString::fromStdString(Utils::doubleToString(val));

		break;
	}

	case TextInputType::PercentIntputType:
		_number = dynamic_cast<OptionNumber *>(option);
		if (!_number)	_number = new OptionNumber();
		else if(_number->value() > 1) //How is this possible? Doesn't matter
				_number->setValue(std::min(100., std::max(0., _number->value() / 100.0)));

		_option = _number;
		_value = _getPercentValue();
		break;

	case TextInputType::IntegerArrayInputType:
		_integerArray = dynamic_cast<OptionIntegerArray *>(option);
		if (!_integerArray)
		{
			_integerArray = new OptionIntegerArray();
			OptionDoubleArray* doubleArray = dynamic_cast<OptionDoubleArray *>(option);
			if (doubleArray)
			{
				std::vector<int> integerArray;
				const std::vector<double>& doubles = doubleArray->value();
				for (double d : doubles)
					integerArray.push_back(int(d));
				_integerArray->setValue(integerArray);
			}
		}
		_option = _integerArray;
		_value = _getIntegerArrayValue();
		break;


	case TextInputType::DoubleArrayInputType:
		_doubleArray = dynamic_cast<OptionDoubleArray *>(option);
		if (!_doubleArray)
			_doubleArray = new OptionDoubleArray();
		_option = _doubleArray;
		_value = _getDoubleArrayValue();
		break;


	case TextInputType::ComputedColumnType:
	case TextInputType::AddColumnType:
		_option = _computedColumn = dynamic_cast<OptionComputedColumn *>(option);
		_value	= QString::fromStdString(_computedColumn->value());
		break;
	case TextInputType::FormulaType:
		_formula = dynamic_cast<OptionString *>(option);
		if (!_formula)
			_formula = new OptionString();
		_option = _formula;
		_value = QString::fromStdString(_formula ? _formula->value() : "");
		runRScript("as.character(" + _value + ")", true);
		break;

	default:
		_string = dynamic_cast<OptionString *>(option);
		if (!_string)
			_string = new OptionString();
		_option = _string;
		_value  = QString::fromStdString(_string->value());
		break;
	}

	setItemProperty("value", _value);
}

Option *BoundQMLTextInput::createOption()
{
	Option* option = nullptr;

	switch (_inputType)
	{
	case TextInputType::IntegerInputType:		option = new OptionInteger();			break;
	case TextInputType::NumberInputType:		option = new OptionNumber();			break;
	case TextInputType::PercentIntputType:		option = new OptionNumber();			break;
	case TextInputType::IntegerArrayInputType:	option = new OptionIntegerArray();		break;
	case TextInputType::DoubleArrayInputType:	option = new OptionDoubleArray();		break;
	case TextInputType::FormulaType:			option = new OptionString();			break;
	case TextInputType::ComputedColumnType:
	{
		option = new OptionComputedColumn();
		option->requestComputedColumnCreation.connect(		boost::bind( &Option::notifyRequestComputedColumnCreation,		form()->options(), _1));
		option->requestComputedColumnDestruction.connect(	boost::bind( &Option::notifyRequestComputedColumnDestruction,	form()->options(), _1));
		break;
	}
	case TextInputType::AddColumnType:
	{
		columnType	colType		= static_cast<columnType>(getItemProperty("columnType").toInt());
					option		= new OptionComputedColumn("", false, int(colType));

		option->requestColumnCreation.connect(boost::bind( &Option::notifyRequestColumnCreation, form()->options(), _1, _2));
		break;
	}
	case TextInputType::StringInputType:
	default:									option = new OptionString();			break;
	}

	_value = getItemProperty("value").toString();
	_setOptionValue(option, _value);
	return option;
}

bool BoundQMLTextInput::isOptionValid(Option *option)
{
	switch (_inputType)
	{
	case TextInputType::IntegerInputType:		return dynamic_cast<OptionInteger*>(option)			!= nullptr;
	case TextInputType::NumberInputType:		return dynamic_cast<OptionNumber*>(option)			!= nullptr;
	case TextInputType::PercentIntputType:		return dynamic_cast<OptionNumber*>(option)			!= nullptr;
	case TextInputType::IntegerArrayInputType:	return dynamic_cast<OptionIntegerArray*>(option)	!= nullptr;
	case TextInputType::DoubleArrayInputType:	return dynamic_cast<OptionDoubleArray*>(option)		!= nullptr;
	case TextInputType::AddColumnType:
	case TextInputType::ComputedColumnType:		return dynamic_cast<OptionComputedColumn*>(option)	!= nullptr;
	case TextInputType::FormulaType:			return dynamic_cast<OptionString*>(option)			!= nullptr;
	case TextInputType::StringInputType:
	default:									return dynamic_cast<OptionString*>(option)			!= nullptr;
	}
	return false;
}

bool BoundQMLTextInput::isJsonValid(const Json::Value &optionValue)
{
	bool valid = false;
	switch (_inputType)
	{
	case TextInputType::IntegerInputType:
	{
		valid = optionValue.type() == Json::intValue;
		if (!valid)
		{
			if (optionValue.type() == Json::realValue)
			{
				double value = optionValue.asDouble();
				if (int(value) == value)
					valid = true;
			}
		}
		break;
	}
	case TextInputType::NumberInputType:		valid = (optionValue.type() == Json::intValue || optionValue.type() == Json::realValue) ;	break;
	case TextInputType::PercentIntputType:		valid = (optionValue.type() == Json::intValue || optionValue.type() == Json::realValue) ;	break;
	case TextInputType::IntegerArrayInputType:	valid = (optionValue.type() == Json::arrayValue);			break;
	case TextInputType::DoubleArrayInputType:	valid = (optionValue.type() == Json::arrayValue);			break;
	case TextInputType::FormulaType:
	case TextInputType::StringInputType:
	default:									valid = (optionValue.type() == Json::stringValue || optionValue.type() == Json::intValue || optionValue.type() == Json::realValue); break;
	}
	return valid;
}

void BoundQMLTextInput::setUp()
{
	JASPControlWrapper::setUp();
	if (form())
		// For unknown reason, when the language is changed, QML reset the default value.
		// We have then to set back the value from the option
		connect(form(), &AnalysisForm::languageChanged, this, &BoundQMLTextInput::resetValue);
}

void BoundQMLTextInput::resetValue()
{
	setItemProperty("value", _value);
}

void BoundQMLTextInput::resetQMLItem(JASPControlBase *item)
{
	BoundQMLItem::resetQMLItem(item);

	setItemProperty("value", _value);
	if (_item)
		QQuickItem::connect(_item, SIGNAL(editingFinished()), this, SLOT(textChangedSlot()));
}

void BoundQMLTextInput::rScriptDoneHandler(const QString &result)
{
	bool succes;
	double val = result.toDouble(&succes);

	if (!succes)
	{
		item()->addControlError(tr("The expression did not return a number."));
		setItemProperty("hasScriptError", true);
	}
	else
	{
		setItemProperty("realValue", val);
		if (_formulaResultInBounds(val))
			emit formulaCheckSucceeded();
	}

	if (_formula)
		_formula->setValue(_value.toStdString());
}

bool BoundQMLTextInput::_formulaResultInBounds(double result)
{
	double min			= getItemProperty("min").toDouble();
	double max			= getItemProperty("max").toDouble();
	JASPControlBase::Inclusive inclusive = JASPControlBase::Inclusive(getItemProperty("inclusive").toInt());
	bool includeMin = (inclusive == JASPControlBase::Inclusive::MinMax || inclusive == JASPControlBase::Inclusive::MinOnly);
	bool includeMax = (inclusive == JASPControlBase::Inclusive::MinMax || inclusive == JASPControlBase::Inclusive::MaxOnly);

	bool tooSmall = includeMin ? result < min : result <= min;
	bool tooLarge = includeMax ? result > max : result >= max;
	bool inBounds = !(tooSmall || tooLarge);

	if (!inBounds)
	{
		QString end;
		if (tooSmall)	end = (includeMin ? "&ge; " : "&gt; ") + getItemProperty("min").toString();
		else			end = (includeMax ? "&le; " : "&lt; ") + getItemProperty("max").toString();
		item()->addControlError(tr("The result (%1) must be %2").arg(result).arg(end));
		setItemProperty("hasScriptError", true);
	}
	else
	{
		setItemProperty("hasScriptError", false);
		item()->clearControlError();
	}

	return inBounds;
}

void BoundQMLTextInput::_setOptionValue(Option* option, QString& text)
{
	switch (_inputType)
	{
	case TextInputType::IntegerInputType:
		dynamic_cast<OptionInteger*>(option)->setValue(text.toInt());
		break;

	case TextInputType::NumberInputType:
		dynamic_cast<OptionNumber*>(option)->setValue(text.toDouble());
		break;

	case TextInputType::PercentIntputType:
		dynamic_cast<OptionNumber*>(option)->setValue(std::min(std::max(text.toDouble(), 0.0), 100.0) / 100);
		break;

	case TextInputType::IntegerArrayInputType:
	{
		text.replace(QString(" "), QString(","));
		std::vector<int> values;
		QStringList chunks = text.split(QChar(','), QString::SkipEmptyParts);

		for (QString &chunk: chunks)
		{
			bool ok;
			int value = chunk.toInt(&ok);

			if (ok)
				values.push_back(value);
		}
		dynamic_cast<OptionIntegerArray*>(option)->setValue(values);
		break;
	}

	case TextInputType::DoubleArrayInputType:
	{
		text.replace(QString(" "), QString(","));
		std::vector<double> values;
		QStringList chunks = text.split(QChar(','), QString::SkipEmptyParts);

		for (QString &chunk: chunks)
		{
			bool ok;
			double value = chunk.toDouble(&ok);
			if (ok)
				values.push_back(value);
		}
		dynamic_cast<OptionDoubleArray*>(option)->setValue(values);
		break;
	}

	case TextInputType::ComputedColumnType:
	case TextInputType::AddColumnType:
		dynamic_cast<OptionComputedColumn*>(option)->setValue(text.toStdString());
		break;

	case TextInputType::FormulaType:
	default:
		dynamic_cast<OptionString*>(option)->setValue(text.toStdString());
		break;
	}
}

void BoundQMLTextInput::textChangedSlot()
{
	if (!isBound() && _inputType != TextInputType::FormulaType)
		// In a TabView, if the name of the tab is edited and, before validating, a new tab is added, the model is first changed because of adding a tab,
		// making possibly the QML item of the TextField invalid (as this TextField depends on the TabView model).
		// But as this TextField is anyway not bound (_option is null), and is not a Formula, we don't need anyway to fetch the value of the item.
		return;

	_value = getItemProperty("value").toString();

	if (_inputType == TextInputType::FormulaType)
	{
		// _formula might be empty (in TableView the FormulaType is not directly bound, and has its own model).
		if (!_formula || (_formula->value() != _value.toStdString()))
		{
			if (!_parseDefaultValue && _defaultValue == _value)
			{
				if (_formula)
					_formula->setValue(_value.toStdString());
				emit formulaCheckSucceeded();
			}
			else
				runRScript("as.character(" + _value + ")", true);
		}
	}
	else if (_option)
		_setOptionValue(_option, _value);

}
