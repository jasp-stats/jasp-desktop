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

#include "textinputbase.h"
#include "analysisform.h"
#include "columnutils.h"

using namespace std;

TextInputBase::TextInputBase(QQuickItem* parent)
	: JASPControl(parent), BoundControlBase(this)
{
	_controlType = ControlType::TextField;
}

QString TextInputBase::_getDoubleArrayValue(const std::vector<double>& doubleValues)
{
	QString value;
	bool first  = true;
	for (double doubleValue : doubleValues)
	{
		if (!first)
			value += ";";
		first = false;
		value += QColumnUtils::doubleToString(doubleValue, false);
	}

	return value;
}

void TextInputBase::bindTo(const Json::Value& value)
{
	switch (_inputType)
	{
	case TextInputType::IntegerInputType:
		int intVal;
		if (value.isNumeric())		
			_value = value.asInt();
		
		else if (value.isString() && QColumnUtils::getIntValue(tq(value.asString()), intVal))
			_value = intVal;
		
		break;
		
	case TextInputType::NumberInputType:
	case TextInputType::PercentIntputType:
	{
		double dblVal = 0;
		if (value.isNumeric())		
			dblVal = value.asDouble();
		
		else if (value.isString() && !QColumnUtils::getDoubleValue(tq(value.asString()), dblVal, false)) // value comes from the analyses.json: it's already an english format string
			dblVal = NAN;
			
		_value = dblVal; //Stored as the user enters (so 0-100), but sent in json / 100 through
		//This mean the "bound value" is 0...1 so:
		if(_inputType == TextInputType::PercentIntputType)
			_value = dblVal * 100.0;

		break;
	}
	case TextInputType::DoubleArrayInputType:
	{
		std::vector<double> arrayVal;
		if (value.isArray())
		{
			for (const Json::Value& oneValue : value)
				if (oneValue.isNumeric())
					arrayVal.push_back(oneValue.asDouble());
		}
		_value = _getDoubleArrayValue(arrayVal);
		break;
	}
	case TextInputType::FormulaType:
	{
		// If it is already numeric, no need to parse it.
		// This also avoid parsing infinite value: a QVariant with an infinite value gives "inf" as string value,
		// which gives an error when parsed by R.
		bool setRealValue = true;

		if (value.isNumeric())
			_value = value.asDouble();
		else if (value.isString())
		{
			double dblVal = 0;
			QString strValue = tq(value.asString());
			if (!strValue.isEmpty() && !QColumnUtils::getDoubleValue(strValue, dblVal, false)) // value comes from the analyses.json: it's already an english format string
			{
				setIsRCode();
				runRScript("as.character(" + _value.toString() + ")", true);
				setRealValue = false;
			}
			else
				_value = dblVal;
		}
		else
			_value = QVariant();

		if (setRealValue)
			setProperty("realValue", _value);

		break;
	}
	case TextInputType::ComputedColumnType:
	{
		if (value.isString())	
			_value = tq(value.asString());
		
		setIsColumn(true);
		checkIfColumnIsFreeOrMine();
		break;
	}
	case TextInputType::CheckColumnFreeOrMineType:
	{
		if (value.isString())	
			_value = tq(value.asString());
		
		checkIfColumnIsFreeOrMine();
		break;
	}
	case TextInputType::AddColumnType:
	{
		if (value.isString())	
			_value = tq(value.asString());
		
		columnType	colType		= static_cast<columnType>(property("columnType").toInt());
		setIsColumn(false, colType);
		checkIfColumnIsFreeOrMine();
		break;
	}
		
	default:
		if (value.isString())
			_value = tq(value.asString());
		else if (value.isInt())
			_value = value.asInt();
		else if (value.isDouble())
			_value = value.asDouble();
		break;
	}

	setDisplayValue();
	emit valueChanged();

	BoundControlBase::bindTo(value);
}

Json::Value TextInputBase::createJson() const
{
	QVariant value = property("displayValue");
	if (value.toString() == "" && !_defaultValue.isNull())	
		value = _defaultValue;

	return _getJsonValue(value);
}

bool TextInputBase::isJsonValid(const Json::Value &value) const
{
	bool valid = false;
	switch (_inputType)
	{
	case TextInputType::DoubleArrayInputType:	valid = value.isArray();						break;
	default:									valid = value.isNumeric() || value.isString();	break;
	}
	return valid;
}

void TextInputBase::setUp()
{
	QString type = property("inputType").toString();

		 if (type == "integer")			_inputType = TextInputType::IntegerInputType;
	else if (type == "number")			_inputType = TextInputType::NumberInputType;
	else if (type == "percent")			_inputType = TextInputType::PercentIntputType;
	else if (type == "doubleArray")		_inputType = TextInputType::DoubleArrayInputType;
	else if (type == "computedColumn")	_inputType = TextInputType::ComputedColumnType;
	else if (type == "checkColumn")		_inputType = TextInputType::CheckColumnFreeOrMineType;
	else if (type == "addColumn")		_inputType = TextInputType::AddColumnType;
	else if (type == "formula")			_inputType = TextInputType::FormulaType;
	else								_inputType = TextInputType::StringInputType;

	_parseDefaultValue = property("parseDefaultValue").toBool();

	QQuickItem::connect(this, SIGNAL(editingFinished()), this, SLOT(valueChangedSlot()));

	if (form())
		// For unknown reason, when the language is changed, QML reset the default value.
		// We have then to set back the value from the option
		connect(form(), &AnalysisForm::languageChanged, this, &TextInputBase::setDisplayValue);

	if (_value.isNull()) // If the value is not directly set, use the default value.
		setValue(_defaultValue, false);

	JASPControl::setUp(); // It might need the _inputType, so call it after it is set.
}

void TextInputBase::setDisplayValue()
{
	int		valueInt;
	double	valueDbl;
	QString showThis	= QColumnUtils::getIntValue(_value, valueInt) ? 
							QString::number(valueInt) 
						:	QColumnUtils::getDoubleValue(_value, valueDbl) ? 
								QColumnUtils::doubleToString(valueDbl, false) 
							:	_value.toString();
																																				  
	setProperty("displayValue", showThis);
}

void TextInputBase::rScriptDoneHandler(const QString &result)
{
	QStringList results;
	QVariantList values;

	if (_inputType == TextInputType::FormulaType)
		results.push_back(result);
	else
		results = result.split("|");

	bool succes = true;
	for (const QString& valStr : results)
	{
		double val;
		succes = QColumnUtils::getDoubleValue(valStr, val, false); // R works with English format doubles

		if (!succes)
		{
			addControlError(tr("The expression did not return a number."));
			setHasScriptError(true);
			break;
		}
		else
		{
			clearControlError();
			setHasScriptError(false);
		}


		if (!_formulaResultInBounds(val))
		{
			succes = false;
			break;
		}

		values.push_back(val);
	}

	if (succes)
	{
		emit formulaCheckSucceeded();
		setProperty("realValues", values);
		if (values.length() > 0)
			setProperty("realValue", values[0]);
	}

	setBoundValue(fq(_value.toString()));
}

QString TextInputBase::friendlyName() const
{
	switch (_inputType)	
	{
	case TextInputType::IntegerInputType:			return tr("Integer Field");
	case TextInputType::NumberInputType:			return tr("Double Field");
	case TextInputType::PercentIntputType:			return tr("Percentage Field");
	case TextInputType::DoubleArrayInputType:		return tr("Doubles Field");
	case TextInputType::AddColumnType:				return tr("Add Column Field");
	case TextInputType::ComputedColumnType:			return tr("Add Computed Column Field");
	case TextInputType::CheckColumnFreeOrMineType:	return tr("Column-name-is-free field");
	case TextInputType::FormulaType:				return tr("Formula Field");
	case TextInputType::StringInputType:
	default:										return tr("Text Field");
	}
}

QVariant TextInputBase::defaultValue() const	
{ 
	return _defaultValue;
}

QVariant TextInputBase::value() const	
{ 
	QVariant showThis = _value.isNull() ? _defaultValue : _value; 
	
	return showThis;	
}

void TextInputBase::checkIfColumnIsFreeOrMine()
{
	QString val = _value.toString();
	
	if(val.isEmpty())
		return;
	
	if(form() && !form()->isColumnFreeOrMine(val))
	{
		setHasScriptError(true);
		addControlError(tr("Column '%1' already exists and is not created by this analysis.").arg(val));
	}
	else
		setHasScriptError(false);
		
}

bool TextInputBase::encodeValue() const
{
	return _inputType == TextInputType::ComputedColumnType || _inputType == TextInputType::AddColumnType || _inputType == TextInputType::CheckColumnFreeOrMineType;
}

bool TextInputBase::_formulaResultInBounds(double result)
{
	double min			= property("min").toDouble();
	double max			= property("max").toDouble();
	JASPControl::Inclusive inclusive = JASPControl::Inclusive(property("inclusive").toInt());
	bool	includeMin	= (inclusive == JASPControl::Inclusive::MinMax || inclusive == JASPControl::Inclusive::MinOnly),
			includeMax	= (inclusive == JASPControl::Inclusive::MinMax || inclusive == JASPControl::Inclusive::MaxOnly),
			tooSmall	= includeMin ? result < min : result <= min,
			tooLarge	= includeMax ? result > max : result >= max,
			inBounds	= !(tooSmall || tooLarge);

	if (!inBounds)
	{
		QString end;
		if (tooSmall)	end = (includeMin ? "&ge; " : "&gt; ") + property("min").toString();
		else			end = (includeMax ? "&le; " : "&lt; ") + property("max").toString();
		addControlError(tr("The value (%1) must be %2").arg(result).arg(end));
		setHasScriptError(true);
	}
	else
	{
		setHasScriptError(false);
		clearControlError();
	}

	return inBounds;
}

Json::Value TextInputBase::_getJsonValue(QVariant value) const
{
	int		valueInt;
	double	valueDbl;
	bool	isInt,
			isDbl;
	
	isInt = QColumnUtils::getIntValue(		value.toString(), valueInt);
	isDbl = QColumnUtils::getDoubleValue(	value.toString(), valueDbl, false); // value is already an english format converted double
	
	switch (_inputType)
	{
	case TextInputBase::FormulaType:			return isDbl ? Json::Value(valueDbl) : fq(value.toString()); // Keep it as string and do not try to make it an integer or a double
	case TextInputType::IntegerInputType:		return isInt ? valueInt : isDbl ? int(valueDbl) : 0;
	case TextInputType::NumberInputType:		return isDbl ? valueDbl : isInt ? valueInt		: 0;
	case TextInputType::PercentIntputType:		return std::min(std::max(isDbl ? valueDbl : isInt ? valueInt : 0, 0.0), 100.0) / 100;
	case TextInputType::DoubleArrayInputType:
	{
		QString		str		= value.toString().replace(QString(" "), QString(";"));
		Json::Value values	= Json::arrayValue;
		QStringList chunks	= str.split(QChar(';'), Qt::SkipEmptyParts);

		for (QString &chunk: chunks)
		{
			isInt = QColumnUtils::getIntValue(		chunk, valueInt);
			isDbl = QColumnUtils::getDoubleValue(	chunk, valueDbl, false);
			
			if (isInt || isDbl)
				values.append(isDbl ? valueDbl : double(valueInt));
		}
		return values;
	}
	default:	
		return isInt ? Json::Value(valueInt) 
					 : isDbl ? Json::Value(valueDbl) 
							 : fq(value.toString());
	}
}

void TextInputBase::valueChangedSlot()
{
	QVariant prop = property("displayValue");

	setValue(prop);
}

void TextInputBase::setValue(QVariant value, bool useLocale)
{
	double valueDbl;
	if(QColumnUtils::getDoubleValue(value.toString(), valueDbl, useLocale))
		value = valueDbl;	
	
	bool hasChanged = _value != value;
	_value = value;
	
	if(_inputType == TextInputType::ComputedColumnType || _inputType == TextInputType::AddColumnType || _inputType == TextInputType::CheckColumnFreeOrMineType)
		checkIfColumnIsFreeOrMine();

	setDisplayValue();

	if (hasChanged)
	{
		emit valueChanged();

		if (initialized())
			_setBoundValue();
	}
}

void TextInputBase::setDefaultValue(QVariant value)
{
	double valueDbl;
	if(QColumnUtils::getDoubleValue(value.toString(), valueDbl, false)) // Don't use locale with default value: they are set by the analysis, not by the user.
		value = valueDbl;
		
	bool	hasChanged	= _defaultValue !=  value,
			curValIsDef	= _defaultValue == _value;
	
	_defaultValue = value;
	
	if(hasChanged)
		emit defaultValueChanged();
	
	if(curValIsDef)
		setValue(_defaultValue, false);
}

void TextInputBase::_setBoundValue()
{
	if (_inputType == TextInputType::FormulaType)
	{
		double valueDbl = 0;
		bool isDbl = QColumnUtils::getDoubleValue(_value.toString(), valueDbl, false); // _value is already an english format converted double

		if (isDbl)
		{
			if (_formulaResultInBounds(valueDbl))
			{
				setProperty("realValue", _value);
				setBoundValue(_getJsonValue(_value));
				clearControlError();
				setHasScriptError(false);
				emit formulaCheckSucceeded();
			}
		}
		else
		{
			QString strValue = _value.toString();

			// _formula might be empty (in TableView the FormulaType is not directly bound, and has its own model).
			if (boundValue().asString() != fq(strValue))
			{
				if (!_parseDefaultValue && _defaultValue == _value)
				{
					// The value is the same as the default value and this default value should not be parsed (this might be just a string like '...')
					// So just set this value and emit that the formula is succesfully checked without running the R script.
					setBoundValue(fq(strValue));
					emit formulaCheckSucceeded();
				}
				runRScript("as.character(" + strValue + ")", true);
			}
		}
	}
	else setBoundValue(_getJsonValue(_value));

}

