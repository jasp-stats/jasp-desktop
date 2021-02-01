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
#include "../analysis/analysisform.h"
#include "utils.h"

using namespace std;

TextInputBase::TextInputBase(QQuickItem* parent)
	: JASPControl(parent), BoundControlBase(this)
{
	_controlType = ControlType::TextField;
}

QString TextInputBase::_getPercentValue(double dblVal)
{
	double doubleValue = dblVal * 100; // The value is stored as a double from 0...1, but is displayed as a percent number
	doubleValue = std::max(0., std::min(100., doubleValue));

	int decimals = property("decimals").toInt();
	return QString::number(doubleValue, 'f', decimals);
}

QString TextInputBase::_getIntegerArrayValue(const std::vector<int>& intValues)
{
	QString value;
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

QString TextInputBase::_getDoubleArrayValue(const std::vector<double>& doubleValues)
{
	QString value;
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

void TextInputBase::bindTo(const Json::Value& value)
{
	BoundControlBase::bindTo(value);

	switch (_inputType)
	{
	case TextInputType::IntegerInputType:
		if (value.isNumeric())		_value = QString::number(value.asInt());
		else if (value.isString())	_value = QString::number(std::stoi(value.asString()));
		break;
	case TextInputType::NumberInputType:
	case TextInputType::PercentIntputType:
	{
		double dblVal = 0;
		if (value.isNumeric())		dblVal = value.asDouble();
		else if (value.isString())	dblVal = std::stod(value.asString());
		if (_inputType == TextInputType::PercentIntputType)
			_value = _getPercentValue(dblVal);
		else
		{
			// Ensure the the option does not have more decimals than authorized for backwards compatibility
			int decimals = property("decimals").toInt();
			int pow = 1;
			for (int i = 0; i < decimals; i++) pow = pow * 10;
			dblVal = (round(dblVal * pow))/pow;

			_value = tq(Utils::doubleToString(dblVal));
		}

		break;
	}
	case TextInputType::IntegerArrayInputType:
	{
		std::vector<int> arrayVal;
		if (value.isArray())
		{
			for (const Json::Value& oneValue : value)
				if (oneValue.isNumeric())	arrayVal.push_back(oneValue.asInt());
		}
		_value = _getIntegerArrayValue(arrayVal);
		break;
	}
	case TextInputType::DoubleArrayInputType:
	{
		std::vector<double> arrayVal;
		if (value.isArray())
		{
			for (const Json::Value& oneValue : value)
				if (oneValue.isNumeric())	arrayVal.push_back(oneValue.asDouble());
		}
		_value = _getDoubleArrayValue(arrayVal);
		break;
	}
	case TextInputType::FormulaType:
	case TextInputType::FormulaArrayType:
	{
		if (value.isString())	_value = tq(value.asString());
		setIsRCode();

		if (!_value.isEmpty())
		{
			if (_inputType == TextInputType::FormulaType)	runRScript("as.character("   + _value + ")",					true);
			else											runRScript("paste(as.array(" + _value + "), collapse=\"|\")",	true);
		}
		break;
	}
	case TextInputType::ComputedColumnType:
	{
		if (value.isString())	_value = tq(value.asString());
		setIsColumn(true);
		break;
	}
	case TextInputType::AddColumnType:
	{
		if (value.isString())	_value = tq(value.asString());
		columnType	colType		= static_cast<columnType>(property("columnType").toInt());
		setIsColumn(false, colType);
		break;
	}
	default:
	{
		if (value.isString())	_value = tq(value.asString());
		break;
	}
	}

	setProperty("value", _value);
}

Json::Value TextInputBase::createJson()
{
	QString startValue = property("startValue").toString();
	QString value = startValue.isEmpty() ? property("value").toString() : startValue;

	return _getJsonValue(value);
}

bool TextInputBase::isJsonValid(const Json::Value &value)
{
	bool valid = false;
	switch (_inputType)
	{
	case TextInputType::IntegerArrayInputType:
	case TextInputType::DoubleArrayInputType:
	case TextInputType::FormulaArrayType:		valid = value.isArray(); break;
	default:									valid = value.isNumeric() || value.isString(); break;
	}
	return valid;
}

void TextInputBase::setUp()
{
	JASPControl::setUp();

	QString type = property("inputType").toString();

		 if (type == "integer")			_inputType = TextInputType::IntegerInputType;
	else if (type == "number")			_inputType = TextInputType::NumberInputType;
	else if (type == "percent")			_inputType = TextInputType::PercentIntputType;
	else if (type == "integerArray")	_inputType = TextInputType::IntegerArrayInputType;
	else if (type == "doubleArray")		_inputType = TextInputType::DoubleArrayInputType;
	else if (type == "computedColumn")	_inputType = TextInputType::ComputedColumnType;
	else if (type == "addColumn")		_inputType = TextInputType::AddColumnType;
	else if (type == "formula")			_inputType = TextInputType::FormulaType;
	else if (type == "formulaArray")	_inputType = TextInputType::FormulaArrayType;
	else								_inputType = TextInputType::StringInputType;

	_parseDefaultValue = property("parseDefaultValue").toBool();
	_defaultValue = property("defaultEmptyValue").toString();

	QQuickItem::connect(this, SIGNAL(editingFinished()), this, SLOT(textChangedSlot()));

	if (form())
		// For unknown reason, when the language is changed, QML reset the default value.
		// We have then to set back the value from the option
		connect(form(), &AnalysisForm::languageChanged, this, &TextInputBase::resetValue);
}

void TextInputBase::resetValue()
{
	setProperty("value", _value);
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
		double val = valStr.toDouble(&succes);

		if (!succes)
		{
			addControlError(tr("The expression did not return a number."));
			setHasScriptError(true);
			break;
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

	setBoundValue(fq(_value));
}

QString TextInputBase::friendlyName() const
{
	switch (_inputType)
	{
	case TextInputType::IntegerInputType:		return tr("Integer Field");
	case TextInputType::NumberInputType:		return tr("Number Field");
	case TextInputType::PercentIntputType:		return tr("Percentage Field");
	case TextInputType::IntegerArrayInputType:	return tr("Integers Field");
	case TextInputType::DoubleArrayInputType:	return tr("Doubles Field");
	case TextInputType::AddColumnType:			return tr("Add Column Field");
	case TextInputType::ComputedColumnType:		return tr("Add Computed Column Field");
	case TextInputType::FormulaType:			return tr("Formula Field");
	case TextInputType::FormulaArrayType:		return tr("Formulas Field");
	case TextInputType::StringInputType:
	default:									return tr("Text Field");
	}
}

bool TextInputBase::_formulaResultInBounds(double result)
{
	double min			= property("min").toDouble();
	double max			= property("max").toDouble();
	JASPControl::Inclusive inclusive = JASPControl::Inclusive(property("inclusive").toInt());
	bool includeMin = (inclusive == JASPControl::Inclusive::MinMax || inclusive == JASPControl::Inclusive::MinOnly);
	bool includeMax = (inclusive == JASPControl::Inclusive::MinMax || inclusive == JASPControl::Inclusive::MaxOnly);

	bool tooSmall = includeMin ? result < min : result <= min;
	bool tooLarge = includeMax ? result > max : result >= max;
	bool inBounds = !(tooSmall || tooLarge);

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

Json::Value TextInputBase::_getJsonValue(QString& text)
{
	switch (_inputType)
	{
	case TextInputType::IntegerInputType:		return (text.toInt());
	case TextInputType::NumberInputType:		return text.toDouble();
	case TextInputType::PercentIntputType:		return std::min(std::max(text.toDouble(), 0.0), 100.0) / 100;
	case TextInputType::IntegerArrayInputType:
	case TextInputType::DoubleArrayInputType:
	{
		text.replace(QString(" "), QString(","));
		Json::Value values(Json::arrayValue);
		QStringList chunks = text.split(QChar(','), Qt::SkipEmptyParts);

		for (QString &chunk: chunks)
		{
			bool ok;
			if (_inputType == TextInputType::IntegerInputType)
			{
				int value = chunk.toInt(&ok);
				if (ok)	values.append(value);
			}
			else
			{
				double value = chunk.toDouble(&ok);
				if (ok)	values.append(value);
			}
		}
		return values;
	}
	default:	return fq(text);
	}
}

void TextInputBase::textChangedSlot()
{
	if (!isBound() && _inputType != TextInputType::FormulaType && _inputType != TextInputType::FormulaArrayType)
		// In a TabView, if the name of the tab is edited and, before validating, a new tab is added, the model is first changed because of adding a tab,
		// possibly making the QML item of the TextField invalid (as this TextField depends on the TabView model).
		// But as this TextField is not bound (_option is null), and is not a Formula, we don't need to fetch the value of the item anyway.
		return;

	_value = property("value").toString();

	if (_inputType == TextInputType::FormulaType || _inputType == TextInputType::FormulaArrayType)
	{
		// _formula might be empty (in TableView the FormulaType is not directly bound, and has its own model).
		if (boundValue().asString() != _value.toStdString())
		{
			if (!_parseDefaultValue && _defaultValue == _value)
			{
				setBoundValue(_value.toStdString());
				emit formulaCheckSucceeded();
			}
			else if (_inputType == TextInputType::FormulaType)
				runRScript("as.character(" + _value + ")", true);
			else
				runRScript("paste(as.array(" + _value + "), collapse=\"|\")", true);

		}
	}
	else setBoundValue(_getJsonValue(_value));
}
