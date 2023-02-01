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
	switch (_inputType)
	{
	case TextInputType::IntegerInputType:
		if (value.isNumeric())		_value = value.asInt();
		else if (value.isString())	_value = std::stoi(value.asString());
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
			_value = dblVal;

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
		QString strValue;
		if (value.isString())	strValue = tq(value.asString());
		_value = strValue;
		setIsRCode();

		if (!strValue.isEmpty())
		{
			if (_inputType == TextInputType::FormulaType)	runRScript("as.character("   + strValue + ")",					true);
			else											runRScript("paste(as.array(" + strValue + "), collapse=\"|\")",	true);
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

	setDisplayValue();
	emit valueChanged();

	BoundControlBase::bindTo(value);
}

Json::Value TextInputBase::createJson() const
{
	QVariant value = property("displayValue");
	if (value.toString() == "" && !_defaultValue.isNull())	value = _defaultValue;

	return _getJsonValue(value);
}

bool TextInputBase::isJsonValid(const Json::Value &value) const
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

	QQuickItem::connect(this, SIGNAL(editingFinished()), this, SLOT(valueChangedSlot()));

	if (form())
		// For unknown reason, when the language is changed, QML reset the default value.
		// We have then to set back the value from the option
		connect(form(), &AnalysisForm::languageChanged, this, &TextInputBase::setDisplayValue);

	if (_value.isNull()) // If the value is not directly set, use the default value.
		setValue(_defaultValue);

	JASPControl::setUp(); // It might need the _inputType, so call it after it is set.
}

void TextInputBase::setDisplayValue()
{
	setProperty("displayValue", _value);
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

	setBoundValue(fq(_value.toString()));
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

QString	TextInputBase::helpMD(SetConst & markdowned, int howDeep, bool) const
{
	markdowned.insert(this);

	if(info() == "" || (label() == "" && afterLabel() == ""))
		return "";

	QStringList md;

	md	<< QString{howDeep, '#' } << " " << friendlyName() << "\n"
		<< "`" << label() << " ... " << afterLabel() << "`\n\n"
		<< info() << "\n";


	return md.join("");
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

Json::Value TextInputBase::_getJsonValue(const QVariant& value) const
{
	switch (_inputType)
	{
	case TextInputType::IntegerInputType:		return (value.toInt());
	case TextInputType::NumberInputType:		return value.toDouble();
	case TextInputType::PercentIntputType:		return std::min(std::max(value.toDouble(), 0.0), 100.0) / 100;
	case TextInputType::IntegerArrayInputType:
	case TextInputType::DoubleArrayInputType:
	{
		QString str = value.toString();
		str.replace(QString(" "), QString(","));
		Json::Value values(Json::arrayValue);
		QStringList chunks = str.split(QChar(','), Qt::SkipEmptyParts);

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
	default:	return fq(value.toString());
	}
}

void TextInputBase::valueChangedSlot()
{
	setValue(property("displayValue"));
}

void TextInputBase::setValue(const QVariant &value)
{
	bool hasChanged = _value != value;
	_value = value;

	setDisplayValue();

	if (hasChanged)
	{
		emit valueChanged();

		if (_initialized)
			_setBoundValue();
	}
}

void TextInputBase::_setBoundValue()
{
	if (_inputType == TextInputType::FormulaType || _inputType == TextInputType::FormulaArrayType)
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
			else if (_inputType == TextInputType::FormulaType)
				runRScript("as.character(" + strValue + ")", true);
			else
				runRScript("paste(as.array(" + strValue + "), collapse=\"|\")", true);

		}
	}
	else setBoundValue(_getJsonValue(_value));

}

