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
#include <QQmlProperty>
#include <QQuickItem>

using namespace std;

BoundQMLTextInput::BoundQMLTextInput(QQuickItem* item, AnalysisQMLForm* form) : BoundQMLItem(item, form)
{
	_integer = NULL;
	_integerArray = NULL;
	_number = NULL;
	_string = NULL;
	
	QString type = QQmlProperty(item, "inputType").read().toString();
	if (type == "integer")
	{
		_inputType = TextInputType::IntegerInputType;
		_integer = new OptionInteger();
		_option = _integer;
	}
	else if (type == "number")
	{
		_inputType = TextInputType::NumberInputType;
		_number = new OptionNumber();
		_option = _number;
	}
	else if (type == "percent")
	{
		_inputType = TextInputType::PercentIntputType;
		_number = new OptionNumber();
		_option = _number;
	}
	else if (type == "integerArray")
	{
		_inputType = TextInputType::IntegerArrayInputType;
		_integerArray = new OptionIntegerArray();
		_option = _integerArray;
	}
	else
	{
		_inputType = TextInputType::StringInputType;
		_string = new OptionString();
		_option = _string;
	}
	
	QQuickItem::connect(item, SIGNAL(editingFinished()), this, SLOT(textChangedSlot()));
}

QString BoundQMLTextInput::_getPercentValue()
{
	double doubleValue = _number->value();
	if (doubleValue <= 1)
		doubleValue = doubleValue * 100;
	// Get only max 1 decimal
	doubleValue = doubleValue * 10;
	int intValue = (int)doubleValue;
	doubleValue = (double)intValue / 10;
	if (doubleValue > 100) doubleValue = 100;
	else if (doubleValue < 0) doubleValue = 0;
	return QString::number(doubleValue);
}

QString BoundQMLTextInput::_getIntegerArrayValue()
{
	QString value;
	vector<int> intValues = _integerArray->value();
	bool first  = true;
	for (int intValue : intValues) 
	{
		if (!first)
			value += ",";
		first = false;
		value += intValue;
	}
	
	return value;
}

void BoundQMLTextInput::bindTo(Option *option)
{
	QString value;
	setLegal();
	
	switch (_inputType) 
	{
		case TextInputType::IntegerInputType:
		{
			_integer = dynamic_cast<OptionInteger *>(option);
			value = QString::number(_integer->value());
		}
		break;
		case TextInputType::NumberInputType:
		{
			_number = dynamic_cast<OptionNumber *>(option);
			value = QString::number(_number->value());
		}
		break;
		case TextInputType::PercentIntputType:
		{
			_number = dynamic_cast<OptionNumber *>(option);
			value = _getPercentValue();
		}
		break;
		case TextInputType::IntegerArrayInputType:
		{
			_integerArray = dynamic_cast<OptionIntegerArray *>(option);
			value = _getIntegerArrayValue();
		}
		break;
		default:
		{
			_string = dynamic_cast<OptionString *>(option);
			value = QString::fromStdString(_string->value());
		}
	}

	_item->setProperty("text", value);
}

void BoundQMLTextInput::unbind()
{
	
}

Option *BoundQMLTextInput::createOption()
{
	QString text = QQmlProperty::read(_item, "text").toString();
	setOptionValue(text);
	return _option;
}

void BoundQMLTextInput::resetQMLItem(QQuickItem *item)
{
	BoundQMLItem::resetQMLItem(item);
	
	QString value;
	switch (_inputType) 
	{
	case TextInputType::IntegerInputType:		value = QString::number(_integer->value()); break;
	case TextInputType::NumberInputType:		value = QString::number(_number->value());	break;
	case TextInputType::PercentIntputType:		value = _getPercentValue();					break;
	case TextInputType::IntegerArrayInputType:	value = _getIntegerArrayValue();			break;
	default:									value = QString::fromStdString(_string->value());
	}
	_item->setProperty("text", value);
	QQuickItem::connect(_item, SIGNAL(editingFinished()), this, SLOT(textChangedSlot()));
}

void BoundQMLTextInput::setOptionValue(QString& text)
{
	switch (_inputType) {
		case TextInputType::IntegerInputType:
			_integer->setValue(text.toInt());
			break;
		case TextInputType::NumberInputType:
			_number->setValue(text.toDouble());
			break;
		case TextInputType::PercentIntputType:
			{
				double value = text.toDouble();
				if (value > 100) value = 100;
				else if (value < 0) value = 0;
				value = value / 100;
				_number->setValue(value);
			}	
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
				_integerArray->setValue(values);			
			}
			break;
		default:
			_string->setValue(text.toStdString());
	}
}

void BoundQMLTextInput::textChangedSlot()
{
	QString text = QQmlProperty::read(_item, "text").toString();
	setOptionValue(text);
}
