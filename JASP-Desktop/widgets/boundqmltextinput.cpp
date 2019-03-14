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
#include "../analysis/options/optiondoublearray.h"
#include <QQmlProperty>
#include <QQuickItem>

using namespace std;

BoundQMLTextInput::BoundQMLTextInput(QQuickItem* item, AnalysisForm* form) 
	: QMLItem(item, form)
	, QObject(form)
	, BoundQMLItem(item, form)
{
	QString type = QQmlProperty(item, "inputType").read().toString();

		 if (type == "integer")			_inputType = TextInputType::IntegerInputType;
	else if (type == "number")			_inputType = TextInputType::NumberInputType;
	else if (type == "percent")			_inputType = TextInputType::PercentIntputType;
	else if (type == "integerArray")	_inputType = TextInputType::IntegerArrayInputType;
	else if (type == "computedColumn")	_inputType = TextInputType::ComputedColumnType;
	else								_inputType = TextInputType::StringInputType;
	
	QQuickItem::connect(item, SIGNAL(editingFinished()), this, SLOT(textChangedSlot()));
}

QString BoundQMLTextInput::_getPercentValue()
{
	double doubleValue = _number->value();
	if (doubleValue <= 1)
		doubleValue = doubleValue * 100;
	// Get only max 1 decimal
	doubleValue = doubleValue * 10;
	int intValue = int(doubleValue);
	doubleValue = double(intValue) / 10;
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
		value += QString::number(intValue);
	}
	
	return value;
}

void BoundQMLTextInput::bindTo(Option *option)
{
	setLegal();
	
	switch (_inputType) 
	{
	case TextInputType::IntegerInputType:
		_option = _integer = dynamic_cast<OptionInteger *>(option);
		_value = QString::number(_integer->value());
		break;

	case TextInputType::NumberInputType:
		_option = _number = dynamic_cast<OptionNumber *>(option);
		_value = QString::number(_number->value());
		break;

	case TextInputType::PercentIntputType:
		_option = _number = dynamic_cast<OptionNumber *>(option);
		_value = _getPercentValue();
		break;

	case TextInputType::IntegerArrayInputType:
		_integerArray = dynamic_cast<OptionIntegerArray *>(option);
		if (!_integerArray)
		{
			OptionDoubleArray* doubleArray = dynamic_cast<OptionDoubleArray *>(option);
			if (doubleArray)
			{
				std::vector<int> integerArray;
				const std::vector<double>& doubles = doubleArray->value();
				for (double d : doubles)
					integerArray.push_back(int(d));
				_integerArray = new OptionIntegerArray();
				_integerArray->setValue(integerArray);
			}
		}
		_option = _integerArray;
		_value = _getIntegerArrayValue();
		break;

	case TextInputType::ComputedColumnType:
		_option = _computedColumn = dynamic_cast<OptionComputedColumn *>(option);
		_value	= QString::fromStdString(_computedColumn->value());
		break;

	default:
		_option = _string = dynamic_cast<OptionString *>(option);
		_value  = QString::fromStdString(_string->value());
		break;
	}

	_item->setProperty("value", _value);
}

Option *BoundQMLTextInput::createOption()
{
	Option* option = nullptr;
	_value = QQmlProperty::read(_item, "value").toString();
	switch (_inputType)
	{
	case TextInputType::IntegerInputType:		option = new OptionInteger();			break;
	case TextInputType::NumberInputType:		option = new OptionNumber();			break;
	case TextInputType::PercentIntputType:		option = new OptionNumber();			break;
	case TextInputType::IntegerArrayInputType:	option = new OptionIntegerArray();		break;
	case TextInputType::ComputedColumnType:
	{
		option = new OptionComputedColumn();

		option->requestComputedColumnCreation.connect(		boost::bind( &Option::notifyRequestComputedColumnCreation,		form()->options(), _1));
		option->requestComputedColumnDestruction.connect(	boost::bind( &Option::notifyRequestComputedColumnDestruction,	form()->options(), _1));

		break;
	}
	case TextInputType::StringInputType:
	default:									option = new OptionString();			break;
	}
	
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
	case TextInputType::ComputedColumnType:		return dynamic_cast<OptionComputedColumn*>(option)	!= nullptr;
	case TextInputType::StringInputType:
	default:									return dynamic_cast<OptionString*>(option)			!= nullptr;
	}

	return false;
}

void BoundQMLTextInput::resetQMLItem(QQuickItem *item)
{
	BoundQMLItem::resetQMLItem(item);
	
	_item->setProperty("value", _value);
	QQuickItem::connect(_item, SIGNAL(editingFinished()), this, SLOT(textChangedSlot()));
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
	case TextInputType::ComputedColumnType:
		dynamic_cast<OptionComputedColumn*>(option)->setValue(text.toStdString());
		break;

	default:
		dynamic_cast<OptionString*>(option)->setValue(text.toStdString());
		break;
	}
}

void BoundQMLTextInput::textChangedSlot()
{
	_value = QQmlProperty::read(_item, "value").toString();
	if (_option)
		_setOptionValue(_option, _value);
}
