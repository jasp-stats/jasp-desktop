//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include "boundtextbox.h"

#include <boost/foreach.hpp>

#include <QIntValidator>
#include <QDoubleValidator>
#include <QKeyEvent>

BoundTextBox::BoundTextBox(QWidget *parent) :
	QLineEdit(parent)
{
	_integer = NULL;
	_integerArray = NULL;
	_number = NULL;

	//connect(this, SIGNAL(textEdited(QString)), this, SLOT(textEditedHandler(QString)));
}

void BoundTextBox::bindTo(Option *option)
{
	setLegal();

	_integer = dynamic_cast<OptionInteger *>(option);

	if (_integer != NULL)
	{
		int min = _integer->min();
		int max = _integer->max();

		this->setValidator(new QIntValidator(min, max, this));
		this->setText(QString::number(_integer->value()));
		return;
	}

	_integerArray = dynamic_cast<OptionIntegerArray *>(option);

	if (_integerArray != NULL)
	{
		this->setValidator(new QIntArrayValidator());
		return;
	}

	_number = dynamic_cast<OptionNumber *>(option);

	if (_number != NULL)
	{
		double v = _number->value();
		double min = _number->min();
		double max = _number->max();

		if (_number->format() == "%")
		{
			v *= 100;
			min *= 100;
			max *= 100;
		}

		this->setValidator(new QDoubleValidator(min, max, 4, this));
		this->setText(QString::number(v));
		return;
	}

}

void BoundTextBox::setLabel(const QString &label)
{
	_label = label;
}

void BoundTextBox::keyPressEvent(QKeyEvent *event)
{
	QLineEdit::keyPressEvent(event);
	if (event->key() == Qt::Key_Return || event->key() == Qt::Key_Enter)
		finalise();
}

void BoundTextBox::focusOutEvent(QFocusEvent *event)
{
	QLineEdit::focusOutEvent(event);
	finalise();
}

void BoundTextBox::finalise()
{
	QString value = text();

	while (value.endsWith(","))
		value = value.left(value.length() - 1);

	if (_integerArray != NULL)
	{
		_integerArray->setValue(QIntArrayValidator::parse(value));
	}
	else if (_integer != NULL)
	{
		//_integer->setValue(value.toInt());

		double v = value.toInt();
		double min = _integer->min();
		double max = _integer->max();

		bool pc = _integer->format() == "%";

		if (pc)
		{
			v /= 100;
			min *= 100;
			max *= 100;
		}

		if (v > _integer->max() || v < _integer->min())
		{
			if (pc)
			{
				setIllegal(QString("%1 must be between %2% and %3%").arg(_label).arg(min).arg(max));
			}
			else
			{
				setIllegal(QString("%1 must be between %2 and %3").arg(_label).arg(min).arg(max));
			}
		}
		else
		{
			_integer->setValue(v);
			setLegal();
		}
	}
	else if (_number != NULL)
	{
		double v = value.toDouble();
		double min = _number->min();
		double max = _number->max();

		bool pc = _number->format() == "%";

		if (pc)
		{
			v /= 100;
			min *= 100;
			max *= 100;
		}

		if (v > _number->max() || v < _number->min())
		{
			if (pc)
			{
				setIllegal(QString("%1 must be between %2% and %3%").arg(_label).arg(min).arg(max));
			}
			else
			{
				setIllegal(QString("%1 must be between %2 and %3").arg(_label).arg(min).arg(max));
			}
		}
		else
		{
			_number->setValue(v);
			setLegal();
		}
	}
}

void BoundTextBox::textEditedHandler(QString text)
{
	this->validator()->fixup(text);
	setText(text);

	/*if (_integerArray != NULL)
		_integerArray->setValue(QIntArrayValidator::parse(text));
	else if (_integer != NULL)
		_integer->setValue(text.toInt());
	else if (_number != NULL)
		_number->setValue(text.toDouble());*/
}

BoundTextBox::QIntArrayValidator::QIntArrayValidator()
{
}

QValidator::State BoundTextBox::QIntArrayValidator::validate(QString &input, int &pos) const
{
	// this needs some TLC

	if (pos > input.length())
		pos = input.length();

	if (pos == 0 || input.at(pos-1) == ',')
		return QValidator::Intermediate;

	fixup(input);

	if (pos > input.size())
		pos = input.size();

	return QValidator::Acceptable;
}

void BoundTextBox::QIntArrayValidator::fixup(QString &input) const
{
	QString trimmed = input.trimmed();

	std::vector<int> array = parse(input);
	input = stringify(array);

	if (trimmed.length() > 0 && trimmed.at(trimmed.length() - 1) == ',')
		input = input + ",";
}

std::vector<int> BoundTextBox::QIntArrayValidator::parse(QString &input)
{
	input.replace(QString(" "), QString(","));

	std::vector<int> result;

	QStringList chunks = input.split(QChar(','), QString::SkipEmptyParts);

	BOOST_FOREACH(QString &chunk, chunks)
	{
		bool ok;
		int value = chunk.toInt(&ok);

		if (ok)
			result.push_back(value);
	}

	return result;
}

QString BoundTextBox::QIntArrayValidator::stringify(std::vector<int> &input)
{
	if (input.size() == 0)
		return QString();

	std::vector<int>::iterator itr = input.begin();

	QString result = QString::number(*itr);
	itr++;

	for (; itr != input.end(); itr++)
		result += QString(",%1").arg(*itr);

	return result;
}

