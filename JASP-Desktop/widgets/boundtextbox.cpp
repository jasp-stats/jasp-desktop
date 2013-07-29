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
	_integer = dynamic_cast<OptionInteger *>(option);

	if (_integer != NULL)
	{
		this->setValidator(new QIntValidator(this));
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
		this->setValidator(new QDoubleValidator(_number->min(), _number->max(), _number->dp(), this));
		return;
	}

}

void BoundTextBox::keyPressEvent(QKeyEvent *event)
{
	QLineEdit::keyPressEvent(event);
	if (event->key() == Qt::Key_Return)
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
		_integerArray->setValue(QIntArrayValidator::parse(value));
	else if (_integer != NULL)
		_integer->setValue(value.toInt());
	else if (_number != NULL)
		_number->setValue(value.toDouble());
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

