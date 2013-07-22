#include "boundtextbox.h"



#include <boost/foreach.hpp>

#include <QIntValidator>

BoundTextBox::BoundTextBox(QWidget *parent) :
	QLineEdit(parent)
{
	_integerArray = NULL;

	connect(this, SIGNAL(textEdited(QString)), this, SLOT(textEditedHandler(QString)));
}

void BoundTextBox::bindTo(Option *option)
{
	OptionInteger *integer = dynamic_cast<OptionInteger *>(option);

	if (integer != NULL)
	{
		this->setValidator(new QIntValidator(this));
		this->setText(QString::number(integer->value()));
		return;
	}

	_integerArray = dynamic_cast<OptionIntegerArray *>(option);

	if (_integerArray != NULL)
	{
		this->setValidator(new QIntArrayValidator());
	}
}

void BoundTextBox::textEditedHandler(QString text)
{
	if (_integerArray != NULL)
		_integerArray->setValue(QIntArrayValidator::parse(text));
}

BoundTextBox::QIntArrayValidator::QIntArrayValidator()
{
}

QValidator::State BoundTextBox::QIntArrayValidator::validate(QString &input, int &pos) const
{
	// this needs some TLC

	if (pos == 0 || input.at(pos-1) == ',')
		return QValidator::Intermediate;

	fixup(input);

	if (pos > input.size())
		pos = input.size();

	return QValidator::Acceptable;
}

void BoundTextBox::QIntArrayValidator::fixup(QString &input) const
{
	std::vector<int> array = parse(input);
	input = stringify(array);
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

