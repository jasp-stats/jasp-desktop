#include "boundcombobox.h"

#include <QDebug>

using namespace std;

BoundComboBox::BoundComboBox(QWidget *parent) :
	QComboBox(parent)
{
	_boundTo = NULL;

	connect(this, SIGNAL(currentIndexChanged(int)), this, SLOT(changeHandler(int)));
}

void BoundComboBox::bindTo(Option *option)
{
	_boundTo = NULL;
	OptionList *optionList = dynamic_cast<OptionList *>(option);

	if (optionList != NULL)
	{
		vector<string> options = optionList->options();

		for (int i = 0; i < options.size(); i++)
		{
			if (options[i] == optionList->value())
			{
				setCurrentIndex(i);
				break;
			}
		}

		_boundTo = optionList;
	}

}

void BoundComboBox::changeHandler(int index)
{
	if (_boundTo != NULL)
		_boundTo->set(index);
}
