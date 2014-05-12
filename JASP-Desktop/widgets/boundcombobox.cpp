#include "boundcombobox.h"

#include <QDebug>
#include <QTimer>

using namespace std;

BoundComboBox::BoundComboBox(QWidget *parent) :
	QComboBox(parent)
{
	_model = &_defaultModel;
	setModel(&_defaultModel);

	connect(this, SIGNAL(currentIndexChanged(int)), this, SLOT(changeHandler(int)));
}

void BoundComboBox::bindTo(Option *option)
{
	if (_model != NULL)
	{
		_model->bindTo(option);
		QTimer::singleShot(0, this, SLOT(updateSelection()));
	}
}

void BoundComboBox::setModel(QAbstractItemModel *newModel)
{
	_model = dynamic_cast<BoundModel *>(newModel);
	QComboBox::setModel(newModel);
}

void BoundComboBox::changeHandler(int row)
{
	QModelIndex index = model()->index(row, 0);
	model()->setData(index, true, Qt::CheckStateRole);
}

void BoundComboBox::updateSelection()
{
	for (int i = 0; i < model()->rowCount(); i++)
	{
		QModelIndex index = model()->index(i, 0);
		QVariant isChecked = model()->data(index, Qt::CheckStateRole);
		if (isChecked.canConvert(QVariant::Bool) && isChecked.toBool())
		{
			setCurrentIndex(i);
			break;
		}
	}
}
