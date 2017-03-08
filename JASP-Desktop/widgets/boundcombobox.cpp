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
