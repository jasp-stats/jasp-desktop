//
// Copyright (C) 2017 University of Amsterdam
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

#include "breadcrumbs.h"

#include <QResizeEvent>
#include <QToolButton>
#include <QPainter>
#include <QStyleOption>

BreadCrumbs::BreadCrumbs(QWidget *parent) : QWidget(parent)
{
	_model = NULL;

	_layout = new QHBoxLayout(this);
	_layout->setSpacing(4);
	setLayout(_layout);

	_buttons = new QButtonGroup(this);

	_dotDotDotButton = new QToolButton(this);
	_dotDotDotButton->setText("<<");

	_layout->addWidget(_dotDotDotButton);
	_layout->addSpacerItem(new QSpacerItem(0, 0, QSizePolicy::Expanding, QSizePolicy::Minimum));

	connect(_dotDotDotButton, SIGNAL(clicked(bool)), this, SLOT(dotDotDotClicked()));
}

void BreadCrumbs::setRootPath(const QString &path)
{
	_rootPath = path;
	_rootPieces = path.split("/", QString::SkipEmptyParts);
}

void BreadCrumbs::setPath(QString path)
{
	if (_path == path)
		return;

	_path = path;

	QStringList pieces = _path.split("/", QString::SkipEmptyParts);

	for (int i = 0; i < _rootPieces.length(); i++)
		pieces.pop_front();

	_pathPieces = pieces;

	populate();
}

const QString &BreadCrumbs::path() const
{
	return _path;
}

void BreadCrumbs::setModel(FSBModel *model)
{
	_model = model;

	setRootPath(_model->rootPath());
	setPath(_model->path());

	connect(_model, SIGNAL(pathChanged(QString)), this, SLOT(setPath(QString)));
}

void BreadCrumbs::buttonClicked()
{
	QToolButton *clicked = qobject_cast<QToolButton*>(this->sender());
	int index = _buttons->id(clicked);

	int toPop = _pathPieces.length() - index - 1;

	for (int i = 0; i < toPop; i++)
		_pathPieces.pop_back();

	_path = _rootPath + "/" + _pathPieces.join("/");

	if (_model != NULL)
		_model->setPath(_path);

	populate();
}

void BreadCrumbs::dotDotDotClicked()
{
	if (_dotDotIndex == -1)
	{
		_pathPieces.clear();
		_path = _rootPath;
	}
	else
	{
		int toPop = _pathPieces.length() - _dotDotIndex;

		QStringList pieces = _pathPieces;
		for (int i = 0; i < toPop; i++)
			pieces.pop_back();

		_pathPieces = pieces;
		_path = _rootPath + "/" + _pathPieces.join("/");
	}

	if (_model != NULL)
		_model->setPath(_path);

	populate();
}

void BreadCrumbs::resizeEvent(QResizeEvent *event)
{
	refresh(event->size());
	QWidget::resizeEvent(event);
}

void BreadCrumbs::populate()
{
	foreach (QAbstractButton *button, _buttons->buttons())
		delete button;

	for (int i = _pathPieces.length() - 1; i >= 0; i--)
	{
		const QString &piece = _pathPieces[i];
		QToolButton *button = new QToolButton(this);
		button->setText(piece);
		button->setCheckable(true);
		_buttons->addButton(button, i);
		_layout->insertWidget(1, button);

		connect(button, SIGNAL(clicked(bool)), this, SLOT(buttonClicked()));

		if (i == _pathPieces.length() - 1)
			button->setChecked(true);
	}

	refresh(size());
}

void BreadCrumbs::refresh(const QSize &size)
{
	int spaceLeft = size.width();
	spaceLeft -= layout()->contentsMargins().left();
	spaceLeft -= layout()->contentsMargins().right();

	QList<QAbstractButton*> buttons = _buttons->buttons();

	int i = 0;

	for (; i < buttons.length(); i++)
	{
		QAbstractButton *button = buttons.at(i);
		int buttonWidth = button->sizeHint().width();

		if (spaceLeft - buttonWidth <= 0)
			break;
		else
			button->show();

		spaceLeft -= layout()->spacing();
		spaceLeft -= buttonWidth;
	}

	if (i == buttons.length())
	{
		QString rootName = "";
		if ( ! _rootPieces.empty())
			rootName = _rootPieces.last();

		_dotDotDotButton->setText(rootName);
		_dotDotIndex = -1;
	}
	else
	{
		_dotDotIndex = buttons.length() - i;
		_dotDotDotButton->setText("<<");
	}

	for (; i < buttons.length(); i++)
	{
		QAbstractButton *button = buttons.at(i);
		button->hide();
	}
}
