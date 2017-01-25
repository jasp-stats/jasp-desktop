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

#include "fsentrywidget.h"

#include <QGridLayout>
#include <QEvent>
#include <QStyleOption>
#include <QPainter>
#include <QIcon>

QPixmap *FSEntryWidget::_smallIcons = NULL;
QPixmap *FSEntryWidget::_largeIcons = NULL;

const QString FSEntryWidget::_uncheckedSS = "QAbstractButton::hover"
											"{"
											"	background-color: rgb(227, 225, 226) ;"
											"	border : 1px solid rgb(207, 205, 206) ;"
											"}";

const QString FSEntryWidget::_checkedSS   = "QAbstractButton"
											"{"
											"	background-color: rgb(220, 218, 219) ;"
											"	border: 1px solid #B0B0B0 ;"
											"}";

FSEntryWidget::FSEntryWidget(const FSEntry &entry, QWidget *parent) :
	QAbstractButton(parent)
{
	_compact = false;
	_clickMeans = ClickIsSelect;
	_entry = entry;

	setCheckable(true);
	installEventFilter(this);

	QGridLayout *leftRight = new QGridLayout(this);
	setLayout(leftRight);
	leftRight->setContentsMargins(6, 2, 6, 2);

	_icon = new QLabel(this);
	leftRight->addWidget(_icon, 0, 0);

	QGridLayout *topBottom = new QGridLayout();
	topBottom->setSpacing(4);
	leftRight->addLayout(topBottom, 0, 1);

	_label = new ElideLabel(this);
	topBottom->addWidget(_label);

	_description = new ElideLabel(this);
	topBottom->addWidget(_description);
	_description->hide();

#ifdef __WIN32__
	QFont f = _label->font();
	QFont nf(f.family(), 11, f.weight(), f.italic());
	_label->setFont(nf);

	f = _description->font();
	QFont df(f.family(), 8, f.weight(), f.italic());
	_description->setFont(df);
#else
	QFont f = _description->font();
	QFont df(f.family(), 10, f.weight(), f.italic());
	_description->setFont(df);
#endif

	setStyleSheet(_uncheckedSS);

	refresh();
}

FSEntryWidget::~FSEntryWidget()
{
}

void FSEntryWidget::setEntryInfo(const FSEntry &entry)
{
	_entry = entry;
	refresh();
}

void FSEntryWidget::setCompact(bool compact)
{
	_compact = compact;
	refresh();
}

const QString &FSEntryWidget::path() const
{
	return _entry.path;
}

FSEntry::EntryType FSEntryWidget::entryType() const
{
	return _entry.entryType;
}

void FSEntryWidget::refresh()
{
	initIcons();

	_label->setText(_entry.name);
	_description->setText(_entry.description);

	if (_compact)
	{
		_description->hide();
		_icon->setPixmap(_smallIcons[_entry.entryType]);
	}
	else
	{
		_description->show();
		_icon->setPixmap(_largeIcons[_entry.entryType]);
	}
}

bool FSEntryWidget::eventFilter(QObject *object, QEvent *event)
{
	if (object == this || children().contains(object))
	{
		if (event->type() == QEvent::MouseButtonPress)
		{
			clickedHandler();
			return true;
		}
		else if (event->type() == QEvent::MouseButtonDblClick)
		{
			doubleClickHandler();
			return true;
		}
	}

	return QWidget::eventFilter(object, event);
}

void FSEntryWidget::paintEvent(QPaintEvent *event)
{
	Q_UNUSED(event);

	QStyleOption option;
	option.initFrom(this);
	QPainter painter(this);
	style()->drawPrimitive(QStyle::PE_Widget, &option, &painter, this);
}

void FSEntryWidget::nextCheckState()
{
	bool checked = ! isChecked();

	setChecked(checked);

	if (checked)
		setStyleSheet(_checkedSS);
	else
		setStyleSheet(_uncheckedSS);
}

void FSEntryWidget::clickedHandler()
{
	if (_clickMeans == ClickIsOpen)
	{
		emit opened();
	}
	else
	{
		nextCheckState();
		emit selected();
	}
}

void FSEntryWidget::doubleClickHandler()
{
	if (_clickMeans == ClickIsSelect)
		emit opened();
}

void FSEntryWidget::initIcons()
{
	if (_smallIcons != NULL)
		return;

	QSize smallSize(32, 32);
	QSize largeSize(40, 40);

	_smallIcons = new QPixmap[FSEntry::NoOfTypes];
	_largeIcons = new QPixmap[FSEntry::NoOfTypes];

	_smallIcons[FSEntry::JASP] = QPixmap(QIcon(":/icons/file-jasp.svg").pixmap(smallSize));
	_smallIcons[FSEntry::CSV] = QPixmap(QIcon(":/icons/spreadsheet.svg").pixmap(smallSize));
	_smallIcons[FSEntry::SPSS] = QPixmap(QIcon(":/icons/spreadsheet.svg").pixmap(smallSize));
	_smallIcons[FSEntry::Other] = QPixmap(QIcon(":/icons/spreadsheet.svg").pixmap(smallSize));
	_smallIcons[FSEntry::Folder] = QPixmap(QIcon(":/icons/folder.svg").pixmap(smallSize));

	_largeIcons[FSEntry::JASP] = QPixmap(QIcon(":/icons/file-jasp.svg").pixmap(largeSize));
	_largeIcons[FSEntry::CSV] = QPixmap(QIcon(":/icons/spreadsheet.svg").pixmap(largeSize));
	_largeIcons[FSEntry::SPSS] = QPixmap(QIcon(":/icons/spreadsheet.svg").pixmap(largeSize));
	_largeIcons[FSEntry::Other] = QPixmap(QIcon(":/icons/spreadsheet.svg").pixmap(largeSize));
	_largeIcons[FSEntry::Folder] = QPixmap(QIcon(":/icons/folder.svg").pixmap(largeSize));
}

