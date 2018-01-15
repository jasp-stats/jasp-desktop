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

#include "baintextedit.h"

#include <QKeyEvent>
#include <QPaintEvent>
#include <QPainter>
#include <QFontDatabase>

#include "qutils.h"

BainTextEdit::BainTextEdit(QWidget *parent) :
	QTextEdit(parent)
{
	_model = new TextModelBain(this);
	setDocument(_model);

	connect(_model, &TextModelBain::contentsChanged,	this,	&BainTextEdit::contentsChangedHandler);
	connect(this,	&BainTextEdit::applyRequest,		_model,	&TextModelBain::apply);

	this->setLineWrapMode(QTextEdit::NoWrap);
	this->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
	this->setAcceptRichText(false);
		
	int id = QFontDatabase::addApplicationFont(":/fonts/FiraCode-Retina.ttf");
	QString family = QFontDatabase::applicationFontFamilies(id).at(0);
	
	QFont font(family);
	font.setStyleHint(QFont::Monospace);
	font.setPointSize(10);
	this->setFont(font);

	QFontMetrics metrics(font);
	this->setTabStopWidth(metrics.width("  ") + 2);
}

void BainTextEdit::bindTo(Option *option)
{
	if (_model != NULL) {
		_model->bindTo(option);

		populateFromOption(option);
	}
}

void BainTextEdit::populateFromOption(Option *option)
{
	OptionString *text = dynamic_cast<OptionString *>(option);
}


void BainTextEdit::contentsChangedHandler()
{
	std::cout << "Contents changed" << std::flush;
}

void BainTextEdit::insertFromMimeData(const QMimeData *source)
{
	QTextEdit::insertFromMimeData(source);
}


void BainTextEdit::keyPressEvent(QKeyEvent *event)
{
	if (_model != NULL)
	{
        int modifiers = Qt::ControlModifier | Qt::MetaModifier;
        if ((event->modifiers() & modifiers) && (event->key() == Qt::Key_Return || event->key() == Qt::Key_Enter))
        {
            emit applyRequest();
        }
		else
		{
			QTextEdit::keyPressEvent(event);
		}
	}
	else
	{
		QTextEdit::keyPressEvent(event);
	}
}
