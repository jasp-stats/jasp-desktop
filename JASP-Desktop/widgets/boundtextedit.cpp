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

#include "boundtextedit.h"

#include <QKeyEvent>
#include <QPaintEvent>
#include <QPainter>

#include "qutils.h"

BoundTextEdit::BoundTextEdit(QWidget *parent) :
	QTextEdit(parent)
{
	_model = new TextModelLavaan(this);
	setDocument(_model);

	connect(this, SIGNAL(cursorPositionChanged()), this, SLOT(cursorPositionChangedHandler()));
	connect(_model, SIGNAL(errorStateChanged()), this, SLOT(errorStateChangedHandler()));
	connect(_model, SIGNAL(contentsChanged()), this, SLOT(contentsChangedHandler()));

	this->setLineWrapMode(QTextEdit::NoWrap);

	QFont font("Monospace");
	font.setStyleHint(QFont::Monospace);
	this->setFont(font);

	QFontMetrics metrics(font);
	this->setTabStopWidth(metrics.width("    ") + 2);

	_errorStylesheet = "padding : 4px 2px ; text-align : right ; background-color : pink ;";
	_okStylesheet    = "padding : 4px 2px ; text-align : right ; ";

#ifdef __APPLE__
	_okMessage = "\u2318 + Enter to apply";
#else
	_okMessage = "Ctrl + Enter to apply";
#endif

	_status = new QLabel(this);
	_status->setStyleSheet(_okStylesheet);
	_status->setText(_okMessage);

	_applied = true;
}

void BoundTextEdit::bindTo(Option *option)
{
	if (_model != NULL)
		_model->bindTo(option);

}

void BoundTextEdit::cursorPositionChangedHandler()
{
	QTextCursor cursor = textCursor();
	_model->cursorPositionChangedHandler(cursor);
}

void BoundTextEdit::errorStateChangedHandler()
{
	if (_model->inError())
	{
		_status->setStyleSheet(_errorStylesheet);
		_status->setText(_model->errorMessage());
	}
	else
	{
		_status->setStyleSheet(_okStylesheet);
		_status->setText(_okMessage + " *");
	}
}

void BoundTextEdit::contentsChangedHandler()
{
	if (_applied)
	{
		_applied = false;
		_status->setText(_okMessage + " *");
	}
}

void BoundTextEdit::keyPressEvent(QKeyEvent *event)
{
	if (_model != NULL)
	{
		int modifiers = Qt::ControlModifier | Qt::MetaModifier;
		if ((event->modifiers() & modifiers) && event->key() == Qt::Key_Return)
		{
			_applied = true;
			_status->setText(_okMessage);
			_model->apply();
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

void BoundTextEdit::resizeEvent(QResizeEvent *e)
{
	_status->move(1, this->height() - _status->height() - 1);
	_status->resize(this->width() - 2, _status->sizeHint().height());
}

void BoundTextEdit::paintEvent(QPaintEvent *event)
{
	QTextEdit::paintEvent(event);

	if (_model->inError() && textCursor().blockNumber() != _model->errorBlock())
	{
		QTextBlock block = _model->findBlockByNumber(_model->errorBlock());

		QTextCursor cursor(block);
		cursor.movePosition(QTextCursor::NextCharacter, QTextCursor::MoveAnchor, _model->errorTokenPos());

		QRect startPos = cursorRect(cursor);
		QRect bounds;

		if (_model->errorTokenLength() > 0)
		{
			cursor.movePosition(QTextCursor::NextCharacter, QTextCursor::KeepAnchor, _model->errorTokenLength());
			bounds = cursorRect(cursor).united(startPos);
		}
		else
		{
			bounds = cursorRect(cursor);
			bounds.setWidth(30);
			bounds.setLeft(bounds.left() + 4);
		}

		QPainter painter(this->viewport());
		painter.setPen(QPen(QBrush(Qt::red), 2));
		painter.drawLine(bounds.bottomLeft(), bounds.bottomRight());

	}
}

void BoundTextEdit::insertFromMimeData(const QMimeData *source)
{
	QTextEdit::insertFromMimeData(source);

	if (_model != NULL)
		_model->checkEverything();
}
