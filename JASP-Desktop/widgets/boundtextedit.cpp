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

#include "boundtextedit.h"

#include <QKeyEvent>
#include <QPaintEvent>
#include <QPainter>
#include <QFontDatabase>

#include "utilities/qutils.h"

BoundTextEdit::BoundTextEdit(QWidget *parent) :
	QTextEdit(parent)
{
	_model = new TextModelLavaan(this);
	setDocument(_model);

	connect(this,	&BoundTextEdit::cursorPositionChanged,	this, &BoundTextEdit::cursorPositionChangedHandler);
	connect(_model, &TextModelLavaan::errorStateChanged,	this, &BoundTextEdit::errorStateChangedHandler);
	connect(_model, &TextModelLavaan::contentsChanged,		this, &BoundTextEdit::contentsChangedHandler);

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

	_errorStylesheet = "padding: 4px; padding-right: 1.5em; background-color: rgba(255,0,0,128); margin-left: 100%;";
	_okStylesheet    = "padding: 4px; padding-right: 1.5em; margin-left: 100%;";

#ifdef __APPLE__
	_okMessage = "\u2318 + Enter to apply";
#else
	_okMessage = "Ctrl + Enter to apply";
#endif

	_status = new QLabel(this);
	_status->setWordWrap(true);
	_status->setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Minimum);
	_status->setAlignment(Qt::AlignBottom | Qt::AlignRight);
	_status->setStyleSheet(_okStylesheet);
	_status->setText(_okMessage);
	_status->setAttribute(Qt::WA_TransparentForMouseEvents);
	
	_applied = true;
}

void BoundTextEdit::bindTo(Option *option)
{
	if (_model != NULL) {
		_model->bindTo(option);

		populateFromOption(option);
	}
}

void BoundTextEdit::populateFromOption(Option *option)
{
	OptionString *text = dynamic_cast<OptionString *>(option);
	this->setPlainText(QString::fromStdString(text->value()));
}

void BoundTextEdit::applyModel(QString result)
{
    //std::cout << "result of length " << result.length() << ": " << result.toStdString() << std::flush;

    if (result.length() == 0) {
		_status->setStyleSheet(_okStylesheet);
		_status->setAlignment(Qt::AlignRight);
		_status->setText("Model applied");
		_model->apply();
		_applied = true;
	} else {
		_status->setStyleSheet(_errorStylesheet);
		_status->setAlignment(Qt::AlignLeft);
		_status->setText(result);
	}
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
		_status->setAlignment(Qt::AlignLeft);
		_status->setText(_model->errorMessage());
	}
	else
	{
		_status->setStyleSheet(_okStylesheet);
		_status->setAlignment(Qt::AlignRight);
		_status->setText(_okMessage);
	}
}

void BoundTextEdit::contentsChangedHandler()
{
	if (_applied)
	{
		_applied = false;
		_status->setText(_okMessage);
	}
}

void BoundTextEdit::keyPressEvent(QKeyEvent *event)
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

void BoundTextEdit::resizeEvent(QResizeEvent *e)
{
	_status->move(1, this->height() - _status->height() - 1);
	_status->resize(this->width() - 2, _status->sizeHint().height());
}


void BoundTextEdit::insertFromMimeData(const QMimeData *source)
{
	QTextEdit::insertFromMimeData(source);
}
