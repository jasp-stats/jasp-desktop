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

#include "textmodellavaan.h"

#include <QTextBlock>
#include <QRegExp>
#include <QDebug>

#include "utilities/qutils.h"

TextModelLavaan::TextModelLavaan(QObject *parent)
	: QTextDocument(parent)
{
	_boundTo = NULL;
	_currentBlock = 0;
	_changed = false;
	_inError = false;
	this->setDocumentMargin(12);
	_highlighter = new TextModelLavaan::SyntaxHighlighter(this);

	connect(this, &TextModelLavaan::contentsChanged, this, &TextModelLavaan::contentChangedHandler);

}

void TextModelLavaan::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionString *>(option);
}

bool TextModelLavaan::inError() const
{
	return _inError;
}

QString TextModelLavaan::errorMessage() const
{
	return _errorMessage;
}

void TextModelLavaan::setErrorState(bool error, QString message)
{
	if (error != _inError || message != _errorMessage)
	{
		_inError = error;
		_errorMessage = message;

		emit errorStateChanged();
	}
}


void TextModelLavaan::cursorPositionChangedHandler(QTextCursor cursor)
{
	int currentBlock = cursor.blockNumber();

	if (currentBlock != _currentBlock)
	{
		_currentBlock = currentBlock;
	}
}

void TextModelLavaan::apply()
{
	if (_boundTo != NULL && !_inError) 
	{
		_boundTo->setValue(this->toPlainText().toStdString());
	}
}

void TextModelLavaan::contentChangedHandler()
{
	_changed = true;
	_content = toPlainText();
}


TextModelLavaan::SyntaxHighlighter::SyntaxHighlighter(QTextDocument *parent)
	: QSyntaxHighlighter(parent)
{
	
	HighlightingRule rule;
	
	// operators
	operatorFormat.setForeground(Qt::darkGreen);
	QStringList operatorPatterns;
	operatorPatterns << "\\=" << "\\~" << "\\<"
					 << "\\*" << "\\>" << "\\:"
					 << "\\%" << "\\|" << "\\+";
	for (const QString &pattern : operatorPatterns) {
		rule.pattern = QRegularExpression(pattern);
		rule.format = operatorFormat;
		highlightingRules.append(rule);
	}
	
	// variables
	variableFormat.setToolTip("variable");
	rule.pattern = QRegularExpression("\\b\\w*\\b");
	rule.format = variableFormat;
	highlightingRules.append(rule);
	
	// comments
	commentFormat.setForeground(Qt::darkGray);
	commentFormat.setFontItalic(true);
	rule.pattern = QRegularExpression("#[^\n]*");
	rule.format = commentFormat;
	highlightingRules.append(rule);
}

void TextModelLavaan::SyntaxHighlighter::highlightBlock(const QString &text)
{
	for (const HighlightingRule &rule : highlightingRules)
	{
		QRegularExpressionMatchIterator matchIterator = rule.pattern.globalMatch(text);
		while (matchIterator.hasNext())
		{
			QRegularExpressionMatch match = matchIterator.next();
			setFormat(match.capturedStart(), match.capturedLength(), rule.format);
		}
	}
}
