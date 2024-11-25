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

#include "rsyntaxhighlighter.h"

RSyntaxHighlighter::RSyntaxHighlighter(QTextDocument *parent)
	: QSyntaxHighlighter(parent), VariableInfoConsumer(), _textDocument(parent)
{
	if(VariableInfo::info())
	{
		connect(VariableInfo::info(), &VariableInfo::namesChanged,		this, &RSyntaxHighlighter::handleNamesChanged);
		connect(VariableInfo::info(), &VariableInfo::rowCountChanged,	this, &RSyntaxHighlighter::handleRowCountChanged);
	}

	HighlightingRule rule;
	// most of these R regExp are copied from: https://github.com/PrismJS/prism/blob/master/components/prism-r.js

	// operators
	_operatorFormat.setForeground(Qt::red);
	rule.pattern = QRegularExpression(R"(->?>?|<(?:=|<?-)?|[>=!]=?|::?|&&?|\|\|?|[+*\/^$@~]|%[^%\s]*%)");
	rule.format = _operatorFormat;
	_highlightingRules.append(rule);

	// variables
	_variableFormat.setToolTip("variable");
	rule.pattern = QRegularExpression(R"(\b\w*\b)");
	rule.format = _variableFormat;
	_highlightingRules.append(rule);

	// string
	_stringFormat.setForeground(Qt::darkGreen);
	rule.pattern = QRegularExpression(R"(([`'"])(?:\\.|(?!\1)[^\\\r\n])*\1)");
	rule.format = _stringFormat;
	_highlightingRules.append(rule);

	// keyword
	_keywordFormat.setForeground(Qt::darkCyan);
	rule.pattern = QRegularExpression(R"(\b(?:NA|NA_character_|NA_complex_|NA_integer_|NA_real_|NULL|break|else|for|function|if|in|next|repeat|while)\b)");
	rule.format = _keywordFormat;
	_highlightingRules.append(rule);

	// boolean and special number
	_booleanFormat.setForeground(Qt::magenta);
	rule.pattern = QRegularExpression(R"(\b(?:FALSE|TRUE|Inf|NaN)\b)");
	rule.format = _booleanFormat;
	_highlightingRules.append(rule);

	// number
	_numberFormat.setForeground(Qt::darkMagenta);
	rule.pattern = QRegularExpression(R"((?:\b0x[\dA-Fa-f]+(?:\.\d*)?|\b\d+(?:\.\d*)?|\B\.\d+)(?:[EePp][+-]?\d+)?[iL]?)");
	rule.format = _numberFormat;
	_highlightingRules.append(rule);

	// punctuation
	_punctuationFormat.setForeground(Qt::blue);
	rule.pattern = QRegularExpression(R"([(){}\[\],;])");
	rule.format = _punctuationFormat;
	_highlightingRules.append(rule);

	// comments
	_commentFormat.setForeground(Qt::darkGray);
	_commentFormat.setFontItalic(true);
	_commentRule.pattern = QRegularExpression(R"(#[^\n]*)");
	_commentRule.format = _commentFormat;
	
	// columns
	_columnFormat.setForeground(Qt::blue);
	_columnFormat.setFontItalic(true);
}

void RSyntaxHighlighter::highlightBlock(const QString &text)
{
	setStringsFormat(text, '"');
	setStringsFormat(text, '\'');
	setStringsFormat(text, '`');
	
	for (const HighlightingRule & rule : _highlightingRules)
		applyRule(text, rule);
	
	//Do columns
	QStringList			names = requestInfo(VariableInfo::InfoType::VariableNames).toStringList();
	
	for(const QString & name : names)
		applyRule(text, QRegularExpression(QString(R"(%1(\.(scale|ordinal|nominal))?)").arg(name)), _columnFormat);
	
	applyRule(text, _commentRule);
}

void RSyntaxHighlighter::setStringsFormat(const QString &text, QChar c)
{
	int start = -1;
	for (int i = 0; i < text.size(); ++i)
	{
		if (text[i] == c && (i == 0 || text[i - 1] != '\\'))
		{
			if (start == -1)
				start = i;
			else
			{
				setFormat(start, i - start + 1, _stringFormat);
				start = -1;
			}
		}
	}
}

void RSyntaxHighlighter::applyRule(const QString & text, const QRegularExpression & pattern, const QTextCharFormat & format)
{
	QRegularExpressionMatchIterator matchIterator = pattern.globalMatch(text);

	while (matchIterator.hasNext())
	{
		QRegularExpressionMatch match = matchIterator.next();
		setFormat(match.capturedStart(), match.capturedLength(), format);
	}
}

void RSyntaxHighlighterQuick::setTextDocument(QQuickTextDocument *textDocument) 
{
	if(_textDocument == textDocument)	
		return;
	
	_textDocument = textDocument;
	
	if(_textDocument)
		_highlighter = new RSyntaxHighlighter(_textDocument->textDocument());
	
	emit textDocumentChanged();
}
