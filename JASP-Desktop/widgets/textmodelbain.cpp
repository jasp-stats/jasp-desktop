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

#include "textmodelbain.h"

#include <QTextBlock>
#include <QRegExp>
#include <QDebug>

#include "qutils.h"

TextModelBain::TextModelBain(QObject *parent)
	: QTextDocument(parent)
{
	_boundTo = NULL;
	this->setDocumentMargin(12);
	_highlighter = new TextModelBain::SyntaxHighlighter(this);

	connect(this, &TextModelBain::contentsChanged, this, &TextModelBain::contentChangedHandler);

}

void TextModelBain::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionString *>(option);
}


void TextModelBain::contentChangedHandler()
{
	_content = toPlainText();
}

void TextModelBain::apply()
{
	if (_boundTo != NULL) 
	{
		_boundTo->setValue(this->toPlainText().toStdString());
	}
}


TextModelBain::SyntaxHighlighter::SyntaxHighlighter(QTextDocument *parent)
	: QSyntaxHighlighter(parent)
{
	
	HighlightingRule rule;
	
	// operators
	operatorFormat.setForeground(Qt::darkGreen);
	QStringList operatorPatterns;
	operatorPatterns << "\\=" << "\\~" << "\\<"
					 << "\\*" << "\\>" << "\\:"
					 << "\\%" << "\\|" << "\\+";
	foreach (const QString &pattern, operatorPatterns) {
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

void TextModelBain::SyntaxHighlighter::highlightBlock(const QString &text)
{
	foreach (const HighlightingRule &rule, highlightingRules) {
		QRegularExpressionMatchIterator matchIterator = rule.pattern.globalMatch(text);
		while (matchIterator.hasNext()) {
			QRegularExpressionMatch match = matchIterator.next();
			setFormat(match.capturedStart(), match.capturedLength(), rule.format);
		}
	}
}
