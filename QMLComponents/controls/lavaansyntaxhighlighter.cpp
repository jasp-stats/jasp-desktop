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

#include "lavaansyntaxhighlighter.h"

LavaanSyntaxHighlighter::LavaanSyntaxHighlighter(QTextDocument *parent)
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

void LavaanSyntaxHighlighter::highlightBlock(const QString &text)
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
