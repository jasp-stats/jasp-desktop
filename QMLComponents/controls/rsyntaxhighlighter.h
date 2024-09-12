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

#ifndef RLANGSYNTAXHIGHLIGHTER_H
#define RLANGSYNTAXHIGHLIGHTER_H

#include <QSyntaxHighlighter>
#include <QTextCursor>
#include <QRegularExpression>
#include <QQuickItem>
#include <QQuickTextDocument>
#include "variableinfo.h"

class RSyntaxHighlighter : public QSyntaxHighlighter, public VariableInfoConsumer
{
	Q_OBJECT
	
	struct HighlightingRule
	{
		QRegularExpression		pattern;
		QTextCharFormat			format;
	};
	
public:
				RSyntaxHighlighter(QTextDocument *parent);
				
	void		highlightBlock(const QString &text) override;
    void		setStringsFormat(const QString &text, QChar c);
	
	void		applyRule(const QString & text,  const HighlightingRule   & rule)											{ applyRule(text, rule.pattern, rule.format); }
	void		applyRule(const QString & text,  const QRegularExpression & pattern, const QTextCharFormat & format);
	
	
	
protected slots:
	void		handleNamesChanged(QMap<QString, QString> changedNames)	{ rehighlight(); }
	void		handleRowCountChanged()									{ rehighlight(); }

private:

	
	QTextDocument			*	_textDocument = nullptr;
	
	QVector<HighlightingRule>	_highlightingRules;
	QTextCharFormat				_punctuationFormat,
								_operatorFormat,
								_variableFormat,
								_commentFormat,
								_keywordFormat,
								_stringFormat,
								_booleanFormat,
								_numberFormat,
								_columnFormat;
	HighlightingRule			_commentRule;
};

class RSyntaxHighlighterQuick : public QQuickItem
{
	Q_OBJECT
	Q_PROPERTY(QQuickTextDocument* textDocument		READ textDocument	WRITE setTextDocument NOTIFY textDocumentChanged)
	
public:
	RSyntaxHighlighterQuick(QQuickItem * parent = nullptr) : QQuickItem(parent)	{}
	
	QQuickTextDocument * textDocument() { return _textDocument; }
	
	void setTextDocument(QQuickTextDocument * textDocument);
	
signals:
	void textDocumentChanged();
	
private:
	RSyntaxHighlighter		* _highlighter  = nullptr;
	QQuickTextDocument		* _textDocument = nullptr;
};

#endif // RLANGSYNTAXHIGHLIGHTER_H
