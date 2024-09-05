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

class RSyntaxHighlighter : public QSyntaxHighlighter
{
public:
    RSyntaxHighlighter(QTextDocument *parent);
	virtual void highlightBlock(const QString &text) override;
    void setStringsFormat(const QString &text, QChar c);
private:
	struct HighlightingRule
	{
		QRegularExpression pattern;
		QTextCharFormat format;
	};
	QVector<HighlightingRule> highlightingRules;
	QTextCharFormat operatorFormat;
	QTextCharFormat variableFormat;
    QTextCharFormat commentFormat;
    QTextCharFormat keywordFormat;
    QTextCharFormat stringFormat;
    QTextCharFormat booleanFormat;
    QTextCharFormat numberFormat;
    QTextCharFormat punctuationFormat;
};

class RSyntaxHighlighterQuick : public QQuickItem
{
	Q_OBJECT
	Q_PROPERTY(QQuickTextDocument* textDocument		READ textDocument	WRITE setTextDocument NOTIFY textDocumentChanged)
	
public:
	RSyntaxHighlighterQuick(QQuickItem * parent = nullptr) : QQuickItem(parent)
	{}
	
	QQuickTextDocument * textDocument() { return _textDocument; }
	
	void setTextDocument(QQuickTextDocument * textDocument)
	{
		if(_textDocument == textDocument)	
			return;
		
		_textDocument = textDocument;
		
		if(_textDocument)
			_highlighter = new RSyntaxHighlighter(_textDocument->textDocument());
		
		emit textDocumentChanged();
	}

signals:
	void textDocumentChanged();
	
private:
	RSyntaxHighlighter	* _highlighter = nullptr;
	QQuickTextDocument		* _textDocument = nullptr;
};

#endif // RLANGSYNTAXHIGHLIGHTER_H
