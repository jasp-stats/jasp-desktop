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

#ifndef TEXTMODELLAVAAN_H
#define TEXTMODELLAVAAN_H

#include "common.h"
#include "boundmodel.h"
#include "analysis/options/optionstring.h"
#include <QTextDocument>
#include <QStringList>
#include <QSyntaxHighlighter>
#include <QTextCursor>
#include <QRegularExpression>

enum TokenType { UnknownToken, Variable, Comment, Operator, Plus, Times, Number, FunctionOpen, FunctionClose, Comma };

class TextModelLavaan : public QTextDocument, public BoundModel
{
	Q_OBJECT

public:
	TextModelLavaan(QObject *parent);

	virtual void bindTo(Option *option) OVERRIDE;
	bool inError() const;
	QString errorMessage() const;

public slots:
	void cursorPositionChangedHandler(QTextCursor cursor);
	void apply();
	
signals:
	void errorStateChanged();

private slots:
	void contentChangedHandler();

private:
	QString _content;
	int _currentBlock;
	bool _changed;

	bool _inError;
	QString _errorMessage;

	void setErrorState(bool error, QString message = "");

	OptionString *_boundTo;
	QSyntaxHighlighter *_highlighter;

public:
	class SyntaxHighlighter : public QSyntaxHighlighter
	{
	public:
		SyntaxHighlighter(QTextDocument *parent);
		virtual void highlightBlock(const QString &text) OVERRIDE;
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
	};

};

#endif // TEXTMODELLAVAAN_H
