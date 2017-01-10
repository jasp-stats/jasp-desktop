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

#ifndef TEXTMODELLAVAAN_H
#define TEXTMODELLAVAAN_H

#include "common.h"
#include "boundmodel.h"
#include "options/optionstring.h"
#include <QTextDocument>
#include <QStringList>
#include <QSyntaxHighlighter>
#include <QTextCursor>

enum TokenType { UnknownToken, Variable, Comment, Operator, Plus, Times, Number, FunctionOpen, FunctionClose, Comma };

typedef struct TokenStruct
{
	QString text;
	int pos;
	int type;

} Token;

class TextModelLavaan : public QTextDocument, public BoundModel
{
	Q_OBJECT

public:
	TextModelLavaan(QObject *parent);

	virtual void bindTo(Option *option) OVERRIDE;
	bool inError() const;
	QString errorMessage() const;
	int errorBlock() const;
	int errorTokenPos() const;
	int errorTokenLength() const;

public slots:
	void cursorPositionChangedHandler(QTextCursor cursor);
	void apply();
	void checkEverything();

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
	int _errorBlock;
	int _errorTokenPos;
	int _errorTokenLength;

	void setErrorState(bool error, QString message = "", int errorBlock = -1, int tokenPos = -1, int tokenLength = -1);

	class BlockStatus : public QTextBlockUserData
	{
	public:
		BlockStatus()
		{
			this->error = false;
		}

		void setError(QString message, int pos, int length)
		{
			this->error = true;
			this->message = message;
			this->pos = pos;
			this->length = length;
		}

		void clearError()
		{
			this->error = false;
		}

		bool isError()
		{
			return this->error;
		}

		bool error;
		QString message;
		int pos;
		int length;
		bool inserted;
	};

	static QList<Token> tokenise(QTextBlock &block);
	static QList<Token> parse(QTextBlock &block);
	void checkBlock(QTextBlock &block);
	static void checkExpression(const QList<Token> &tokens, int &i, BlockStatus *status);
	static BlockStatus *blockStatus(QTextBlock &block);

	OptionString *_boundTo;
	QSyntaxHighlighter *_highlighter;

	QStringList _latents;
	QStringList _manifest;

	class SyntaxHighlighter : public QSyntaxHighlighter
	{
	public:
		SyntaxHighlighter(QTextDocument *parent);
		virtual void highlightBlock(const QString &text) OVERRIDE;
	};

};

#endif // TEXTMODELLAVAAN_H
