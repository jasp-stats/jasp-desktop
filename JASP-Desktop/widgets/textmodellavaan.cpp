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

#include "textmodellavaan.h"

#include <QTextBlock>
#include <QRegExp>
#include <QDebug>

#include "qutils.h"

TextModelLavaan::TextModelLavaan(QObject *parent)
	: QTextDocument(parent)
{
	_boundTo = NULL;
	_currentBlock = 0;
	_changed = false;
	this->setDocumentMargin(12);

	connect(this, SIGNAL(contentsChanged()), this, SLOT(contentChangedHandler()));

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

int TextModelLavaan::errorBlock() const
{
	return _errorBlock;
}

int TextModelLavaan::errorTokenPos() const
{
	return _errorTokenPos;
}

int TextModelLavaan::errorTokenLength() const
{
	return _errorTokenLength;
}

void TextModelLavaan::setErrorState(bool error, QString message, int errorBlock, int tokenPos, int tokenLength)
{
	if (error != _inError || message != _errorMessage || errorBlock != _errorBlock || tokenPos != _errorTokenPos || tokenLength != _errorTokenLength)
	{
		_inError = error;
		_errorMessage = message;
		_errorBlock = errorBlock;
		_errorTokenPos = tokenPos;
		_errorTokenLength = tokenLength;

		emit errorStateChanged();
	}
}

QList<Token> TextModelLavaan::tokenise(QTextBlock &block)
{
	BlockStatus *status = blockStatus(block);

	Token token;
	QList<Token> tokens;

	enum ParseStatus { OutsideToken, InSingleQuotedToken, InDoubleQuotedToken, InUnquotedToken, InFunctionCall, InNumber, InOperator, AComment };

	ParseStatus parseState = OutsideToken;
	int tokenStart = 0;

	QString text = block.text() + "\n";

	for (int i = 0; i < text.length(); i++)
	{
		QChar c = text.at(i);

		switch (parseState)
		{
		case OutsideToken:
		{
			if (c == ' ' || c == '\t' || c == '\n')
			{
				// do nothing
			}
			else if (c == '#')
			{
				parseState = AComment;
				tokenStart = i;
			}
			else if (c == '\'')
			{
				parseState = InSingleQuotedToken;
				tokenStart = i + 1;
			}
			else if (c == '\"')
			{
				parseState = InDoubleQuotedToken;
				tokenStart = i + 1;
			}
			else if (c.isDigit())
			{
				parseState = InNumber;
				tokenStart = i;
			}
			else if (c == '~' || c == '=' || c == ':' || c == '>' || c == '<')
			{
				parseState = InOperator;
				tokenStart = i;
			}
			else if (c == '+')
			{
				token.text = "+";
				token.pos = i;
				token.type = Plus;

				tokens.append(token);
			}
			else if (c == '*')
			{
				token.text = "*";
				token.pos = i;
				token.type = Times;

				tokens.append(token);
			}
			else if (c == ',')
			{
				token.text = ",";
				token.pos = i;
				token.type = Comma;

				tokens.append(token);
			}
			else if (c == ')')
			{
				token.text = ")";
				token.pos = i;
				token.type = FunctionClose;

				tokens.append(token);
			}
			else if (c.isLetter())
			{
				parseState = InUnquotedToken;
				tokenStart = i;
			}
			else
			{
				token.text = text.at(i);
				token.pos = i;
				token.type = UnknownToken;

				tokens.append(token);

				status->setError("Unexpected character", i, 1);
				return tokens;
			}
			break;
		}
		case InNumber:
		{
			if (c == ' ' || c == '\t' || c == '*' || c == '+' || c == '#' || c == '\n' || c == '~' || c == '=' || c == ':' || c == '>' || c == '<' || c == ')' || c == ',')
			{
				token.text = text.mid(tokenStart, i - tokenStart);
				token.pos = tokenStart;

				bool isNumber;
				token.text.toDouble(&isNumber);

				if (isNumber)
				{
					token.type = Number;
					tokens.append(token);
				}
				else
				{
					token.type = Variable;
					tokens.append(token);
					status->setError("Variable names which begin with a number must appear in quotes", tokenStart, i - tokenStart);

					return tokens;
				}

				parseState = OutsideToken;
				i--; // push back
			}
			break;
		}
		case InSingleQuotedToken:
		{
			if (c == '\'')
			{
				token.text = text.mid(tokenStart, i - tokenStart);
				token.pos = tokenStart;
				token.type = Variable;
				tokens.append(token);
				parseState = OutsideToken;
			}
			else if (c == '\n')
			{
				token.text = text.mid(tokenStart);
				token.pos = tokenStart;
				token.type = Variable;
				tokens.append(token);

				status->setError("No closing quote", tokenStart - 1, token.text.length());
			}
			break;
		}
		case InDoubleQuotedToken:
		{
			if (c == '"')
			{
				token.text = text.mid(tokenStart, i - tokenStart);
				token.pos = tokenStart;
				token.type = Variable;
				tokens.append(token);
				parseState = OutsideToken;
			}
			else if (c == '\n')
			{
				token.text = text.mid(tokenStart);
				token.pos = tokenStart;
				token.type = Variable;
				tokens.append(token);

				status->setError("No closing quote", tokenStart - 1, token.text.length());
			}
			break;
		}
		case InOperator:
		{
			if (c != '~' && c != '=' && c != ':' && c != '>' && c != '<')
			{
				token.text = text.mid(tokenStart, i - tokenStart);
				token.pos = tokenStart;
				token.type = Operator;
				tokens.append(token);

				parseState = OutsideToken;
				i--;
			}
			break;
		}
		case InFunctionCall:
		{
			if (c == '(')
			{
				token.text = text.mid(tokenStart, i - tokenStart + 1);
				token.pos = tokenStart;
				token.type = FunctionOpen;
				tokens.append(token);

				parseState = OutsideToken;
			}
			else if (c != ' ' && c != '\t')
			{
				token.text = text.mid(tokenStart, i - tokenStart);
				token.pos = tokenStart;
				token.type = FunctionOpen;
				tokens.append(token);

				status->setError("function call requires ()", token.pos, token.text.length());
				return tokens;
			}
			break;
		}
		case AComment:
		{
			if (c == '\n')
			{
				token.text = text.mid(tokenStart, text.length() - tokenStart - 1);
				token.pos = tokenStart;
				token.type = Comment;
				tokens.append(token);
			}
			break;
		}
		default:
		{
			if (c.isLetterOrNumber() == false)
			{
				QString tokenText = text.mid(tokenStart, i - tokenStart);

				if (tokenText == "start" || tokenText == "c" || tokenText == "equal")
				{
					parseState = InFunctionCall;
					i--;
				}
				else
				{
					token.text = tokenText;
					token.pos = tokenStart;
					token.type = Variable;
					tokens.append(token);

					parseState = OutsideToken;
					i--; // push back
				}
			}
			break;
		}
		}

	}

	/*QString debug;
	foreach (Token token, tokens)
	{
		debug.append(token.text).append(", ");
	}
	qDebug() << debug << "\n";*/

	return tokens;
}

QList<Token> TextModelLavaan::parse(QTextBlock &block)
{
	BlockStatus *status = blockStatus(block);
	status->clearError();

	QList<Token> tokens = tokenise(block);

	if (status->isError())
	{
		qDebug() << "error : " << status->message << "\n";
		return tokens;
	}

	if (tokens.length() == 0 || tokens.at(0).type == Comment)
		return tokens;

	Token &first = tokens[0];

	if (first.type != Variable)
	{
		status->setError("Expected a variable", first.pos, first.text.length());
		return tokens;
	}
	else if (tokens.length() == 1)
	{
		status->setError("Expected an operator", first.pos + first.text.length(), -1);
		return tokens;
	}

	if (tokens.length() >= 2)
	{
		Token &second = tokens[1];

		if (second.type == Comment)
		{
			status->setError("Expected an operator", first.pos + first.text.length(), -1);
			return tokens;
		}
		else if (second.type == Operator)
		{
			if (second.text != "~~" &&
				 second.text != "~" &&
				 second.text != "=~" &&
				 second.text != "==" &&
				 second.text != ">" &&
				 second.text != "<" &&
				 second.text != ":=")
			{
				status->setError("Unrecognised operator", second.pos, second.text.length());
				return tokens;
			}
			else if (tokens.length() == 2)
			{
				status->setError("Expected an expression", second.pos + second.text.length(), -1);
				return tokens;
			}
		}
		else
		{
			status->setError("Expected an operator", second.pos, second.text.length());
			return tokens;
		}
	}

	int i = 2;

	checkExpression(tokens, i, status);
	if (status->isError())
		return tokens;

	while (i < tokens.length())
	{
		Token token = tokens.at(i);

		if (token.type == Comment)
			break;

		if (token.type != Plus)
		{
			status->setError("Expected a plus", token.pos, token.text.length());
			return tokens;
		}

		i++;

		if (i >= tokens.length())
		{
			status->setError("Expected an expression", token.pos + token.text.length(), -1);
			return tokens;
		}

		checkExpression(tokens, i, status);
		if (status->isError())
			return tokens;
	}


	return tokens;
}

void TextModelLavaan::checkExpression(const QList<Token> &tokens, int &i, TextModelLavaan::BlockStatus *status)
{
	const Token &token = tokens[i];

	if (token.type == Variable)
	{
		i++;
		return;
	}
	else if (token.type == Number)
	{
		i++;
		return;
	}
	else if (token.type == FunctionOpen)
	{
		i++;

		if (i >= tokens.size())
		{
			status->setError("Missing closing bracket", token.pos, token.text.length());
			return;
		}

		Token next = tokens[i];

		if (next.type == FunctionClose)
		{
			status->setError("Missing function arguments", token.pos, token.text.length());
			return;
		}
		else if (next.type == FunctionOpen)
		{
			checkExpression(tokens, i, status);

			if (status->isError())
				return;
		}
		else if (next.type == Number)
		{
			i++;

			for (;;) {

				if (i >= tokens.length())
					break;

				next = tokens[i];
				if (next.type != Comma)
					break;

				i++;
				if (i >= tokens.length())
					break;

				next = tokens[i];
				if (next.type != Number)
				{
					status->setError("Expected a number", next.pos, next.text.length());
					return;
				}

				i++;
			}

		}
		else
		{
			status->setError("Expected a number (or vector)", next.pos, next.text.length());
		}


		if (i >= tokens.length())
		{
			status->setError("Missing closing bracket", token.pos, token.text.length());
			return;
		}

		next = tokens[i];

		if (next.type != FunctionClose)
		{
			status->setError("Expecting a closing bracket", next.pos, next.text.length());
			return;
		}

		i++;
	}
	else
	{
		status->setError("Expected an expression", token.pos, token.text.length());
		return;
	}



}

TextModelLavaan::BlockStatus *TextModelLavaan::blockStatus(QTextBlock &block)
{
	BlockStatus *status = (BlockStatus*) block.userData();

	if (status == NULL)
	{
		status = new BlockStatus();
		block.setUserData(status);
	}

	return status;
}

void TextModelLavaan::cursorPositionChangedHandler(QTextCursor cursor)
{
	int currentBlock = cursor.blockNumber();

	if (currentBlock != _currentBlock)
	{
		//checkEverything();
		_currentBlock = currentBlock;

		if (_changed)
		{
			//if ( ! inError())
			//	apply();
			_changed = false;
		}
	}
}

void TextModelLavaan::apply()
{
	//checkEverything();
	if (_boundTo != NULL && inError() == false)
		_boundTo->setValue(fq(this->toPlainText()));
}

void TextModelLavaan::contentChangedHandler()
{
	_changed = true;
	_content = toPlainText();

	QTextBlock current = findBlockByNumber(_currentBlock);
	checkBlock(current);
}

void TextModelLavaan::checkEverything()
{
	bool foundError = false;

	for (int i = 0; i < blockCount(); i++)
	{
		QTextBlock block = findBlockByNumber(i);

		checkBlock(block);
		BlockStatus *status = blockStatus(block);

		if (foundError == false && status->isError())
		{
			setErrorState(true, status->message, i, status->pos, status->length);
			foundError = true;

			QTextCursor cursor(block);

			blockSignals(true);

			/*QTextCharFormat format;

			format.setUnderlineStyle(QTextCharFormat::WaveUnderline);
			format.setUnderlineColor(Qt::red);

			if (status->length == -1)
			{
				if (i != _currentBlock)
				{
					if (status->inserted == false)
					{
						cursor.movePosition(QTextCursor::StartOfBlock);
						cursor.movePosition(QTextCursor::Right, QTextCursor::MoveAnchor, status->pos);

						cursor.insertText(" ");
						cursor.insertText("    ", format);
						status->inserted = true;
					}
					else
					{
						cursor.movePosition(QTextCursor::StartOfBlock);
						cursor.movePosition(QTextCursor::Right, QTextCursor::MoveAnchor, status->pos + 1);
						cursor.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor, 4);

						cursor.mergeCharFormat(format);
					}
				}
			}
			else
			{
				cursor.movePosition(QTextCursor::StartOfBlock);
				cursor.movePosition(QTextCursor::Right, QTextCursor::MoveAnchor, status->pos);
				cursor.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor, status->length);

				cursor.mergeCharFormat(format);
			}*/

			qDebug() << "error " << status->pos << " " << status->length << "\n";

			blockSignals(false);
		}
	}

	if ( ! foundError)
		setErrorState(false);
}

void TextModelLavaan::checkBlock(QTextBlock &block)
{
	blockSignals(true);

	QList<Token> tokens = parse(block);

	QTextCursor manipCursor(block);

	manipCursor.movePosition(QTextCursor::EndOfBlock, QTextCursor::KeepAnchor);
	manipCursor.setCharFormat(QTextCharFormat());

	foreach (const Token &token, tokens)
	{
		manipCursor.movePosition(QTextCursor::StartOfBlock);
		manipCursor.movePosition(QTextCursor::Right, QTextCursor::MoveAnchor, token.pos);
		manipCursor.movePosition(QTextCursor::Right, QTextCursor::KeepAnchor, token.text.size());

		//qDebug() << token.pos << ", " << token.text << token.type << "\n";

		QTextCharFormat format;

		switch (token.type)
		{
		case Comment:
			format.setForeground(QBrush(Qt::darkGreen));
			format.setFontItalic(true);
			break;
		case Variable:
			format.setForeground(QBrush(Qt::blue));
			break;
		case Operator:
			format.setForeground(QBrush(QColor(217, 119, 0)));
			break;
		case Number:
			format.setForeground(Qt::red);
			break;
		case FunctionOpen:
		case FunctionClose:
			format.setForeground(Qt::darkRed);
			break;
		default:
			format.setForeground(QBrush(Qt::black));
			break;
		}

		manipCursor.setCharFormat(format);
	}

	blockSignals(false);
}


TextModelLavaan::SyntaxHighlighter::SyntaxHighlighter(QTextDocument *parent)
	: QSyntaxHighlighter(parent)
{
}

void TextModelLavaan::SyntaxHighlighter::highlightBlock(const QString &text)
{
	int i = 0;

	int variableStart = -1;

	for (; i < text.length(); i++)
	{
		if (text.at(i).isSpace() && variableStart != -1)
			break;
		if (text.at(i).isLetterOrNumber() && variableStart == -1)
			variableStart = i;
	}

	QTextCharFormat format;
	format.setUnderlineStyle(QTextCharFormat::SingleUnderline);
	format.setUnderlineColor(Qt::red);

	int count = i - variableStart;
	if (count > 0)
		setFormat(variableStart, count, format);
}
