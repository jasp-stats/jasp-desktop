//
// Copyright (C) 2013-2020 University of Amsterdam
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

#include "boundcontrollavaantextarea.h"
#include "controls/textareabase.h"
#include "log.h"
#include "columnencoder.h"
#include "analysisform.h"
#include <QQuickTextDocument>

BoundControlLavaanTextArea::BoundControlLavaanTextArea(TextAreaBase *textArea)
	: BoundControlTextArea(textArea)
{

	QVariant textDocumentVariant = textArea->property("textDocument");
	QQuickTextDocument* textDocumentQQuick = textDocumentVariant.value<QQuickTextDocument *>();
	if (textDocumentQQuick)
	{
		QTextDocument* doc = textDocumentQQuick->textDocument();
		_lavaanHighlighter = new LavaanSyntaxHighlighter(doc);
		//connect(doc, &QTextDocument::contentsChanged, this, &BoundQMLTextArea::contentsChangedHandler);
	}
	else
		Log::log()  << "No document object found!" << std::endl;
}

void BoundControlLavaanTextArea::bindTo(const Json::Value &value)
{
	if (value.type() != Json::objectValue)	return;
	BoundControlBase::bindTo(value);

	_textArea->setText(tq(value["modelOriginal"].asString()));

	checkSyntax();

}

Json::Value BoundControlLavaanTextArea::createJson() const
{
	Json::Value result;
	std::string text = _textArea->text().toStdString();

	result["modelOriginal"] = text;
	result["model"]			= text;
	result["columns"]		= Json::Value(Json::arrayValue);

	return result;
}

bool BoundControlLavaanTextArea::isJsonValid(const Json::Value &value) const
{
	if (!value.isObject())					return false;
	if (!value["modelOriginal"].isString())	return false;
	//If we have modelOriginal the rest follows automatically because of checkSyntax and the result
	//if (!value["model"].isString())			return false;
	//if (!value["columns"].isArray())		return false;

	return true;
}

void BoundControlLavaanTextArea::checkSyntax()
{
	QString text = _textArea->text();

	// get the column names of the data set
	_usedColumnNames.clear();
	_textEncoded = tq(ColumnEncoder::columnEncoder()->encodeRScript(stringUtils::stripRComments(fq(text)), &_usedColumnNames));

	// Create R code string
	QString encodedColNames = "c(";
	for (const std::string& column : _usedColumnNames)
	{
		encodedColNames.append("'" + tq(ColumnEncoder::columnEncoder()->encode(column)) + "'");
		if (column != *_usedColumnNames.rbegin()) // avoid trailing ,
			encodedColNames.append(", ");
	}
	encodedColNames.append(")");

	QString checkCode = tq(_checkSyntaxRFunctionName());
	checkCode
		.append("('")
		.append(_textEncoded)
		.append("', ")
		.append(encodedColNames)
		.append(")");

	_textArea->runRScript(checkCode, false);

}

QString BoundControlLavaanTextArea::rScriptDoneHandler(const QString & result)
{
	if (!result.isEmpty())
		return result;

	Json::Value boundValue(Json::objectValue);

	boundValue["modelOriginal"] = _textArea->text().toStdString();
	boundValue["model"]			= _textEncoded.toStdString();

	Json::Value columns(Json::arrayValue);
	for (const std::string& column : _usedColumnNames)
		columns.append(ColumnEncoder::columnEncoder()->encode(column));

	boundValue["columns"] = columns;

	setBoundValue(boundValue, !_control->form()->wasUpgraded());

	return QString();

}
