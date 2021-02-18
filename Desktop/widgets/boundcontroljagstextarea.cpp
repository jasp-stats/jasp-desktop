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

#include "boundcontroljagstextarea.h"
#include "textareabase.h"
#include "columnencoder.h"

void BoundControlJAGSTextArea::bindTo(const Json::Value &value)
{
	if (value.type() != Json::objectValue)	return;
	BoundControlBase::bindTo(value);

	_textArea->setText(tq(value["modelOriginal"].asString()));

	checkSyntax();

}

Json::Value BoundControlJAGSTextArea::createJson()
{
	Json::Value result;
	std::string text = _textArea->text().toStdString();

	result["modelOriginal"] = text;
	result["model"]			= text;
	result["columns"]		= Json::Value(Json::arrayValue);
	result["parameters"]	= Json::Value(Json::arrayValue);

	return result;
}

bool BoundControlJAGSTextArea::isJsonValid(const Json::Value &value)
{
	if (!value.isObject())					return false;
	if (!value["modelOriginal"].isString())	return false;
	if (!value["model"].isString())			return false;
	if (!value["columns"].isArray())		return false;
	if (!value["parameters"].isArray())		return false;

	return true;
}

void BoundControlJAGSTextArea::checkSyntax()
{
	QString text = _textArea->text();

	// google: jags_user_manual (4.3.0) for documentation on JAGS symbols

	// get the column names of the data set
	_usedColumnNames.clear();
	_textEncoded = tq(ColumnEncoder::columnEncoder()->encodeRScript(stringUtils::stripRComments(fq(text)), &_usedColumnNames));

	QRegularExpression relationSymbol = QRegularExpression("<-|=|~");
	QStringList textByLine = _textEncoded.split(QRegularExpression(";|\n"));
	_usedParameters.clear();

	for (QString & line : textByLine)
	{
		// comments were already removed by stringUtils::stripRComments
		if (line.contains(relationSymbol))
		{
			// extract parameter and remove whitespace
			QString paramName = line.split(relationSymbol).first().trimmed();
			// remove any link functions (cloglog|log|probit|logit)
			if (paramName.contains("(") && paramName.contains(")"))
			{
				int idxStart, idxEnd;
				idxStart = paramName.indexOf("(") + 1;
				idxEnd   = paramName.indexOf(")") - idxStart;
				paramName = paramName.midRef(idxStart, idxEnd).toString();
			}

			// get rid of any indexing
			if (paramName.contains("["))
				paramName = paramName.leftRef(paramName.indexOf("[")).toString();

			if (paramName != "" && !ColumnEncoder::columnEncoder()->shouldDecode(fq(paramName)))
				_usedParameters.insert(paramName);

		}
	}

	Json::Value boundValue(Json::objectValue);

	boundValue["modelOriginal"] = text.toStdString();
	boundValue["model"] = _textEncoded.toStdString();
	Json::Value columns(Json::arrayValue);
	for (const std::string& column : _usedColumnNames)
		columns.append(column);
	boundValue["columns"] = columns;
	Json::Value parameters(Json::arrayValue);
	for (const QString& parameter : _usedParameters)
		parameters.append(parameter.toStdString());
	boundValue["parameters"] = parameters;

	setBoundValue(boundValue);

	ListModelTermsAvailable* model = _textArea->availableModel();
	model->initTerms(_usedParameters.values());
}


