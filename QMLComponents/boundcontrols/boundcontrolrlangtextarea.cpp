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

#include "boundcontrolrlangtextarea.h"
#include "controls/textareabase.h"
#include "log.h"
#include "columnencoder.h"
#include "analysisform.h"
#include "variableinfo.h"
#include "filter.h"
#include <QQuickTextDocument>

ColumnEncoder * BoundControlRlangTextArea::_encoder() const
{
	//Desktop-only: never use the process-global ColumnEncoder (that is only meaningful inside the engine's
	//request context). Resolve the encoder for the data this control's form is bound to instead.
	if (VariableInfo * vi = _textArea->form()->varInfo())
		if (VariableInfoProvider * provider = vi->provider())
			if (ColumnEncoder * encoder = provider->columnEncoder())
				return encoder;

	return ColumnEncoder::fallbackEncoder();
}

BoundControlRlangTextArea::BoundControlRlangTextArea(TextAreaBase *textArea, RLangType type)
	: BoundControlTextArea(textArea), _langType(type)
{
	QVariant textDocumentVariant = textArea->property("textDocument");
	QQuickTextDocument* textDocumentQQuick = textDocumentVariant.value<QQuickTextDocument *>();
	if (textDocumentQQuick)
	{
		QTextDocument* doc = textDocumentQQuick->textDocument();
        _rLangHighlighter = new RSyntaxHighlighter(doc, textArea->form()->varInfo());
		
		//connect(doc, &QTextDocument::contentsChanged, this, &BoundQMLTextArea::contentsChangedHandler);
	}
	else
		Log::log()  << "No document object found!" << std::endl;
}

void BoundControlRlangTextArea::bindTo(const Json::Value &value)
{
	_previouslyUsedTextEncoded	= "";
	
	if (value.type() != Json::objectValue)	return;
	BoundControlBase::bindTo(value);

	_textArea->setText(tq(value["modelOriginal"].asString()));

	checkSyntax();

}

void BoundControlRlangTextArea::resetBoundValue()
{
	_setBoundValues(false);
}

Json::Value BoundControlRlangTextArea::createJson() const
{
	Json::Value result;
	std::string text = _textArea->text().toStdString();

	result["modelOriginal"] = text;
	result["model"]			= text;
	result["columns"]		= Json::Value(Json::arrayValue);

	return result;
}

bool BoundControlRlangTextArea::isJsonValid(const Json::Value &value) const
{
	if (!value.isObject())					return false;
	if (!value["modelOriginal"].isString())	return false;

	return true;
}

void BoundControlRlangTextArea::checkSyntax()
{
	QString text = _textArea->text();

	// get the column names of the data set
	_prefixedUsedColumnNames.clear();
	_textEncoded = tq(_encoder()->encodeRScript(stringUtils::stripRComments(fq(text)), _prefixedUsedColumnNames, _allowedVarPrefixes));

	if(_prefixedUsedColumnNames.find("") != _prefixedUsedColumnNames.end()) 
	{
		_noPrefixUsedColumnNames = stringset(_prefixedUsedColumnNames[""]);
		_prefixedUsedColumnNames.erase(_prefixedUsedColumnNames.find("")); //just for clarity remove the noPrefix "" items
	}
	else
		Log::log() << "Warning: no non prefixed entries returned from column encoder?";


	if (!_textArea->initialized() || _langType == RLangType::RCode)
	{
		// Do not run the engine to check the script if the control in not yet initialized
		_setBoundValues();
		return;
	}

	// Create R code string
	QString encodedColNames = "c(";
	bool firstCol = true;

	for (const std::string& column : _noPrefixUsedColumnNames)
	{
		if (!firstCol) encodedColNames.append(", ");
		encodedColNames.append("'" + tq(_encoder()->encode(column)) + "'");
		firstCol = false;
	}

	for(auto& prefixSet : _prefixedUsedColumnNames)
		for (const std::string& column : prefixSet.second)
		{
			if (!firstCol) encodedColNames.append(", ");
			encodedColNames.append("'" + tq(prefixSet.first) + tq(_encoder()->encode(column)) + "'");
			firstCol = false;
		}

	if (_langType == RLangType::MetaSem)
	{
		stringset sourceVariables;
		Terms sourceColumns = _textArea->model()->getSourceTerms();
		QString separator = _textArea->variableSeparator();

		for (const Term& term : sourceColumns)
		{
			QStringList variables = term.label().split(separator);
			for (const QString& variable : variables)
				sourceVariables.insert(fq(variable));
		}

		for (const std::string& variable : sourceVariables)
		{
			if (!firstCol) encodedColNames.append(", ");
			encodedColNames.append("'" + variable + "'");
			firstCol = false;
		}
	}

	encodedColNames.append(")");

	if(_textEncoded.length() > 0) {
		QString checkCode = QString("%1('%2', %3)")
			.arg(tq(_checkSyntaxRFunctionName()))
			.arg(_textEncoded)
			.arg(encodedColNames);
		
		if(_previouslyUsedTextEncoded != checkCode)
			_textArea->runRScript(checkCode, false);
		
		_previouslyUsedTextEncoded = checkCode;
	}

}

QString BoundControlRlangTextArea::rScriptDoneHandler(const QString & result)
{
	if (!result.isEmpty())
		return result;

	_setBoundValues();
	return QString();
}

void BoundControlRlangTextArea::_setBoundValues(bool setModel)
{
	Json::Value boundValue(Json::objectValue);

	std::string text = _textArea->text().toStdString();

	boundValue["modelOriginal"] = text;
	boundValue["model"]			= _textEncoded.toStdString();

	Json::Value columns(Json::arrayValue),
				value(Json::arrayValue);
	Terms		terms;

	for (const std::string& column : _noPrefixUsedColumnNames)
	{
		terms.add(Term(column, _textArea->getVariableType(tq(column))));
		columns.append(_encoder()->encode(column));
		value.append(column);
	}

	if (setModel && _textArea->model())
		_textArea->model()->initTerms(terms);
	boundValue["columns"]	= columns;
	boundValue["value"]		= value;
	boundValue["types"]		= terms.types();
	boundValue["optionKey"] = "value";

	Json::Value prefixedColumns(Json::objectValue);
	for(auto& prefixSet : _prefixedUsedColumnNames) {
		prefixedColumns[prefixSet.first] = Json::Value(Json::arrayValue);
		for (const std::string& column : prefixSet.second) {
			prefixedColumns[prefixSet.first].append(_encoder()->encode(column));
		}
	}
	boundValue["prefixedColumns"] = prefixedColumns;

	setBoundValue(boundValue, !_control->form()->wasUpgraded());
}

const char* BoundControlRlangTextArea::_checkSyntaxRFunctionName()
{
	switch (_langType)
	{
	case RLangType::CSem:		return "jaspSem:::checkCSemModel";
	case RLangType::MetaSem:	return "jaspMetaAnalysis::checkMetaModel";
	case RLangType::Lavaan:		return "jaspSem:::checkLavaanModel";
	default:					return "";
	}
}
