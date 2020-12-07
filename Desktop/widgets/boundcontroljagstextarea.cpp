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
#include "analysis/options/optionvariables.h"
#include "analysis/options/optionstring.h"
#include "textareabase.h"
#include "columnencoder.h"


void BoundControlJAGSTextArea::bindTo(Option *option)
{
	_options = dynamic_cast<Options *>(option);
	if (_options != nullptr)
	{
		OptionString* modelOption = dynamic_cast<OptionString*>(_options->get("modelOriginal"));
		if (modelOption)
			_textArea->setText(QString::fromStdString(modelOption->value()));

		OptionString* modelEncodedOption = dynamic_cast<OptionString*>(_options->get("model"));
		if (modelEncodedOption)
			_textEncoded = QString::fromStdString(modelEncodedOption->value());

		OptionVariables* columnsOption = dynamic_cast<OptionVariables*>(_options->get("columns"));
		if (columnsOption)
		{
			std::vector<std::string> variables = columnsOption->variables();
			for (const std::string& variable : variables)
				_usedColumnNames.insert(variable);
		}
		OptionVariables* parametersOption = dynamic_cast<OptionVariables*>(_options->get("parameters"));
		if (parametersOption)
		{
			std::vector<std::string> variables = parametersOption->variables();
			for (const std::string& variable : variables)
				_usedParameters.insert(QString::fromStdString(variable));
		}

		checkSyntax();
	}
}

Option *BoundControlJAGSTextArea::createOption()
{
	Options* result = new Options();
	std::string text = _textArea->text().toStdString();

	result->add("modelOriginal",	new OptionString(text));
	result->add("model",			new OptionString(text));
	result->add("columns",			new OptionVariables());
	result->add("parameters",		new OptionVariables());

	return result;
}

bool BoundControlJAGSTextArea::isOptionValid(Option *option)
{
	return dynamic_cast<Options*>(option) != nullptr;
}

bool BoundControlJAGSTextArea::isJsonValid(const Json::Value &optionValue)
{
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

	if (_options != nullptr)
	{
		OptionString* modelOption = dynamic_cast<OptionString*>(_options->get("modelOriginal"));
		if (!modelOption)
		{
			modelOption = new OptionString();
			_options->add("modelOriginal", modelOption);
		}
		modelOption->setValue(text.toStdString());

		OptionString* modelEncodedOption = dynamic_cast<OptionString*>(_options->get("model"));
		if (!modelEncodedOption)
		{
			modelEncodedOption = new OptionString();
			_options->add("model", modelEncodedOption);
		}
		modelEncodedOption->setValue(_textEncoded.toStdString());
		OptionVariables* columns = dynamic_cast<OptionVariables*>(_options->get("columns"));
		if (!columns)
		{
			columns = new OptionVariables();
			_options->add("columns", columns);
		}
		std::vector<std::string> columnsVec;
		for (const std::string& col : _usedColumnNames)
			columnsVec.push_back(col);
		columns->setValue(columnsVec);

		OptionVariables* parameters = dynamic_cast<OptionVariables*>(_options->get("parameters"));
		if (!parameters)
		{
			parameters = new OptionVariables();
			_options->add("parameters", parameters);
		}
		std::vector<std::string> parametersVec;
		for (const QString& param : _usedParameters)
			parametersVec.push_back(param.toStdString());
		parameters->setValue(parametersVec);
	}

	ListModelTermsAvailable* model = _textArea->availableModel();
	model->initTerms(_usedParameters.toList());
}


