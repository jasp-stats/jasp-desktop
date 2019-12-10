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

#include "boundqmljagstextarea.h"
#include "analysis/analysisform.h"


BoundQMLJAGSTextArea::BoundQMLJAGSTextArea(JASPControlBase* item)
	: JASPControlWrapper(item)
	, BoundQMLTextArea(item)
{
	int id = QFontDatabase::addApplicationFont(":/fonts/FiraCode-Retina.ttf");
	QString family = QFontDatabase::applicationFontFamilies(id).at(0);

	QFont font(family);
	font.setStyleHint(QFont::Monospace);
	font.setPointSize(10);
	setItemProperty("font", font);
	_model = new ListModelTermsAvailable(this);
	_model->setTermsAreVariables(false);
}

void BoundQMLJAGSTextArea::bindTo(Option *option)
{
	_options = dynamic_cast<Options *>(option);
	if (_options != nullptr)
	{
		OptionString* modelOption = dynamic_cast<OptionString*>(_options->get("model"));
		if (modelOption)
			_text = QString::fromStdString(modelOption->value());
		OptionVariables* columnsOption = dynamic_cast<OptionVariables*>(_options->get("columns"));
		if (columnsOption)
		{
			std::vector<std::string> variables = columnsOption->variables();
			for (const std::string& variable : variables)
				_usedColumnNames.insert(QString::fromStdString(variable));
		}
	}
}

Option *BoundQMLJAGSTextArea::createOption()
{
	Options* result = new Options();
	std::string text = getItemProperty("text").toString().toStdString();

	result->add("model", new OptionString(text));
	result->add("columns", new OptionVariables());

	return result;
}

bool BoundQMLJAGSTextArea::isOptionValid(Option *option)
{
	return dynamic_cast<Options*>(option) != nullptr;
}

bool BoundQMLJAGSTextArea::isJsonValid(const Json::Value &optionValue)
{
	return true;
}

void BoundQMLJAGSTextArea::checkSyntax()
{
	_text = getItemProperty("text").toString();

	// google: jags_user_manual (4.3.0) for documentation on JAGS symbols

	// get the column names of the data set
	std::vector<std::string> colnms = DataSetPackage::pkg()->getColumnNames();
	std::set<std::string> columnNames(std::make_move_iterator(colnms.begin()), std::make_move_iterator(colnms.end()));


	QRegularExpression relationSymbol = QRegularExpression("<-|=|~");
	QStringList textByLine = _text.split(QRegularExpression(";|\n"));
	QSet<QString> parameterNames;
	_usedColumnNames.clear();

	for (QString & line : textByLine)
	{
		if (!line.startsWith("#") && line.contains(relationSymbol))
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

			if (paramName.contains("["))
				paramName = paramName.leftRef(paramName.indexOf("[")).toString();

			if (paramName != "")
			{
				if (columnNames.find(paramName.toUtf8().constData()) == columnNames.end())
					parameterNames << paramName;
				else
					_usedColumnNames << paramName;
			}
		}
	}

	if (_options != nullptr)
	{
		OptionString* modelOption = dynamic_cast<OptionString*>(_options->get("model"));
		if (!modelOption)
		{
			modelOption = new OptionString();
			_options->add("model", modelOption);
		}
		modelOption->setValue(_text.toStdString());
		OptionVariables* columns = dynamic_cast<OptionVariables*>(_options->get("columns"));
		if (!columns)
		{
			columns = new OptionVariables();
			_options->add("columns", columns);
		}
		std::vector<std::string> columnsVec;
		for (const QString& col : _usedColumnNames)
			columnsVec.push_back(col.toStdString());
		columns->setValue(columnsVec);
	}


	if (_model)
	{
		_model->initTerms(parameterNames.toList());
		emit _model->modelChanged();
	}
}

