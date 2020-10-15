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

#include "boundqmllavaantextarea.h"
#include "analysis/analysisform.h"
#include "analysis/jaspcontrolbase.h"
#include "gui/preferencesmodel.h"
#include "log.h"

#include <QQuickTextDocument>


BoundQMLLavaanTextArea::BoundQMLLavaanTextArea(JASPControlBase* item)
	: JASPControlWrapper(item)
	, BoundQMLTextArea(item)
{
	connect(form(), &AnalysisForm::dataSetChanged,	this, &BoundQMLTextArea::dataSetChangedHandler,	Qt::QueuedConnection	);

	_model = new ListModelTermsAvailable(this);
	_modelHasAllVariables = true;

	QVariant textDocumentVariant = _item->property("textDocument");
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

void BoundQMLLavaanTextArea::checkSyntax()
{
	_text = getItemProperty("text").toString();

	// create an R vector of available column names
	// TODO: Proper handling of end-of-string characters and funny colnames
	QString colNames = "c(";
	bool firstCol = true;
	if (_model)
	{
		QList<QString> vars = _model->allTerms().asQList();
		for (QString &var : vars)
		{
			if (!firstCol)
				colNames.append(',');
			colNames.append('\'')
					.append(var.replace("\'", "\\u0027")
							   .replace("\"", "\\u0022")
							   .replace("\\", "\\\\"))
					.append('\'');
			firstCol = false;
		}
	}
	colNames.append(')');

	// replace ' and " with their unicode counterparts
	// This protects against arbitrary code being run through string escaping.
	_text.replace("\'", "\\u0027").replace("\"", "\\u0022");
	// This protects against crashes due to backslashes
	_text.replace("\\", "\\\\");

	// Create R code string
	QString checkCode = "checkLavaanModel('";
	checkCode
		.append(_text)
		.append("', ")
		.append(colNames)
		.append(")");

	runRScript(checkCode, false);
}
