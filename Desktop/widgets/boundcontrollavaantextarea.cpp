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
#include "textareabase.h"
#include "log.h"

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

void BoundControlLavaanTextArea::checkSyntax()
{
	QString text = _textArea->text();

	// create an R vector of available column names
	// TODO: Proper handling of end-of-string characters and funny colnames
	QString colNames = "c(";
	bool firstCol = true;

	QList<QString> vars = _textArea->availableModel()->allTerms().asQList();
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

	colNames.append(')');

	// replace ' and " with their unicode counterparts
	// This protects against arbitrary code being run through string escaping.
	text.replace("\'", "\\u0027").replace("\"", "\\u0022");
	// This protects against crashes due to backslashes
	text.replace("\\", "\\\\");

	// Create R code string
	QString checkCode = "checkLavaanModel('";
	checkCode
		.append(text)
		.append("', ")
		.append(colNames)
		.append(")");

	_textArea->runRScript(checkCode, false);
}
