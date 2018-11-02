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

#include "boundqmltextarea.h"
#include "analysis/analysisqmlform.h"
#include "qmllistviewtermsavailable.h"
#include <QQmlProperty>
#include <QQuickItem>
#include <QQuickTextDocument>
#include <QFontDatabase>
#include <QDebug>

BoundQMLTextArea::BoundQMLTextArea(QQuickItem* item, AnalysisQMLForm* form) 
	: QMLItem(item, form)
	, QObject(form)
	, BoundQMLItem(item, form)
{
	_boundTo = NULL;
	_lavaanHighlighter = NULL;
	QString textType = _item->property("textType").toString();
	if (textType == "lavaan")
	{
		_textType = TextType::Lavaan;
		QMLListViewTermsAvailable* listView = new QMLListViewTermsAvailable(item, form); // Hack to get allVariablesModel
		_allVariablesModel = dynamic_cast<ListModelTermsAvailable*>(listView->model());
#ifdef __APPLE__
		_applyScriptInfo = "\u2318 + Enter to apply";
#else
		_applyScriptInfo = "Ctrl + Enter to apply";
#endif
		_item->setProperty("applyScriptInfo", _applyScriptInfo);
		
		int id = QFontDatabase::addApplicationFont(":/fonts/FiraCode-Retina.ttf");
		QString family = QFontDatabase::applicationFontFamilies(id).at(0);
		
		QFont font(family);
		font.setStyleHint(QFont::Monospace);
		font.setPointSize(10);
		_item->setProperty("font", font);
		
				
		QVariant textDocumentVariant = _item->property("textDocument");
		QQuickTextDocument* textDocumentQQuick = textDocumentVariant.value<QQuickTextDocument *>();
		if (textDocumentQQuick)
		{
			QTextDocument* doc = textDocumentQQuick->textDocument();
			_lavaanHighlighter = new TextModelLavaan::SyntaxHighlighter(doc);
			//connect(doc, &QTextDocument::contentsChanged, this, &BoundQMLTextArea::contentsChangedHandler);
			
		}
		else
			qDebug() << "No document object found!";
	
	}
	else
		_textType = TextType::Default;
	
	QQuickItem::connect(item, SIGNAL(applyRequest()), this, SLOT(checkSyntax()));
	
}

void BoundQMLTextArea::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionString *>(option);

	if (_boundTo != NULL)
	{
		_text = QString::fromStdString(_boundTo->value());
		_item->setProperty("text", _text);
	}
	else
		qDebug() << "could not bind to OptionBoolean in BoundQuickCheckBox.cpp";
}

Option *BoundQMLTextArea::createOption()
{
	std::string text = _item->property("text").toString().toStdString();
	return new OptionString(text);
}

void BoundQMLTextArea::resetQMLItem(QQuickItem *item)
{
	BoundQMLItem::resetQMLItem(item);
	_item->setProperty("text", _text);
	QQuickItem::connect(item, SIGNAL(applyRequest()), this, SLOT(checkSyntax()));	
}

void BoundQMLTextArea::checkSyntax()
{
	_text = _item->property("text").toString();
	if (_textType == TextType::Lavaan)
	{
		// create an R vector of available column names
		// TODO: Proper handling of end-of-string characters and funny colnames
		QString colNames = "c(";
		bool firstCol = true;
		QList<QString> vars = _allVariablesModel->allTerms().asQList();
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
		
		runRScript(checkCode);
	}
	else
	{
		if (_boundTo != NULL)
			_boundTo->setValue(_text.toStdString());
	}
		
}

void BoundQMLTextArea::rScriptDoneHandler(const QString & result)
{
    if (result.length() == 0) {
		_item->setProperty("hasScriptError", false);
		_item->setProperty("infoText", "Model applied");
		if (_boundTo != NULL)
			_boundTo->setValue(_text.toStdString());
		
	} else {
		_item->setProperty("hasScriptError", true);
		_item->setProperty("infoText", result);
	}
}


