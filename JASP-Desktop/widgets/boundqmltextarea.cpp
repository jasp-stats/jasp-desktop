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
#include "../analysis/analysisform.h"
#include "../analysis/jaspcontrolbase.h"
#include "qmllistviewtermsavailable.h"
#include "r_functionwhitelist.h"
#include <QQmlProperty>
#include <QFontDatabase>

#include <QString>
#include <QRegularExpression>
#include <QList>
#include <QSet>

#include "log.h"

BoundQMLTextArea::BoundQMLTextArea(JASPControlBase* item)
	: JASPControlWrapper(item)
	, QMLListView(item)
	, BoundQMLItem()
{
	QString textType = getItemProperty("textType").toString();

	if (textType == "model")
	{
		_textType = TextType::Model;

		int id = QFontDatabase::addApplicationFont(":/fonts/FiraCode-Retina.ttf");
		QString family = QFontDatabase::applicationFontFamilies(id).at(0);

		QFont font(family);
		font.setStyleHint(QFont::Monospace);
		font.setPointSize(10);
		setItemProperty("font", font);
	}
	else if (textType == "Rcode")
	{
		_textType = TextType::Rcode;

		int id = QFontDatabase::addApplicationFont(":/fonts/FiraCode-Retina.ttf");
		QString family = QFontDatabase::applicationFontFamilies(id).at(0);

		QFont font(family);
		font.setStyleHint(QFont::Monospace);
		font.setPointSize(10);
		setItemProperty("font", font);
				
	}
	else
		_textType = TextType::Default;

	QQuickItem::connect(item, SIGNAL(applyRequest()), this, SLOT(checkSyntax()));
	
}

void BoundQMLTextArea::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionString *>(option);

	if (_boundTo != nullptr)
	{
		_text = QString::fromStdString(_boundTo->value());
		setItemProperty("text", _text);
	}
	else
		Log::log()  << "could not bind to OptionBoolean in BoundQuickCheckBox.cpp" << std::endl;
}

Option *BoundQMLTextArea::createOption()
{
	std::string text = getItemProperty("text").toString().toStdString();
	return new OptionString(text);
}

bool BoundQMLTextArea::isOptionValid(Option *option)
{
	return dynamic_cast<OptionString*>(option) != nullptr;
}

bool BoundQMLTextArea::isJsonValid(const Json::Value &optionValue)
{
	return optionValue.type() == Json::stringValue;
}

void BoundQMLTextArea::resetQMLItem(JASPControlBase *item)
{
	BoundQMLItem::resetQMLItem(item);
	setItemProperty("text", _text);

	if (_item)
		QQuickItem::connect(item, SIGNAL(applyRequest()), this, SLOT(checkSyntax()));
}

void BoundQMLTextArea::checkSyntax()
{
	_text = getItemProperty("text").toString();

	if (_textType == TextType::Rcode)
	{
		try							
		{ 
			R_FunctionWhiteList::scriptIsSafe(_text.toStdString()); 
			setItemProperty("hasScriptError", false);
			setItemProperty("infoText", tr("valid R code"));
			if (_boundTo != nullptr)
				_boundTo->setValue(_text.toStdString());			
		}
		catch(filterException & e)
		{
			setItemProperty("hasScriptError", true);
			std::string errorMessage(e.what());
			setItemProperty("infoText", errorMessage.c_str());
		}		
		
	}
	else
	{
		if (_boundTo != nullptr)
			_boundTo->setValue(_text.toStdString());
	}

}

void BoundQMLTextArea::dataSetChangedHandler()
{
	form()->refreshAnalysis();
}

void BoundQMLTextArea::rScriptDoneHandler(const QString & result)
{
	//This is for the lavaan model, but can be used by other type
	if (result.length() == 0)
	{
		setItemProperty("hasScriptError", false);
		setItemProperty("infoText", tr("Model applied"));
		if (_boundTo != nullptr)
			_boundTo->setValue(_text.toStdString());
		
	}
	else
	{
		setItemProperty("hasScriptError", true);
		setItemProperty("infoText", result);
	}
}
