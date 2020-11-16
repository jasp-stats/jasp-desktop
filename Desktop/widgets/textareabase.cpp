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

#include "textareabase.h"
#include "../analysis/analysisform.h"
#include "../analysis/jaspcontrol.h"
#include "boundcontrolsourcetextarea.h"
#include "boundcontroljagstextarea.h"
#include "boundcontrollavaantextarea.h"

#include <QFontDatabase>
#include <QRegularExpression>
#include <QList>
#include <QSet>

#include "log.h"

TextAreaBase::TextAreaBase(QQuickItem* parent)
	: JASPListControl(parent)
{
	_controlType = ControlType::TextArea;
}


void TextAreaBase::setUpModel()
{
	switch (_textType)
	{
	case TextType::Source:		_boundControl = new BoundControlSourceTextArea(this);	break;
	case TextType::Lavaan:		_boundControl = new BoundControlLavaanTextArea(this);	break;
	case TextType::JAGSmodel:	_boundControl = new BoundControlJAGSTextArea(this);		break;
	default:					_boundControl = new BoundControlTextArea(this);			break;
	}

	QList<QVariant> separators = property("separators").toList();
	if (separators.isEmpty())
		_separators.push_back(property("separator").toString());
	else
	{
		for (QVariant& separator : separators)
			_separators.push_back(separator.toString());
	}

	if (_textType == TextType::Source || _textType == TextType::JAGSmodel || _textType == TextType::Lavaan)
	{
		_model = new ListModelTermsAvailable(this);

		if (_textType == TextType::Lavaan)
		{
			connect(form(), &AnalysisForm::dataSetChanged,	this, &TextAreaBase::dataSetChangedHandler,	Qt::QueuedConnection	);
			_modelNeedsAllVariables = true;
		}
		else
			_model->setTermsAreVariables(false);

		JASPListControl::setUpModel();
	}

	QQuickItem::connect(this, SIGNAL(applyRequest()), this, SLOT(checkSyntaxHandler()));

}


void TextAreaBase::dataSetChangedHandler()
{
	form()->refreshAnalysis();
}



void TextAreaBase::rScriptDoneHandler(const QString & result)
{
	//This is for the lavaan model, but can be used by other type
	if (result.length() == 0)
	{
		setProperty("hasScriptError", false);
		setProperty("infoText", tr("Model applied"));
		OptionString* option = dynamic_cast<OptionString*>(boundTo());
		if (option != nullptr)
			option->setValue(text().toStdString());
	}
	else
	{
		setProperty("hasScriptError", true);
		setProperty("infoText", result);
	}
}

QString TextAreaBase::text()
{
	return property("text").toString();
}

void TextAreaBase::setText(const QString& text)
{
	setProperty("text", text);
}
