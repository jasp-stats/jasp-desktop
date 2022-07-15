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
#include "analysisform.h"
#include "boundcontrols/boundcontrolsourcetextarea.h"
#include "boundcontrols/boundcontroljagstextarea.h"
#include "boundcontrols/boundcontrollavaantextarea.h"
#include "boundcontrols/boundcontrolcsemtextarea.h"


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
	if (_textType == TextType::TextTypeSource || _textType == TextType::TextTypeJAGSmodel || _textType == TextType::TextTypeLavaan || _textType == TextType::TextTypeCSem)
	{
		_model = new ListModelTermsAvailable(this);
		_model->setNeedsSource(_textType == TextType::TextTypeLavaan || _textType == TextType::TextTypeCSem);

		JASPListControl::setUpModel();
	}
}

void TextAreaBase::setUp()
{
	switch (_textType)
	{
	case TextType::TextTypeSource:		_boundControl = new BoundControlSourceTextArea(this);	break;
	case TextType::TextTypeLavaan:		_boundControl = new BoundControlLavaanTextArea(this);	break;
	case TextType::TextTypeJAGSmodel:	_boundControl = new BoundControlJAGSTextArea(this);		break;
	case TextType::TextTypeCSem:		_boundControl = new BoundControlCSemTextArea(this);		break;
	default:							_boundControl = new BoundControlTextArea(this);			break;
	}

	JASPListControl::setUp();

	QList<QVariant> separators = property("separators").toList();
	if (separators.isEmpty())
		_separators.push_back(property("separator").toString());
	else
	{
		for (QVariant& separator : separators)
			_separators.push_back(separator.toString());
	}

	connect(this, SIGNAL(applyRequest()), this, SLOT(checkSyntaxHandler()));
}

void TextAreaBase::rScriptDoneHandler(const QString & result)
{
	QString error = _boundControl->rScriptDoneHandler(result);
	if (error.isEmpty())
	{
		setHasScriptError(false);
		setProperty("infoText", tr("Model applied"));
	}
	else
	{
		setHasScriptError(true);
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

void TextAreaBase::termsChangedHandler()
{
	if (_textType == TextType::TextTypeLavaan || _textType == TextType::TextTypeCSem && form() && initialized())
		form()->refreshAnalysis();

}
