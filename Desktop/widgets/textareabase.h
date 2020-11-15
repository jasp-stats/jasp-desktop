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


#ifndef TEXTAREABASE_H
#define TEXTAREABASE_H

#include "jasplistcontrol.h"
#include "analysis/options/boundcontrol.h"
#include "analysis/options/optionstring.h"
#include "listmodeltermsavailable.h"
#include "boundcontroltextarea.h"

#include <QObject>

#include <QString>
#include <QRegularExpression>
#include <QList>
#include <QDebug>

class TextAreaBase : public JASPListControl, public BoundControl
{
	Q_OBJECT

	Q_PROPERTY( TextType	textType	READ textType	WRITE setTextType						)
	Q_PROPERTY(	QString		text		READ text		WRITE setText		NOTIFY textChanged	)

public:
	TextAreaBase(QQuickItem* parent = nullptr);

	void						bindTo(Option *option)						override	{ _boundControl->bindTo(option);				}
	Option*						createOption()								override	{ return _boundControl->createOption();			}
	bool						isOptionValid(Option* option)				override	{ return _boundControl->isOptionValid(option);	}
	bool						isJsonValid(const Json::Value& optionValue) override	{ return _boundControl->isJsonValid(optionValue); }
	Option*						boundTo()									override	{ return _boundControl->boundTo();				}
	ListModel*					model()								const	override	{ return _model; }
	ListModelTermsAvailable*	availableModel()					const				{ return _model; }
	void						setUpModel()								override;
	bool						modelNeedsAllVariables()			const				{ return _modelNeedsAllVariables; }

	void						rScriptDoneHandler(const QString &result)	override;

	TextType					textType()							const	{ return _textType; }
	QString						text()								const	{ return _text;		}
	const QList<QString>&		separators()				const	{ return _separators; }

public slots:
	void setTextType(TextType textType)				{ _textType = textType; }
	void setText(QString	text);
	void dataSetChangedHandler();
	void checkSyntaxHandler()						{ _boundControl->checkSyntax(); }

signals:
			void textChanged();
    
protected:

	BoundControlTextArea*		_boundControl			= nullptr;
	QString						_text;
	TextType					_textType				= TextType::Default;
	QList<QString>				_separators;
	
	ListModelTermsAvailable*	_model					= nullptr;
	bool						_modelNeedsAllVariables = false;
	
};


#endif // TEXTAREABASE_H
