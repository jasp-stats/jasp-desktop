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
#include "analysis/boundcontrolbase.h"
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

	Q_PROPERTY( TextType	textType			READ textType				WRITE setTextType			NOTIFY textTypeChanged				)
	Q_PROPERTY( bool		hasScriptError		READ hasScriptError			WRITE setHasScriptError		NOTIFY hasScriptErrorChanged		)

public:
	TextAreaBase(QQuickItem* parent = nullptr);

	void						bindTo(const Json::Value &value)			override	{ _boundControl->bindTo(value);						}
	bool						isJsonValid(const Json::Value& optionValue) override	{ return _boundControl->isJsonValid(optionValue);	}
	void						resetBoundValue()							override	{ return _boundControl->resetBoundValue();				}
	const Json::Value&			boundValue()								override	{ return _boundControl->boundValue();				}
	Json::Value					createJson()								override	{ return _boundControl->createJson();				}
	Json::Value					createMeta()								override	{ return _boundControl->createMeta();				}
	void						setBoundValue(const Json::Value& value, 
											  bool emitChange = true)		override	{ return _boundControl->setBoundValue(value, emitChange);	}

	ListModel*					model()								const	override	{ return _model; }
	ListModelTermsAvailable*	availableModel()					const				{ return _model; }
	void						setUp()										override;
	void						setUpModel()								override;

	void						rScriptDoneHandler(const QString &result)	override;

	TextType					textType()							const	{ return _textType;				}
	bool						hasScriptError()					const	{ return _hasScriptError;		}
	const QList<QString>&		separators()						const	{ return _separators;			}
	QString						text();
	void						setText(const QString& text);

public slots:
	GENERIC_SET_FUNCTION(TextType,			_textType,			textTypeChanged,		TextType	)
	GENERIC_SET_FUNCTION(HasScriptError,	_hasScriptError,	hasScriptErrorChanged,	bool		)

	void	checkSyntaxHandler()						{ _boundControl->checkSyntax(); }

signals:
	void	textTypeChanged();
	void	hasScriptErrorChanged();

protected slots:
	void	termsChangedHandler()		override;
    
protected:

	BoundControlTextArea*		_boundControl			= nullptr;
	TextType					_textType				= TextType::TextTypeDefault;
	bool						_hasScriptError			= false;
	QList<QString>				_separators;
	
	ListModelTermsAvailable*	_model					= nullptr;
	
};


#endif // TEXTAREABASE_H
