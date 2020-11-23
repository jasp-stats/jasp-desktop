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

#ifndef COMBOBOXBASE_H
#define COMBOBOXBASE_H

#include "jasplistcontrol.h"
#include "analysis/options/boundcontrol.h"
#include "analysis/options/optionlist.h"
#include "listmodel.h"
#include "listmodellabelvalueterms.h"
#include <QMap>

class ComboBoxBase : public JASPListControl, public BoundControl
{
	Q_OBJECT

	Q_PROPERTY( QString		currentText			READ currentText		WRITE setCurrentText			NOTIFY currentTextChanged			)
	Q_PROPERTY( QString		currentValue		READ currentValue		WRITE setCurrentValue			NOTIFY currentValueChanged			)
	Q_PROPERTY( QString		startValue			READ startValue			WRITE setStartValue				NOTIFY startValueChanged			)
	Q_PROPERTY( QString		currentColumnType	READ currentColumnType	WRITE setCurrentColumnType		NOTIFY currentColumnTypeChanged		)
	Q_PROPERTY( QString		textRole			READ textRole			WRITE setTextRole				NOTIFY textRoleChanged				)
	Q_PROPERTY( QString		valueRole			READ valueRole			WRITE setValueRole				NOTIFY valueRoleChanged				)
	Q_PROPERTY( int			indexDefaultValue	READ indexDefaultValue	WRITE setIndexDefaultValue		NOTIFY indexDefaultValueChanged		)

public:
	ComboBoxBase(QQuickItem* parent = nullptr);

	void				bindTo(Option *option)						override;
	Option*				createOption()								override;
	bool				isOptionValid(Option* option)				override;
	bool				isJsonValid(const Json::Value& optionValue) override;
	Option*				boundTo()									override	{ return _boundTo;				}
	void				setUp()										override;
	ListModel*			model()								const	override	{ return _model;				}
	void				setUpModel()								override;

	void				setLabelValues();

	const QString&		currentText()						const				{ return _currentText;			}
	const QString&		currentValue()						const				{ return _currentValue;			}
	const QString&		startValue()						const				{ return _startValue;			}
	const QString&		currentColumnType()					const				{ return _currentColumnType;	}
	const QString&		textRole()							const				{ return _textRole;				}
	const QString&		valueRole()							const				{ return _valueRole;			}
	int					indexDefaultValue()					const				{ return _indexDefaultValue;	}

signals:
	void currentTextChanged();
	void currentValueChanged();
	void startValueChanged();
	void currentColumnTypeChanged();
	void indexDefaultValueChanged();
	void textRoleChanged();
	void valueRoleChanged();

protected slots:
	void termsChangedHandler() override;
	void comboBoxChangeValueSlot(int index);
	void languageChangedHandler();

	GENERIC_SET_FUNCTION(CurrentText,		_currentText,		currentTextChanged,			QString	)
	GENERIC_SET_FUNCTION(CurrentValue,		_currentValue,		currentValueChanged,		QString	)
	GENERIC_SET_FUNCTION(StartValue,		_startValue,		startValueChanged,			QString	)
	GENERIC_SET_FUNCTION(CurrentColumnType,	_currentColumnType, currentColumnTypeChanged,	QString	)
	GENERIC_SET_FUNCTION(IndexDefaultValue,	_indexDefaultValue,	indexDefaultValueChanged,	int		)
	GENERIC_SET_FUNCTION(TextRole,			_textRole,			textRoleChanged,			QString	)
	GENERIC_SET_FUNCTION(ValueRole,			_valueRole,			valueRoleChanged,			QString	)

protected:
	OptionList*					_boundTo				= nullptr;
	ListModelLabelValueTerms*	_model					= nullptr;
	QString						_currentText,
								_currentValue,
								_startValue,
								_currentColumnType,
								_textRole				= "label",
								_valueRole				= "value";
	int							_indexDefaultValue		= 0;

	void _resetItemWidth();
	void _resetOptions();
	void _setCurrentValue(int index, bool setOption = true);


};

#endif // COMBOBOXBASE_H
