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

#ifndef RADIOBUTTONSGROUPBASE_H
#define RADIOBUTTONSGROUPBASE_H

#include "jaspcontrol.h"
#include "boundcontrolbase.h"
#include <QMap>

class RadioButtonBase;

class RadioButtonsGroupBase : public JASPControl, public BoundControlBase
{
	Q_OBJECT
	
	Q_PROPERTY( QString		value	READ value	WRITE setValue	NOTIFY valueChanged	)

public:
	RadioButtonsGroupBase(QQuickItem* parent = nullptr);

	bool		isJsonValid(const Json::Value& value)		override;
	Json::Value createJson()								override;
	void		bindTo(const Json::Value& value)			override;
	void		setUp()										override;
    
	const QString&	value()										const	{ return _value; }

signals:
	void valueChanged();
	void clicked(const QVariant& button);

protected slots:
	void clickedSlot(const QVariant& button);

protected:
	GENERIC_SET_FUNCTION(Value,		_value,		valueChanged,	QString	)

	void _setCheckedButton(RadioButtonBase* button);
	void _getRadioButtons(QQuickItem* item, QList<RadioButtonBase* >& buttons);

	QMap<QString, RadioButtonBase *>	_buttons;
	QString								_value;
	
};

#endif // RADIOBUTTONSGROUPBASE_H
