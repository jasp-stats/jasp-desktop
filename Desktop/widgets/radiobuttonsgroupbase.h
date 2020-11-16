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

#include "analysis/jaspcontrol.h"
#include "analysis/options/optionlist.h"
#include "analysis/options/boundcontrol.h"
#include <QObject>
#include <QMap>

class RadioButtonBase;

class RadioButtonsGroupBase : public JASPControl, public BoundControl
{
	Q_OBJECT
	
public:
	RadioButtonsGroupBase(QQuickItem* parent = nullptr);
	void	bindTo(Option *option)						override;
	Option* boundTo()									override { return _boundTo; }	
	Option* createOption()								override;
	bool	isOptionValid(Option* option)				override;
	bool	isJsonValid(const Json::Value& optionValue) override;
	void	setUp()										override;

signals:
	
private slots:
	void radioButtonClickedHandler(const QVariant& button);
    
protected:
	OptionList*							_boundTo		= nullptr;
	QMap<QString, RadioButtonBase *>	_buttons;
	RadioButtonBase*					_checkedButton	= nullptr;
	
	void _getRadioButtons(QQuickItem* item, QList<RadioButtonBase* >& buttons);
};

#endif // RADIOBUTTONSGROUPBASE_H
