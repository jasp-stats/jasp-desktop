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

#ifndef BOUNDQMLRADIOBUTTONS_H
#define BOUNDQMLRADIOBUTTONS_H

#include "analysis/boundqmlitem.h"
#include "analysis/options/optionlist.h"
#include <QObject>
#include <QMap>

class BoundQMLRadioButtons : public QObject, public BoundQMLItem
{
	Q_OBJECT
	
public:
	BoundQMLRadioButtons(JASPControl* item);
	void	bindTo(Option *option)						override;
	void	unbind()									override;
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
	QMap<QString, JASPControlWrapper *>	_buttons;
	JASPControlWrapper*					_checkedButton	= nullptr;
	
	void _getRadioButtons(QQuickItem* item, QList<JASPControlWrapper* >& buttons);
};

#endif // BOUNDQMLRADIOBUTTONS_H
