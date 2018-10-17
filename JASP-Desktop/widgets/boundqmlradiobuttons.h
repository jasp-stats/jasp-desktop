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
#include "analysis/options/optionstring.h"
#include <QObject>

class BoundQMLRadioButtons : public QObject, public BoundQMLItem
{
	Q_OBJECT
	
public:
	BoundQMLRadioButtons(QQuickItem* item, AnalysisQMLForm* form);
	virtual void bindTo(Option *option) OVERRIDE;
	virtual void unbind() OVERRIDE;
	virtual Option* boundTo() OVERRIDE { return _boundTo; }	
	
	virtual Option* createOption() OVERRIDE;

signals:
	
private slots:
	void radioButtonClickedHandler(const QVariant& button);
    
protected:
	OptionString *_boundTo;
	std::map<std::string, QQuickItem *> _buttons;
	QQuickItem *_checkedButton;
	
	void _getRadioButtons(QQuickItem* item, QList<QQuickItem* >& buttons);
};

#endif // BOUNDQMLRADIOBUTTONS_H
