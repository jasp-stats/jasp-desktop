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

#ifndef BOUNDQMLCHECKBOX_H
#define BOUNDQMLCHECKBOX_H

#include "analysis/boundqmlitem.h"
#include "analysis/options/optionboolean.h"
#include <QObject>

class BoundQMLCheckBox : public QObject, public BoundQMLItem
{
	Q_OBJECT
	
public:
	BoundQMLCheckBox(QQuickItem* item, AnalysisQMLForm* form);
	virtual void bindTo(Option *option) OVERRIDE;
	virtual void unbind() OVERRIDE;
	
	virtual Option* createOption() OVERRIDE;
	virtual Option* boundTo() OVERRIDE { return _boundTo; }
	virtual void resetQMLItem(QQuickItem *item) OVERRIDE;

signals:
	
private slots:
	void checkBoxClickedSlot();
    
protected:
	OptionBoolean *_boundTo;
	bool _checked;

};

#endif // BOUNDQMLCHECKBOX_H
