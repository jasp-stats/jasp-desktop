//
// Copyright (C) 2013-2017 University of Amsterdam
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

#ifndef ASSIGNBUTTON_H
#define ASSIGNBUTTON_H

#include "listview.h"
#include "button.h"

class AssignButton : public Button
{
	Q_OBJECT
public:
	explicit AssignButton(QWidget *parent = 0);

	void setAssignDirection(bool assign);
	bool isAssign();

	void setSourceAndTarget(DropTarget *source, DropTarget *target);

protected:

	DropTarget *_source;
	DropTarget *_target;

private:
	bool _assignDirection;

private slots:
	void buttonClicked();
	void sourceChanged();
	void targetChanged();

};

#endif // ASSIGNBUTTON_H
