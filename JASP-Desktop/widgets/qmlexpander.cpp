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

#include "qmlexpander.h"
#include "../analysis/analysisform.h"
#include "../analysis/jaspcontrolbase.h"

QMLExpander::QMLExpander(JASPControlBase *item)
	: JASPControlWrapper(item)
	, QObject(item)
{
}

JASPControlBase* QMLExpander::_findFirstControl(QObject* obj)
{
	JASPControlBase* result = nullptr;

	for (QObject* child : obj->children())
	{
		result = qobject_cast<JASPControlBase*>(child);

		if (!result)
			result = _findFirstControl(child);

		if (result)
			break;
	}

	return result;
}

void QMLExpander::setUp()
{
	if (!form())
		return;

	QMLExpander* nextExpander = form()->nextExpander(this);

	if (nextExpander)
		setItemProperty("nextExpander", QVariant::fromValue(nextExpander->item()));

	QQuickItem* childControlsArea = item()->childControlsArea();

	if (childControlsArea)
	{
		JASPControlBase* firstControl = _findFirstControl(childControlsArea);
		if (firstControl)
			setItemProperty("firstControl", QVariant::fromValue(firstControl));
	}
}
