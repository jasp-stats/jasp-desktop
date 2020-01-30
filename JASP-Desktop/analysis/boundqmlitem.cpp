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

#include "boundqmlitem.h"
#include "analysisform.h"

void BoundQMLItem::runRScript(const QString &script, bool whiteListedVersion)
{
	QString id = parentListControl() ? (parentListControl()->name() + "." + parentListControlKey() + "." + name()) : name();

	form()->runRScript(script, id, whiteListedVersion);
}

void BoundQMLItem::rScriptDoneHandler(const QString &)
{
	throw std::runtime_error("runRScript done but handler not implemented!\nImplement an override for RScriptDoneHandler\n");
}
