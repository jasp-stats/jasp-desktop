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

#include "expanderbuttonbase.h"
#include "analysisform.h"

ExpanderButtonBase::ExpanderButtonBase(QQuickItem *parent)
	: JASPControl(parent)
{
	_controlType = ControlType::Expander;
}

void ExpanderButtonBase::setUp()
{
	if (!form())
		return;

	setInitialized();
}

QString ExpanderButtonBase::generateMDHelp(int depth) const
{
	if (!hasInfoSomewhere())
		return QString();

	QStringList markdown;

	if (!useCollapsibleSection(depth))
		// For sub-section, draw first a line, and reset the depth to 0.
		markdown <<  "\n---\n\n";

	markdown << JASPControl::generateMDHelp(0);

	if (useCollapsibleSection(depth))
		markdown << "\n</details>";

	return markdown.join("");
}

QString ExpanderButtonBase::printLabelMD(int depth) const
{
	if (useCollapsibleSection(depth))
		// Use collapsible section
		return "<details>\n<summary><b>" + fullLabel() + "</b></summary>\n";
	else
		return JASPControl::printLabelMD(depth);
}

bool ExpanderButtonBase::useCollapsibleSection(int depth) const
{
	return depth == 0 && !fullLabel().isEmpty();
}
