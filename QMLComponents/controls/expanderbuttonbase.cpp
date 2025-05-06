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
	if (!hasInfo())
		return "";

	QString label = (infoLabel().isEmpty() ? title() : infoLabel()).trimmed();
	// For sub-section, draw first a line, and reset the depth to 0.
	if (label.isEmpty() || depth > 0)
		return "\n---\n\n" + JASPControl::generateMDHelp(0);

	// Use collapsible section
	return "<details>\n<summary><b>" + label + "</b></summary>\n" + JASPControl::generateMDHelp(0) + "\n</details>";
}

bool ExpanderButtonBase::printLabelMD(QStringList &md, int depth) const
{
	return depth == 0 || JASPControl::printLabelMD(md, depth);
}
