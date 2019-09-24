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
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form
{
	Text
	{
		Layout.columnSpan: 2
		text: "This example shows when you should assign a jasp element (container, table, etc) to jaspResults.\n" +
		"If you do it too early, then the output element might still be lacking some markup, as is the case here.\n\n" +
		"Checking `Good table` shows a table that was assigned to jaspResults once it was presentable.\n" +
		"Checking `Bad table` shows a table that was assigned to jaspResults too soon."
	}
	
	CheckBox
	{
		label: "Good table"
		name: "goodTable"
	}

	CheckBox
	{
		label: "Poor table"
		name: "poorTable"
	}
}
