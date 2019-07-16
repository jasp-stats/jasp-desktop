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
		text: "This example shows a method of creating a jaspTable that should work for most situations.\n" +
		"It involves no duplication and adds an empty table if the analysis is not ready to show actual results.\n\n" +
		"Checking `Table` shows the table.\n" +
		"Checking `We are ready` emulates the analysis being ready to compute and show results."
	}
	
	CheckBox
	{
		label: "Table"
		name: "weWantThisTable"
		checked: true
	}
	
	CheckBox
	{
		label: "We are ready"
		name: "weAreReady"
	}
	
}