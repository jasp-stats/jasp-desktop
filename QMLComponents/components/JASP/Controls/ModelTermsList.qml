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

import QtQuick	2.11
import JASP		1.0


VariablesList
{
	dropMode				: JASP.DropInsert
	name					: "modelTerms"
	title					: qsTr("Model Terms")
	listViewType			: JASP.Interaction

	rowComponentTitle		: qsTr("Add to null model")
	interactionHighOrderCheckBox : "isNuisance"
	rowComponent			: CheckBox
	{
		name: "isNuisance"
		Component.onCompleted:
		{
			if ((typeof(isNew) !== 'undefined') && isNew)
				checked = (listView.getSourceType(rowValue) === "randomFactors")
		}
	}
}
