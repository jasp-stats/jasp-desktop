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

import QtQuick
import JASP.Controls
import JASP


/*!
    \qmltype ModelTermsList
    \inqmlmodule JASP.Controls 1.0
    \brief A preset VariablesList for building ANOVA model terms.

    Extends VariablesList with interaction-mode drag-and-drop, an "Add to null model"
    checkbox per row, and automatic nuisance flagging based on a source list
    (e.g. randomFactors).

    \section1 R Binding

    \list
    \li \b{R Type:} list (each element contains component variable names and isNuisance flag)
    \li \b{Default:} [] (empty)
    \endlist

    \section1 Properties

    \list
    \li \b checkedPerDefault (string) - Name of the source list whose variables should be checked as nuisance by default. Default: "randomFactors".
    \endlist

    \section1 Inherited Properties from VariablesList

    \list
    \li \b name (string) - R option name this control binds to. Default: "modelTerms".
    \li \b title (string) - Title above the list. Default: "Model Terms".
    \li \b listViewType (enum) - List interaction mode. Default: JASP.Interaction.
    \li \b dropMode (enum) - How dropped items are inserted. Default: JASP.DropInsert.
    \endlist

    \section1 Other Inherited Properties

    \list
    \li \b enabled (bool) - Whether the control is interactive. Default: true.
    \li \b visible (bool) - Whether the control is visible. Default: true.
    \li \b info (string) - Info that will be used by tooltip and to generate the help. Default: "".
    \li \b toolTip (string) - This property overwrite info property, in order to display a simpler tooltip text. Default: "".
    \endlist

    \section1 Example

    \qml
    ModelTermsList {
        name: "modelTerms"
    }
    \endqml
*/
VariablesList
{
	id						: modelTermsList
	dropMode				: JASP.DropInsert
	name					: "modelTerms"
	title					: qsTr("Model Terms")
	listViewType			: JASP.Interaction
	allowTypeChange			: false

	rowComponentTitle		: qsTr("Add to null model")
	interactionHighOrderCheckBox : "isNuisance"

	property string checkedPerDefault: "randomFactors"

	rowComponent			: CheckBox
	{
		name: "isNuisance"
		Component.onCompleted:
		{
			var varList = form.getControl(modelTermsList.checkedPerDefault)
			if ((typeof(isNew) !== 'undefined') && isNew && varList)
				checked = varList.columnsNames.includes(rowValue)
		}
	}
}
