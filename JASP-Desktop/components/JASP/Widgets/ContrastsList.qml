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

import JASP.Controls 1.0
import QtQuick.Layouts 1.3 as L

VariablesList {
    title: qsTr("Factors")
    source: "fixedFactors"
    name: "contrasts"
    listViewType: "AssignedVariables"
    height: 200
    draggable: false
	
	L.Layout.columnSpan: parent.columns	

    ExtraControlColumn {
        type: "ComboBox"
        name: "contrast"
        values: ["none", "deviation", "simple", "difference", "Helmert", "repeated", "polynomial"]
    }
}
