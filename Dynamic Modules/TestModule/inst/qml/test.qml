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

Form {
    id: form

    VariablesForm { id: variablesFormPlanning; implicitHeight: 110
            AvailableVariablesList { name: "variablesAva" }
            AssignedVariablesList { name: "variables"; title: qsTr("something numbers"); id: recordNumberVariable }
           
        }

 /*   TableView
    {
        id:         table
        name:       "filteredData"
        modelType:  "FilteredDataEntryModel"
        filter:     filterID.value
        source:     "recordNumberVariable"
        colName:    "auditResult"
    }

    TextField
    {
        id:     filterID
        name:   "hahahaFilter"
        value:  table.filter
        
    }*/


	ComputedColumnField { name: "hallo"; text: "fill me!"; }
    CheckBox
    {
        name: "checkbox_0"
        checked: false
    }

    CheckBox
    {
        name: "checkbox_1"
        checked: false
    }
}
