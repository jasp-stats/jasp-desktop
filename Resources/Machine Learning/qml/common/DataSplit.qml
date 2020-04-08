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

import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0


Section
{
    property alias leaveOneOutVisible:              leaveOneOut.visible
    property alias kFoldsVisible:                   kFolds.visible
    property alias trainingValidationSplit:         trainingValidationSplit.visible
    
    title: qsTr("Data Split Preferences")

    RadioButtonGroup 
    {
        title: qsTr("Holdout Test Data")
        name: "holdoutData"

        RadioButton 
        {
            id: holdoutManual
            name: "holdoutManual"
            childrenOnSameRow: true
            text: qsTr("Sample")

            RowLayout 
            {
                PercentField 
                {    
                    name: "testDataManual"
                    defaultValue: 20
                    min: 5
                    max: 95 
                    afterLabel: qsTr("% of all data")
                }
            }
        }

        CheckBox 
        { 
            id: addIndicator  
            name: "addIndicator"
            text: qsTr("Add generated indicator to data")
            Layout.leftMargin: 20
            enabled: holdoutManual.checked

            ComputedColumnField 
            { 
                name: 		"testIndicatorColumn"
				text: 		qsTr("Column name: ")
                fieldWidth: 120
                visible:    addIndicator.checked
            }
        }

        RadioButton 
        {
            name: "testSetIndicator"
            label: qsTr("Test set indicator:")
            childrenOnSameRow: true

            DropDown 
            {
                id: testSetIndicatorVariable
                name: "testSetIndicatorVariable"
                showVariableTypeIcon: true
                addEmptyValue: true
                placeholderText: qsTr("None")
            }
        }
    }

    RadioButtonGroup 
    {
        id: trainingValidationSplit
        title: qsTr("Training and Validation Data")
        name: "modelValid"

        RadioButton 
        {
            name: "validationManual"
            childrenOnSameRow: true
            checked: true
            text: qsTr("Sample")

            RowLayout {

                PercentField {
                    name: "validationDataManual"
                    defaultValue: 20
                    min: 5
                    max: 95
                    afterLabel: qsTr("% for validation data")
                }
            }
        }

        RadioButton 
        { 
            id: kFolds
            name: "validationKFold"
            childrenOnSameRow: true
            text: qsTr("K-fold with")

            RowLayout 
            {

                IntegerField {
                    name: "noOfFolds"
                    defaultValue: 5
                    min: 2
                    max: 999
                    fieldWidth: 30
                    afterLabel: qsTr("folds")
                } 
            }
        }

        RadioButton { 
            id: leaveOneOut
            text: qsTr("Leave-one-out")                 
            name: "validationLeaveOneOut"
        }
    }
}
