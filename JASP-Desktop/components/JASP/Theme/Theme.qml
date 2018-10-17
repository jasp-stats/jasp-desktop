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

pragma Singleton

import QtQuick 2.8

QtObject {
    readonly property color white: "white"
    readonly property color whiteBroken: "#F5F5F5"
    readonly property color black: "black"
    readonly property color gray: "#d1d1d1"
    readonly property color grayDarker: Qt.darker(gray, 1.5)
    readonly property color grayLighter: "#d4d4d4"
    readonly property color grayMuchLighter: "#ECECEC"
    readonly property color blue: "#419BF9"
    readonly property color blueDarker: "#0069D9"
    readonly property color blueLighter: "#97C4F2"
    readonly property color blueMuchLighter: "#DCF1FB"
    readonly property color red: "#FC625D"
    readonly property color green: "#36CE4C"
    readonly property color yellowLight: "#FFFFCA"
    readonly property color rose: "#FFC0CB"
    
    readonly property int formWidth: 500
    readonly property int formMargin: 10
    
    readonly property int borderRadius: 4
    readonly property int rowSpacing: 3
    readonly property int columnSpacing: 30
    
    readonly property int checkBoxIndicatorLength: 15
    readonly property int radioIndicatorDiameter: 16
    readonly property int sliderHandleDiameter: 16
    readonly property int sliderLength: 80
    readonly property int sliderWidth: 4
    readonly property int comboBoxHeight: 22
    readonly property int textFieldHeight: 20
    readonly property int textFieldWidth: 40
    readonly property int switchHeight: 15
    readonly property int groupContentPadding: 10
    readonly property int defaultListHeight: 350
    readonly property int defaultSingleItemListHeight: 45
    readonly property int variablesListTitle: 20

	readonly property int generalAnchorMargin: 8

    property font font
    font.bold: true
    font.underline: false
    font.pixelSize: 14
    font.family: "arial"
    
    readonly property color borderColor: gray
    readonly property color focusBorderColor: blueLighter
    readonly property color containsDragBorderColor: green
    readonly property color itemHoverColor: blueMuchLighter
    readonly property color itemSelectedColor: blueDarker
    readonly property color itemSelectedNoFocusColor: grayLighter
    property color analysisBackgroundColor: grayMuchLighter
    readonly property color controlBackgroundColor: white
    readonly property color disableControlBackgroundColor: whiteBroken
    readonly property color rowEvenColor: controlBackgroundColor
    readonly property color rowOnevenColor: whiteBroken
    readonly property color buttonBackgroundColor: blue
    readonly property color tooltipBackgroundColor: yellowLight
    readonly property color debugBackgroundColor: rose
    readonly property color errorMessagesBackgroundColor: red
    readonly property color sliderPartOn: blue
    readonly property color sliderPartOff: grayDarker
    
}
