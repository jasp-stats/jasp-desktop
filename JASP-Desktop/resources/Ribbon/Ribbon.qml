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

import QtQuick 2.11
import QtQuick.Controls 2.4
import QtQuick.Controls.Material 2.1


Item {
    Rectangle {
        id        : jaspRibbon
        objectName: "jaspRibbon"
        width     : 500
        height    : 60

        signal ribbonButtonClicked(variant clusterButton)

        function dispatchButtonClickSignal(clusterButton) {
            jaspRibbon.ribbonButtonClicked(clusterButton)
        }

        function getIconSourcePath(isDynamic) {

            if (isDynamic) {
                return "file:" + modulesPath + currentActiveModule + "/icons/"
            } else {
                return "qrc:/icons/"
            }
        }

        Row {
            id          : ribbonRow
            objectName  : "ribbonRow"
            padding     : 10
            spacing     : 10

            Repeater {
                id      : tabs
                model   : ribbonButtonModel
                delegate: Loader {
                    sourceComponent: displayText === "???" ? toolSeparator : ribbonButtonDelegate

                    Component {
                        id: ribbonButtonDelegate

                        RibbonButton {
                            text   : displayText
                            source : jaspRibbon.getIconSourcePath(ribbonButtonModel.isDynamic()) + iconSource
                            menu   : analysisMenu
                            enabled: ribbonIsEnabled
                        }
                    }

                    Component {
                        id: toolSeparator

                        ToolSeparator {
                            height  : jaspRibbon.height
                        }
                    }
                }
            }
        }
    }
}
