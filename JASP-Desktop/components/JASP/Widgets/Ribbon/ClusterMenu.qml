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


Menu {

    // width: {
    //     // https://martin.rpdev.net/2018/03/13/qt-quick-controls-2-automatically-set-the-width-of-menus.html
    //     var result = 0;
    //     var padding = 0;
    //
    //     for (var i = 0; i < count; ++i) {
    //         var item = itemAt(i);
    //         result = Math.max(item.implicitWidth, result);
    //         padding = Math.max(item.padding, padding);
    //     }
    //     return result + padding * 2;
    // }

    Repeater {
        model   : ribbonButton.menu

        delegate: Loader {
            sourceComponent: displayText === "???" ? menuSeparator : analysisDelegate

            Component {
                id: analysisDelegate
                MenuItem {
                    text: displayText
                }
            }

            Component {
                id: menuSeparator
                MenuSeparator { }
            }
        }
    }
}
