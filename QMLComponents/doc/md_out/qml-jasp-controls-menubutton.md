[JASP.Controls](jasp-controls-qmlmodule.md)

MenuButton


### Contents

- [Detailed Description](#details)
- [Properties](#properties)
- [Signals](#signals)
- [Example](#example)


# MenuButton QML Type

A button with optional submenu arrow and hover-to-open behaviour.


|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | [RoundedButton](qml-jasp-controls-roundedbutton.md) |
| Inherited By: | [HelpButton](qml-jasp-controls-helpbutton.md) and [SortMenuButton](qml-jasp-controls-sortmenubutton.md) |


- [List of all members, including inherited
  members](qml-jasp-controls-menubutton-members.md)

## Detailed Description

Extends [RoundedButton](qml-jasp-controls-roundedbutton.md) with hover-delay logic and a submenu
indicator arrow. Used internally for ribbon menus and toolbar items.


**Note:** This is primarily an internal UI component. Module developers
typically use Button or [HelpButton](qml-jasp-controls-helpbutton.md) instead.


## Properties

- **hasSubMenu** (bool) - Show a submenu arrow and enable hover-to-open.
  Default: false.
- **defaultColor** (color) - Background color when idle. Default:
  "transparent".

## Signals

- **hoverClicked()** - Emitted when hover-delay triggers on a submenu
  button.

## Example

``` qml
MenuButton {
    text: qsTr("Options")
    hasSubMenu: true
}
```
