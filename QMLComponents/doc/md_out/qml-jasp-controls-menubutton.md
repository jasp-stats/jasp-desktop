<a href="jasp-controls-qmlmodule.html" translate="no">JASP.Controls</a>

MenuButton

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [Properties](#properties)
- [Signals](#signals)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# MenuButton QML Type

A button with optional submenu arrow and hover-to-open behaviour.
[More...](#details)

<div class="table">

|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | <a href="qml-jasp-controls-roundedbutton.html"
translate="no">RoundedButton</a> |
| Inherited By: | <a href="qml-jasp-controls-helpbutton.html"
translate="no">HelpButton</a> and <a href="qml-jasp-controls-sortmenubutton.html"
translate="no">SortMenuButton</a> |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-menubutton-members.html)

## Detailed Description

Extends <a href="qml-jasp-controls-roundedbutton.html"
translate="no">RoundedButton</a> with hover-delay logic and a submenu
indicator arrow. Used internally for ribbon menus and toolbar items.

<div class="admonition note">

**Note:** This is primarily an internal UI component. Module developers
typically use Button or <a href="qml-jasp-controls-helpbutton.html"
translate="no">HelpButton</a> instead.

</div>

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
