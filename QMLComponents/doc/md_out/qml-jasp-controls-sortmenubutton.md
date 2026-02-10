<a href="jasp-controls-qmlmodule.html" translate="no">JASP.Controls</a>

SortMenuButton

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [Properties](#properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# SortMenuButton QML Type

A circular sort button that opens a sort-order popup.
[More...](#details)

<div class="table">

|                   |                                             |
|-------------------|---------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                  |
| Inherits:         | <a href="qml-jasp-controls-menubutton.html" 
                     translate="no">MenuButton</a>                |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-sortmenubutton-members.html)

## Detailed Description

Extends <a href="qml-jasp-controls-menubutton.html"
translate="no">MenuButton</a> with a sort icon. When clicked, opens a
popup menu driven by sortMenuModel to let users re-order list items.

<div class="admonition note">

**Note:** This is primarily an internal UI component used by list
controls.

</div>

## Properties

- **sortMenuModel** (var) - Model providing sort options. Default: null.

## Example

``` qml
SortMenuButton {
    sortMenuModel: myList.sortMenuModel
}
```
