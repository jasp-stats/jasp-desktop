[JASP.Controls](jasp-controls-qmlmodule.md)

SortMenuButton


### Contents

- [Detailed Description](#details)
- [Properties](#properties)
- [Example](#example)


# SortMenuButton QML Type

A circular sort button that opens a sort-order popup.


|                   |                                             |
|-------------------|---------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                  |
| Inherits:         | [MenuButton](qml-jasp-controls-menubutton.md)                |


- [List of all members, including inherited
  members](qml-jasp-controls-sortmenubutton-members.md)

## Detailed Description

Extends [MenuButton](qml-jasp-controls-menubutton.md) with a sort icon. When clicked, opens a
popup menu driven by sortMenuModel to let users re-order list items.


**Note:** This is primarily an internal UI component used by list
controls.


## Properties

- **sortMenuModel** (var) - Model providing sort options. Default: null.

## Example

``` qml
SortMenuButton {
    sortMenuModel: myList.sortMenuModel
}
```
