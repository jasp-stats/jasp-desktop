[JASP.Controls](jasp-controls-qmlmodule.md)

ColumnLayout


# ColumnLayout QML Type

A vertical layout container with JASP-themed spacing.


|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |


- [List of all members, including inherited
  members](qml-jasp-controls-columnlayout-members.md)

## Detailed Description

Wraps Qt's ColumnLayout with default spacing and alignment suited for
JASP analysis forms. Child items are arranged vertically from top to
bottom.


**Note:** ColumnLayout does not bind to R options. It is a layout-only
control.


## Properties

- **spacing** (real) - Vertical spacing between child items. Default: 5.

## Example

``` qml
ColumnLayout {
    CheckBox { name: "option1"; label: qsTr("Option 1") }
    CheckBox { name: "option2"; label: qsTr("Option 2") }
}
```
