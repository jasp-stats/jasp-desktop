[JASP.Controls](jasp-controls-qmlmodule.md)

RowLayout


# RowLayout QML Type

A horizontal layout with JASP-themed spacing. [More...](#details)


|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |


- [List of all members, including inherited
  members](qml-jasp-controls-rowlayout-members.md)

## Detailed Description

Wraps Qt's RowLayout with default spacing from jaspTheme.rowGridSpacing
and top-left alignment.


**Note:** RowLayout does not bind to R options. It is a layout-only
control.


## Example

``` qml
RowLayout {
    CheckBox { name: "mean"; label: qsTr("Mean") }
    CheckBox { name: "sd";   label: qsTr("Std. deviation") }
}
```
