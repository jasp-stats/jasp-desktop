<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

RowLayout

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# RowLayout QML Type

A horizontal layout with JASP-themed spacing. [More...](#details)

<div class="table">

|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-rowlayout-members.md)

## Detailed Description

Wraps Qt's RowLayout with default spacing from jaspTheme.rowGridSpacing
and top-left alignment.

<div class="admonition note">

**Note:** RowLayout does not bind to R options. It is a layout-only
control.

</div>

## Example

``` qml
RowLayout {
    CheckBox { name: "mean"; label: qsTr("Mean") }
    CheckBox { name: "sd";   label: qsTr("Std. deviation") }
}
```
