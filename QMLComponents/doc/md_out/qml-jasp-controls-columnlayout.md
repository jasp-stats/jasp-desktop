<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

ColumnLayout

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

# ColumnLayout QML Type

A vertical layout container with JASP-themed spacing.
[More...](#details)

<div class="table">

|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-columnlayout-members.md)

## Detailed Description

Wraps Qt's ColumnLayout with default spacing and alignment suited for
JASP analysis forms. Child items are arranged vertically from top to
bottom.

<div class="admonition note">

**Note:** ColumnLayout does not bind to R options. It is a layout-only
control.

</div>

## Properties

- **spacing** (real) - Vertical spacing between child items. Default: 5.

## Example

``` qml
ColumnLayout {
    CheckBox { name: "option1"; label: qsTr("Option 1") }
    CheckBox { name: "option2"; label: qsTr("Option 2") }
}
```
