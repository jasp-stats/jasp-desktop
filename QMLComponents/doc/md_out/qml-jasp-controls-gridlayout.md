<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

GridLayout

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

# GridLayout QML Type

A responsive grid layout with JASP-themed spacing. [More...](#details)

<div class="table">

|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-gridlayout-members.md)

## Detailed Description

Wraps Qt's GridLayout with default row/column spacing and a 2-column
layout. Automatically reduces column count or spacing when content
overflows the available width. Used as the default layout container
inside Form.

<div class="admonition note">

**Note:** GridLayout does not bind to R options. It is a layout-only
control.

</div>

## Properties

- **columns** (int) - Number of grid columns. Auto-reduces on overflow.
  Default: 2.
- **rowSpacing** (real) - Vertical spacing between rows. Default:
  jaspTheme.rowGridSpacing.
- **columnSpacing** (real) - Horizontal spacing between columns.
  Default: jaspTheme.columnGridSpacing.

## Example

``` qml
GridLayout {
    columns: 3
    CheckBox { name: "opt1"; label: qsTr("Option 1") }
    CheckBox { name: "opt2"; label: qsTr("Option 2") }
    CheckBox { name: "opt3"; label: qsTr("Option 3") }
}
```
