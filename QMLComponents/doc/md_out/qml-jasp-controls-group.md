<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

Group

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [Properties](#properties)
- [Inherited Properties](#inherited-properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# Group QML Type

A layout container that groups related controls together.
[More...](#details)

<div class="table">

|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherited By: | <a href="qml-jasp-controls-setseed.md" translate="no">SetSeed</a> |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-group-members.md)

## Detailed Description

Provides automatic label alignment and optional title.

<div class="admonition note">

**Note:** Group is a layout-only control. It does not bind to R options.
Child controls within the Group handle their own bindings independently.

</div>

## Properties

- **title** (string) - Optional title displayed above the group.
  Default: "".
- **columns** (int) - Number of columns for child layout. Default: 1.
- **rowSpacing** (int) - Vertical spacing between rows. Default: 5.
- **columnSpacing** (int) - Horizontal spacing between columns. Default:
  10.
- **indent** (bool) - Add left indentation to the group. Default: false.
- **alignFields** (bool) - Auto-align input fields (e.g.,
  <a href="qml-jasp-controls-textfield.md" translate="no">TextField</a>
  and
  <a href="qml-jasp-controls-dropdown.md" translate="no">DropDown</a>
  labels). Default: true.

## Inherited Properties

- **enabled** (bool) - Whether the control is interactive. Default:
  true.
- **visible** (bool) - Whether the control is visible. Default: true.
- **info** (string) - Info that will be used by tooltip and to generate
  the help. Default: "".
- **toolTip** (string) - This property overwrite info property, in order
  to display a simpler tooltip text. Default: "".

## Example

``` qml
Group {
    title: qsTr("Descriptive Statistics")

    CheckBox { name: "mean";   label: qsTr("Mean")   }
    CheckBox { name: "median"; label: qsTr("Median") }
    CheckBox { name: "mode";   label: qsTr("Mode")   }
}
```
