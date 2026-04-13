[JASP.Controls](jasp-controls-qmlmodule.md)

Group


# Group QML Type

A layout container that groups related controls together.


|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherited By: | [SetSeed](qml-jasp-controls-setseed.md) |


- [List of all members, including inherited
  members](qml-jasp-controls-group-members.md)

## Detailed Description

Provides automatic label alignment and optional title.


**Note:** Group is a layout-only control. It does not bind to R options.
Child controls within the Group handle their own bindings independently.


## Properties

- **title** (string) - Optional title displayed above the group.
  Default: "".
- **columns** (int) - Number of columns for child layout. Default: 1.
- **rowSpacing** (int) - Vertical spacing between rows. Default: 5.
- **columnSpacing** (int) - Horizontal spacing between columns. Default:
  10.
- **indent** (bool) - Add left indentation to the group. Default: false.
- **alignFields** (bool) - Auto-align input fields (e.g.,
  [TextField](qml-jasp-controls-textfield.md)
  and
  [DropDown](qml-jasp-controls-dropdown.md)
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
