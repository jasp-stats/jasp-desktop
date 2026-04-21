[JASP.Controls](jasp-controls-qmlmodule.md)

InputListView


# InputListView QML Type

An editable scrollable list for entering free-text values.


|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |


- [List of all members, including inherited
  members](qml-jasp-controls-inputlistview-members.md)

## Detailed Description

Displays a scrollable grid of text fields where users can type custom
values. New items are added by typing in a virtual placeholder row.
Items can be deleted via a cross icon. Optionally supports a row
component displayed alongside each entry.

## R Binding

- **R Type:** array of strings
- **Default:** \[\] (empty array, or defaultValues if set)

## Properties

- **name** (string) - R option name this control binds to. Default: "".
- **title** (string) - Title displayed above the list. Alias: label.
  Default: "".
- **rowComponentTitle** (string) - Title for the row component column.
  Default: "".
- **defaultValues** (array) - Initial values to populate the list.
  Default: \[\].
- **minRows** (int) - Minimum number of rows. Default: 0.
- **addVirtual** (bool) - Show a placeholder row for adding new items.
  Default: true.
- **placeHolder** (string) - Placeholder text for new items. Default:
  "New Value".
- **cellHeight** (real) - Height of each row cell. Default: 20.
- **cellWidth** (real) - Width of each row cell. Default: list width.

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
InputListView {
    name: "customValues"
    title: qsTr("Values")
    defaultValues: ["0.5", "1.0", "1.5"]
}
```
