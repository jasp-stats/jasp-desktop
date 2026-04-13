[JASP.Controls](jasp-controls-qmlmodule.md)

BasicThreeButtonTableView


# BasicThreeButtonTableView QML Type

A table input control with Add, Delete, and Reset buttons.


|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherited By: | [Chi2TestTableView](qml-jasp-controls-chi2testtableview.md), [CustomContrastsTableView](qml-jasp-controls-customcontraststableview.md), [JagsTableView](qml-jasp-controls-jagstableview.md), and [SimpleTableView](qml-jasp-controls-simpletableview.md) |


- [List of all members, including inherited
  members](qml-jasp-controls-basicthreebuttontableview-members.md)

## Detailed Description

Wraps a
[TableView](qml-jasp-controls-tableview.md)
with three action buttons for managing rows. Used for entering
structured data such as matrices or custom data tables.

## R Binding

- **R Type:** data.frame or matrix
- **Default:** Empty table with initialRowCount rows and
  initialColumnCount columns

## Properties

- **name** (string) - R option name this control binds to. Default: "".
- **source** (var) - Source for populating the table.
- **values** (var) - Values to populate the table with.
- **initialColumnCount** (int) - Number of columns at creation.
- **initialRowCount** (int) - Number of rows at creation.
- **cornerText** (string) - Text displayed in the top-left corner cell.
- **columnNames** (list) - Custom column header names.
- **rowNames** (list) - Custom row header names.
- **defaultValue** (var) - Default value for new cells.
- **minimum** (var) - Minimum allowed value for cells.
- **decimals** (int) - Number of decimal places for numeric cells.
- **isFirstColEditable** (bool) - Whether the first column is editable.
- **buttonsInRow** (bool) - Place buttons in a row above the table
  instead of a column on the left. Default: false.
- **showButtons** (bool) - Show the Add/Delete/Reset buttons. Default:
  true.
- **showAddButton** (bool) - Show the Add button. Default: true.
- **showDeleteButton** (bool) - Show the Delete button. Default: true.
- **showResetButton** (bool) - Show the Reset button. Default: true.

## Inherited Properties

- **enabled** (bool) - Whether the control is interactive. Default:
  true.
- **visible** (bool) - Whether the control is visible. Default: true.
- **info** (string) - Info that will be used by tooltip and to generate
  the help. Default: "".
- **toolTip** (string) - This property overwrite info property, in order
  to display a simpler tooltip text. Default: "".

## Signals

- **addClicked()** - Emitted when the Add button is clicked.
- **deleteClicked()** - Emitted when the Delete button is clicked.
- **resetClicked()** - Emitted when the Reset button is clicked.
- **tableViewCompleted()** - Emitted when the internal
  [TableView](qml-jasp-controls-tableview.md)
  has completed initialization.

## Example

``` qml
BasicThreeButtonTableView {
    name: "priorCounts"
    initialColumnCount: 3
    initialRowCount: 2
    columnNames: [qsTr("Group 1"), qsTr("Group 2"), qsTr("Group 3")]
}
```
