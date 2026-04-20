[JASP.Controls](jasp-controls-qmlmodule.md)

ComponentsList


# ComponentsList QML Type

A dynamic list that repeats a user-defined component for each row.


|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |


- [List of all members, including inherited
  members](qml-jasp-controls-componentslist-members.md)

## Detailed Description

Displays a scrollable list of repeated QML components defined via a
rowComponent. Rows can be added and removed by the user (when
addItemManually is true) or populated from a source control. Each row's
controls are bound to separate R list entries.

## R Binding

- **R Type:** list (array of objects, one per row)
- **Default:** \[\] (empty array)

## Properties

- **name** (string) - R option name this control binds to. Default: "".
- **title** (string) - Title displayed above the list. Alias: label.
  Default: "".
- **source** (var) - Source control for populating rows. Default:
  undefined. This can be an id or name (or an array of names/ids) of
  another controls.
- **rSource** (string) - R source for populating rows. Default: "".
- **rowComponent** (Component) - One QML component (use Row or
  [RowLayout](qml-jasp-controls-rowlayout.md)
  if more items are needed), that will be repeated for each row. In each
  row, you can use the rowValue, rowLabel, rowType or rowIndex that
  gives you resp. the value, label, type (if it is a variable) and index
  linked to each row.
- **addItemManually** (bool) - Allow user to add/remove rows via
  buttons. Default: false when source is set, true otherwise
- **minimumItems** (int) - Minimum number of rows that must remain.
  Default: 0.
- **maximumItems** (int) - Maximum number of rows allowed (-1 for
  unlimited). Default: -1.
- **rows** (int) - Number of grid rows. Default: equals row count. Read
  only.
- **rowSpacing** (real) - Vertical spacing between rows. Default: 1.
- **showAddIcon** (bool) - Show the add-row icon button. Default: equals
  addItemManually.
- **addIcon** (string) - Icon file for the add button. Default:
  "round_addition.png".
- **removeIcon** (string) - Icon file for the remove button. Default:
  "cross.png".
- **addTooltip** (string) - Tooltip for the add button. Default: "Add a
  row".
- **removeTooltip** (string) - Tooltip for the remove button. Default:
  "Remove a row".
- **addBorder** (bool) - Draw a border around the list. Default: true.
- **headerLabels** (array) - Column header labels for the component
  grid. Default: \[\].
- **newItemValue** (string) - Default value key for new rows. Default:
  "#".
- **duplicateWhenAdding** (bool) - Duplicate the last row when adding.
  Default: false.

## Inherited Properties

- **enabled** (bool) - Whether the control is interactive. Default:
  true.
- **visible** (bool) - Whether the control is visible. Default: true.
- **info** (string) - Info that will be used by tooltip and to generate
  the help. Default: "".
- **toolTip** (string) - This property overwrite info property, in order
  to display a simpler tooltip text. Default: "".

## Signals

- **addItem()** - Emitted when a new row should be added.
- **removeItem(int index)** - Emitted when a row should be removed.

## Example

``` qml
ComponentsList {
    name: "contrasts"
    title: qsTr("Contrasts")
    source: "fixedFactors"
            headerLabels: [qsTr("Contrast")]
            rowComponent: Row {
                    Text { text: rowValue } // rowValue contains the name of the variable
                    DropDown {
                            name: "contrast"
                            source: [
                                    { label: qsTr("None"),       value: "none"       },
                                    { label: qsTr("Deviation"),  value: "deviation"  },
                                    { label: qsTr("Helmert"),    value: "helmert"    }
                            ]
                    }
    }
    }
```

``` qml
ComponentsList { // Here no source is given, so addItemManually is true, and the user will see a '+' button to add more rows
        name: "extraValues"
        title: qsTr("Extra values")
        headerLabels: [qsTr("Alpha"), qtStr("Beta")]
        minimumItems: 2 // 2 rows will be uatomatically initialized. If more rows are added, a delete icon will be added beside each new row, so that the user can delete this row
        rowComponent: Row {
                IntegerField    { name: "alphaValue" }
                DoubleValue             { name: "betaValue" }
        }
}
```
