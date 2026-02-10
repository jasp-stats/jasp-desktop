<a href="jasp-controls-qmlmodule.html" translate="no">JASP.Controls</a>

ComponentsList

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [R Binding](#r-binding)
- [Properties](#properties)
- [Inherited Properties](#inherited-properties)
- [Signals](#signals)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# ComponentsList QML Type

A dynamic list that repeats a user-defined component for each row.
[More...](#details)

<div class="table">

|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-componentslist-members.html)

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
  undefined.
- **rSource** (string) - R source for populating rows. Default: "".
- **addItemManually** (bool) - Allow user to add/remove rows via
  buttons. Default: false when source is set.
- **minimumItems** (int) - Minimum number of rows that must remain.
  Default: 0.
- **maximumItems** (int) - Maximum number of rows allowed (-1 for
  unlimited). Default: -1.
- **columns** (int) - Number of grid columns for the component layout.
  Default: 2 when addItemManually, 1 otherwise.
- **rows** (int) - Number of grid rows. Default: equals row count.
- **rowSpacing** (real) - Vertical spacing between rows. Default: 1.
- **columnSpacing** (real) - Horizontal spacing between columns.
  Default: 10.
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
    rowComponent: DropDown {
        name: "contrast"
        source: [
            { label: qsTr("None"),       value: "none"       },
            { label: qsTr("Deviation"),  value: "deviation"  },
            { label: qsTr("Helmert"),    value: "helmert"    }
        ]
    }
}
```
