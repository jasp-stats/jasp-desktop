<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

DropDown

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

# DropDown QML Type

A selection control that presents a list of options in a dropdown menu.
[More...](#details)

<div class="table">

|                   |                                               |
|-------------------|-----------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                    |
| Inherited By:     | <a href="qml-jasp-controls-colorpalette.md" 
                     translate="no">ColorPalette</a>                |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-dropdown-members.md)

## Detailed Description

Returns the value of the selected item as a string to R.

## R Binding

- **R Type:** `character`
- **Default:** First item's value, or "" if addEmptyValue is true

## Properties

- **name** (string) - R option name this control binds to. Default: "".
- **label** (string) - Label displayed before the dropdown. Default: "".
- **currentValue** (var) - The value of the currently selected item.
- **currentIndex** (int) - Index of currently selected item. Default: 0.
- **values** (array) - Simple array of values (creates value=label
  pairs). Default: \[\].
- **source** (var) - Source for populating from variables or other
  controls. Can be the id or the name (or an array of ids/names) of the
  controls.
- **addEmptyValue** (bool) - Add an empty option at the start. Default:
  false.
- **placeholderText** (string) - Text shown when empty value is
  selected. Default: "\<no choice\>".

## Inherited Properties

- **enabled** (bool) - Whether the control is interactive. Default:
  true.
- **visible** (bool) - Whether the control is visible. Default: true.
- **info** (string) - Info that will be used by tooltip and to generate
  the help. Default: "".
- **toolTip** (string) - This property overwrite info property, in order
  to display a simpler tooltip text. Default: "".

## Signals

- **activated(int index)** - Emitted when user selects an item.

## Example

``` qml
Column {
    DropDown {
        name: "correlationType"
        label: qsTr("Correlation coefficient")
        values: [
            { label: qsTr("Pearson"),  value: "pearson"  },
            { label: qsTr("Spearman"), value: "spearman" }
        ]
        indexDefaultValue: 0
    }
    DropDown {
        name: "factor"
        label: qsTr("Choose factor variable")
        source: factors // id of the factors VariablesList
    }
}
```
