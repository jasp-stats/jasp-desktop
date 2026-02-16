<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

TabView

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [R Binding](#r-binding)
- [Properties](#properties)
- [Inherited Properties](#inherited-properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# TabView QML Type

A tabbed container that manages dynamic panels. [More...](#details)

<div class="table">

|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-tabview-members.md)

## Detailed Description

Displays a tab bar where each tab shows its own panel of child controls.
Tabs can be added, removed, and renamed (double-click). Commonly used
when an analysis needs a variable number of configuration panels (e.g.
one per group). It has in fact the same functinality as
<a href="qml-jasp-controls-componentslist.md"
translate="no">ComponentsList</a>, buut instead of displaying the
components in rows, it display them as Tabs.

## R Binding

- **R Type:** list (each tab produces one element in the list)
- **Default:** \[\] (one tab created by default)

## Properties

- **name** (string) - R option name this control binds to. Default: "".
- **label** (string) - Title displayed above the tab bar. Alias: title.
  Default: "".
- **source** (var) - Source control for populating the tabs. Default:
  undefined. This can be an id or name (or an array of names/ids) of
  another controls.
- **content** (Component) - One QML component (use Row or
  <a href="qml-jasp-controls-rowlayout.md" translate="no">RowLayout</a>
  if more items are needed), that will be repeated for each row. In each
  row, you can use the rowValue, rowLabel, rowType or rowIndex that
  gives you resp. the value, label, type (if it is a variable) and index
  linked to each row.
- **showAddIcon** (bool) - Show a "+" button to add tabs. Default: true
  when addItemManually.
- **showRemoveIcon** (bool) - Show a "×" icon on each tab. Default: true
  when addItemManually.
- **addItemManually** (bool) - Allow user to add/remove tabs. Default:
  false when source is set, true otherwise
- **tabNameEditable** (bool) - Allow double-click to rename tabs.
  Default: true when addItemManually.
- **newTabName** (string) - Default name for newly added tabs. Default:
  "New tab".
- **currentIndex** (int) - Index of the currently selected tab. Default:
  0.

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
TabView {
    name: "models"
    title: qsTr("Models")
    newTabName: qsTr("Model 1")
            content:  VariablesList { name: "predictors"; title: qsTr("Predictors") }
}
```
