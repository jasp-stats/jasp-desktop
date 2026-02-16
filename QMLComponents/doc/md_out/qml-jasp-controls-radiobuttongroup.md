<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

RadioButtonGroup

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

# RadioButtonGroup QML Type

A group of mutually exclusive radio button options. [More...](#details)

<div class="table">

|                   |                                                  |
|-------------------|--------------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                       |
| Inherited By:     | <a href="qml-jasp-controls-bayesfactortype.md" 
                     translate="no">BayesFactorType</a>                |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-radiobuttongroup-members.md)

## Detailed Description

Contains <a href="qml-jasp-controls-radiobutton.md"
translate="no">RadioButton</a> children, of which exactly one can be
checked at a time. The checked button's name is sent as the option value
to R.

## R Binding

- **R Type:** `character` (the name/value of the selected
  <a href="qml-jasp-controls-radiobutton.md"
  translate="no">RadioButton</a>)
- **Default:** Value of the initially checked
  <a href="qml-jasp-controls-radiobutton.md"
  translate="no">RadioButton</a>

## Properties

- **name** (string) - R option name this control binds to. Default: "".
- **title** (string) - Title label displayed above or beside the group.
  Alias: text. Default: "".
- **radioButtonsOnSameRow** (bool) - Place all radio buttons on one row.
  Default: false.
- **columns** (int) - Number of columns in the content area. Default: 1
  (or children.length when radioButtonsOnSameRow).
- **leftPadding** (int) - Left padding for the content area. Default:
  10.

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
RadioButtonGroup {
    name: "hypothesis"
    title: qsTr("Alt. Hypothesis")
    RadioButton { value: "twoSided"; label: qsTr("≠ Test value"); checked: true }
    RadioButton { value: "greater";  label: qsTr("> Test value") }
    RadioButton { value: "less";     label: qsTr("< Test value") }
}
```
