[JASP.Controls](jasp-controls-qmlmodule.md)

RadioButtonGroup


# RadioButtonGroup QML Type

A group of mutually exclusive radio button options. [More...](#details)


|                   |                                                  |
|-------------------|--------------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                       |
| Inherited By:     | [BayesFactorType](qml-jasp-controls-bayesfactortype.md)                |


- [List of all members, including inherited
  members](qml-jasp-controls-radiobuttongroup-members.md)

## Detailed Description

Contains [RadioButton](qml-jasp-controls-radiobutton.md) children, of which exactly one can be
checked at a time. The checked button's name is sent as the option value
to R.

## R Binding

- **R Type:** `character` (the name/value of the selected
  [RadioButton](qml-jasp-controls-radiobutton.md))
- **Default:** Value of the initially checked
  [RadioButton](qml-jasp-controls-radiobutton.md)

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
