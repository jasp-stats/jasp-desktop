[JASP.Controls](jasp-controls-qmlmodule.md)

Button


### Contents

- [Detailed Description](#details)
- [Properties](#properties)
- [Inherited Properties](#inherited-properties)
- [Signals](#signals)
- [Example](#example)


# Button QML Type

A generic clickable button. 


|                   |                                               |
|-------------------|-----------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                    |
| Inherited By:     | [AssignButton](qml-jasp-controls-assignbutton.md)                |


- [List of all members, including inherited
  members](qml-jasp-controls-button-members.md)

## Detailed Description


**Note:** Button does not bind to R options. It is used for triggering
actions in the UI.


## Properties

- **text** (string) - Button label text. Alias: label. Default: "".
- **iconSource** (string) - Path to an icon displayed on the button.

## Inherited Properties

- **enabled** (bool) - Whether the control is interactive. Default:
  true.
- **visible** (bool) - Whether the control is visible. Default: true.
- **info** (string) - Info that will be used by tooltip and to generate
  the help. Default: "".
- **toolTip** (string) - This property overwrite info property, in order
  to display a simpler tooltip text. Default: "".

## Signals

- **clicked()** - Emitted when the button is clicked.

## Example

``` qml
Button {
    text: qsTr("Run Analysis")
}
```
