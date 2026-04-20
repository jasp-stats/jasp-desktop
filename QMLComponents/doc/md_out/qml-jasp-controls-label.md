[JASP.Controls](jasp-controls-qmlmodule.md)

Label


# Label QML Type

A JASP-themed text label. [More...](#details)


|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |


- [List of all members, including inherited
  members](qml-jasp-controls-label-members.md)

## Detailed Description

Wraps Qt's Label with the JASP default font and color scheme. Text color
automatically adjusts when the control is disabled.


**Note:** Label does not bind to R options. It is a display-only
control.


## Properties

- **text** (string) - Text to display. Default: "".

## Example

``` qml
Label { text: qsTr("Effect size:") }
```
