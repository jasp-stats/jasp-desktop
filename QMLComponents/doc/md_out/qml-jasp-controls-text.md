[JASP.Controls](jasp-controls-qmlmodule.md)

Text


# Text QML Type

A JASP-themed text display element. [More...](#details)


|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |


- [List of all members, including inherited
  members](qml-jasp-controls-text-members.md)

## Detailed Description

Wraps Qt's Text with the JASP default font and color scheme. Text color
automatically adjusts when the control is disabled.


**Note:** Text does not bind to R options. It is a display-only control.


## Example

``` qml
Text { text: qsTr("Note: values are log-transformed.") }
```
