[JASP.Controls](jasp-controls-qmlmodule.md)

HelpButton


### Contents

- [Detailed Description](#details)
- [Properties](#properties)
- [Example](#example)


# HelpButton QML Type

A small info button that opens a help page for the current analysis.


|                   |                                             |
|-------------------|---------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                  |
| Inherits:         | [MenuButton](qml-jasp-controls-menubutton.md)                |


- [List of all members, including inherited
  members](qml-jasp-controls-helpbutton-members.md)

## Detailed Description

Extends [MenuButton](qml-jasp-controls-menubutton.md) with an info icon. When clicked, opens or
toggles the specified help page in the JASP help panel. Only works
within a Form.


**Note:** HelpButton does not bind to R options. It is a UI-only
control.


## Properties

- **helpPage** (string) - Name of the help page to display. Default: "".

## Example

``` qml
HelpButton { helpPage: "anova" }
```
