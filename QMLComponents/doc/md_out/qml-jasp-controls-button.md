<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

Button

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [Properties](#properties)
- [Inherited Properties](#inherited-properties)
- [Signals](#signals)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# Button QML Type

A generic clickable button. [More...](#details)

<div class="table">

|                   |                                               |
|-------------------|-----------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                    |
| Inherited By:     | <a href="qml-jasp-controls-assignbutton.md" 
                     translate="no">AssignButton</a>                |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-button-members.md)

## Detailed Description

<div class="admonition note">

**Note:** Button does not bind to R options. It is used for triggering
actions in the UI.

</div>

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
