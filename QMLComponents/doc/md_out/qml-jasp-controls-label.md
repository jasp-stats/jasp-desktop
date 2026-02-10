<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

Label

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [Properties](#properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# Label QML Type

A JASP-themed text label. [More...](#details)

<div class="table">

|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-label-members.md)

## Detailed Description

Wraps Qt's Label with the JASP default font and color scheme. Text color
automatically adjusts when the control is disabled.

<div class="admonition note">

**Note:** Label does not bind to R options. It is a display-only
control.

</div>

## Properties

- **text** (string) - Text to display. Default: "".

## Example

``` qml
Label { text: qsTr("Effect size:") }
```
