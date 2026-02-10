<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

Text

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# Text QML Type

A JASP-themed text display element. [More...](#details)

<div class="table">

|                   |                            |
|-------------------|----------------------------|
| Import Statement: | `import JASP.Controls 1.0` |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-text-members.md)

## Detailed Description

Wraps Qt's Text with the JASP default font and color scheme. Text color
automatically adjusts when the control is disabled.

<div class="admonition note">

**Note:** Text does not bind to R options. It is a display-only control.

</div>

## Example

``` qml
Text { text: qsTr("Note: values are log-transformed.") }
```
