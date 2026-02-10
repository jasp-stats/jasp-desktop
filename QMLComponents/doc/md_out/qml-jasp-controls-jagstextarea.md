<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

JAGSTextArea

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [R Binding](#r-binding)
- [Inherited Properties from
  TextArea](#inherited-properties-from-textarea)
- [Other Inherited Properties](#other-inherited-properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# JAGSTextArea QML Type

A text area preset for writing JAGS model code. [More...](#details)

<div class="table">

|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | <a href="qml-jasp-controls-textarea.md" translate="no">TextArea</a> |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-jagstextarea-members.md)

## Detailed Description

Extends
<a href="qml-jasp-controls-textarea.md" translate="no">TextArea</a>
with the JAGS model text type, line numbers, and R syntax highlighting.
Used in analyses that require user-specified JAGS models.

## R Binding

- **R Type:** `character` (JAGS model string)
- **Default:** ""

## Inherited Properties from TextArea

- **name** (string) - R option name this control binds to. Default: "".
- **text** (string) - Current text content. Default: "".
- **label** (string) - Label displayed above the text area. Default: "".

## Other Inherited Properties

- **enabled** (bool) - Whether the control is interactive. Default:
  true.
- **visible** (bool) - Whether the control is visible. Default: true.
- **info** (string) - Info that will be used by tooltip and to generate
  the help. Default: "".
- **toolTip** (string) - This property overwrite info property, in order
  to display a simpler tooltip text. Default: "".

## Example

``` qml
JAGSTextArea {
    name: "model"
    label: qsTr("JAGS Model")
}
```
