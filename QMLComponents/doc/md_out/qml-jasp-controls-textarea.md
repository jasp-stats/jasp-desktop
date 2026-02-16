<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

TextArea

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

# TextArea QML Type

A multi-line text input with optional line numbers and syntax
highlighting. [More...](#details)

<div class="table">

|                   |                                               |
|-------------------|-----------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                    |
| Inherited By:     | <a href="qml-jasp-controls-jagstextarea.md" 
                     translate="no">JAGSTextArea</a>                |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-textarea-members.md)

## Detailed Description

Supports multiple text types (default, source, JAGS model, lavaan model)
with corresponding syntax highlighting. Includes Ctrl+Enter to apply,
undo/redo support, and scrollable editing.

## R Binding

- **R Type:** `character` (string)
- **Default:** ""

## Properties

- **name** (string) - R option name this control binds to. Default: "".
- **title** (string) - Label displayed above the text area. Default: "".
- **text** (string) - Current text content. Default: "".
- **textType** (enum) - Type of text (JASP.TextTypeDefault,
  JASP.TextTypeSource, JASP.TextTypeJAGSmodel, JASP.TextTypeLavaan).
  Default: JASP.TextTypeDefault.
- **showLineNumber** (bool) - Show line numbers in the gutter. Default:
  false.
- **wrapMode** (enum) - Text wrapping mode. Default: TextEdit.Wrap.
- **separator** (string) - Separator used to split text into list
  values. Default: "\n".
- **trim** (bool) - Trim whitespace before applying. Default: false.
- **useTabAsSpaces** (bool) - Convert Tab key to spaces. Default: true.
- **placeholderText** (string) - Placeholder text. Default: "".

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
TextArea {
    name: "rCode"
    title: qsTr("R Script")
    textType: JASP.TextTypeSource
    showLineNumber: true
}
```
