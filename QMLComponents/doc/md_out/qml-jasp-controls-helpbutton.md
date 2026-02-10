<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

HelpButton

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

# HelpButton QML Type

A small info button that opens a help page for the current analysis.
[More...](#details)

<div class="table">

|                   |                                             |
|-------------------|---------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                  |
| Inherits:         | <a href="qml-jasp-controls-menubutton.md" 
                     translate="no">MenuButton</a>                |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-helpbutton-members.md)

## Detailed Description

Extends <a href="qml-jasp-controls-menubutton.md"
translate="no">MenuButton</a> with an info icon. When clicked, opens or
toggles the specified help page in the JASP help panel. Only works
within a Form.

<div class="admonition note">

**Note:** HelpButton does not bind to R options. It is a UI-only
control.

</div>

## Properties

- **helpPage** (string) - Name of the help page to display. Default: "".

## Example

``` qml
HelpButton { helpPage: "anova" }
```
