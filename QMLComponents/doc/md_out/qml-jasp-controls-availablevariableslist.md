<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

AvailableVariablesList

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [Inherited Properties from
  VariablesList](#inherited-properties-from-variableslist)
- [Other Inherited Properties](#other-inherited-properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# AvailableVariablesList QML Type

The source list showing all available dataset variables.
[More...](#details)

<div class="table">

|                   |                                                |
|-------------------|------------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                     |
| Inherits:         | <a href="qml-jasp-controls-variableslist.md" 
                     translate="no">VariablesList</a>                |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-availablevariableslist-members.md)

## Detailed Description

Extends <a href="qml-jasp-controls-variableslist.md"
translate="no">VariablesList</a> configured as the source (available)
list in a <a href="qml-jasp-controls-variablesform.md"
translate="no">VariablesForm</a>. This control is not bound to R
options. It displays all dataset variables from which users can drag
variables into assigned lists.

<div class="admonition note">

**Note:** AvailableVariablesList does not bind to R options. It is
automatically managed by <a href="qml-jasp-controls-variablesform.md"
translate="no">VariablesForm</a>.

</div>

## Inherited Properties from VariablesList

- **title** (string) - Title displayed above the list. Alias: label.
  Default: "".

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
VariablesForm {
    AvailableVariablesList {
        name: "allVariables"
    }
    AssignedVariablesList {
        name: "dependent"
        singleVariable: true
    }
}
```
