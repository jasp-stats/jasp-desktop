<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

ModelTermsList

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [R Binding](#r-binding)
- [Properties](#properties)
- [Inherited Properties from
  VariablesList](#inherited-properties-from-variableslist)
- [Other Inherited Properties](#other-inherited-properties)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# ModelTermsList QML Type

A preset <a href="qml-jasp-controls-variableslist.md"
translate="no">VariablesList</a> for building ANOVA model terms.
[More...](#details)

<div class="table">

|                   |                                                |
|-------------------|------------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                     |
| Inherits:         | <a href="qml-jasp-controls-variableslist.md" 
                     translate="no">VariablesList</a>                |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-modeltermslist-members.md)

## Detailed Description

Extends <a href="qml-jasp-controls-variableslist.md"
translate="no">VariablesList</a> with interaction-mode drag-and-drop, an
"Add to null model" checkbox per row, and automatic nuisance flagging
based on a source list (e.g. randomFactors).

## R Binding

- **R Type:** list (each element contains component variable names and
  isNuisance flag)
- **Default:** \[\] (empty)

## Properties

- **checkedPerDefault** (string) - Name of the source list whose
  variables should be checked as nuisance by default. Default:
  "randomFactors".

## Inherited Properties from VariablesList

- **name** (string) - R option name this control binds to. Default:
  "modelTerms".
- **title** (string) - Title above the list. Default: "Model Terms".
- **listViewType** (enum) - List interaction mode. Default:
  JASP.Interaction.
- **dropMode** (enum) - How dropped items are inserted. Default:
  JASP.DropInsert.

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
ModelTermsList {
    name: "modelTerms"
}
```
