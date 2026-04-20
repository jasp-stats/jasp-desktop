[JASP.Controls](jasp-controls-qmlmodule.md)

ModelTermsList


# ModelTermsList QML Type

A preset [VariablesList](qml-jasp-controls-variableslist.md) for building ANOVA model terms.


|                   |                                                |
|-------------------|------------------------------------------------|
| Import Statement: | `import JASP.Controls 1.0`                     |
| Inherits:         | [VariablesList](qml-jasp-controls-variableslist.md)                |


- [List of all members, including inherited
  members](qml-jasp-controls-modeltermslist-members.md)

## Detailed Description

Extends [VariablesList](qml-jasp-controls-variableslist.md) with interaction-mode drag-and-drop, an
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
