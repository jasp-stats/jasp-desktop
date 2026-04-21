[JASP.Controls](jasp-controls-qmlmodule.md)

AssignButton


# AssignButton QML Type

Internal arrow button for moving variables between source and target
lists. [More...](#details)


|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | [Button](qml-jasp-controls-button.md) |


- [List of all members, including inherited
  members](qml-jasp-controls-assignbutton-members.md)

## Detailed Description


**Note:** This is an internal UI component used by
[VariablesForm](qml-jasp-controls-variablesform.md). Module developers do not use this
control directly. It renders a left/right arrow button that moves
selected variables between an available variables list and an assigned
variables list.


## Properties

- **leftSource** (var) - The variables list on the left side.
- **rightSource** (var) - The variables list on the right side.
- **leftToRight** (bool) - Direction of the arrow. Default: true.
