<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

AssignButton

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [Properties](#properties)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# AssignButton QML Type

Internal arrow button for moving variables between source and target
lists. [More...](#details)

<div class="table">

|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | <a href="qml-jasp-controls-button.md" translate="no">Button</a> |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-assignbutton-members.md)

## Detailed Description

<div class="admonition note">

**Note:** This is an internal UI component used by
<a href="qml-jasp-controls-variablesform.md"
translate="no">VariablesForm</a>. Module developers do not use this
control directly. It renders a left/right arrow button that moves
selected variables between an available variables list and an assigned
variables list.

</div>

## Properties

- **leftSource** (var) - The variables list on the left side.
- **rightSource** (var) - The variables list on the right side.
- **leftToRight** (bool) - Direction of the arrow. Default: true.
