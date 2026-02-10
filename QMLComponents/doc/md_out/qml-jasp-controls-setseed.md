<a href="jasp-controls-qmlmodule.md" translate="no">JASP.Controls</a>

SetSeed

<div class="sidebar">

<div class="toc">

### Contents

- [Detailed Description](#details)
- [R Binding](#r-binding)
- [Example](#example)

</div>

<div id="sidebar-content" class="sidebar-content">

</div>

</div>

# SetSeed QML Type

A preset "Repeatability" group with a seed checkbox and integer field.
[More...](#details)

<div class="table">

|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | <a href="qml-jasp-controls-group.md" translate="no">Group</a> |

</div>

- [List of all members, including inherited
  members](qml-jasp-controls-setseed-members.md)

## Detailed Description

Provides a standardized Group titled "Repeatability" containing a "Set
seed"
<a href="qml-jasp-controls-checkbox.md" translate="no">CheckBox</a>
with an <a href="qml-jasp-controls-integerfield.md"
translate="no">IntegerField</a> for the seed value. Drop this into any
analysis that uses random sampling.

## R Binding

- **R Options:**
  - `setSeed` (bool) — Whether seeding is enabled
  - `seed` (integer) — The seed value

## Example

``` qml
SetSeed {}
```
