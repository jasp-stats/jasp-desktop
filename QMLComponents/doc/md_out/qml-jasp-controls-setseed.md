[JASP.Controls](jasp-controls-qmlmodule.md)

SetSeed


# SetSeed QML Type

A preset "Repeatability" group with a seed checkbox and integer field.


|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | [Group](qml-jasp-controls-group.md) |


- [List of all members, including inherited
  members](qml-jasp-controls-setseed-members.md)

## Detailed Description

Provides a standardized Group titled "Repeatability" containing a "Set
seed"
[CheckBox](qml-jasp-controls-checkbox.md)
with an [IntegerField](qml-jasp-controls-integerfield.md) for the seed value. Drop this into any
analysis that uses random sampling.

## R Binding

- **R Options:**
  - `setSeed` (bool) — Whether seeding is enabled
  - `seed` (integer) — The seed value

## Example

``` qml
SetSeed {}
```
