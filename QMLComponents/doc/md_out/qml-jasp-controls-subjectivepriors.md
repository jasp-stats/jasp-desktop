[JASP.Controls](jasp-controls-qmlmodule.md)

SubjectivePriors


# SubjectivePriors QML Type

A preset "Prior" section with default and informed prior distribution
options. [More...](#details)


|  |  |
|----|----|
| Import Statement: | `import JASP.Controls 1.0` |
| Inherits: | [Section](qml-jasp-controls-section.md) |


- [List of all members, including inherited
  members](qml-jasp-controls-subjectivepriors-members.md)

## Detailed Description

Extends Section with a complete prior specification panel for Bayesian
analyses. Contains radio buttons for standardized and raw effect sizes,
supporting Cauchy, Normal, t, Uniform, and Half-normal distributions
with configurable parameters (location, scale, mean, std, df).

## R Binding

- **R Options:**
  - `effectSize` (string) — "standardized" or "dienes"
  - `effectSizeStandardized` (string) — "default" or "informative"
  - `defaultStandardizedEffectSize` (string) — distribution name
  - `priorWidth` (numeric) — Cauchy scale parameter
  - Additional distribution parameters (location, scale, mean, std, df)

## Properties

- **informedPriorsEnabled** (bool) - Alias for the enabled state of
  informed priors. Default: true.
- **defaultPriorsChecked** (bool) - Alias for whether default priors are
  checked. Default: true.

## Example

``` qml
SubjectivePriors {}
```
