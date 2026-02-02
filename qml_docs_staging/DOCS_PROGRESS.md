# Documentation Plan

This file tracks the progress of documenting QML components in the JASP Statistical Software project.

**Source Directory:** `QMLComponents/components/JASP/Controls`  
**Total Files to Document:** 64

---

## Phase 0: Context Mapping ✅

> [!NOTE]
> Phase 0 complete. All C++ headers indexed and mappings established.

- [x] **Read Module Definition**: Confirmed `JASP.Controls` module in `QMLComponents/CMakeLists.txt` (Qt6 CMake-based registration, no traditional `qmldir`).
- [x] **Index C++ Headers**: Scanned `QMLComponents/controls/*.h` (18 classes) and `QMLComponents/boundcontrols/*.h` (14 classes).

### C++ Class Hierarchy

```
JASPControl (jaspcontrol.h) ← Base for ALL controls
├── JASPListControl (jasplistcontrol.h) ← Controls with list models
│   ├── ComboBoxBase        → DropDown
│   ├── VariablesListBase   → VariablesList, AssignedVariablesList, etc.
│   ├── TableViewBase       → TableView, SimpleTableView, etc.
│   ├── TextAreaBase        → TextArea, JAGSTextArea
│   ├── ComponentsListBase  → ComponentsList
│   ├── FactorsFormBase     → FactorsForm
│   ├── FactorLevelListBase → FactorLevelList
│   └── InputListBase       → InputListView
├── CheckBoxBase            → CheckBox, Switch
├── RadioButtonsGroupBase   → RadioButtonGroup
├── RadioButtonBase         → RadioButton (child of group)
├── TextInputBase           → TextField, IntegerField, DoubleField, PercentField, etc.
├── SliderBase              → Slider
├── GroupBoxBase            → Group
├── ExpanderButtonBase      → Section
└── VariablesFormBase       → VariablesForm (layout only)
```

### QML → C++ Mapping Table

| QML Component | C++ Base Class | Bound Control | R Type | Notes |
| :------------ | :------------- | :------------ | :----- | :---- |
| **Input Controls** |
| CheckBox.qml | `CheckBoxBase` | Built-in | `logical` | Boolean |
| Switch.qml | `CheckBoxBase` | Built-in | `logical` | Boolean toggle style |
| RadioButtonGroup.qml | `RadioButtonsGroupBase` | Built-in | `character` | String from selected value |
| RadioButton.qml | `RadioButtonBase` | None | — | Child of RadioButtonGroup |
| DropDown.qml | `ComboBoxBase` | Built-in | `character` | String from `currentValue` |
| TextField.qml | `TextInputBase` | Built-in | `character` | Text string |
| IntegerField.qml | `TextInputBase` | Built-in | `integer` | `inputType: IntegerInputType` |
| DoubleField.qml | `TextInputBase` | Built-in | `numeric` | `inputType: NumberInputType` |
| PercentField.qml | `TextInputBase` | Built-in | `numeric` | `inputType: PercentInputType` |
| FormulaField.qml | `TextInputBase` | Built-in | `character` | `inputType: FormulaType` |
| ComputedColumnField.qml | `TextInputBase` | Built-in | `character` | `inputType: ComputedColumnType` |
| AddColumnField.qml | `TextInputBase` | Built-in | `character` | `inputType: AddColumnType` |
| CheckColumnIsFreeOrMineField.qml | `TextInputBase` | Built-in | `character` | `inputType: CheckColumnFreeOrMineType` |
| Slider.qml | `SliderBase` | Built-in | `numeric` | Continuous value |
| **Variable Lists** |
| VariablesList.qml | `VariablesListBase` | `BoundControlTerms` | `list`/`character[]` | Main variable picker |
| AssignedVariablesList.qml | `VariablesListBase` | `BoundControlTerms` | `list` | Preset for assigned |
| AssignedPairsVariablesList.qml | `VariablesListBase` | `BoundControlTerms` | `list` | Paired variables |
| AssignedRepeatedMeasuresCells.qml | `VariablesListBase` | `BoundControlMeasuresCells` | `list` | RM cells |
| AvailableVariablesList.qml | `VariablesListBase` | `BoundControlTerms` | — | Source list (usually unbound) |
| ModelTermsList.qml | `VariablesListBase` | `BoundControlMultiTerms` | `list` | Model terms |
| FactorsList.qml | `VariablesListBase` | `BoundControlTerms` | `list` | Factor selection |
| ContrastsList.qml | `VariablesListBase` | `BoundControlContrastsTableView` | `list` | Contrasts |
| **Tables** |
| TableView.qml | `TableViewBase` | `BoundControlTableView` | `matrix`/`data.frame` | Main table control |
| SimpleTableView.qml | `TableViewBase` | `BoundControlTableView` | `matrix` | Simplified table |
| BasicThreeButtonTableView.qml | `TableViewBase` | `BoundControlTableView` | `matrix` | 3-button variant |
| Chi2TestTableView.qml | `TableViewBase` | `BoundControlTableView` | `matrix` | Chi² input |
| CustomContrastsTableView.qml | `TableViewBase` | `BoundControlContrastsTableView` | `matrix` | Custom contrasts |
| JagsTableView.qml | `TableViewBase` | `BoundControlTableView` | `matrix` | JAGS model input |
| **Text Areas** |
| TextArea.qml | `TextAreaBase` | `BoundControlTextArea` / `BoundControlRLangTextArea` | `character` | Multi-line text; `TextType` enum |
| JAGSTextArea.qml | `TextAreaBase` | `BoundControlJAGSTextArea` | `character` | JAGS syntax highlighting |
| **Complex Lists** |
| ComponentsList.qml | `ComponentsListBase` | Built-in | `list` | Dynamic component array |
| FactorsForm.qml | `FactorsFormBase` | Built-in | `list` | Factor definition form |
| FactorLevelList.qml | `FactorLevelListBase` | Built-in | `list` | Factor/level grid |
| InputListView.qml | `InputListBase` | Built-in | `character[]` | User-input list |
| **Layout Controls** |
| Form.qml | `AnalysisForm` (special) | — | — | Root container |
| Group.qml | `GroupBoxBase` | None | — | Grouping container |
| Section.qml | `ExpanderButtonBase` | None | — | Collapsible section |
| VariablesForm.qml | `VariablesFormBase` | None | — | Available + Assigned layout |
| RowLayout.qml | Qt `RowLayout` wrapper | None | — | Horizontal layout |
| ColumnLayout.qml | Qt `ColumnLayout` wrapper | None | — | Vertical layout |
| GridLayout.qml | Qt `GridLayout` | None | — | Grid layout |
| TabView.qml | Custom | None | — | Tabbed interface |
| Divider.qml | `JASPControl` | None | — | Visual separator |
| **Buttons & UI** |
| Button.qml | `JASPControl` | None | — | Generic button |
| RectangularButton.qml | `JASPControl` | None | — | Styled button |
| RoundedButton.qml | `RectangularButton` wrapper | None | — | Rounded variant |
| MenuButton.qml | `JASPControl` | None | — | Menu trigger |
| CrossButton.qml | `JASPControl` | None | — | Delete/remove button |
| AssignButton.qml | `JASPControl` | None | — | Variable assignment arrow |
| SortMenuButton.qml | `JASPControl` | None | — | Sort dropdown trigger |
| HelpButton.qml | `JASPControl` | None | — | Help icon button |
| **Text & Labels** |
| Label.qml | `JASPControl` | None | — | Static text label |
| Text.qml | `JASPControl` | None | — | Text display |
| **File & Color** |
| FileSelector.qml | `TextInputBase` | Built-in | `character` | File path |
| ColorPalette.qml | `ComboBoxBase` | Built-in | `character` | Color scheme selection |
| **Specialized** |
| BayesFactorType.qml | `RadioButtonsGroupBase` composite | Built-in | `character` | BF₁₀/BF₀₁ selector |
| CIField.qml | `TextInputBase` | Built-in | `numeric` | Confidence interval % |
| SetSeed.qml | Composite | Built-in | `integer` | Random seed + checkbox |
| SubjectivePriors.qml | Composite | Complex | `list` | Prior specification |
| **Internal/Utility** |
| ALTNavTag.qml | `JASPControl` | None | — | Accessibility navigation |
| AllowedTypeIcons.qml | `JASPControl` | None | — | Variable type icons |
| ControlErrorMessage.qml | `JASPControl` | None | — | Error display |
| JASPScrollBar.qml | Qt ScrollBar | None | — | Styled scrollbar |
| ScrollMoreIndicator.qml | `JASPControl` | None | — | Scroll hint indicator |

### Key Enums (from C++)

| Enum | Location | Values |
| :--- | :------- | :----- |
| `TextInputType` | `textinputbase.h` | `IntegerInputType`, `StringInputType`, `NumberInputType`, `PercentInputType`, `DoubleArrayInputType`, `ComputedColumnType`, `AddColumnType`, `CheckColumnFreeOrMineType`, `FormulaType` |
| `TextType` | `textareabase.h` | `TextTypeDefault`, `TextTypeModel`, `TextTypeRcode`, `TextTypeLavaan`, etc. |
| `ListViewType` | `jaspcontrol.h` | `AssignedVariables`, `Interaction`, `AvailableVariables`, `RepeatedMeasures`, `Layers`, `MultiFactor` |
| `ModelType` | `tableviewbase.h` | `Simple`, `MultinomialChi2`, `JAGSDataInput`, `Contrasts`, `Filtered`, `GridInput` |

---

## Progress Checklist

- [ ] ALTNavTag.qml
- [ ] AddColumnField.qml
- [ ] AllowedTypeIcons.qml
- [ ] AssignButton.qml
- [ ] AssignedPairsVariablesList.qml
- [ ] AssignedRepeatedMeasuresCells.qml
- [ ] AssignedVariablesList.qml
- [ ] AvailableVariablesList.qml
- [ ] BasicThreeButtonTableView.qml
- [ ] BayesFactorType.qml
- [ ] Button.qml
- [ ] CIField.qml
- [x] CheckBox.qml
- [ ] CheckColumnIsFreeOrMineField.qml
- [ ] Chi2TestTableView.qml
- [ ] ColorPalette.qml
- [ ] ColumnLayout.qml
- [ ] ComponentsList.qml
- [ ] ComputedColumnField.qml
- [ ] ContrastsList.qml
- [ ] ControlErrorMessage.qml
- [ ] CrossButton.qml
- [ ] CustomContrastsTableView.qml
- [ ] Divider.qml
- [ ] DoubleField.qml
- [x] DropDown.qml
- [ ] FactorLevelList.qml
- [ ] FactorsForm.qml
- [ ] FactorsList.qml
- [ ] FileSelector.qml
- [ ] Form.qml
- [ ] FormulaField.qml
- [ ] GridLayout.qml
- [x] Group.qml
- [ ] HelpButton.qml
- [ ] InputListView.qml
- [ ] IntegerField.qml
- [ ] JAGSTextArea.qml
- [ ] JASPScrollBar.qml
- [ ] JagsTableView.qml
- [ ] Label.qml
- [ ] MenuButton.qml
- [ ] ModelTermsList.qml
- [ ] PercentField.qml
- [ ] RadioButton.qml
- [ ] RadioButtonGroup.qml
- [ ] RectangularButton.qml
- [ ] RoundedButton.qml
- [ ] RowLayout.qml
- [ ] ScrollMoreIndicator.qml
- [ ] Section.qml
- [ ] SetSeed.qml
- [ ] SimpleTableView.qml
- [ ] Slider.qml
- [ ] SortMenuButton.qml
- [ ] SubjectivePriors.qml
- [ ] Switch.qml
- [ ] TabView.qml
- [ ] TableView.qml
- [ ] Text.qml
- [ ] TextArea.qml
- [x] TextField.qml
- [ ] VariablesForm.qml
- [x] VariablesList.qml

---

## Style Guide (v2 - Serialization Aware)

This template captures the critical **QML → C++ → R** data flow in JASP.

### Architecture Overview

```
┌──────────────────┐     ┌─────────────────────┐     ┌──────────────┐
│   QML Control    │────▶│  BoundControl (C++) │────▶│   R Options  │
│  (CheckBox.qml)  │     │   boundValue()      │     │  (JSON→R)    │
└──────────────────┘     └─────────────────────┘     └──────────────┘
        │                         │
        │ `name` property         │ setBoundValue(Json::Value)
        │ binds to R option       │ createJson() → default value
        ▼                         ▼
    options$name              JSON serialization
```

**Key C++ Interface:** `BoundControl` (see `boundcontrols/boundcontrol.h`)
- `createJson()` – Returns the default JSON structure
- `boundValue()` – Returns current JSON value sent to R
- `setBoundValue()` – Updates value when user interacts

---

### Documentation Template

Use this exact structure for each component's `.md` file:

````markdown
# [ComponentName]

**Inherits:** [ParentComponent]  
**Path:** `QMLComponents/components/JASP/Controls/[ComponentName].qml`  
**Valid Parents:** [e.g., `VariablesForm`, `Group`, `Section`, or "Any Layout"]

## Brief

[One sentence description of what this component does from a user/developer perspective.]

## R Data Signature

| Aspect | Details |
| :----- | :------ |
| **Bound Control** | [C++ class, e.g., `CheckBoxBase`, `ComboBoxBase`, `VariablesListBase`] |
| **Serialization** | [e.g., "Standard boolean", "String from `value` property", "Array of variable names"] |
| **R Data Type** | [e.g., `logical`, `character`, `character vector`, `list`] |
| **Default Value** | [e.g., `FALSE`, `""`, `[]`] |

**Example R Access:**
```r
# In your R analysis function:
options$optionName  # Returns: TRUE/FALSE
```

## Properties

| Name | Type | Default | Bound | Description |
| :--- | :--- | :------ | :---- | :---------- |
| `name` | string | `""` | **Yes** | The R option name this control binds to. |
| [prop] | [type] | [default] | [Yes/No] | [Description] |

### Complex Property Notes

*(Include only when a property requires special JSON structure or has non-obvious behavior)*

**`model`** (for DropDown):  
Must be an array of objects with `value` and `label` keys:
```json
[{"value": "pearson", "label": "Pearson"}, {"value": "spearman", "label": "Spearman"}]
```

## Signals

| Signal | Description |
| :----- | :---------- |
| `clicked()` | Emitted when user activates the control. |

## Usage Example

```qml
// Qt6 CMake module - no version number needed (Qt6 style)
import JASP.Controls

[ComponentName] {
    name: "myOption"
    label: qsTr("My Label")
    // Other commonly used properties
}
```
````

---

### Serialization Categories

Reference these when documenting the "R Data Signature" section:

| Category | Bound Control (C++) | R Type | Example Components |
| :------- | :------------------ | :----- | :----------------- |
| **Boolean** | `CheckBoxBase` | `logical` | CheckBox, Switch |
| **Single Value** | `ComboBoxBase`, `TextInputBase` | `character`, `numeric` | DropDown, TextField, IntegerField, DoubleField |
| **Variable Selection** | `VariablesListBase` + `BoundControlTerms` | `character vector` or `list` | VariablesList, AssignedVariablesList |
| **Table Data** | `TableViewBase` + `BoundControlTableView` | `data.frame` or `matrix` | TableView, CustomContrastsTableView |
| **Grouped Options** | `RadioButtonGroupBase` | `character` (selected value) | RadioButtonGroup |
| **Text Block** | `TextAreaBase` + `BoundControlTextArea` | `character` | TextArea, JAGSTextArea |
| **Non-Bound (Layout)** | None | N/A | Group, Section, RowLayout, ColumnLayout |

---

### Guidelines

1. **Component Name**: Extract from filename (e.g., `CheckBox.qml` → `CheckBox`).
2. **Inherits**: Look for the root element in the QML file (e.g., `CheckBoxBase`).
3. **Bound Control**: Search for the corresponding C++ class in `QMLComponents/controls/` or `QMLComponents/boundcontrols/`.
4. **Properties**:
   - Mark `Bound: Yes` if the property affects the JSON sent to R.
   - Include default values when explicitly set in QML.
   - Note aliases (e.g., `label` → `text` → `control.text`).
5. **R Data Signature**: This is the **most important section** for module developers.
   - Explain exactly what R code like `options$myOption` will return.
   - Include the R data type and a concrete example.
6. **Valid Parents**: Note which container types the control is designed to work with.
7. **Inheritance Flattening**: If a component inherits from a JASP control (e.g., `JASPControl`), check the parent's C++ header. Document inherited properties like `toolTip`, `enabled`, `visible`, `info`, and `indent` in the child's documentation so developers know they are available.

---

## QDoc Configuration (Critical)

> [!IMPORTANT]
> QDoc has strict requirements for parsing QML files. Follow these rules exactly.

### Comment Placement (CRITICAL)

QDoc comments must be placed **immediately above** the QML type declaration, NOT before `import` statements.

**✅ CORRECT:**
```qml
import QtQuick
import JASP.Controls

/*!
    \qmltype CheckBox
    \inqmlmodule JASP.Controls 1.0
    \brief A boolean toggle control.
*/
CheckBoxBase {
    // ...
}
```

**❌ WRONG (comments ignored):**
```qml
/*!
    \qmltype CheckBox
    \inqmlmodule JASP.Controls 1.0
*/

import QtQuick        // <-- imports AFTER comment = QDoc ignores the comment
import JASP.Controls

CheckBoxBase {
```

### Required Files

| File | Location | Purpose |
|:-----|:---------|:--------|
| `qmldir` | `components/JASP/Controls/` | Registers all QML types for QDoc resolution |
| `module.qdoc` | `doc/` | Defines `\qmlmodule JASP.Controls 1.0` |
| `jasp_qml.qdocconf` | `doc/` | QDoc configuration with source paths |

### Version Consistency

All version numbers must match across files:

- `qmldir`: `CheckBox 1.0 CheckBox.qml`
- `module.qdoc`: `\qmlmodule JASP.Controls 1.0`  
- QML files: `\inqmlmodule JASP.Controls 1.0`

### Running QDoc

```powershell
cd QMLComponents/doc
C:\Qt\6.10.2\mingw_64\bin\qdoc.exe jasp_qml.qdocconf
```

Output: `html_out/` directory with generated HTML documentation.

### Link Syntax

Use fully-qualified names with the `JASP.Controls::` prefix:
```
\l {JASP.Controls::VariablesList} {VariablesList}
```

---

## Documentation Status

| Status | Count | Notes |
| :----- | :---- | :---- |
| ✅ Embedded (QDoc) | 5 | CheckBox, DropDown, TextField, VariablesList, Group |
| 🔄 In Progress | 0 | |
| ⏳ Pending | 59 | |

> [!TIP]
> MVP complete! 5 core controls now have QDoc-style comments embedded in source.
> QDoc successfully generates HTML with working type links.
