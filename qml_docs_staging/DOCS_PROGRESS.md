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

- [x] ALTNavTag.qml
- [x] AddColumnField.qml
- [x] AllowedTypeIcons.qml
- [x] AssignButton.qml
- [x] AssignedPairsVariablesList.qml
- [x] AssignedRepeatedMeasuresCells.qml
- [x] AssignedVariablesList.qml
- [x] AvailableVariablesList.qml
- [x] BasicThreeButtonTableView.qml
- [x] BayesFactorType.qml
- [x] Button.qml
- [x] CIField.qml
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

## Style Guide (v3 - Simplified for Users)

This style guide defines the format for QDoc comments embedded in QML source files.
The documentation is written for **module developers** who use JASP controls in their analysis forms.

### Key Principles

1. **No internal C++ details** — Do not expose `BoundControlBase`, `CheckBoxBase`, or other C++ class names. Users only need to know the QML API.
2. **No `Serialization` line** — Serialization is an internal implementation detail.
3. **Use actual values** — Replace `jaspTheme.*` references with their resolved numeric defaults (e.g., `jaspTheme.textFieldWidth` → `200`, `jaspTheme.rowGroupSpacing` → `5`, `jaspTheme.columnGroupSpacing` → `10`).
4. **Inherited Properties section** — Use the heading `\section1 Inherited Properties` (not "Inherited from JASPControl").
5. **Always include `info`** — The `info` property must appear in every Inherited Properties section. It is the primary mechanism for generating help documentation.

---

### QDoc Comment Template

Use this format inside each `.qml` file, placed **immediately above** the root element:

```qml
/*!
    \qmltype [ComponentName]
    \inqmlmodule JASP.Controls 1.0
    \brief [One-line description.]

    [Optional longer description.]

    \section1 R Binding

    \list
    \li \b{R Type:} [e.g., \c logical, \c character, list or character vector]
    \li \b{Default:} [e.g., \c FALSE, "" , [] (empty array)]
    \endlist

    \section1 Properties

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b [prop] ([type]) - [Description]. Default: [value].
    \endlist

    \section1 Inherited Properties

    \list
    \li \b enabled (bool) - Whether the control is interactive. Default: true.
    \li \b visible (bool) - Whether the control is visible. Default: true.
    \li \b info (string) - Info that will be used by tooltip and to generate the help. Default: "".
    \li \b toolTip (string) - This property overwrite info property, in order to display a simpler tooltip text. Default: "".
    \endlist

    \section1 Signals

    \list
    \li \b [signalName]([params]) - [Description].
    \endlist

    \section1 Example

    \qml
    [ComponentName] {
        name: "myOption"
        label: qsTr("My Label")
    }
    \endqml
*/
```

> [!IMPORTANT]
> For layout-only controls (e.g., `Group`) that do not bind to R options, omit the
> `\section1 R Binding` section entirely. Use a `\note` to indicate the control is layout-only.

---

### Guidelines

1. **Component Name**: Extract from filename (e.g., `CheckBox.qml` → `CheckBox`).
2. **R Binding**: Only include `R Type` and `Default`. Do **not** include `Bound Control` or `Serialization`.
3. **Properties**: List all user-facing QML properties with type, description, and default.
   - Use resolved numeric values instead of `jaspTheme.*` references.
   - Note aliases where helpful (e.g., `title` / `label`).
4. **Inherited Properties**: Always include `enabled`, `visible`, `info`, and `toolTip` with the exact descriptions shown in the template above.
5. **Signals**: List only signals that module developers would connect to.
6. **Examples**: Provide at least one realistic usage example. Show multiple examples when the control has distinct usage patterns (e.g., `DropDown` with static values vs. with `source`).

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
| ✅ Embedded (QDoc) | 17 | CheckBox, DropDown, TextField, VariablesList, Group, ALTNavTag, AddColumnField, AllowedTypeIcons, AssignButton, AssignedPairsVariablesList, AssignedRepeatedMeasuresCells, AssignedVariablesList, AvailableVariablesList, BasicThreeButtonTableView, BayesFactorType, Button, CIField |
| 🔄 In Progress | 0 | |
| ⏳ Pending | 47 | |

> [!TIP]
> MVP complete! 5 core controls now have QDoc-style comments embedded in source.
> QDoc successfully generates HTML with working type links.
