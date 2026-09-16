# ScriptConstructor Review — Uitvoerplan

Branch `filterConstructorCPP` t.o.v. `upstream/development` (merge-base `aa30860`).
Dit bestand is de source of truth; vink items af (`[ ]` → `[x]`) en noteer afwijkingen
onder "Log". Regelnummers hieronder zijn een indicatie — code wijkt per fase af, zoek op
symbool/tekst.

## Build & verifieer (geldt na ELKE fase)

```bash
cmake --build build   # of eerst: cmake build/  (GLOB: na file add/remove ALTID opnieuw)
cmake --build build --target JASPTest JASPTestEngine JASPTestDebugData JASPTestCsvPrev JASPQuickTest JASPTestColumnEncoderContext
xvfb-run build/Tests/JASPTest
xvfb-run build/Tests/JASPTestEngine
xvfb-run build/Tests/JASPTestDebugData
xvfb-run build/Tests/JASPTestCsvPrev
QT_QPA_PLATFORM=offscreen xvfb-run build/Tests/JASPQuickTest
xvfb-run build/Tests/JASPTestColumnEncoderContext
```

`JASPTestEngine` heeft een R-engine nodig (duurt lang, `testScriptConstructorFuzz` draait 40
R-cases). Fases 1 t/m 4 raken alleen model/view-code; draai JASPTestEngine minimaal na fase 1
(gouden R-tests) en na de laatste fase.

Commits per fase, report-stijl zoals bestaand (`ScriptConstructor: ...`). **Nooit pushen.**

## Betrokken bestanden

- `CommonData/scriptnode.{h,cpp}`, `CommonData/scriptconstructormodel.{h,cpp}`,
  `CommonData/scriptconstructorregistry.{h,cpp}`
- `Desktop/qquick/scriptconstructorview.{h,cpp}`, `Desktop/qquick/scriptnodeitem.{h,cpp}`
- `Desktop/components/JASP/Widgets/FilterWindow.qml`, `ComputeColumnWindow.qml`, `DataSetTabButton.qml`
- `Desktop/mainwindow.cpp` (qmlRegisterType)
- `Tests/testall.{h,cpp}`, `Tests/qmlTests/tst_scriptconstructor.qml`, `Tests/testengine.{h,cpp}`
- Nieuw: `CommonData/scriptnodeenums.cpp`; te verwijderen: `Desktop/components/JASP/Widgets/FilterConstructor/` (26 bestanden)

## Referenties gedrag oud QML (is verdampt, voor semantiek)

- Oude `FilterConstructor.qml` checkAndApplyFilter: `allCorrect && allBoolean`, GEEN count-check
  (meerdere formules OK, join met `&`).
- Oude `ComputedColumnsConstructor.qml`: `onlyOneFormula = children.length === 1`, boodschap
  "Only one formula per computed column allowed." alleen bij `!onlyOneFormula && !noFormulas`.
- Besloten: nieuwe semantiek = filter: `complete && allBoolean` (elk aantal formules);
  column: `complete && formulaCount() <= 1` (LEEG STAAT TOE, past dan lege code toe).
- `toR()` old filter: per formule `& ` prefix (i>0) + `"\n"` per regel; column idem maar geen
  laatste `\n`. Huidige implementatie klopt — niet aan zitten.

## Fase 1 — Bugfixes

- [x] 1.1 `Desktop/qquick/scriptconstructorview.cpp` `checkAndApply()`: de `oneFormula`-check is
  omgekeerd. Nu: `bool oneFormula = !isFilter || _model.formulaCount() <= 1;` → moet
  `isFilter || _model.formulaCount() <= 1`. Bericht "Only one formula per computed column
  allowed." mag alleen in column-modus verschijnen.
- [x] 1.2 Lege constructor: "Filter cleared"/"Filter applied" alleen in filter-modus; in
  column-modus de oude tekst "Computed columns code clear(ed)" resp. "Computed columns code
  applied" gebruiken (zie oude ComputedColumnsConstructor).
- [x] 1.3 Hint-opbouw: mix van `"\n"` en `"<br>"`-separatoren uniformeren (één stijl, `\n`).
- [x] 1.4 `componentComplete()`: het singleton-connectblok (~15 regels `connect(singleton, ...)`
  + de twee no-op aanroepen `buildColumnPalette()`/`rebuildFormulaItems()` met commentaar
  "No-op until the chrome exists") vervangen door `setColumnsModel(ColumnsModel::singleton())`
  wanneer `!_columnsModel`. Daarmee verdwijnt ook het dubbele connect-risico.
- [x] 1.5 `updateBackgroundDecoration()`: null-check op `JaspTheme::currentTheme()` (nu
  ongecheckte deref; patroon elders: `theme ? theme->iconPath() : QString()`).
- [x] 1.6 Nieuwe tests in `Tests/qmlTests/tst_scriptconstructor.qml`:
  - filter-modus, 2 formules → `checkAndApply()` == true;
  - ComputedColumn-modus, 2 formules → `checkAndApply()` == false;
  - ComputedColumn-modus, 0 formules → true (leeg staat toe, applyRequested met lege rCode).
- [x] 1.7 Build + JASPTest + JASPQuickTest (+ tst_scriptconstructor via JASPQuickTest) groen;
  commit `ScriptConstructor: fix inverted only-one-formula check, singleton connects and theme null-check`.

## Fase 2 — enumutilities

- [x] 2.1 `CommonData/scriptnode.h`: `ScriptNode::Type` (nu handgeschreven enum class in de
  class) → `DECLARE_ENUM(ScriptNodeType, Operator, OperatorVertical, Function, RowFunction,
  Column, Number, Boolean, String)` op file-scope (na include `enumutilities.h`), met in
  `ScriptNode`: `using Type = ScriptNodeType;` (alle `ScriptNode::Type::X` call-sites blijven
  werken). JSON-stringnamen = enumeratorenamen (kloppen nu al).
- [x] 2.2 Nieuw `CommonData/scriptnodeenums.cpp`: `#define ENUM_DECLARATION_CPP` +
  `#include "scriptnode.h"` (patroon: `CommonData/dataenums.cpp`).
- [x] 2.3 `CommonData/scriptnode.cpp`: `nodeTypeString()` → `ScriptNodeTypeToString(type())`;
  `typeFromString()` → `ScriptNodeTypeFromString()` (gooit `missingEnumVal`, gedrag equals oude
  runtime_error — check of tests/roepers daar niet op tekenen); `fromJson()` if/else-keten over
  nodeType-string → `switch (ScriptNodeTypeFromString(...))` (onbekende/lege string moet nog
  steeds nullptr teruggeven i.p.v. gooien: `ScriptNodeTypeFromString(str, default)` of try/catch,
  lege string != "Number"…). Let op: default-waarde-truc van enumutilities: gebruik
  `if(!ScriptNodeTypeValidName(nodeType)) return nullptr;` vóór de switch.
- [x] 2.4 `toJson()`-consistentie: `ScriptNodeFunction` (`json["nodeType"] = "Function"`),
  `ScriptNodeRowFunction`, `ScriptNodeColumn` → `nodeTypeString()` (of direct
  `ScriptNodeTypeToString(type())`).
- [x] 2.5 `CommonData/scriptconstructorregistry.h`: `enum class ScriptConstructorMode {...}` →
  `DECLARE_ENUM(ScriptConstructorMode, Filter, ComputedColumn, ComputedDataSet)` +
  `Q_NAMESPACE`-registratie: in een header in CommonData `Q_NAMESPACE` (bijv. nieuwe
  `namespace ScriptConstructorEnums { Q_NAMESPACE Q_ENUM_NS(...) }` OF op bestaande patroon
  `CommonData/dataenums`—kijk hoe `columnType` e.d. QML-bereikbaar zijn; indien geen Q_ENUM_NS
  bestaat voor CommonData-enums: voeg `Q_NAMESPACE` + `Q_ENUM_NS(ScriptConstructorMode)` toe in
  de registry-header zelf). Registreer in `Desktop/mainwindow.cpp` via
  `qmlRegisterUncreatableMetaObject(<metaObject>, "JASP", 1, 0, "ScriptConstructorMode", ...)`
  (ook in `Tests/testqml.cpp` naast de bestaande qmlRegisterType).
- [x] 2.6 View: `enum Mode` + `Q_ENUM(Mode)` + `modeInt()/setModeInt()` VERWIJDEREN;
  `Q_PROPERTY(ScriptConstructorMode mode READ mode WRITE setMode NOTIFY modeChanged)`.
  QML call-sites: `mode: ScriptConstructor.Filter` → `mode: ScriptConstructorMode.Filter`
  (`FilterWindow.qml`, `tst_scriptconstructor.qml`, `ComputeColumnWindow.qml`).
  Fallback indien Q_ENUM_NS niet werkt: view-`Mode` houden + static_assert + range-check.
- [x] 2.7 Alle 6 test-executables bouwen + draaien groen; commit
  `ScriptConstructor: use enumutilities DECLARE_ENUM for ScriptNodeType and ScriptConstructorMode`.

## Fase 3 — Ontdubbelen

- [x] 3.1 `ScriptNode` virtualen: `virtual int slotCount() const`,
  `virtual stringvec slotDropKeys(int slot) const` (operator: 0=left 1=right; function:
  argument dropKeys; rowfunction: `{"number"}`; leaf: leeg),
  `virtual void setSlot(int slot, ScriptNode *node)` (setParent + opslaan; operator left/right,
  func argument, rowfunc kind). `DropTarget::Kind` blijft zoals het is (tests bouwen aggregates).
  Hierdoor kunnen in `scriptconstructormodel.cpp` de 5× `dynamic_cast`-triades
  (`detachFromParent`, `placeAt`, `containingSlotKeys`, `leftMostEmptyDropSpotRec`,
  `rightMostFilledDropSpotRec`) naar virtuele dispatch (Kind bepaalt alleen nog root vs slot;
  OperatorLeft/Right mappen op slot 0/1, index-veld voor func/rowfunc).
  NB: `setSlot` voor RowFunctionArg moet NIET `ensureTrailingEmptySlot()` doen (die blijft in
  `placeAt` zoals nu).
- [x] 3.2 `virtual ScriptNode * cloneEmpty() const` op elke node-klasse (clone zonder children;
  rowfunction: met één null-slot zoals `clonePrototype` nu doet);
  `scriptconstructorview.cpp` `clonePrototype()` schrappen, `spawnFromPrototype` gebruikt
  `proto->cloneEmpty()`.
- [x] 3.3 Registry: `ScriptOperatorDef::toolTipForMode` en `ScriptFunctionDef::toolTipForMode`
  zijn identiek → ene vrije helper in de cpp (`static QString logicalSuffixToolTip(...)`), beide
  methods 1-liner. `dropKeysLeft/dropKeysRight` hebben 4 identieke regels + 1 verschil → ene
  private helper met side-flag.
- [x] 3.4 View: helper `static bool isInsideOrSame(ScriptNodeItem*, QQuickItem *start)` voor de
  2× gekopieerde insideDragged-loop; `collectDropSpots` const-maken en beide `const_cast`s weg.
- [x] 3.5 View: palette-item-bouw (5×: `placeOperator`, `placeFunction`, de 2 lussen in
  `buildFunctionPalette`, lus in `buildColumnPalette`) → ene helper
  `ScriptNodeItem * addPrototypeItem(ScriptNode *proto, QQuickItem *content)`; positionering
  (x/y-flow + maxW/maxH) via kleine locals of retour-waarden.
- [x] 3.6 `scriptnodeitem.cpp`: helper `QFont viewFont(bool bold=false)` (patroon
  `theme->font(); setPixelSize(fontPixelSize())` staat 7×); `ScriptDropSpot` default-maten
  `blockDim()*3 : 60` / `blockDim() : 20` (3×) → `QSizeF defaultSpotSize() const`.
- [x] 3.7 `ScriptNodeItem::rebuild()`: Number/String-literal-blok (vrijwel identiek) → ene
  lambda `makeLiteralInput(text)`; boolean-blok blijft apart.
- [x] 3.8 `ScriptNodeItem::layout()`: de twee nagenoeg identieke leaf-lussen + de inline
  placeNext-variant (operator-visual met width-precedence) → één helper
  `placeLeaf(QQuickItem *item, qreal &x, qreal &maxH, qreal block, qreal spacing, bool preferWidth)`.
- [x] 3.9 `shouldDrag(qreal x, qreal)` → parameterloze y schrappen (signature + call);
  `(void)op;` (`layout()`) weg.
- [x] 3.10 Rowfunctie-dropkey `{"number"}` (5 plekken: model `containingSlotKeys`,
  `leftMostEmptyDropSpotRec` ×2, `rightMostFilledDropSpotRec`, `scriptnodeitem.cpp` makeDropSpot,
  `ScriptNodeRowFunction::dragKeys`) → ene gedeelde constante (bv.
  `ScriptConstructorRegistry::rowFunctionKeys()` of `inline const stringvec` in registry header).
- [x] 3.11 Magie `1` = scale defaults → `int(columnType::scale)`
  (`scriptconstructorview.cpp columnType()` fallbacks ×2 + `columnTransformedPreview`,
  `scriptconstructormodel.cpp resolveColumnTypeDrop`, `scriptnodeitem.cpp rebuild Column`).
  Type-cycling `cur < 1 || cur >= 3` in `mousePressEvent` → columnType enums.
- [x] 3.12 Alle tests groen; commit.

## Fase 4 — Opschonen

- [x] 4.1 `git rm -r Desktop/components/JASP/Widgets/FilterConstructor/` (26 dode bestanden) en
  de `import "FilterConstructor"`-regels uit `FilterWindow.qml`, `ComputeColumnWindow.qml`,
  `DataSetTabButton.qml` (geen type-uses meer; wel controleren: geen enkele actieve QML referent
  meer naar types uit die map — `grep -rn "FilterConstructor" Desktop/components | grep -v FilterConstructor/`).
  Let op: `cmake build/` opnieuw (QML wordt gebundld via GLOB/dir).
- [x] 4.2 `Tests/testall.cpp`: dubbele `#include "data/asyncloader.h"` → ene.
- [x] 4.3 `desiredMinimumHeightChanged` emit: cached laatste waarde; emit in `layoutAll()` bij
  wijziging (property is READ-only).
- [x] 4.4 `ScriptTrashItem::debugPressCount/debugDoubleClickCount`: achter `#ifdef
  JASP_TESTHOOKS` (test-targets via target_compile_definitions in `Tests/CMakeLists.txt`), of —
  indien te gedoe — publiek laten maar dan nette comment + eigen header-sectie. Tests
  (`testall.cpp` uses ze) passen.
- [x] 4.5 Commentaar `scriptnodeitem.cpp layout()` ("...preserved by construction (leaves and
  spots appended in visual order is not guaranteed)...") herschrijven tot 2 heldere regels.
- [x] 4.6 `ScriptConstructorModel::toR()`: if/else-if `\n`-logica → `if(_mode ==
  ScriptConstructorMode::Filter || i < size-1) out += "\n";`.
- [x] 4.7 `RuntimeTimerMeasure` (view cpp, `#ifdef PROFILE_JASP`) → verplaatsen naar
  `Common/timers.h` als `_JaspRuntimeTimerScopeMeasure`.
- [x] 4.8 Trailing whitespace in de aangepaste QML-regels (`ComputeColumnWindow.qml`
  `deferUntilVisible`-blok) opkuisen.
- [x] 4.9 Drag-hover: `dragMove()` `accepted` is altijd true (bestDropSpotFor filtert al op
  accepts) — `setHoverState(true)` volstaat; `setHoverState(bool hovered, bool accepted)`
  Vereenvoudigen naar state-enum? Minimaal: dode `accepted`-berekening weg.
- [x] 4.10 Alle tests groen; commit.

## Fase 5 — Data-driven registry + native QtQuick items

- [x] 5.1 `ScriptFunctionDef` extra velden: `stringvec dragKeys` (data i.p.v. naam-switch),
  `bool naRm` (was `addsNaRm()` naam-set), `bool radix` (sqrt), `bool parensSingleArg`
  (nest-regel `childCount()==1 && != "abs"` → `funcDef->parensSingleArg` of equivalent).
  `dragKeys()`/`addsNaRm()` worden member-reads. `scriptnodeitem.cpp`: `functionName()=="sqrt"`
  (2×) en `!= "abs"` → velden via `funcDef`.
- [x] 5.2 `ScriptOperatorDef`: left/right dropkeys als data-velden (default per operator in de
  tabel), `booleanResult`-veld; alleen `%|%` mode-afhankelijkheid blijft een kleine functie
  (left: filter→boolean else number; right: `{"string","boolean"}`). `dragKeys(mode)` =
  booleanResult(mode) ? {"boolean"} : {"number"}.
- [x] 5.3 Operator-bar volgorde data-driven: registry-veld `barAfter` (string: na welke op dit
  item ingevoegd wordt; sqrt: `barAfter="^"`, `!`: laatste) i.p.v. `if(def.op=="^")` + los
  `placeFunction("!")` in `buildOperatorBar`.
- [x] 5.4 Native items: `QQuickText`, `QQuickRectangle`, `QQuickTextInput` direct construeren;
  voor images een mini-`ScriptImage : QQuickImage` (ctor: smooth, asynchronous,
  fillMode=PreserveAspectFit; geometryChange: sourceSize = 2× size voor de watermark; voor
  node-icons blijven width/height/implicit fixed zoals nu). `QCheckBox` (header
  `<QtQuickControls2/QCheckBox>` of `<QtQuickControls2/qquickcheckbox.h>`; Qt::QuickControls2 is al
  gelinkt) met `toggled()`-connect. De 5 QQmlComponent-members + lazy makers + `newLeaf` +
  `RuntimeTimerMeasure`-gebruik vervallen (of `newLeaf`-overload voor geen-kind). Property-writes
  (`fillMode, 1`, `wrapMode, 4`, `horizontalAlignment, 4`) worden type-safe setters.
  RISICO: als QCheckBox uit C++ niet rendert/stijlt, alleen checkbox op inline QML houden en dat
  hier documenteren. → NATIEF gelukt, GEEN fallback nodig (zie Log).
- [x] 5.5 `columnTransformedPreview`: `switch` met `default:`-eerst → expliciete cases.
- [x] 5.6 DropTarget raw-pointer invariant in een comment bij `struct DropTarget`
  (alleen geldig tot volgende rebuild).
- [x] 5.7 Alle 6 test-executables (incl. JASPTestEngine fuzz) groen; commit
  `ScriptConstructor: data-driven registry, native QtQuick leaf items`.

## Log

- (fase-status wordt bijgehouden door de uitvoerende subagents)
- Fase 1: JASPQuickTest 74 passed / 0 failed (incl. 9 TestScriptConstructor-tests: filter 2 formules
  → true, computed-column 2 formules → false, computed-column leeg → true met lege rCode).
  JASPTest breekt (SIGABRT, assert `dataSetId > -1` in `databaseinterface.cpp:1342`) in
  `testJaspDataImport(debug-0.18.3.jasp)` en 2× `testDataImport` (CSV/TSV "Hardcoded json is
  different!") — pre-existing, identiek bewezen met `git stash`-baseline van alleen de 2 wijzigde
  bestanden. Afwijking: volledige JASPTest-suite daardoor niet groen draaibaar (crash stopt run);
  overige 10   testDataImport-rows slagen. JASPTestEngine: 8 passed / 0 failed (incl.
  `testScriptConstructorFuzz`).
- Fase 2: Q_ENUM_NS-weg gebruikt (GEEN fallback): `DECLARE_ENUM(ScriptConstructorMode, ...)` in
  `namespace ScriptConstructorEnums` + `Q_NAMESPACE`/`Q_ENUM_NS` in de registry-header (let op:
  volgorde `Q_NAMESPACE` vóór `Q_ENUM_NS`, anders Qt6 compilefout); global `using`-alias houdt alle
  bestaande call-sites intact. Qt6-moc expandeert `DECLARE_ENUM` uit de include, dus de enum-keywords
  worden correct geparset. Registratie `qmlRegisterUncreatableMetaObject(ScriptConstructorEnums::staticMetaObject,
  "JASP", 1, 0, "ScriptConstructorMode", ...)` in mainwindow.cpp + testqml.cpp; QML gebruikt nu
  `ScriptConstructorMode.Filter` / `.ComputedColumn` (flat-toegang onder de geregistreerde naam bevestigd).
  Enum-impl (ENUM_DECLARATION_CPP) leeft in `CommonData/scriptnodeenums.cpp` (dekt via de transitieve
  include van scriptconstructorregistry.h ook ScriptConstructorMode af — registry.cpp dus gewoon laten).
  Tests: 9 ScriptConstructor-slots individief groen (3 passed elk); JASPQuickTest 74/0 (incl. alle
  TestScriptConstructor — bewijst QML-enumregistratie); JASPTestEngine 8/0 (incl. fuzz); DebugData 15/0,
  CsvPrev 8/0, ColumnEncoderContext 5/0; testSyncerStartStopFileSyncing 3/0. Afwijking: bij snel
  achter elkaar draaien van JASPTest onder xvfb-run geeft ~elke tweede run een SIGABRT vóór de QTest-banner
  (`QMessageLogger::fatal` in `QGuiApplicationPrivate::createEventDispatcher` — X-display-connectionrace,
  core backtrace via coredumpctl); pre-existing, identiek reproduceerbaar op de gestashte baseline, en
  verdwijnt met korte pauzes tussen runs.
- Fase 3: virtuele slot-API (`slotCount/slotDropKeys/setSlot/cloneEmpty`) op ScriptNode; de 5 dynamic_cast-triades
  in `scriptconstructormodel.cpp` (detach/placeAt/containingSlotKeys/leftMostEmpty/rightMostFilled) zijn nu generiek.
  Bewuste behoud-keuze: `rightMostFilledDropSpotRec` houdt voor operatoren de rechts-only-regel expliciet (generiek
  "eerste lege slot beëindigt de keten" zou gedrag veranderen bij een operator met lege linker- en gevulde
  rechterkind); `placeAt` roept `ensureTrailingEmptySlot()` ook bij een out-of-range RowFunctionArg-index aan (oud
  gedrag), en de operator-slot wordt uit de Kind afgeleid (0/1) i.p.v. uit het index-veld. Afwijking 3.4: helper heet
  `itemHasAncestor(QQuickItem*, QQuickItem*)` (geen ScriptNodeItem nodig; both call-sites liepen tegen de parentItem-ketting
  aan); `collectDropSpots` was al const — alleen de twee `const_cast`s verwijderd. Afwijking 3.11: in de view moet
  `int(::columnType::scale)` geschreven worden want de memberfunctie `columnType()` verbergt de enum.
  Tests: 9 ScriptConstructor-slots + testMainWindowShowsFilterWindow elk 3/0; JASPQuickTest 74/0;
  JASPTestEngine 8/0 (incl. fuzz); DebugData 15/0 (1× xvfb-race, retry groen); CsvPrev 8/0; ColumnEncoderContext 5/0.
- Fase 4: `git rm -r` van `Desktop/components/JASP/Widgets/FilterConstructor/` (26 bestanden) + de dode
  `import "FilterConstructor"`-regels; let op: die import stond ook in `FilterWindowTabButton.qml` (4e bestand,
  plan noemde er 3). Rest-Grep geeft alleen nog het `easyFilterConstructor`-id (geen module-types meer).
  Afwijking 4.4: de tellers ín de header `#ifdef`en betekent dat JASPDesktopLib (scriptnodeitem.cpp) mét dezelfde
  vlag gebouwd móét worden, anders wijkt de class-layout af tussen bibliotheek en test (ODR); daarom
  `if(BUILD_TESTS) target_compile_definitions(JASPDesktopLib PUBLIC JASP_TESTHOOKS)` in Desktop/CMakeLists.txt,
  plus de `PRIVATE JASP_TESTHOOKS` op JASPTest in Tests/CMakeLists.txt zoals voorgeschreven.
  Afwijking 4.7: `RuntimeTimerMeasure` was een anonieme-namespace struct met resume() zónder try/catch in de ctor;
  de verplaatsde `_JaspRuntimeTimerScopeMeasureC` volgt het `_JaspTimerScopeMeasure`-patroon (try/catch in ctor+dtor).
  4.9: `setHoverState(bool accepted)`-parameter verwijderd (hover is altijd groen; rood loopt via setError).
  Tests: idem als fase 3 — 10 JASPTest-slots elk 3/0; JASPQuickTest 74/0; JASPTestEngine 8/0 (incl. fuzz);
  DebugData 15/0 en CsvPrev 8/0 (elk 1× xvfb-race, retry groen); ColumnEncoderContext 5/0; JASP-app target bouwt mee.
- Fase 5: registry volledig data-driven: `ScriptFunctionDef` kreeg `dragKeysData`/`naRm`/`radix`/
  `parensSingleArg` (default true, alleen `abs` false)/`barAfter`, `ScriptOperatorDef` per-side/per-mode
  dropkey-vectors + `booleanResultFilter/Column` + `keysMirrored` (was de naam-check van `mirrorKeys()`,
  die methode is overigens ongebruikt); `dragKeys()/addsNaRm()/dropKeysLeft/Right(mode)/returnsBoolean(mode)`
  zijn nu triviaal. De addOp/addFunc/addRowFunc-tabel gebruikt C++20 designated initializers (geen
  positieele bools meer); addOp vult lege mode/side-vectors uit de andere (default `{"number"}`), alleen
  `%|%` zet alle vier expliciet. Operator-bar: `buildOperatorBar` plaatst elke `operatorBarOnly`-functie
  achter `barAfter` (sqrt→`^`), lege `barAfter` na de lijst (in registratevolgorde: `!`); visuele volgorde
  identiek. Native QtQuick leaves: GEEN QQmlComponent/incubatie meer. AFWIJKING headers: op deze Qt 6.11
  bestaan `<QtQuick/qquicktext.h>`/`<QtQuickControls2/QCheckBox>` NIET (QQuickText/Image/Rectangle/TextInput
  en QQuickCheckBox zijn private classes) → `#include <QtQuick/private/qquick{text,rectangle,textinput}_p.h>`,
  `qquickimage_p.h` (via `ScriptImage` in `scriptnodeitem.h`) en
  `<QtQuickTemplates2/private/qquickcheckbox_p.h>`; daarvoor `QuickPrivate` + `QuickTemplates2Private`
  toegevoegd aan `find_package` (Tools/CMake/Libraries.cmake, beide takken) en aan de link van
  JASPDesktopLib (PUBLIC, dus test-targets en JASP erven het). AFWIJKING setters: QQuickText heet hier
  `setVAlign`/`setHAlign` (niet setVerticalAlignment/setHorizontalAlignment). Magie-cijfers ontrafeld:
  `fillMode 1`==PreserveAspectFit ✓, `wrapMode 4`==`QQuickText::Wrap` (commentaar "WordWrap" was onjuist) ✓,
  `verticalAlignment 128`==AlignVCenter ✓, `horizontalAlignment 4`==AlignHCenter (AlignRight is 0x2 — het
  oorspronkelijke commentaar klopte; typed gezet naar `AlignHCenter`, gedrag identiek).
  `smooth: true` uit de oude inline-QML Image vervallen: dat property bestaat in Qt 6 niet meer (de write was
  een no-op-warning). `ScriptImage` (in scriptnodeitem.h/.cpp, geen nieuwe bestanden): ctor asynchronous +
  PreserveAspectFit, `geometryChange` houdt `sourceSize = qRound(2×size)` (repliceert de oude binding; voor
  icons immaterieel zoals toen); implicitWidth/Height-changed-connects voor de watermark blijven werken.
  Border van de drop-marker via `rect->border()->setWidth/setColor` (QQuickPen) i.p.v. QQmlProperty.
  QCheckBox: NATIEF, geen fallback — bewijs: tijdelijke QML-test die een Boolean-literal door de view laat
  renderen (windowShown, offscreen) PASSDE (run: 75/0), daarna teruggezet → suite weer 74/0; geen style-
  of renderwarnings. `newLeaf` + de 5 lazy makers + het `PROFILE_JASP`-timergebruik daarin zijn weg
  (`_JaspRuntimeTimerScopeMeasureC` blijft in Common/timers.h); `columnTransformedPreview` heeft nu
  expliciete scale/ordinal/nominal-cases + default=scale; `struct DropTarget` kreeg de raw-pointer-
  geldigheidcomment. `source`-writes zijn `QUrl(string)` (zelfde conversie als de oude property-write;
  iconPath is een `qrc:/`-prefix, dus juist géén fromLocalFile).
  Tests: 10 JASPTest-slots elk 3/0 (individueel, i.v.m. pre-existing testJaspDataImport-crash);
  JASPQuickTest 74/0; JASPTestEngine 8/0 (incl. 40-R-case fuzz); DebugData 15/0; CsvPrev 8/0;
  ColumnEncoderContext 5/0; JASP-app target bouwt mee. Nooit gepusht.
- Eindverificatie (na alle fases, hoofdagent): volledige build incl. `JASP`-app-target groen; alle suites
  onafhankelijk hernomen: JASPQuickTest 74/0; de 10 JASPTest-slots elk 3/0; JASPTestEngine 8/0 (incl. fuzz);
  DebugData 15/0; CsvPrev 8/0; ColumnEncoderContext 5/0. Nazicht fase 5: dubbele link-entry
  `Qt::QuickTemplates2` in Desktop/CMakeLists.txt verwijderd (stond er al); Log-notitie over
  `horizontalAlignment 4` gecorrigeerd (4 is AlignHCenter, niet AlignRight — gedrag was en is identiek).
