#include "scriptconstructorview.h"
#include "scriptnodeitem.h"
#include "jasptheme.h"
#include "qutils.h"
#include "data/columnsmodel.h"
#include "variableinfo.h"
#include "timers.h"
#include "log.h"

// QtQuick items are constructed natively (they live in the QtQuick private headers).
#include <QtQuick/private/qquicktext_p.h>
#include <QtQuick/private/qquickrectangle_p.h>

#include <QQuickWindow>
#include <QKeyEvent>
#include <QKeySequence>
#include <QTimer>
#include <QToolTip>
#include <QHoverEvent>
#include <algorithm>

/// Width of the frame drawn around the script area, as in the old QML constructors
/// (rectangularColumnContainer in FilterConstructor.qml / ComputedColumnsConstructor.qml).
static constexpr qreal scriptAreaBorderWidth = 1.0;

ScriptConstructorView::ScriptConstructorView(QQuickItem * parent)
	: QQuickItem(parent)
{
	JASPTIMER_START(ScriptConstructorView ctorToComponentComplete);

	setClip(true);
	setFlag(QQuickItem::ItemIsFocusScope);

	// The view resolves actual column types from the columns model for R generation.
	_model.setColumnTypeProvider(this);

	// Constructor editing is undone locally (Ctrl+Z while focused), independently of the dataset.
	_model.setUndoStack(&_localUndoStack);
	connect(&_localUndoStack, &QUndoStack::canUndoChanged,	this, &ScriptConstructorView::canUndoChanged);
	connect(&_localUndoStack, &QUndoStack::canRedoChanged,	this, &ScriptConstructorView::canRedoChanged);

	connect(&_model, &ScriptConstructorModel::reset,	this, [this](){
		// A reset rebuilds the whole node tree (fromJson deletes the old nodes), so any
		// in-flight drag holds a dangling ScriptNode: abort it before rebuilding.
		cancelDrag();
		rebuildFormulaItems();
	});
	connect(&_model, &ScriptConstructorModel::changed,	this, [this](){
		setSomethingChanged(true);
		emit rCodeChanged(rCode());
		// Keep the generated-R display (computed-column mode) in sync with the model.
		if(_rCodeDisplay && _showGeneratedRCode)
			_rCodeDisplay->setProperty("text", rCode());
	});
}

// -------------------------------------------------------------------------------------
// Q_PROPERTY accessors
// -------------------------------------------------------------------------------------

void ScriptConstructorView::setMode(ScriptConstructorMode newMode)
{
	if(newMode == _model.mode()) return;

	_model.setMode(newMode);

	// The generated-R display only makes sense for computed columns.
	if(_rCodeDisplay)
		_rCodeDisplay->setVisible(_showGeneratedRCode && newMode != ScriptConstructorMode::Filter);

	updateBackgroundDecoration();

	emit modeChanged();

	if(_chromeBuilt)
	{
		// The operator bar, function palette and the formulas' drop spots (e.g. the left side of
		// %|%) are mode-dependent, so rebuild them (currently QML sets `mode` before the deferred
		// build, but never rely on that).
		buildOperatorBar();
		buildColumnPalette();
		buildFunctionPalette();
		rebuildFormulaItems();
	}
}

QString ScriptConstructorView::constructorJson() const
{
	return tq(_model.toString());
}

void ScriptConstructorView::setConstructorJson(const QString & json)
{
	JASPTIMER_SCOPE(ScriptConstructor setConstructorJson);
	std::string s = fq(json);
	if(s == _model.toString())
	{
		// Nothing to reload, but this json is (still) the clean baseline: keep _lastAppliedJson
		// in sync. Otherwise a freshly opened, untouched constructor reports jsonChanged()==true
		// because _lastAppliedJson is still the empty-string default while the (empty) model
		// serialises to DEFAULT_FILTER_JSON ("{\"formulas\":[]}"), so closing would wrongly ask
		// to apply/discard even though the user did nothing.
		_lastAppliedJson = tq(_model.toString());
		setSomethingChanged(false);
		return;
	}

	_localUndoStack.clear();
	_model.fromJson(s);
	_lastAppliedJson = tq(_model.toString());
	setSomethingChanged(false);
	emit constructorJsonChanged();
}

QString ScriptConstructorView::rCode() const
{
	return tq(_model.toR());
}

void ScriptConstructorView::setSomethingChanged(bool v)
{
	if(v == _somethingChanged) return;
	_somethingChanged = v;
	emit somethingChangedChanged();
}

void ScriptConstructorView::setShowGeneratedRCode(bool v)
{
	if(v == _showGeneratedRCode) return;
	_showGeneratedRCode = v;

	if(_rCodeDisplay)
	{
		_rCodeDisplay->setProperty("text", rCode());
		_rCodeDisplay->setVisible(v && _model.mode() != ScriptConstructorMode::Filter);
	}

	emit showGeneratedRCodeChanged();

	if(_chromeBuilt)
		layoutAll();
}

void ScriptConstructorView::setDeferUntilVisible(bool v)
{
	if(v == _deferUntilVisible) return;
	_deferUntilVisible = v;
	emit deferUntilVisibleChanged();
}

void ScriptConstructorView::setColumnsModel(QAbstractItemModel * m)
{
	if(m == _columnsModel) return;

	if(_columnsModel)
		disconnect(_columnsModel, nullptr, this, nullptr);

	_columnsModel = m;

	if(_columnsModel)
	{
		connect(_columnsModel, &QAbstractItemModel::modelReset,				this, [this](){ schedulePaletteRebuild(); });
		connect(_columnsModel, &QAbstractItemModel::rowsInserted,			this, [this](){ schedulePaletteRebuild(); });
		connect(_columnsModel, &QAbstractItemModel::rowsRemoved,			this, [this](){ schedulePaletteRebuild(); });
		connect(_columnsModel, &QAbstractItemModel::dataChanged,			this, [this](){ schedulePaletteRebuild(); });
		connect(_columnsModel, &QAbstractItemModel::headerDataChanged,		this, [this](){ schedulePaletteRebuild(); });
	}

	emit columnsModelChanged();

	if(_chromeBuilt)
		buildColumnPalette();
}

int ScriptConstructorView::columnType(const std::string & columnName) const
{
	JASPTIMER_SCOPE(ScriptConstructor columnType lookup);
	QString wanted = tq(columnName);

	// Fast path: O(1) cache built in rebuildColumnCache().
	if(_columnTypesByName.contains(wanted))
		return _columnTypesByName.value(wanted);

	// Fallback when the model changed without a cache rebuild (should be rare).
	QAbstractItemModel * model = _columnsModel ? _columnsModel : ColumnsModel::singleton();
	if(!model)
		return int(::columnType::scale);

	int nameRole = _nameRole		>= 0 ? _nameRole		: static_cast<int>(model->roleNames().key("columnName"));
	int typeRole = _typeRole		>= 0 ? _typeRole		: static_cast<int>(model->roleNames().key("columnType"));

	for(int r = 0; r < model->rowCount(); r++)
	{
		QModelIndex idx = model->index(r, 0);
		if(model->data(idx, nameRole).toString() == wanted)
		{
			int t = model->data(idx, typeRole).toInt();
			return t > 0 ? t : int(::columnType::scale);
		}
	}

	return int(::columnType::scale);
}

QString ScriptConstructorView::columnDescription(const QString & name) const
{
	if(_columnDescriptionsByName.contains(name))
		return _columnDescriptionsByName.value(name);

	ColumnsModel * cols = ColumnsModel::singleton();
	return cols ? cols->getColumnDescription(name) : QString();
}

QString ScriptConstructorView::columnTransformedPreview(const QString & name, int transformedTo) const
{
	ColumnsModel * cols = ColumnsModel::singleton();
	if(!cols)
		return "";

	int idx = _columnIndexByName.value(name, -1);

	// Very rare (cache stale): fall back to the ColumnsModel's own lookup.
	if(idx < 0)
		return cols->getColumnTransformedToolTip(name, transformedTo);

	::columnType realType	= static_cast<::columnType>(cols->provideInfoAt(varInfoType::VariableType, idx).toInt());
	::columnType chosenType	= static_cast<::columnType>(transformedTo);

	if(chosenType == realType)
		return "";

	varInfoType previewType;

	switch(chosenType)
	{
	case ::columnType::scale:		previewType = varInfoType::PreviewScale;		break;
	case ::columnType::ordinal:		previewType = varInfoType::PreviewOrdinal;	break;
	case ::columnType::nominal:		previewType = varInfoType::PreviewNominal;	break;
	default:						previewType = varInfoType::PreviewScale;		break; // unknown type: treat as scale
	}

	return cols->provideInfoAt(previewType, idx).toString();
}

void ScriptConstructorView::rebuildColumnCache()
{
	JASPTIMER_SCOPE(ScriptConstructor rebuildColumnCache);

	_columnTypesByName.clear();
	_columnIndexByName.clear();
	_columnDescriptionsByName.clear();

	QAbstractItemModel * model = _columnsModel ? _columnsModel : ColumnsModel::singleton();
	if(!model)
		return;

	_nameRole = static_cast<int>(model->roleNames().key("columnName"));
	_typeRole = static_cast<int>(model->roleNames().key("columnType"));

	// Descriptions can only be read in O(1) when the model is (the) ColumnsModel itself;
	// otherwise columnDescription() falls back to the singleton on demand.
	ColumnsModel * cols = qobject_cast<ColumnsModel*>(model);

	int rows = model->rowCount();
	for(int r = 0; r < rows; r++)
	{
		QModelIndex idx = model->index(r, 0);
		QString name = model->data(idx, _nameRole).toString();
		if(name.isEmpty())
			continue;

		int t = model->data(idx, _typeRole).toInt();
		_columnTypesByName[name]   = t > 0 ? t : int(::columnType::scale);
		_columnIndexByName[name]   = r;

		if(cols)
			_columnDescriptionsByName[name] = cols->provideInfoAt(varInfoType::ColumnDescription, r).toString().trimmed();
	}
}

void ScriptConstructorView::schedulePaletteRebuild()
{
	if(!_chromeBuilt || _paletteRebuildScheduled)
		return;

	_paletteRebuildScheduled = true;
	QTimer::singleShot(0, this, [this]()
	{
		_paletteRebuildScheduled = false;
		if(!_chromeBuilt)
			return;
		// Rebuilding the palettes destroys the pressed prototype (the mouse grabber), which
		// would leave a spawned drag dangling: abort any in-flight drag first.
		cancelDrag();
		rebuildColumnCache();
		buildColumnPalette();
	});
}

void ScriptConstructorView::setFilterErrorMsg(const QString & msg)
{
	if(msg == _filterErrorMsg) return;
	_filterErrorMsg = msg;
	emit filterErrorMsgChanged();
	refreshHint();
}

void ScriptConstructorView::setHintText(const QString & text)
{
	if(text == _hintText) return;
	_hintText = text;
	refreshHint();
}

// -------------------------------------------------------------------------------------
// QML-callable API
// -------------------------------------------------------------------------------------

QString ScriptConstructorView::returnFilterJSON() const
{
	return constructorJson();
}

bool ScriptConstructorView::jsonChanged() const
{
	return _model.toString() != _lastAppliedJson;
}

void ScriptConstructorView::initializeFromJSON(const QString & json)
{
	JASPTIMER_SCOPE(ScriptConstructor initializeFromJSON);
	_localUndoStack.clear();
	std::string s = json.isEmpty() ? fq(_lastAppliedJson) : fq(json);
	_model.fromJson(s);
	setSomethingChanged(false);
	rebuildFormulaItems();
}

void ScriptConstructorView::undo()
{
	_localUndoStack.undo();
	syncDirtyFlag();
}

void ScriptConstructorView::redo()
{
	_localUndoStack.redo();
	syncDirtyFlag();
}

void ScriptConstructorView::syncDirtyFlag()
{
	setSomethingChanged(tq(_model.toString()) != _lastAppliedJson);
}

bool ScriptConstructorView::checkAndApply()
{
	setSomethingChanged(false);

	bool complete	= _model.checkCompleteness();
	bool isFilter	= _model.mode() == ScriptConstructorMode::Filter;
	bool booleanOk	= !isFilter || _model.allBoolean();
	// Filter mode joins any number of formulas with `&`, so no count limit there; a computed
	// column is a single expression and therefore allows at most one formula (empty is allowed
	// and applies empty code).
	bool oneFormula	= isFilter || _model.formulaCount() <= 1;

	_lastCheckPassed = complete && booleanOk && oneFormula;
	emit lastCheckPassedChanged();

	// Mark empty required drop spots in red after a failed check (mirrors old iWasChecked behaviour).
	for(auto & pair : _nodeItems)
	{
		if(!pair.second) continue;
		for(ScriptDropSpot * spot : pair.second->dropSpots())
			spot->setError(!complete && !spot->target().optional && spot->filledItem() == nullptr);
	}

	// Hint lines are joined with a single style of separator ("\n").
	QString hint;
	auto appendLine = [&hint](const QString & line)
	{
		if(!hint.isEmpty()) hint += "\n";
		hint += line;
	};

	if(_lastCheckPassed)
	{
		if(isFilter)
			appendLine(_model.formulaCount() == 0 ? tr("Filter cleared") : tr("Filter applied"));
		else
			appendLine(_model.formulaCount() == 0 ? tr("Computed columns code clear(ed)") : tr("Computed columns code applied"));
	}
	else
	{
		if(!complete)
			appendLine(tr("Please enter all arguments - see fields marked in red."));
		if(!booleanOk)
			appendLine(tr("Formula does not return a set of logical values, and therefore cannot be used in the filter."));
		// Only reachable in computed-column mode (in filter mode oneFormula is always true).
		if(!oneFormula)
			appendLine(tr("Only one formula per computed column allowed."));
	}
	setHintText(hint);

	if(_lastCheckPassed)
	{
		_lastAppliedJson = tq(_model.toString());
		emit applyRequested(constructorJson(), rCode());
	}

	return _lastCheckPassed;
}

void ScriptConstructorView::nodeEdited()
{
	setSomethingChanged(true);
	_hintText = "";
	refreshHint();
	emit rCodeChanged(rCode());
}

// -------------------------------------------------------------------------------------
// Theme metrics
// -------------------------------------------------------------------------------------

qreal ScriptConstructorView::blockDim() const
{
	JaspTheme * theme = JaspTheme::currentTheme();
	return 20.0 * (theme ? theme->uiScale() : 1.0);
}

qreal ScriptConstructorView::fontPixelSize() const
{
	JaspTheme * theme = JaspTheme::currentTheme();
	return 16.0 * (theme ? theme->uiScale() : 1.0);
}

qreal ScriptConstructorView::spacing() const
{
	return 2.0 * (JaspTheme::currentTheme() ? JaspTheme::currentTheme()->uiScale() : 1.0);
}

qreal ScriptConstructorView::desiredMinimumHeight() const
{
	// Operator bar + hint line + a little breathing room (mirrors the old constructors).
	return blockDim() * 1.75 + fontPixelSize() * 2 + blockDim() * 3;
}

// -------------------------------------------------------------------------------------
// Chrome + item tree
// -------------------------------------------------------------------------------------

void ScriptConstructorView::componentComplete()
{
	JASPTIMER_FINISH(ScriptConstructorView ctorToComponentComplete);
	JASPTIMER_SCOPE(ScriptConstructorView componentComplete);

	QQuickItem::componentComplete();
	_componentComplete = true;

	// With deferUntilVisible the (expensive) chrome + palettes are only built once the
	// view is effectively visible (e.g. the computed-column constructor in a hidden
	// StackLayout tab). The singleShot re-check runs after the surrounding layout has
	// applied its page visibility, so a tab that is current from the start still builds.
	if(!_deferUntilVisible)
		ensureChromeBuilt();
	else
		QTimer::singleShot(0, this, [this]()
		{
			if(_componentComplete && !_chromeBuilt && isVisible())
				ensureChromeBuilt();
		});

	// If no columns model was bound from QML (e.g. the property name shadows the
	// `columnsModel` context property), fall back to the ColumnsModel singleton so the palette
	// tracks dataset changes. setColumnsModel() wires the model signals and, when the chrome is
	// already built, populates the palette directly; in the deferred-chrome case it only connects
	// (buildColumnPalette() early-returns while the palette item does not exist) and buildChrome()
	// populates the palette when it runs later.
	if(!_columnsModel)
		setColumnsModel(ColumnsModel::singleton());
}

void ScriptConstructorView::ensureChromeBuilt()
{
	if(_chromeBuilt)
		return;

	JASPTIMER_SCOPE(ScriptConstructor ensureChromeBuilt);

	buildChrome();
	_chromeBuilt = true;

	rebuildFormulaItems();
}

void ScriptConstructorView::itemChange(ItemChange change, const ItemChangeData & value)
{
	QQuickItem::itemChange(change, value);

	// When the item becomes effectively visible (including ancestor-driven changes like
	// a StackLayout switching to its tab) build the chrome if it hasn't been built yet.
	if(change == ItemVisibleHasChanged && value.boolValue && _componentComplete && !_chromeBuilt)
		ensureChromeBuilt();
}

void ScriptConstructorView::buildChrome()
{
	JASPTIMER_SCOPE(ScriptConstructor buildChrome);
	JaspTheme * theme = JaspTheme::currentTheme();

	QQuickRectangle * background = new QQuickRectangle();
	background->setParentItem(this);
	background->setZ(-3);
	background->setColor(theme ? theme->white() : QColor("white"));
	_background = background;

	// Faint centred decoration distinguishing a filter from a computed-column constructor.
	ScriptImage * decoration = new ScriptImage(this);
	decoration->setParentItem(this);
	decoration->setZ(-2);

	// The source image loads asynchronously; re-layout once its intrinsic size is known so the
	// watermark gets sized (it is otherwise left at 0x0 until an unrelated relayout happens).
	connect(decoration, &QQuickItem::implicitWidthChanged,		this, [this](){ if(_chromeBuilt) layoutAll(); });
	connect(decoration, &QQuickItem::implicitHeightChanged,		this, [this](){ if(_chromeBuilt) layoutAll(); });
	_backgroundDecoration = decoration;
	updateBackgroundDecoration();

	_operatorBar = new QQuickItem(this);
	_operatorBar->setParentItem(this);
	_operatorBar->setZ(3);

	// The operator prototypes live in a content item that is centred within the top bar.
	_operatorBarContent = new QQuickItem(_operatorBar);

	_columnPalette = new ScriptPalette(this);
	_columnPalette->setParentItem(this);

	_functionPalette = new ScriptPalette(this);
	_functionPalette->setParentItem(this);

	// The drop area is framed, like rectangularColumnContainer in the old constructors:
	// a transparent rectangle with a thin border around the script column and the trash.
	QQuickRectangle * scriptArea = new QQuickRectangle();
	scriptArea->setParentItem(this);
	scriptArea->setClip(true);
	scriptArea->setColor(Qt::transparent);
	scriptArea->border()->setWidth(scriptAreaBorderWidth);
	scriptArea->border()->setColor(theme ? theme->uiBorder() : QColor("grey"));
	_scriptArea = scriptArea;

	_scriptColumn = new QQuickItem(_scriptArea);
	_scriptColumn->setParentItem(_scriptArea);

	ScriptTrashItem * trash = new ScriptTrashItem(this);
	trash->setToolTipText(tr("Dump unwanted snippets here; double-click to erase the entire slate"));
	trash->setParentItem(_scriptArea);
	trash->setZ(10);
	_trash = trash;

	// Trash icon; centred inside the drop zone by layoutAll(), which owns the trash geometry.
	ScriptImage * icon = new ScriptImage(this); // `this` for the QQmlContext; _trash is C++-created and has none
	icon->setParentItem(_trash);
	icon->setSource(QUrl((theme ? theme->iconPath() : QString()) + "/trashcan.png")); // same string->QUrl conversion as the old property write
	icon->setAcceptedMouseButtons(Qt::NoButton);
	_trashIcon = icon;

	QQuickText * hint = new QQuickText();
	hint->setParentItem(this);
	hint->setVAlign(QQuickText::AlignVCenter);
	hint->setHAlign(QQuickText::AlignHCenter);
	hint->setWrapMode(QQuickText::Wrap);
	hint->setColor(theme ? theme->textEnabled() : QColor("black"));
	QFont f = theme ? theme->font() : QFont();
	f.setPixelSize(static_cast<int>(fontPixelSize()));
	hint->setFont(f);
	hint->setZ(5);
	_hint = hint;

	// The hint wraps, so its height changes with its text and with the width it gets. Re-run the
	// layout when that happens, or a message that grew to two lines would run under the script
	// area. Queued, because this also fires from inside layoutAll() (which sets the hint's width).
	connect(hint, &QQuickText::contentHeightChanged, this, [this]()
	{
		if(!_chromeBuilt || _hintRelayoutScheduled)
			return;

		_hintRelayoutScheduled = true;
		QTimer::singleShot(0, this, [this]()
		{
			_hintRelayoutScheduled = false;
			if(_chromeBuilt)
				layoutAll();
		});
	});

	// Generated R code display (computed-column mode, toggled via showGeneratedRCode).
	QQuickText * rCodeDisplay = new QQuickText();
	rCodeDisplay->setParentItem(this);
	rCodeDisplay->setVAlign(QQuickText::AlignVCenter);
	rCodeDisplay->setWrapMode(QQuickText::Wrap);
	rCodeDisplay->setColor(theme ? theme->textEnabled() : QColor("black"));
	rCodeDisplay->setFont(theme ? theme->fontRCode() : QFont());
	rCodeDisplay->setVisible(false);
	rCodeDisplay->setZ(5);
	_rCodeDisplay = rCodeDisplay;

	buildOperatorBar();
	buildColumnPalette();
	buildFunctionPalette();
	refreshHint();
}

ScriptNodeItem * ScriptConstructorView::makeNodeItem(ScriptNode * node, QQuickItem * parent)
{
	ScriptNodeItem * item = new ScriptNodeItem(this, node, parent);
	item->rebuild();
	_nodeItems[node] = item;
	return item;
}

ScriptNodeItem * ScriptConstructorView::addPrototypeItem(ScriptNode * proto, QQuickItem * content, qreal maxTextWidth)
{
	ScriptNodeItem * item = new ScriptNodeItem(this, proto, content);
	item->setAcceptsDrops(false);
	item->setMaxTextWidth(maxTextWidth);
	item->rebuild();
	return item;
}

void ScriptConstructorView::clearFormulaItems()
{
	// Delete only root items; child node items are QQuickItem children of their
	// parent node/drop-spot and are destroyed transitively. Deleting every entry in
	// _nodeItems would double-free the children.
	for(ScriptNodeItem * item : _rootItems)
		if(item)
			item->deleteLater();

	_nodeItems.clear();
	_rootItems.clear();
}

void ScriptConstructorView::_clearPaletteChildren(QQuickItem * palette)
{
	if(!palette) return;

	JASPTIMER_SCOPE(ScriptConstructor clearPaletteChildren);

	for(QQuickItem * child : palette->childItems())
	{
		// Palette prototype items own their ScriptNode prototype; free it too.
		if(auto * ni = qobject_cast<ScriptNodeItem*>(child))
			if(ni->node())
				ni->node()->deleteLater();
		child->deleteLater();
	}
}

void ScriptConstructorView::rebuildFormulaItems()
{
	if(!_chromeBuilt || !_scriptColumn)
		return;

	JASPTIMER_SCOPE(ScriptConstructor rebuildFormulaItems);

	clearFormulaItems();

	for(ScriptNode * formula : _model.formulas())
	{
		ScriptNodeItem * item = makeNodeItem(formula, _scriptColumn);
		_rootItems.append(item);
	}

	layoutAll();
}

void ScriptConstructorView::layoutAll()
{
	JASPTIMER_SCOPE(ScriptConstructor layoutAll);
	qreal w = width(), h = height();
	qreal barH = blockDim() * 1.75;

	// Each palette autosizes to its own widest entry: the columns on the left are usually much
	// narrower than the functions on the right, and one shared width left a wide empty gap
	// between the column names and the script area. Both keep a floor (so a dataset of very
	// short names still reads as a column) and a cap (so the script area keeps its room).
	auto paletteWidth = [&](qreal contentWidth)
	{
		return std::min(std::max(contentWidth, blockDim() * 4), w / 3.0);
	};

	// Both palettes keep a margin towards the script area, so that neither the widest column name
	// nor the function entries touch its border. The function palette keeps the same margin on
	// its right as well, so its widest entry does not run into the edge of the window.
	const qreal paletteMargin = blockDim() / 2;

	qreal columnPaletteW	= paletteWidth(_columnPaletteContentWidth + paletteMargin),
		  functionPaletteW	= paletteWidth(_functionPaletteContentWidth + 2 * paletteMargin);

	// The hint wraps, so reserve the height it actually needs instead of a single line: the
	// script area above it then shrinks to match, rather than the text running underneath it.
	// Its width does not depend on its height, so it can be set here already.
	qreal hintH = 0;
	if(_hint)
	{
		_hint->setWidth(std::max(qreal(0), w - columnPaletteW - functionPaletteW));
		hintH = std::max(fontPixelSize(), _hint->property("contentHeight").toReal()) + 2 * spacing();
	}

	// Reserve space at the bottom for the generated-R display (computed columns only).
	bool showRCode = _showGeneratedRCode && _model.mode() != ScriptConstructorMode::Filter;
	qreal rCodeH = (showRCode && _rCodeDisplay) ? fontPixelSize() * 2 + spacing() * 2 : 0;

	// What is left for the operator bar's siblings once the bottom rows have taken their share.
	const qreal contentH = std::max(qreal(0), h - barH - hintH - rCodeH);

	if(_background)
	{
		_background->setWidth(w);
		_background->setHeight(h);
	}

	if(_backgroundDecoration)
	{
		// Cache the image's natural size on the first layout where it is known (the load).
		// After that the Image's `sourceSize = width * 2` binding makes implicitWidth follow
		// width, so we must use the cached natural size rather than re-reading implicitWidth.
		if(_backgroundImageSize.isEmpty())
		{
			const qreal iw = _backgroundDecoration->property("implicitWidth").toReal();
			const qreal ih = _backgroundDecoration->property("implicitHeight").toReal();
			if(iw > 0 && ih > 0)
				_backgroundImageSize = QSizeF(iw, ih);
		}

		if(!_backgroundImageSize.isEmpty())
		{
			const qreal iw = _backgroundImageSize.width();
			const qreal ih = _backgroundImageSize.height();

			if(w > 0 && h > 0)
			{
				// Fit within half the view, centred (matches the old fadeCollector watermark).
				const qreal ratio = std::min(std::min(w / iw, h / ih), qreal(1.0)) * 0.5;
				const qreal dw = iw * ratio, dh = ih * ratio;
				_backgroundDecoration->setWidth(dw);
				_backgroundDecoration->setHeight(dh);
				_backgroundDecoration->setX((w - dw) / 2);
				_backgroundDecoration->setY((h - dh) / 2);
			}
			else
			{
				_backgroundDecoration->setWidth(iw);
				_backgroundDecoration->setHeight(ih);
			}
		}
	}

	if(_operatorBar)
	{
		_operatorBar->setX(0);
		_operatorBar->setY(0);
		_operatorBar->setWidth(w);
		_operatorBar->setHeight(barH);

		// Centre the operator row within the top bar.
		if(_operatorBarContent)
		{
			_operatorBarContent->setX((w - _operatorBarContent->width()) / 2);
			_operatorBarContent->setY((barH - _operatorBarContent->height()) / 2);
		}
	}

	if(_columnPalette)
	{
		_columnPalette->setX(0);
		_columnPalette->setY(barH);
		_columnPalette->setWidth(columnPaletteW);
		_columnPalette->setHeight(contentH);
	}

	if(_functionPalette)
	{
		_functionPalette->setX(w - functionPaletteW);
		_functionPalette->setY(barH);
		_functionPalette->setWidth(functionPaletteW);
		_functionPalette->setHeight(contentH);

		// Indent the entries; the palette only ever scrolls vertically, so its content keeps this x.
		if(QQuickItem * content = _functionPalette->content())
			content->setX(paletteMargin);
	}

	if(_scriptArea)
	{
		_scriptArea->setX(columnPaletteW);
		_scriptArea->setY(barH);
		_scriptArea->setWidth(w - columnPaletteW - functionPaletteW);
		_scriptArea->setHeight(contentH);
	}

	if(_hint)
	{
		_hint->setX(columnPaletteW);
		_hint->setY(h - hintH - rCodeH);
		_hint->setWidth(w - columnPaletteW - functionPaletteW);
		_hint->setHeight(hintH);
	}

	if(_rCodeDisplay)
	{
		_rCodeDisplay->setVisible(showRCode);
		_rCodeDisplay->setX(columnPaletteW);
		_rCodeDisplay->setY(h - rCodeH);
		_rCodeDisplay->setWidth(w - columnPaletteW - functionPaletteW);
		_rCodeDisplay->setHeight(rCodeH);
	}

	if(_trash)
	{
		qreal trashDim = blockDim() * 3;
		_trash->setWidth(trashDim);
		_trash->setHeight(trashDim);
		_trash->setX(_scriptArea->width() - trashDim - spacing());
		_trash->setY(_scriptArea->height() - trashDim - spacing());

		// Icon centred in the drop zone, at the 0.9 padding of the old DropTrash.qml
		// (PreserveAspectFit then paints it at the icon's own aspect, as it did before).
		if(_trashIcon)
		{
			qreal iconDim = trashDim * 0.9;
			_trashIcon->setWidth(iconDim);
			_trashIcon->setHeight(iconDim);
			_trashIcon->setX((trashDim - iconDim) / 2);
			_trashIcon->setY((trashDim - iconDim) / 2);
		}
	}

	layoutScriptArea();

	// Theme metrics (and thus the desired minimum height) can change with the ui scale;
	// only notify the QML binding when the value really moved.
	const qreal minH = desiredMinimumHeight();
	if(!qFuzzyCompare(minH, _lastDesiredMinimumHeight))
	{
		_lastDesiredMinimumHeight = minH;
		emit desiredMinimumHeightChanged();
	}
}

void ScriptConstructorView::layoutScriptArea()
{
	if(!_scriptColumn) return;

	JASPTIMER_SCOPE(ScriptConstructor layoutScriptArea);

	qreal y = spacing();
	qreal x = spacing();

	for(ScriptNodeItem * item : _rootItems)
	{
		item->layout();
		item->setX(x);
		item->setY(y);
		y += item->preferredHeight() + spacing() * 2;
	}

	// Inset by the frame so snippets don't sit on the script area's border.
	_scriptColumn->setX(scriptAreaBorderWidth);
	_scriptColumn->setY(scriptAreaBorderWidth);
	_scriptColumn->setWidth((_scriptArea ? _scriptArea->width() : width()) - 2 * scriptAreaBorderWidth);
	_scriptColumn->setHeight(y);
}

void ScriptConstructorView::geometryChange(const QRectF & newGeometry, const QRectF & oldGeometry)
{
	QQuickItem::geometryChange(newGeometry, oldGeometry);

	if(newGeometry.size() != oldGeometry.size())
		layoutAll();
}

void ScriptConstructorView::keyPressEvent(QKeyEvent * event)
{
	if(event->matches(QKeySequence::Undo))
	{
		undo();
		event->accept();
		return;
	}
	if(event->matches(QKeySequence::Redo))
	{
		redo();
		event->accept();
		return;
	}

	QQuickItem::keyPressEvent(event);
}

void ScriptConstructorView::refreshHint()
{
	if(!_hint) return;

	JaspTheme * theme = JaspTheme::currentTheme();

	if(!_filterErrorMsg.isEmpty())
	{
		_hint->setProperty("text", _filterErrorMsg);
		_hint->setProperty("color", theme ? theme->redDarker() : QColor("darkred"));
	}
	else
	{
		QString text = _hintText.isEmpty() ? defaultHintText() : _hintText;
		_hint->setProperty("text", text);
		_hint->setProperty("color", theme ? theme->textEnabled() : QColor("black"));
	}
}

QString ScriptConstructorView::defaultHintText() const
{
	return _model.mode() == ScriptConstructorMode::Filter
		? tr("Welcome to the drag and drop filter!")
		: tr("Welcome to the drag and drop computed column constructor!");
}

void ScriptConstructorView::updateBackgroundDecoration()
{
	JASPTIMER_SCOPE(ScriptConstructor updateBackgroundDecoration);

	if(!_backgroundDecoration) return;

	const QString file = _model.mode() == ScriptConstructorMode::Filter
		? QString("filterConstructorBackground.png")
		: QString("columnConstructorBackground.png");

	JaspTheme * theme = JaspTheme::currentTheme();

	_backgroundImageSize = QSizeF();
	_backgroundDecoration->setProperty("source", (theme ? theme->iconPath() : QString()) + "/" + file);
}

// =====================================================================================
// Palettes + operator bar
// =====================================================================================

void ScriptConstructorView::buildOperatorBar()
{
	if(!_operatorBar) return;

	JASPTIMER_SCOPE(ScriptConstructor buildOperatorBar);

	// Clear any previously built operator prototypes (buildOperatorBar also runs again on
	// every mode change / deferred rebuild; without this the prototypes accumulate).
	_clearPaletteChildren(_operatorBarContent);

	// Left-to-right flow along the bar; operatorBarOnly functions (sqrt, !) are functions
	// interspersed among the operators (they belong only in the bar, not in the function
	// palette): each is placed directly after the operator named by its barAfter, and the
	// ones without a barAfter are appended after the operator list in registration order.
	auto placeItem = [this](qreal & x, ScriptNode * proto)
	{
		ScriptNodeItem * item = addPrototypeItem(proto, _operatorBarContent);
		item->setX(x);
		item->setY(0);
		x += item->preferredWidth() + spacing() * 2;
	};

	const ScriptConstructorRegistry & registry = ScriptConstructorRegistry::instance();

	qreal x = 0;
	for(const ScriptOperatorDef & def : registry.operatorsForMode(_model.mode()))
	{
		placeItem(x, new ScriptNodeOperator(def.op, def.vertical));
		for(const ScriptFunctionDef & func : registry.functions())
			if(func.operatorBarOnly && func.barAfter == def.op)
				placeItem(x, new ScriptNodeFunction(func.name));
	}
	for(const ScriptFunctionDef & func : registry.functions())
		if(func.operatorBarOnly && func.barAfter.empty())
			placeItem(x, new ScriptNodeFunction(func.name));

	_operatorBarContent->setWidth(x);
	_operatorBarContent->setHeight(blockDim());

#ifdef PROFILE_JASP
	Log::log() << "ScriptConstructor buildOperatorBar created " << _operatorBarContent->childItems().size() << " items" << std::endl;
#endif
}

void ScriptConstructorView::buildFunctionPalette()
{
	if(!_functionPalette) return;

	JASPTIMER_SCOPE(ScriptConstructor buildFunctionPalette);

	QQuickItem * content = _functionPalette->content();
	_clearPaletteChildren(content);

	qreal maxW = 0;
	qreal y = spacing();
	for(const ScriptFunctionDef & def : ScriptConstructorRegistry::instance().functionsForMode(_model.mode()))
	{
		ScriptNodeItem * item = addPrototypeItem(new ScriptNodeFunction(def.name), content);
		item->setX(spacing());
		item->setY(y);
		y += item->preferredHeight() + spacing();
		maxW = std::max(maxW, item->preferredWidth());
	}

	for(const ScriptFunctionDef & def : ScriptConstructorRegistry::instance().rowFunctions())
	{
		ScriptNodeItem * item = addPrototypeItem(new ScriptNodeRowFunction(def.name), content);
		item->setX(spacing());
		item->setY(y);
		y += item->preferredHeight() + spacing();
		maxW = std::max(maxW, item->preferredWidth());
	}

	_functionPaletteContentWidth = maxW + spacing() * 2;
	_functionPalette->setContentHeight(y);
#ifdef PROFILE_JASP
	Log::log() << "ScriptConstructor buildFunctionPalette created " << content->childItems().size() << " items" << std::endl;
#endif
	if(_chromeBuilt) layoutAll();
}

void ScriptConstructorView::buildColumnPalette()
{
	if(!_columnPalette) return;

	JASPTIMER_SCOPE(ScriptConstructor buildColumnPalette);

	// (Re)build the O(1) column cache in a single pass before creating items, so each
	// ScriptNodeItem::rebuild() below avoids its own O(N) scan of the columns model.
	rebuildColumnCache();

	QQuickItem * content = _columnPalette->content();

	// Clear any previously built column prototypes (rebuilt when the dataset changes).
	_clearPaletteChildren(content);

	// Columns come from the bound model, falling back to the ColumnsModel singleton.
	QAbstractItemModel * model = _columnsModel ? _columnsModel : ColumnsModel::singleton();
	if(!model)
		return;

	// One very long column name must not widen the whole palette: cap it at the theme's
	// text-field width and elide the rest. ScriptNodeItem moves an elided name into its tooltip,
	// so the full name stays reachable. A column item is icon + spacing + name, and
	// buildColumnPalette pads the content by spacing() on either side.
	JaspTheme  *	theme		= JaspTheme::currentTheme();
	const qreal		maxItemW	= (theme ? theme->textFieldWidth() : blockDim() * 10) - spacing() * 2;
	const qreal		maxTextW	= std::max(blockDim(), maxItemW - blockDim() - spacing());

	qreal maxW = 0;
	qreal y = spacing();
	int rows = model->rowCount();
	int nameRole = _nameRole >= 0 ? _nameRole : static_cast<int>(model->roleNames().key("columnName"));

	for(int r = 0; r < rows; r++)
	{
		QModelIndex idx = model->index(r, 0);
		QString name = model->data(idx, nameRole).toString();

		if(name.isEmpty())
			continue;

		ScriptNodeItem * item = addPrototypeItem(new ScriptNodeColumn(fq(name)), content, maxTextW);
		item->setX(spacing());
		item->setY(y);
		y += item->preferredHeight() + spacing();
		maxW = std::max(maxW, item->preferredWidth());
	}

	_columnPaletteContentWidth = maxW + spacing() * 2;
	_columnPalette->setContentHeight(y);
#ifdef PROFILE_JASP
	Log::log() << "ScriptConstructor buildColumnPalette created " << content->childItems().size() << " items (rows: " << rows << ")" << std::endl;
#endif
	if(_chromeBuilt) layoutAll();
}

// =====================================================================================
// Drag & drop orchestration
// =====================================================================================

void ScriptConstructorView::spawnFromPrototype(ScriptNode * proto, const QPointF & scenePos)
{
	if(!proto) return;

	startDragNew(proto->cloneEmpty(), scenePos);
}

void ScriptConstructorView::startDragExisting(ScriptNodeItem * item, const QPointF & scenePos)
{
	if(!item) return;

	QToolTip::hideText();

	_draggedItem	= item;
	_dragIsNew		= false;

	_dragOffset = item->mapFromScene(scenePos);

	item->setParentItem(this);
	item->setZ(100);
	item->setPosition(mapFromScene(scenePos) - _dragOffset);

	// Deliberately NOT marking the constructor dirty here: dropping the node back into its
	// own spot (a plain click) is a no-op and must not enable the Apply button.
}

void ScriptConstructorView::startDragNew(ScriptNode * newNode, const QPointF & scenePos)
{
	if(!newNode) return;

	QToolTip::hideText();

	ScriptNodeItem * item = makeNodeItem(newNode, this);
	item->setZ(100);
	item->setPosition(mapFromScene(scenePos));

	_draggedItem	= item;
	_dragIsNew		= true;
	_dragOffset		= QPointF(0, 0);
}

void ScriptConstructorView::cancelDrag()
{
	if(!_draggedItem)
		return;

	QToolTip::hideText();
	clearHover();

	// A freshly spawned node is not owned by the model until it is dropped, so it would
	// leak here; an existing node stays owned by (and alive inside) the model.
	if(_dragIsNew && _draggedItem->node())
		_draggedItem->node()->deleteLater();

	_draggedItem->deleteLater();
	_draggedItem = nullptr;
	_dragIsNew = false;
}

void ScriptConstructorView::collectDropSpots(QList<ScriptDropSpot*> & out) const
{
	for(const auto & pair : _nodeItems)
		if(pair.second)
			out.append(pair.second->dropSpots());
}

// True when 'ancestor' is a strict parent-item ancestor of 'item'.
static bool itemHasAncestor(QQuickItem * item, QQuickItem * ancestor)
{
	for(QQuickItem * p = item ? item->parentItem() : nullptr; p; p = p->parentItem())
		if(p == ancestor)
			return true;
	return false;
}

ScriptDropSpot * ScriptConstructorView::dropSpotAt(const QPointF & scenePos, ScriptNodeItem * dragged) const
{
	QList<ScriptDropSpot*> spots;
	collectDropSpots(spots);

	ScriptDropSpot * bestSpot = nullptr;
	int bestDepth = -1;

	for(ScriptDropSpot * spot : spots)
	{
		if(!spot) continue;

		// Skip spots that are already filled, unless filled by the item being dragged
		// (dropping back into its own spot is a no-op move).
		if(spot->filledItem() && spot->filledItem() != dragged)
			continue;

		// Skip spots that live inside the dragged item's subtree.
		if(dragged && itemHasAncestor(spot, dragged)) continue;

		QPointF local = spot->mapFromScene(scenePos);
		if(!spot->contains(local)) continue;

		// Prefer the deepest spot under the cursor.
		int depth = 0;
		for(QQuickItem * p = spot->parentItem(); p; p = p->parentItem())
			depth++;

		if(depth > bestDepth)
		{
			bestDepth = depth;
			bestSpot = spot;
		}
	}

	return bestSpot;
}

ScriptDropSpot * ScriptConstructorView::bestDropSpotFor(ScriptNode * node, const QPointF & scenePos, ScriptNodeItem * dragged) const
{
	if(!node) return nullptr;

	// 1) A precise hit on a spot that accepts the node always wins (dropSpotAt already skips
	// filled spots and anything inside the dragged subtree).
	if(ScriptDropSpot * hit = dropSpotAt(scenePos, dragged))
		if(hit->target().accepts(node, _model.mode()))
			return hit;

	// Candidate spots: empty (or holding the dragged item itself), accepting the node's keys,
	// and not inside the dragged subtree.
	QList<ScriptDropSpot*> spots;
	collectDropSpots(spots);

	QList<QPair<QPointF, ScriptDropSpot*>> candidates; // scene top-left of the spot
	for(ScriptDropSpot * spot : spots)
	{
		if(!spot || (spot->filledItem() && spot->filledItem() != dragged))
			continue;

		if(dragged && itemHasAncestor(spot, dragged))
			continue;

		if(!spot->target().accepts(node, _model.mode()))
			continue;

		candidates.append({ spot->mapToScene(QPointF(0, 0)), spot });
	}

	if(candidates.isEmpty())
		return nullptr;

	// Sort topmost, then leftmost ("fill the constructor left-to-right, top-to-bottom").
	std::sort(candidates.begin(), candidates.end(), [](const auto & a, const auto & b)
	{
		if(!qFuzzyCompare(a.first.y(), b.first.y()))
			return a.first.y() < b.first.y();
		return a.first.x() < b.first.x();
	});

	// 2) Dropped on a formula: use its leftmost accepting empty spot.
	ScriptNodeItem * formulaUnderCursor = nullptr;
	for(ScriptNodeItem * root : _rootItems)
	{
		if(!root) continue;
		QPointF topLeft = root->mapToScene(QPointF(0, 0));
		if(QRectF(topLeft, QSizeF(root->width(), root->height())).contains(scenePos))
		{
			formulaUnderCursor = root;
			break;
		}
	}

	if(formulaUnderCursor)
	{
		for(const auto & candidate : candidates)
			if(itemHasAncestor(candidate.second, formulaUnderCursor))
				return candidate.second;
		return nullptr;
	}

	// 3) Dropped on empty space: the topmost, then leftmost accepting empty spot anywhere.
	return candidates.first().second;
}

void ScriptConstructorView::clearHover()
{
	if(_hoveredSpot)
	{
		_hoveredSpot->setHoverState(false);
		_hoveredSpot = nullptr;
	}
}

void ScriptConstructorView::dragMove(const QPointF & scenePos)
{
	// A reset/cancel may have freed the node under an active drag: bail out safely.
	if(!_draggedItem || !_draggedItem->node())
		return;

	_draggedItem->setPosition(mapFromScene(scenePos) - _dragOffset);

	// Preview the resolved "best spot" (precise hit, else the spot the drop would take), so
	// the green highlight shows the actual destination while dragging.
	ScriptDropSpot * spot = bestDropSpotFor(_draggedItem->node(), scenePos, _draggedItem);

	if(spot != _hoveredSpot)
	{
		clearHover();
		_hoveredSpot = spot;
	}

	// bestDropSpotFor only returns spots that accept the dragged node, so hover is always green.
	if(_hoveredSpot)
		_hoveredSpot->setHoverState(true);
}

void ScriptConstructorView::endDrag(const QPointF & scenePos)
{
	if(!_draggedItem)
		return;

	ScriptNodeItem * item = _draggedItem.data();
	const bool dragIsNew = _dragIsNew;

	// Detach the drag state *before* the model operations: they emit changed(), which must
	// not be interpreted as an in-flight-drag invalidation (cancelDrag would kill the item).
	_draggedItem = nullptr;
	_dragIsNew = false;
	clearHover();

	ScriptNode * node = item ? item->node() : nullptr;

	if(!item || !node)
	{
		// The node was freed by a reset while the drag was in flight; drop it on the floor.
		if(item)
			item->deleteLater();
		return;
	}

	const std::string beforeDragJson = _model.toString();

	// Trash zone: bottom-right of the script area.
	bool overTrash = _trash && _trash->contains(_trash->mapFromScene(scenePos));

	if(overTrash)
	{
		if(dragIsNew)
			ScriptNode::deleteTree(node);
		else
			_model.removeNode(node);
	}
	else
	{
		ScriptDropSpot * spot = bestDropSpotFor(node, scenePos, item);
		DropTarget target = spot ? spot->target() : DropTarget::none();

		if(dragIsNew)
			_model.insertNode(node, target);
		else
			_model.moveNode(node, target);
	}

	// The dragged item was reparented to the view root (or created there), so it
	// is not cleaned up by clearFormulaItems(); remove it explicitly.
	item->deleteLater();

	if(beforeDragJson != _model.toString())
	{
		rebuildFormulaItems();
		nodeEdited();
	}
	else
	{
		// No-op (e.g. a plain click dropped the node back into its own spot): restore the
		// item hierarchy without flagging the constructor dirty.
		rebuildFormulaItems();
	}
}
