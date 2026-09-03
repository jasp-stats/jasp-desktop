#include "scriptconstructorview.h"
#include "scriptnodeitem.h"
#include "jasptheme.h"
#include "qutils.h"
#include "data/columnsmodel.h"
#include "variableinfo.h"
#include "timers.h"
#include "log.h"

#include <QQmlComponent>
#include <QQmlIncubator>
#include <QQmlEngine>
#include <QQmlContext>
#include <QQmlProperty>
#include <QQuickWindow>
#include <QKeyEvent>
#include <QKeySequence>
#include <QTimer>
#include <QToolTip>
#include <QHoverEvent>
#include <algorithm>

#ifdef PROFILE_JASP
namespace
{
// JASPTIMER_SCOPE takes a compile-time name; this variant takes a runtime (std::string) name
// so leaf creation can be timed per component kind.
struct RuntimeTimerMeasure
{
	explicit RuntimeTimerMeasure(std::string name) : _name(std::move(name)) { _getTimer(_name)->resume(); }
	~RuntimeTimerMeasure() { try { _getTimerC(_name)->stop(); } catch(...) {} }

	std::string _name;
};
}
#endif

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

	connect(&_model, &ScriptConstructorModel::reset,	this, [this](){ rebuildFormulaItems(); });
	connect(&_model, &ScriptConstructorModel::changed,	this, [this](){
		setSomethingChanged(true);
		emit rCodeChanged(rCode());
		// Keep the generated-R display (computed-column mode) in sync with the model.
		if(_rCodeDisplay && _showGeneratedRCode)
			_rCodeDisplay->setProperty("text", rCode());
	});
}

ScriptConstructorView::~ScriptConstructorView()
{
	for(auto & comp : {_textComp, _imageComp, _textInputComp, _checkBoxComp, _rectComp})
		delete comp.data();
}

// -------------------------------------------------------------------------------------
// Q_PROPERTY accessors
// -------------------------------------------------------------------------------------

void ScriptConstructorView::setModeInt(int m)
{
	ScriptConstructorMode mode = static_cast<ScriptConstructorMode>(m);
	if(mode == _model.mode()) return;

	_model.setMode(mode);

	// The generated-R display only makes sense for computed columns.
	if(_rCodeDisplay)
		_rCodeDisplay->setVisible(_showGeneratedRCode && mode != ScriptConstructorMode::Filter);

	updateBackgroundDecoration();

	emit modeChanged();

	if(_chromeBuilt)
		layoutAll();
}

QString ScriptConstructorView::constructorJson() const
{
	return tq(_model.toString());
}

void ScriptConstructorView::setConstructorJson(const QString & json)
{
	JASPTIMER_SCOPE(ScriptConstructor setConstructorJson);
	std::string s = fq(json);
	if(s == _model.toString()) return;

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
		return 1; // scale

	int nameRole = _nameRole		>= 0 ? _nameRole		: static_cast<int>(model->roleNames().key("columnName"));
	int typeRole = _typeRole		>= 0 ? _typeRole		: static_cast<int>(model->roleNames().key("columnType"));

	for(int r = 0; r < model->rowCount(); r++)
	{
		QModelIndex idx = model->index(r, 0);
		if(model->data(idx, nameRole).toString() == wanted)
		{
			int t = model->data(idx, typeRole).toInt();
			return t > 0 ? t : 1;
		}
	}

	return 1; // scale
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
	default:					previewType = varInfoType::PreviewScale;		break;
	case ::columnType::ordinal:	previewType	= varInfoType::PreviewOrdinal;		break;
	case ::columnType::nominal:	previewType	= varInfoType::PreviewNominal;		break;
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
		_columnTypesByName[name]   = t > 0 ? t : 1;
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
}

void ScriptConstructorView::redo()
{
	_localUndoStack.redo();
}

bool ScriptConstructorView::checkAndApply()
{
	setSomethingChanged(false);

	bool complete	= _model.checkCompleteness();
	bool isFilter	= _model.mode() == ScriptConstructorMode::Filter;
	bool booleanOk	= !isFilter || _model.allBoolean();
	bool oneFormula	= !isFilter || _model.formulaCount() <= 1;

	_lastCheckPassed = complete && booleanOk && oneFormula;
	emit lastCheckPassedChanged();

	// Mark empty required drop spots in red after a failed check (mirrors old iWasChecked behaviour).
	for(auto & pair : _nodeItems)
	{
		if(!pair.second) continue;
		for(ScriptDropSpot * spot : pair.second->dropSpots())
			spot->setError(!complete && !spot->target().optional && spot->filledItem() == nullptr);
	}

	QString hint;
	if(complete && booleanOk && oneFormula)
		hint = _model.formulaCount() == 0 ? tr("Filter cleared\n") : (isFilter ? tr("Filter applied\n") : tr("Computed columns code applied"));
	if(!complete)
		hint += tr("Please enter all arguments - see fields marked in red.\n");
	if(!booleanOk)
		hint += (!complete ? "\n" : QString()) + tr("Formula does not return a set of logical values, and therefore cannot be used in the filter.\n");
	if(!oneFormula)
		hint += (!complete ? "<br>" : QString()) + tr("Only one formula per computed column allowed.");
	setHintText(hint.trimmed());

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
// Leaf components (inline QML, incubated on demand)
// -------------------------------------------------------------------------------------

QQmlComponent * ScriptConstructorView::textComponent()
{
	if(!_textComp)
	{
		JASPTIMER_SCOPE(ScriptConstructor compile textComponent);
		_textComp = new QQmlComponent(qmlEngine(this));
		_textComp->setData("import QtQuick\nText { verticalAlignment: Text.AlignVCenter }", QUrl("ScriptConstructorText"));
	}
	return _textComp;
}

QQmlComponent * ScriptConstructorView::imageComponent()
{
	if(!_imageComp)
	{
		JASPTIMER_SCOPE(ScriptConstructor compile imageComponent);
		_imageComp = new QQmlComponent(qmlEngine(this));
		// asynchronous: true so icon decoding happens on the loader thread instead of blocking
		// the GUI thread; sizes are pinned via width/height + implicitWidth/Height by makeImage,
		// so layout never depends on the load having finished.
		_imageComp->setData("import QtQuick\nImage { smooth: true; asynchronous: true; sourceSize.width: width * 2; sourceSize.height: height * 2; }", QUrl("ScriptConstructorImage"));
	}
	return _imageComp;
}

QQmlComponent * ScriptConstructorView::textInputComponent()
{
	if(!_textInputComp)
	{
		JASPTIMER_SCOPE(ScriptConstructor compile textInputComponent);
		_textInputComp = new QQmlComponent(qmlEngine(this));
		_textInputComp->setData("import QtQuick\nTextInput { selectByMouse: true }", QUrl("ScriptConstructorTextInput"));
	}
	return _textInputComp;
}

QQmlComponent * ScriptConstructorView::checkBoxComponent()
{
	if(!_checkBoxComp)
	{
		JASPTIMER_SCOPE(ScriptConstructor compile checkBoxComponent);
		_checkBoxComp = new QQmlComponent(qmlEngine(this));
		_checkBoxComp->setData("import QtQuick\nimport QtQuick.Controls\nCheckBox {}", QUrl("ScriptConstructorCheckBox"));
	}
	return _checkBoxComp;
}

QQmlComponent * ScriptConstructorView::rectangleComponent()
{
	if(!_rectComp)
	{
		JASPTIMER_SCOPE(ScriptConstructor compile rectangleComponent);
		_rectComp = new QQmlComponent(qmlEngine(this));
		_rectComp->setData("import QtQuick\nRectangle {}", QUrl("ScriptConstructorRectangle"));
	}
	return _rectComp;
}

QQuickItem * ScriptConstructorView::newLeaf(QQmlComponent * comp, const char * kind)
{
	if(!comp || comp->isError())
		return nullptr;

	Q_UNUSED(kind);

#ifdef PROFILE_JASP
	RuntimeTimerMeasure incubateScope(std::string("ScriptConstructor incubate ") + kind);
#endif

	QQmlIncubator incubator(QQmlIncubator::Synchronous);

#ifdef PROFILE_JASP
	{
		// Time only the raw synchronous create (compilation on first use happens here).
		RuntimeTimerMeasure createScope(std::string("ScriptConstructor createSync ") + kind);
		comp->create(incubator);
	}
#else
	comp->create(incubator);
#endif

	if(incubator.isError())
		return nullptr;

	return qobject_cast<QQuickItem*>(incubator.object());
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
	// `columnsModel` context property), fall back to the ColumnsModel singleton and
	// keep the palette in sync with dataset changes.
	if(!_columnsModel)
	{
		if(ColumnsModel * singleton = ColumnsModel::singleton())
		{
			connect(singleton, &QAbstractItemModel::modelReset,				this, [this](){ schedulePaletteRebuild(); });
			connect(singleton, &QAbstractItemModel::rowsInserted,			this, [this](){ schedulePaletteRebuild(); });
			connect(singleton, &QAbstractItemModel::rowsRemoved,			this, [this](){ schedulePaletteRebuild(); });
			connect(singleton, &QAbstractItemModel::dataChanged,			this, [this](){ schedulePaletteRebuild(); });
			connect(singleton, &QAbstractItemModel::headerDataChanged,		this, [this](){ schedulePaletteRebuild(); });
		}
		// Only build here if ensureChromeBuilt() hasn't already done so (the deferred
		// case); otherwise buildChrome() already populated the column palette.
		if(!_chromeBuilt)
			buildColumnPalette(); // No-op until the chrome exists (deferred case).
	}

	// ensureChromeBuilt() already ran this after building the chrome.
	if(!_chromeBuilt)
		rebuildFormulaItems(); // No-op until the chrome exists (deferred case).
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

	_background = newLeaf(rectangleComponent(), "rectangle");
	if(_background)
	{
		_background->setParentItem(this);
		_background->setZ(-3);
		_background->setProperty("color", theme ? theme->white() : QColor("white"));
	}

	// Faint centred decoration distinguishing a filter from a computed-column constructor.
	_backgroundDecoration = newLeaf(imageComponent(), "image");
	if(_backgroundDecoration)
	{
		_backgroundDecoration->setParentItem(this);
		_backgroundDecoration->setZ(-2);
		_backgroundDecoration->setProperty("fillMode", 1); // Image.PreserveAspectFit

		// The source image loads asynchronously; re-layout once its intrinsic size is known so the
		// watermark gets sized (it is otherwise left at 0x0 until an unrelated relayout happens).
		connect(_backgroundDecoration, &QQuickItem::implicitWidthChanged,		this, [this](){ if(_chromeBuilt) layoutAll(); });
		connect(_backgroundDecoration, &QQuickItem::implicitHeightChanged,		this, [this](){ if(_chromeBuilt) layoutAll(); });
	}
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

	_scriptArea = new QQuickItem(this);
	_scriptArea->setClip(true);
	_scriptArea->setParentItem(this);

	_scriptColumn = new QQuickItem(_scriptArea);
	_scriptColumn->setParentItem(_scriptArea);

	ScriptTrashItem * trash = new ScriptTrashItem(this);
	trash->setToolTipText(tr("Dump unwanted snippets here; double-click to erase the entire slate"));
	trash->setParentItem(_scriptArea);
	trash->setZ(10);
	_trash = trash;

	// Trash icon centred inside the drop zone.
	QQuickItem * icon = newLeaf(imageComponent(), "image");
	if(icon)
	{
		icon->setParentItem(_trash);
		icon->setProperty("source", (theme ? theme->iconPath() : QString()) + "/trashcan.png");
		icon->setProperty("fillMode", 1); // Image.PreserveAspectFit
		icon->setAcceptedMouseButtons(Qt::NoButton);
		qreal dim = blockDim() * 1.6;
		icon->setWidth(dim);
		icon->setHeight(dim);
		icon->setX((blockDim() * 3 - dim) / 2);
		icon->setY((blockDim() * 3 - dim) / 2);
	}

	_hint = newLeaf(textComponent(), "text");
	if(_hint)
	{
		_hint->setParentItem(this);
		_hint->setProperty("wrapMode", 4);			// Text.WordWrap
		_hint->setProperty("horizontalAlignment", 4);	// Text.AlignHCenter
		_hint->setProperty("color", theme ? theme->textEnabled() : QColor("black"));
		QFont f = theme ? theme->font() : QFont();
		f.setPixelSize(static_cast<int>(fontPixelSize()));
		_hint->setProperty("font", f);
		_hint->setZ(5);
	}

	// Generated R code display (computed-column mode, toggled via showGeneratedRCode).
	_rCodeDisplay = newLeaf(textComponent(), "text");
	if(_rCodeDisplay)
	{
		_rCodeDisplay->setParentItem(this);
		_rCodeDisplay->setProperty("wrapMode", 4);		// Text.WordWrap
		_rCodeDisplay->setProperty("color", theme ? theme->textEnabled() : QColor("black"));
		QFont rf = theme ? theme->fontRCode() : QFont();
		_rCodeDisplay->setProperty("font", rf);
		_rCodeDisplay->setVisible(false);
		_rCodeDisplay->setZ(5);
	}

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

	// Widen the palettes to fit their widest entry (autosize), but never beyond a third of the
	// view so the script area keeps enough room.
	qreal paletteW = blockDim() * 8;
	paletteW = std::max(paletteW, std::max(_columnPaletteContentWidth, _functionPaletteContentWidth));
	paletteW = std::min(paletteW, w / 3.0);

	qreal hintH = _hint ? fontPixelSize() + 2 * spacing() : 0;

	// Reserve space at the bottom for the generated-R display (computed columns only).
	bool showRCode = _showGeneratedRCode && _model.mode() != ScriptConstructorMode::Filter;
	qreal rCodeH = (showRCode && _rCodeDisplay) ? fontPixelSize() * 2 + spacing() * 2 : 0;

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
		_columnPalette->setWidth(paletteW);
		_columnPalette->setHeight(h - barH - hintH - rCodeH);
	}

	if(_functionPalette)
	{
		_functionPalette->setX(w - paletteW);
		_functionPalette->setY(barH);
		_functionPalette->setWidth(paletteW);
		_functionPalette->setHeight(h - barH - hintH - rCodeH);
	}

	if(_scriptArea)
	{
		_scriptArea->setX(paletteW);
		_scriptArea->setY(barH);
		_scriptArea->setWidth(w - 2 * paletteW);
		_scriptArea->setHeight(h - barH - hintH - rCodeH);
	}

	if(_hint)
	{
		_hint->setX(paletteW);
		_hint->setY(h - hintH - rCodeH);
		_hint->setWidth(w - 2 * paletteW);
		_hint->setHeight(hintH);
	}

	if(_rCodeDisplay)
	{
		_rCodeDisplay->setVisible(showRCode);
		_rCodeDisplay->setX(paletteW);
		_rCodeDisplay->setY(h - rCodeH);
		_rCodeDisplay->setWidth(w - 2 * paletteW);
		_rCodeDisplay->setHeight(rCodeH);
	}

	if(_trash)
	{
		qreal trashDim = blockDim() * 3;
		_trash->setWidth(trashDim);
		_trash->setHeight(trashDim);
		_trash->setX(_scriptArea->width() - trashDim - spacing());
		_trash->setY(_scriptArea->height() - trashDim - spacing());
	}

	layoutScriptArea();
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

	_scriptColumn->setWidth(width());
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
		_localUndoStack.undo();
		event->accept();
		return;
	}
	if(event->matches(QKeySequence::Redo))
	{
		_localUndoStack.redo();
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

	_backgroundImageSize = QSizeF();
	_backgroundDecoration->setProperty("source", JaspTheme::currentTheme()->iconPath() + "/" + file);
}

// =====================================================================================
// Palettes + operator bar
// =====================================================================================

void ScriptConstructorView::buildOperatorBar()
{
	if(!_operatorBar) return;

	JASPTIMER_SCOPE(ScriptConstructor buildOperatorBar);

	auto placeOperator = [this](qreal & x, const ScriptOperatorDef & def)
	{
		ScriptNode * proto = new ScriptNodeOperator(def.op, def.vertical);
		ScriptNodeItem * item = new ScriptNodeItem(this, proto, _operatorBarContent);
		item->setAcceptsDrops(false);
		item->rebuild();
		item->setX(x);
		item->setY(0);
		x += item->preferredWidth() + spacing() * 2;
	};

	// sqrt and ! are functions interspersed among the operators (they belong only in the bar).
	auto placeFunction = [this](qreal & x, const std::string & name)
	{
		ScriptNode * proto = new ScriptNodeFunction(name);
		ScriptNodeItem * item = new ScriptNodeItem(this, proto, _operatorBarContent);
		item->setAcceptsDrops(false);
		item->rebuild();
		item->setX(x);
		item->setY(0);
		x += item->preferredWidth() + spacing() * 2;
	};

	const ScriptConstructorRegistry & registry = ScriptConstructorRegistry::instance();

	qreal x = 0;
	for(const ScriptOperatorDef & def : registry.operatorsForMode(_model.mode()))
	{
		placeOperator(x, def);
		if(def.op == "^")
			placeFunction(x, "sqrt");
	}
	placeFunction(x, "!");

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
		ScriptNode * proto = new ScriptNodeFunction(def.name);
		ScriptNodeItem * item = new ScriptNodeItem(this, proto, content);
		item->setAcceptsDrops(false);
		item->rebuild();
		item->setX(spacing());
		item->setY(y);
		y += item->preferredHeight() + spacing();
		maxW = std::max(maxW, item->preferredWidth());
	}

	for(const ScriptFunctionDef & def : ScriptConstructorRegistry::instance().rowFunctions())
	{
		ScriptNode * proto = new ScriptNodeRowFunction(def.name);
		ScriptNodeItem * item = new ScriptNodeItem(this, proto, content);
		item->setAcceptsDrops(false);
		item->rebuild();
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

		ScriptNode * proto = new ScriptNodeColumn(fq(name));
		ScriptNodeItem * item = new ScriptNodeItem(this, proto, content);
		item->setAcceptsDrops(false);
		item->rebuild();
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

static ScriptNode * clonePrototype(ScriptNode * proto)
{
	if(!proto) return nullptr;

	switch(proto->type())
	{
	case ScriptNode::Type::Operator:
	case ScriptNode::Type::OperatorVertical:
	{
		auto * op = static_cast<ScriptNodeOperator*>(proto);
		return new ScriptNodeOperator(op->op(), op->isVertical());
	}
	case ScriptNode::Type::Function:
	{
		auto * func = static_cast<ScriptNodeFunction*>(proto);
		return new ScriptNodeFunction(func->functionName());
	}
	case ScriptNode::Type::RowFunction:
	{
		auto * rowFunc = static_cast<ScriptNodeRowFunction*>(proto);
		auto * out = new ScriptNodeRowFunction(rowFunc->functionName());
		out->addChild(nullptr);
		return out;
	}
	case ScriptNode::Type::Column:
	{
		auto * col = static_cast<ScriptNodeColumn*>(proto);
		return new ScriptNodeColumn(col->columnName());
	}
	case ScriptNode::Type::Number:
		return new ScriptNodeLiteral(ScriptNode::Type::Number);
	case ScriptNode::Type::Boolean:
		return new ScriptNodeLiteral(ScriptNode::Type::Boolean);
	case ScriptNode::Type::String:
		return new ScriptNodeLiteral(ScriptNode::Type::String);
	}

	return nullptr;
}

void ScriptConstructorView::spawnFromPrototype(ScriptNode * proto, const QPointF & scenePos)
{
	ScriptNode * newNode = clonePrototype(proto);
	if(!newNode) return;

	startDragNew(newNode, scenePos);
}

void ScriptConstructorView::startDragExisting(ScriptNodeItem * item, const QPointF & scenePos)
{
	if(!item) return;

	QToolTip::hideText();

	_draggedItem	= item;
	_dragIsNew		= false;
	_draggedNewNode	= nullptr;

	_dragOffset = item->mapFromScene(scenePos);

	item->setParentItem(this);
	item->setZ(100);
	item->setPosition(mapFromScene(scenePos) - _dragOffset);

	setSomethingChanged(true);
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
	_draggedNewNode	= newNode;
	_dragOffset		= QPointF(0, 0);

	setSomethingChanged(true);
}

void ScriptConstructorView::collectDropSpots(QList<ScriptDropSpot*> & out) const
{
	for(const auto & pair : _nodeItems)
		if(pair.second)
			out.append(pair.second->dropSpots());
}

ScriptDropSpot * ScriptConstructorView::dropSpotAt(const QPointF & scenePos, ScriptNodeItem * dragged) const
{
	QList<ScriptDropSpot*> spots;
	const_cast<ScriptConstructorView*>(this)->collectDropSpots(spots);

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
		if(dragged)
		{
			bool insideDragged = false;
			for(QQuickItem * p = spot->parentItem(); p; p = p->parentItem())
			{
				if(p == dragged)
				{
					insideDragged = true;
					break;
				}
			}
			if(insideDragged) continue;
		}

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
		if(hit->target().accepts(node))
			return hit;

	// Candidate spots: empty (or holding the dragged item itself), accepting the node's keys,
	// and not inside the dragged subtree.
	QList<ScriptDropSpot*> spots;
	const_cast<ScriptConstructorView*>(this)->collectDropSpots(spots);

	QList<QPair<QPointF, ScriptDropSpot*>> candidates; // scene top-left of the spot
	for(ScriptDropSpot * spot : spots)
	{
		if(!spot || (spot->filledItem() && spot->filledItem() != dragged))
			continue;

		if(dragged)
		{
			bool insideDragged = false;
			for(QQuickItem * p = spot->parentItem(); p; p = p->parentItem())
			{
				if(p == dragged)
				{
					insideDragged = true;
					break;
				}
			}
			if(insideDragged) continue;
		}

		if(!spot->target().accepts(node))
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
			for(QQuickItem * p = candidate.second->parentItem(); p; p = p->parentItem())
				if(p == formulaUnderCursor)
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
		_hoveredSpot->setHoverState(false, false);
		_hoveredSpot = nullptr;
	}
}

void ScriptConstructorView::dragMove(const QPointF & scenePos)
{
	if(!_draggedItem) return;

	_draggedItem->setPosition(mapFromScene(scenePos) - _dragOffset);

	// Preview the resolved "best spot" (precise hit, else the spot the drop would take), so
	// the green highlight shows the actual destination while dragging.
	ScriptDropSpot * spot = bestDropSpotFor(_draggedItem->node(), scenePos, _draggedItem);

	if(spot != _hoveredSpot)
	{
		clearHover();
		_hoveredSpot = spot;
	}

	if(_hoveredSpot)
	{
		bool accepted = _draggedItem && _hoveredSpot->target().accepts(_draggedItem->node());
		_hoveredSpot->setHoverState(true, accepted);
	}
}

void ScriptConstructorView::endDrag(const QPointF & scenePos)
{
	if(!_draggedItem) return;

	ScriptNode * node = _draggedItem->node();
	ScriptDropSpot * spot = bestDropSpotFor(node, scenePos, _draggedItem);

	clearHover();

	// Trash zone: bottom-right of the script area.
	bool overTrash = _trash && _trash->contains(_trash->mapFromScene(scenePos));

	if(overTrash)
	{
		if(_dragIsNew)
			ScriptNode::deleteTree(node);
		else
			_model.removeNode(node);
	}
	else if(spot)
	{
		DropTarget target = spot->target();

		if(_dragIsNew)
			_model.insertNode(node, target);
		else
			_model.moveNode(node, target);
	}
	else
	{
		// No spot the node fits in: new nodes resolve a reasonable insertion point (topmost
		// formula's leftmost slot) and, for operators with a free left slot, absorb
		// ("gobble") an existing formula; existing nodes are re-rooted.
		if(_dragIsNew)
			_model.insertNode(node, DropTarget::none());
		else
			_model.moveNode(node, DropTarget::root());
	}

	// The dragged item was reparented to the view root (or created there), so it
	// is not cleaned up by clearFormulaItems(); remove it explicitly.
	if(_draggedItem)
		_draggedItem->deleteLater();

	_draggedItem = nullptr;
	_draggedNewNode = nullptr;

	rebuildFormulaItems();
	nodeEdited();
}
