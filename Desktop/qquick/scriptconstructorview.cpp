#include "scriptconstructorview.h"
#include "scriptnodeitem.h"
#include "jasptheme.h"
#include "qutils.h"
#include "data/columnsmodel.h"

#include <QQmlComponent>
#include <QQmlIncubator>
#include <QQmlEngine>
#include <QQmlContext>
#include <QQuickWindow>
#include <algorithm>

ScriptConstructorView::ScriptConstructorView(QQuickItem * parent)
	: QQuickItem(parent)
{
	setClip(true);

	// The view resolves actual column types from the columns model for R generation.
	_model.setColumnTypeProvider(this);

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
	for(auto & comp : {_textComp, _imageComp, _textInputComp, _checkBoxComp, _rectComp, _tooltipAreaComp})
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
	std::string s = fq(json);
	if(s == _model.toString()) return;

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

void ScriptConstructorView::setColumnsModel(QAbstractItemModel * m)
{
	if(m == _columnsModel) return;

	if(_columnsModel)
		disconnect(_columnsModel, nullptr, this, nullptr);

	_columnsModel = m;

	if(_columnsModel)
	{
		connect(_columnsModel, &QAbstractItemModel::modelReset,		this, [this](){ if(_chromeBuilt) buildColumnPalette(); });
		connect(_columnsModel, &QAbstractItemModel::rowsInserted,	this, [this](){ if(_chromeBuilt) buildColumnPalette(); });
		connect(_columnsModel, &QAbstractItemModel::rowsRemoved,	this, [this](){ if(_chromeBuilt) buildColumnPalette(); });
	}

	emit columnsModelChanged();

	if(_chromeBuilt)
		buildColumnPalette();
}

int ScriptConstructorView::columnType(const std::string & columnName) const
{
	QAbstractItemModel * model = _columnsModel ? _columnsModel : ColumnsModel::singleton();
	if(!model)
		return 1; // scale

	int nameRole = static_cast<int>(model->roleNames().key("columnName"));
	int typeRole = static_cast<int>(model->roleNames().key("columnType"));

	QString wanted = tq(columnName);
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
	std::string s = json.isEmpty() ? fq(_lastAppliedJson) : fq(json);
	_model.fromJson(s);
	setSomethingChanged(false);
	rebuildFormulaItems();
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
		_textComp = new QQmlComponent(qmlEngine(this));
		_textComp->setData("import QtQuick\nText { verticalAlignment: Text.AlignVCenter }", QUrl("ScriptConstructorText"));
	}
	return _textComp;
}

QQmlComponent * ScriptConstructorView::imageComponent()
{
	if(!_imageComp)
	{
		_imageComp = new QQmlComponent(qmlEngine(this));
		_imageComp->setData("import QtQuick\nImage { smooth: true }", QUrl("ScriptConstructorImage"));
	}
	return _imageComp;
}

QQmlComponent * ScriptConstructorView::textInputComponent()
{
	if(!_textInputComp)
	{
		_textInputComp = new QQmlComponent(qmlEngine(this));
		_textInputComp->setData("import QtQuick\nTextInput { selectByMouse: true }", QUrl("ScriptConstructorTextInput"));
	}
	return _textInputComp;
}

QQmlComponent * ScriptConstructorView::checkBoxComponent()
{
	if(!_checkBoxComp)
	{
		_checkBoxComp = new QQmlComponent(qmlEngine(this));
		_checkBoxComp->setData("import QtQuick\nimport QtQuick.Controls\nCheckBox {}", QUrl("ScriptConstructorCheckBox"));
	}
	return _checkBoxComp;
}

QQmlComponent * ScriptConstructorView::rectangleComponent()
{
	if(!_rectComp)
	{
		_rectComp = new QQmlComponent(qmlEngine(this));
		_rectComp->setData("import QtQuick\nRectangle {}", QUrl("ScriptConstructorRectangle"));
	}
	return _rectComp;
}

QQmlComponent * ScriptConstructorView::tooltipAreaComponent()
{
	if(!_tooltipAreaComp)
	{
		_tooltipAreaComp = new QQmlComponent(qmlEngine(this));
		_tooltipAreaComp->setData(
			"import QtQuick\n"
			"import QtQuick.Controls\n"
			"MouseArea {\n"
			"  anchors.fill: parent\n"
			"  z: 5\n"
			"  acceptedButtons: Qt.NoButton\n"
			"  hoverEnabled: true\n"
			"  ToolTip.delay: 500\n"
			"  ToolTip.text: parent.toolTip\n"
			"  ToolTip.visible: ToolTip.text !== '' && containsMouse\n"
			"  ToolTip.toolTip.background: Rectangle { color: jaspTheme.tooltipBackgroundColor; radius: jaspTheme.borderRadius }\n"
			"}\n", QUrl("ScriptConstructorToolTipArea"));
	}
	return _tooltipAreaComp;
}

QQuickItem * ScriptConstructorView::newLeaf(QQmlComponent * comp)
{
	if(!comp || comp->isError())
		return nullptr;

	QQmlIncubator incubator(QQmlIncubator::Synchronous);
	comp->create(incubator);

	if(incubator.isError())
		return nullptr;

	return qobject_cast<QQuickItem*>(incubator.object());
}

// -------------------------------------------------------------------------------------
// Chrome + item tree
// -------------------------------------------------------------------------------------

void ScriptConstructorView::componentComplete()
{
	QQuickItem::componentComplete();

	if(!_chromeBuilt)
	{
		buildChrome();
		_chromeBuilt = true;
	}

	// If no columns model was bound from QML (e.g. the property name shadows the
	// `columnsModel` context property), fall back to the ColumnsModel singleton and
	// keep the palette in sync with dataset changes.
	if(!_columnsModel)
	{
		if(ColumnsModel * singleton = ColumnsModel::singleton())
		{
			connect(singleton, &QAbstractItemModel::modelReset,		this, [this](){ if(_chromeBuilt) buildColumnPalette(); });
			connect(singleton, &QAbstractItemModel::rowsInserted,	this, [this](){ if(_chromeBuilt) buildColumnPalette(); });
			connect(singleton, &QAbstractItemModel::rowsRemoved,	this, [this](){ if(_chromeBuilt) buildColumnPalette(); });
		}
		buildColumnPalette();
	}

	rebuildFormulaItems();
}

void ScriptConstructorView::buildChrome()
{
	JaspTheme * theme = JaspTheme::currentTheme();

	_background = newLeaf(rectangleComponent());
	if(_background)
	{
		_background->setParentItem(this);
		_background->setZ(-3);
		_background->setProperty("color", theme ? theme->white() : QColor("white"));
	}

	// Faint centred decoration distinguishing a filter from a computed-column constructor.
	_backgroundDecoration = newLeaf(imageComponent());
	if(_backgroundDecoration)
	{
		_backgroundDecoration->setParentItem(this);
		_backgroundDecoration->setZ(-2);
		_backgroundDecoration->setProperty("fillMode", 1); // Image.PreserveAspectFit

		// The source image loads asynchronously; re-layout once its intrinsic size is known so the
		// watermark gets sized (it is otherwise left at 0x0 until an unrelated relayout happens).
		connect(_backgroundDecoration, &QQuickItem::implicitWidthChanged,	 this, [this](){ if(_chromeBuilt) layoutAll(); });
		connect(_backgroundDecoration, &QQuickItem::implicitHeightChanged, this, [this](){ if(_chromeBuilt) layoutAll(); });
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

	_scriptColumn = new QQuickItem(_scriptArea);
	_scriptColumn->setParentItem(_scriptArea);

	_trash = newLeaf(rectangleComponent());
	if(_trash)
	{
		_trash->setParentItem(_scriptArea);
		_trash->setProperty("color", QColor(0, 0, 0, 0));
		_trash->setProperty("border.color", theme ? theme->gray() : QColor("gray"));
		_trash->setProperty("border.width", 1);
		_trash->setProperty("radius", 6.0);
		_trash->setZ(10);

		// Double-click erases the entire slate (handled via eventFilter).
		_trash->setAcceptedMouseButtons(Qt::LeftButton);
		_trash->installEventFilter(this);

		// Hover tooltip (mirrors the old DropTrash.qml).
		_trash->setProperty("toolTip", tr("Dump unwanted snippets here; double-click to erase the entire slate"));
		if(QQuickItem * overlay = newLeaf(tooltipAreaComponent()))
			overlay->setParentItem(_trash);

		// Trash icon centred inside the drop zone.
		QQuickItem * icon = newLeaf(imageComponent());
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
	}

	_hint = newLeaf(textComponent());
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
	_rCodeDisplay = newLeaf(textComponent());
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
		const qreal iw = _backgroundDecoration->property("implicitWidth").toReal();
		const qreal ih = _backgroundDecoration->property("implicitHeight").toReal();
		if(iw > 0 && ih > 0)
		{
			// Fit within half the view, centred (matches the old fadeCollector watermark).
			const qreal ratio = std::min(std::min(w / iw, h / ih), qreal(1.0)) * 0.5;
			const qreal dw = iw * ratio, dh = ih * ratio;
			_backgroundDecoration->setWidth(dw);
			_backgroundDecoration->setHeight(dh);
			_backgroundDecoration->setX((w - dw) / 2);
			_backgroundDecoration->setY((h - dh) / 2);
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

bool ScriptConstructorView::eventFilter(QObject * obj, QEvent * event)
{
	// Double-clicking the trash zone erases the whole slate (mirrors the old DropTrash.qml).
	if(obj == _trash && event->type() == QEvent::MouseButtonDblClick)
	{
		_model.clear();
		return true;
	}

	return QQuickItem::eventFilter(obj, event);
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
	if(!_backgroundDecoration) return;

	const QString file = _model.mode() == ScriptConstructorMode::Filter
		? QString("filterConstructorBackground.png")
		: QString("columnConstructorBackground.png");

	_backgroundDecoration->setProperty("source", JaspTheme::currentTheme()->iconPath() + "/" + file);
}

// =====================================================================================
// Palettes + operator bar
// =====================================================================================

void ScriptConstructorView::buildOperatorBar()
{
	if(!_operatorBar) return;

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
}

void ScriptConstructorView::buildFunctionPalette()
{
	if(!_functionPalette) return;

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
	if(_chromeBuilt) layoutAll();
}

void ScriptConstructorView::buildColumnPalette()
{
	if(!_columnPalette) return;

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
	int nameRole = static_cast<int>(model->roleNames().key("columnName"));

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

	ScriptDropSpot * spot = dropSpotAt(scenePos, _draggedItem);

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
	ScriptDropSpot * spot = dropSpotAt(scenePos, _draggedItem);

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
	else if(spot && spot->target().accepts(node))
	{
		DropTarget target = spot->target();

		if(_dragIsNew)
			_model.insertNode(node, target);
		else
			_model.moveNode(node, target);
	}
	else
	{
		// No specific spot under the cursor.
		if(_dragIsNew)
		{
			// A brand-new node resolves a reasonable insertion point and, for operators
			// with a free left slot, absorbs ("gobbles") the preceding formula.
			_model.insertNode(node, DropTarget::none());
		}
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
