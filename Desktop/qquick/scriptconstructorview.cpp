#include "scriptconstructorview.h"
#include "scriptnodeitem.h"
#include "jasptheme.h"
#include "qutils.h"

#include <QQmlComponent>
#include <QQmlIncubator>
#include <QQmlEngine>
#include <QQmlContext>
#include <QQuickWindow>
#include <QSGRectangleNode>
#include <QSGFlatColorMaterial>

ScriptConstructorView::ScriptConstructorView(QQuickItem * parent)
	: QQuickItem(parent)
{
	setFlag(QQuickItem::ItemHasContents, true);
	setClip(true);

	connect(&_model, &ScriptConstructorModel::reset,	this, [this](){ rebuildFormulaItems(); });
	connect(&_model, &ScriptConstructorModel::changed,	this, [this](){
		setSomethingChanged(true);
		emit rCodeChanged(rCode());
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
	emit modeChanged();
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
	emit showGeneratedRCodeChanged();
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

	if(_lastCheckPassed)
	{
		_lastAppliedJson = tq(_model.toString());
		emit applyRequested(constructorJson(), rCode());
	}

	refreshHint();
	return _lastCheckPassed;
}

void ScriptConstructorView::nodeEdited()
{
	setSomethingChanged(true);
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

	_operatorBar = new QQuickItem(this);
	_operatorBar->setParentItem(this);
	_operatorBar->setZ(3);

	_columnPalette = new QQuickItem(this);
	_columnPalette->setParentItem(this);

	_functionPalette = new QQuickItem(this);
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
	}

	buildOperatorBar();
	buildColumnPalette();
	buildFunctionPalette();
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
	for(auto & pair : _nodeItems)
		if(pair.second)
			pair.second->deleteLater();

	_nodeItems.clear();
	_rootItems.clear();
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
	qreal paletteW = blockDim() * 6;

	if(_background)
	{
		_background->setWidth(w);
		_background->setHeight(h);
	}

	if(_operatorBar)
	{
		_operatorBar->setX(0);
		_operatorBar->setY(0);
		_operatorBar->setWidth(w);
		_operatorBar->setHeight(barH);
	}

	if(_columnPalette)
	{
		_columnPalette->setX(0);
		_columnPalette->setY(barH);
		_columnPalette->setWidth(paletteW);
		_columnPalette->setHeight(h - barH);
	}

	if(_functionPalette)
	{
		_functionPalette->setX(w - paletteW);
		_functionPalette->setY(barH);
		_functionPalette->setWidth(paletteW);
		_functionPalette->setHeight(h - barH);
	}

	if(_scriptArea)
	{
		_scriptArea->setX(paletteW);
		_scriptArea->setY(barH);
		_scriptArea->setWidth(w - 2 * paletteW);
		_scriptArea->setHeight(h - barH);
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

QSGNode * ScriptConstructorView::updatePaintNode(QSGNode * oldNode, UpdatePaintNodeData *)
{
	QSGRectangleNode * rect = static_cast<QSGRectangleNode*>(oldNode);

	if(!rect)
	{
		rect = window()->createRectangleNode();
		QSGFlatColorMaterial * material = new QSGFlatColorMaterial();
		material->setColor(JaspTheme::currentTheme() ? JaspTheme::currentTheme()->white() : QColor("white"));
		rect->setMaterial(material);
		rect->setFlag(QSGNode::OwnsMaterial);
	}

	rect->setRect(boundingRect());
	return rect;
}

void ScriptConstructorView::refreshHint()
{
	// Hint text rendering is handled by the surrounding window (as before); kept as a hook.
}

// =====================================================================================
// Palettes + operator bar
// =====================================================================================

void ScriptConstructorView::buildOperatorBar()
{
	if(!_operatorBar) return;

	qreal x = spacing();
	for(const ScriptOperatorDef & def : ScriptConstructorRegistry::instance().operatorsForMode(_model.mode()))
	{
		ScriptNode * proto = new ScriptNodeOperator(def.op, def.vertical);
		ScriptNodeItem * item = new ScriptNodeItem(this, proto, _operatorBar);
		item->setAcceptsDrops(false);
		item->rebuild();
		item->setX(x);
		item->setY(0);
		x += item->preferredWidth() + spacing() * 2;
	}

	_operatorBar->setWidth(x);
	_operatorBar->setHeight(blockDim());
}

void ScriptConstructorView::buildFunctionPalette()
{
	if(!_functionPalette) return;

	qreal y = spacing();
	for(const ScriptFunctionDef & def : ScriptConstructorRegistry::instance().functionsForMode(_model.mode()))
	{
		ScriptNode * proto = new ScriptNodeFunction(def.name);
		ScriptNodeItem * item = new ScriptNodeItem(this, proto, _functionPalette);
		item->setAcceptsDrops(false);
		item->rebuild();
		item->setX(spacing());
		item->setY(y);
		y += item->preferredHeight() + spacing();
	}

	for(const ScriptFunctionDef & def : ScriptConstructorRegistry::instance().rowFunctions())
	{
		ScriptNode * proto = new ScriptNodeRowFunction(def.name);
		ScriptNodeItem * item = new ScriptNodeItem(this, proto, _functionPalette);
		item->setAcceptsDrops(false);
		item->rebuild();
		item->setX(spacing());
		item->setY(y);
		y += item->preferredHeight() + spacing();
	}

	_functionPalette->setWidth(blockDim() * 6);
	_functionPalette->setHeight(y);
}

void ScriptConstructorView::buildColumnPalette()
{
	if(!_columnPalette) return;

	// Clear any previously built column prototypes (rebuilt when the dataset changes).
	for(QQuickItem * child : _columnPalette->childItems())
		child->deleteLater();

	// Columns come from _columnsModel (set from QML). Rendered as prototype Column nodes.
	if(!_columnsModel)
		return;

	qreal y = spacing();
	int rows = _columnsModel->rowCount();

	for(int r = 0; r < rows; r++)
	{
		QModelIndex idx = _columnsModel->index(r, 0);
		QString name = _columnsModel->data(idx, static_cast<int>(_columnsModel->roleNames().key("columnName"))).toString();

		if(name.isEmpty())
			continue;

		ScriptNode * proto = new ScriptNodeColumn(fq(name));
		ScriptNodeItem * item = new ScriptNodeItem(this, proto, _columnPalette);
		item->setAcceptsDrops(false);
		item->rebuild();
		item->setX(spacing());
		item->setY(y);
		y += item->preferredHeight() + spacing();
	}

	_columnPalette->setWidth(blockDim() * 6);
	_columnPalette->setHeight(y);
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

	QPointF local = item->mapFromScene(scenePos);
	_dragOffset = local;

	item->setParentItem(this);
	item->setZ(100);
	item->setPosition(scenePos - _dragOffset);

	setSomethingChanged(true);
}

void ScriptConstructorView::startDragNew(ScriptNode * newNode, const QPointF & scenePos)
{
	if(!newNode) return;

	ScriptNodeItem * item = makeNodeItem(newNode, this);
	item->setZ(100);
	item->setPosition(scenePos);

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

ScriptDropSpot * ScriptConstructorView::dropSpotAt(const QPointF & scenePos) const
{
	QList<ScriptDropSpot*> spots;
	const_cast<ScriptConstructorView*>(this)->collectDropSpots(spots);

	for(ScriptDropSpot * spot : spots)
	{
		if(!spot) continue;
		QPointF local = spot->mapFromScene(scenePos);
		if(spot->contains(local))
			return spot;
	}

	return nullptr;
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

	_draggedItem->setPosition(scenePos - _dragOffset);

	ScriptDropSpot * spot = dropSpotAt(scenePos);

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
	ScriptDropSpot * spot = dropSpotAt(scenePos);

	clearHover();

	// Trash zone: bottom-right of the script area.
	bool overTrash = _trash && _trash->contains(_trash->mapFromScene(scenePos));

	if(overTrash)
	{
		if(_dragIsNew)
			ScriptNode::deleteTree(node);
		else
			_model.removeNode(node);

		_draggedItem = nullptr;
		_draggedNewNode = nullptr;
		rebuildFormulaItems();
		nodeEdited();
		return;
	}

	if(spot && spot->target().accepts(node))
	{
		DropTarget target = spot->target();

		if(_dragIsNew)
			_model.insertNode(node, target);
		else
			_model.moveNode(node, target);
	}
	else
	{
		// No valid spot: drop at root (model resolves a reasonable insertion point).
		if(_dragIsNew)
			_model.insertNode(node, DropTarget::root());
		else
			_model.moveNode(node, DropTarget::root());
	}

	_draggedItem = nullptr;
	_draggedNewNode = nullptr;

	rebuildFormulaItems();
	nodeEdited();
}
