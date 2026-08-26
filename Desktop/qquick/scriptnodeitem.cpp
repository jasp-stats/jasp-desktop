#include "scriptnodeitem.h"
#include "scriptconstructorview.h"
#include "jasptheme.h"
#include "qutils.h"
#include "utilities/messageforwarder.h"
#include <QQmlComponent>
#include <QQmlIncubator>
#include <QQmlEngine>
#include <QFontMetricsF>
#include <QMouseEvent>
#include <QWheelEvent>
#include <algorithm>

// =====================================================================================
// ScriptPalette
// =====================================================================================

ScriptPalette::ScriptPalette(QQuickItem * parent)
	: QQuickItem(parent)
{
	setClip(true);
	setAcceptedMouseButtons(Qt::LeftButton);
	_content = new QQuickItem(this);
	_content->setParentItem(this);
	_content->setX(0);
	_content->setY(0);
}

void ScriptPalette::setContentHeight(qreal height)
{
	if(_content)
		_content->setHeight(height);
	clampScroll();
}

void ScriptPalette::clampScroll()
{
	if(!_content) return;

	qreal maxScroll = std::max(qreal(0), _content->height() - height());
	if(_scrollY < 0) _scrollY = 0;
	if(_scrollY > maxScroll) _scrollY = maxScroll;
	_content->setY(-_scrollY);
}

void ScriptPalette::wheelEvent(QWheelEvent * event)
{
	qreal step = event->angleDelta().y() / 120.0;
	_scrollY -= step * 3.0 * 20.0; // a few lines per notch
	clampScroll();
	event->accept();
}

void ScriptPalette::mousePressEvent(QMouseEvent * event)
{
	// Only reached when the press lands on empty palette background (child
	// prototype items accept their own presses to start drags).
	if(event->button() == Qt::LeftButton)
	{
		_dragScrolling = true;
		_dragStartY = event->scenePosition().y();
		_dragStartScroll = _scrollY;
		grabMouse();
		event->accept();
	}
	else
		event->ignore();
}

void ScriptPalette::mouseMoveEvent(QMouseEvent * event)
{
	if(_dragScrolling)
	{
		_scrollY = _dragStartScroll - (event->scenePosition().y() - _dragStartY);
		clampScroll();
		event->accept();
	}
	else
		event->ignore();
}

void ScriptPalette::mouseReleaseEvent(QMouseEvent * event)
{
	if(_dragScrolling)
	{
		_dragScrolling = false;
		ungrabMouse();
		event->accept();
	}
	else
		event->ignore();
}

void ScriptPalette::geometryChange(const QRectF & newGeometry, const QRectF & oldGeometry)
{
	QQuickItem::geometryChange(newGeometry, oldGeometry);
	if(newGeometry.size() != oldGeometry.size())
	{
		if(_content)
			_content->setWidth(newGeometry.width());
		clampScroll();
	}
}

// =====================================================================================
// ScriptDropSpot
// =====================================================================================

ScriptDropSpot::ScriptDropSpot(ScriptConstructorView * view, QQuickItem * parent)
	: QQuickItem(parent)
	, _view(view)
{
	setImplicitWidth(_view ? _view->blockDim() * 3 : 60);
	setImplicitHeight(_view ? _view->blockDim() : 20);
	setAcceptedMouseButtons(Qt::LeftButton);
}

void ScriptDropSpot::setTarget(const DropTarget & target)
{
	_target = target;
}

QQuickItem * ScriptDropSpot::ensurePlaceholder()
{
	if(_placeholder)
		return _placeholder;

	_placeholder = _view->newLeaf(_view->textComponent());
	if(_placeholder)
	{
		_placeholder->setParentItem(this);
		_placeholder->setProperty("verticalAlignment", 128); // Text.AlignVCenter
		JaspTheme * theme = JaspTheme::currentTheme();
		QFont f = theme->font();
		f.setPixelSize(static_cast<int>(_view->fontPixelSize()));
		_placeholder->setProperty("font", f);
		_placeholder->setProperty("color", theme->textDisabled());
		_placeholder->setProperty("text", _defaultText);
		_placeholder->setX(0);
		_placeholder->setY(0);
	}
	return _placeholder;
}

QQuickItem * ScriptDropSpot::ensureMarker()
{
	if(_marker)
		return _marker;

	_marker = _view->newLeaf(_view->rectangleComponent());
	if(_marker)
	{
		_marker->setParentItem(this);
		_marker->setZ(-3);
		_marker->setProperty("color", QColor("transparent"));
		_marker->setProperty("radius", 4.0);
		_marker->setProperty("border.width", 2.0);
		_marker->setProperty("border.color", JaspTheme::currentTheme()->blue());
		_marker->setVisible(false);
	}
	return _marker;
}

void ScriptDropSpot::setDefaultText(const QString & text)
{
	_defaultText = text;
	if(_acceptsDrops)
		ensurePlaceholder();
	if(_placeholder)
		_placeholder->setProperty("text", _defaultText);
}

void ScriptDropSpot::setAcceptsDrops(bool accepts)
{
	_acceptsDrops = accepts;
}

void ScriptDropSpot::setFilledItem(ScriptNodeItem * item)
{
	_filled = item;
	if(item)
	{
		item->setParentItem(this);
		item->setX(0);
		item->setY(0);
		if(_placeholder) _placeholder->setVisible(false);
		if(_input) _input->setVisible(false);
		setImplicitWidth(item->preferredWidth());
		setImplicitHeight(item->preferredHeight());
		setWidth(item->preferredWidth());
		setHeight(item->preferredHeight());
	}
}

void ScriptDropSpot::clearFilled()
{
	_filled = nullptr;
	if(_placeholder) _placeholder->setVisible(_acceptsDrops);
	qreal w = _view ? _view->blockDim() * 3 : 60;
	qreal h = _view ? _view->blockDim() : 20;
	setImplicitWidth(w);
	setImplicitHeight(h);
	setWidth(w);
	setHeight(h);
}

void ScriptDropSpot::setHoverState(bool hovered, bool accepted)
{
	QQuickItem * m = ensureMarker();
	if(!m) return;

	m->setVisible(hovered);
	if(hovered)
	{
		JaspTheme * theme = JaspTheme::currentTheme();
		m->setProperty("border.color", accepted ? theme->green() : theme->red());
		m->setWidth(width());
		m->setHeight(height());
	}
}

void ScriptDropSpot::setError(bool error)
{
	QQuickItem * m = ensureMarker();
	if(!m) return;

	if(error)
	{
		m->setVisible(true);
		m->setProperty("border.color", QColor("#BB0000"));
		m->setWidth(width());
		m->setHeight(height());
	}
	else if(!_filled)
		m->setVisible(false);
}

void ScriptDropSpot::layout()
{
	qreal w, h;

	if(_filled)
	{
		_filled->layout();
		w = _filled->preferredWidth();
		h = _filled->preferredHeight();
	}
	else
	{
		if(_placeholder)
			_placeholder->setProperty("text", _defaultText);
		qreal pw = _placeholder ? _placeholder->property("implicitWidth").toReal() : 0;
		qreal minW = _acceptsDrops && _view ? _view->blockDim() * 3 : 0;
		w = std::max(pw, minW);
		h = _view ? _view->blockDim() : 20;
	}

	setImplicitWidth(w);
	setImplicitHeight(h);
	setWidth(w);
	setHeight(h);

	if(_marker)
	{
		_marker->setWidth(w);
		_marker->setHeight(h);
	}
	if(_placeholder)
	{
		_placeholder->setWidth(w);
		_placeholder->setHeight(h);
	}
	if(_input)
	{
		_input->setWidth(w);
		_input->setHeight(h);
	}
}

QQuickItem * ScriptDropSpot::ensureInput()
{
	if(_input)
		return _input;

	_input = _view->newLeaf(_view->textInputComponent());
	if(_input)
	{
		_input->setParentItem(this);
		_input->setProperty("text", _defaultText);
		JaspTheme * theme = JaspTheme::currentTheme();
		QFont f = theme->font();
		f.setPixelSize(static_cast<int>(_view->fontPixelSize()));
		_input->setProperty("font", f);
		_input->setProperty("color", theme->textEnabled());
		_input->setX(0);
		_input->setY(0);
		_input->setVisible(false);
		connect(_input, SIGNAL(editingFinished()), this, SLOT(onInputEditingFinished()));
	}
	return _input;
}

void ScriptDropSpot::mousePressEvent(QMouseEvent * event)
{
	// Clicking an empty drop spot lets the user type a literal value directly.
	if(event->button() == Qt::LeftButton && _acceptsDrops && !_filled)
	{
		QQuickItem * input = ensureInput();
		if(input)
		{
			input->setProperty("text", "");
			if(_placeholder) _placeholder->setVisible(false);
			input->setVisible(true);
			input->forceActiveFocus();
			event->accept();
			return;
		}
	}
	event->ignore();
}

void ScriptDropSpot::onInputEditingFinished()
{
	parseAndCreateLiteral();

	if(_input)
	{
		_input->setVisible(false);
		_input->setFocus(false);
	}
	if(_placeholder && !_filled) _placeholder->setVisible(true);
}

void ScriptDropSpot::parseAndCreateLiteral()
{
	QString text = _input ? _input->property("text").toString().trimmed() : QString();

	bool isNum = false;
	double numVal = text.toDouble(&isNum);

	const stringvec & keys = _target.dropKeys;
	auto has = [&keys](const char * k) { return std::find(keys.begin(), keys.end(), std::string(k)) != keys.end(); };

	ScriptNodeLiteral * lit = nullptr;

	// match the old DropSpot.tryConvertToObject order: number, then string, then boolean
	if(isNum && has("number"))
	{
		lit = new ScriptNodeLiteral(ScriptNode::Type::Number);
		lit->setNumberValue(numVal);
	}
	else if(has("string") && !text.isEmpty())
	{
		lit = new ScriptNodeLiteral(ScriptNode::Type::String);
		lit->setStringValue(fq(text));
	}
	else if(has("boolean"))
	{
		if(isNum)
		{
			lit = new ScriptNodeLiteral(ScriptNode::Type::Boolean);
			lit->setBoolValue(numVal != 0);
		}
		else if(text.compare("true", Qt::CaseInsensitive) == 0)
		{
			lit = new ScriptNodeLiteral(ScriptNode::Type::Boolean);
			lit->setBoolValue(true);
		}
		else if(text.compare("false", Qt::CaseInsensitive) == 0)
		{
			lit = new ScriptNodeLiteral(ScriptNode::Type::Boolean);
			lit->setBoolValue(false);
		}
	}

	if(lit)
	{
		_view->model()->insertNode(lit, _target);
		_view->nodeEdited();
		_view->refresh();
	}
	else if(_input)
		_input->setProperty("text", _defaultText);
}

// =====================================================================================
// ScriptNodeItem
// =====================================================================================

ScriptNodeItem::ScriptNodeItem(ScriptConstructorView * view, ScriptNode * node, QQuickItem * parent)
	: QQuickItem(parent)
	, _view(view)
	, _node(node)
{
	setAcceptedMouseButtons(Qt::LeftButton | Qt::RightButton);
}

ScriptNodeItem::~ScriptNodeItem()
{
	clearLeaves();
}

void ScriptNodeItem::clearLeaves()
{
	for(QQuickItem * leaf : _leaves)
		if(leaf)
			leaf->deleteLater();
	_leaves.clear();

	for(ScriptDropSpot * spot : _dropSpots)
		if(spot)
			spot->deleteLater();
	_dropSpots.clear();

	if(_openParen)	{ _openParen->deleteLater();	_openParen	= nullptr; }
	if(_closeParen)	{ _closeParen->deleteLater();	_closeParen	= nullptr; }
}

QQuickItem * ScriptNodeItem::makeText(const QString & text, bool bold)
{
	QQuickItem * item = _view->newLeaf(_view->textComponent());
	if(!item) return nullptr;

	item->setParentItem(this);
	item->setProperty("text", text);

	JaspTheme * theme = JaspTheme::currentTheme();
	QFont f = theme->font();
	f.setPixelSize(static_cast<int>(_view->fontPixelSize()));
	f.setBold(bold);
	item->setProperty("font", f);
	item->setProperty("color", theme->textEnabled());

	addLeaf(item);
	return item;
}

QQuickItem * ScriptNodeItem::makeImage(const QString & iconFile)
{
	QQuickItem * item = _view->newLeaf(_view->imageComponent());
	if(!item) return nullptr;

	item->setParentItem(this);
	item->setProperty("source", JaspTheme::currentTheme()->iconPath() + "/" + iconFile);
	item->setProperty("fillMode", 1); // Image.PreserveAspectFit

	qreal dim = _view->blockDim();
	item->setWidth(dim);
	item->setHeight(dim);

	addLeaf(item);
	return item;
}

QQuickItem * ScriptNodeItem::makeParenText(const QString & text)
{
	QQuickItem * item = _view->newLeaf(_view->textComponent());
	if(!item) return nullptr;

	item->setParentItem(this);
	item->setProperty("text", text);

	JaspTheme * theme = JaspTheme::currentTheme();
	QFont f = theme->font();
	f.setPixelSize(static_cast<int>(_view->fontPixelSize()));
	item->setProperty("font", f);
	item->setProperty("color", theme->textEnabled());
	item->setVisible(false); // shown only when the node is nested

	return item;
}

ScriptDropSpot * ScriptNodeItem::makeDropSpot(const DropTarget & target, const QString & placeholder)
{
	ScriptDropSpot * spot = new ScriptDropSpot(_view, this);
	spot->setTarget(target);
	spot->setAcceptsDrops(_acceptsDrops);
	spot->setDefaultText(placeholder);
	_dropSpots.append(spot);
	return spot;
}

void ScriptNodeItem::addLeaf(QQuickItem * leaf)
{
	_leaves.append(leaf);
}

qreal ScriptNodeItem::textWidth(QQuickItem * textItem) const
{
	if(!textItem) return 0;
	return textItem->property("implicitWidth").toReal();
}

void ScriptNodeItem::setAcceptsDrops(bool accepts)
{
	_acceptsDrops = accepts;
	for(ScriptDropSpot * spot : _dropSpots)
		spot->setAcceptsDrops(accepts);
}

void ScriptNodeItem::setNested(bool nested)
{
	_nested = nested;
}

bool ScriptNodeItem::shouldDrag(qreal x, qreal) const
{
	// For columns the icon (leftmost blockDim) is a click target for changing the type, not a drag handle.
	if(_node && _node->type() == ScriptNode::Type::Column && _acceptsDrops)
		return x >= _view->blockDim();

	return true;
}

void ScriptNodeItem::rebuild()
{
	clearLeaves();

	if(!_node) return;

	JaspTheme * theme = JaspTheme::currentTheme();
	qreal block = _view->blockDim();

	// Create a child item for an existing model child and place it into the drop spot.
	auto fillSpot = [&](ScriptDropSpot * spot, ScriptNode * child)
	{
		if(!child) return;
		ScriptNodeItem * childItem = _view->makeNodeItem(child, spot);
		childItem->setNested(spot->target().dropsNested);
		spot->setFilledItem(childItem);
	};

	switch(_node->type())
	{
	case ScriptNode::Type::Number:
	{
		auto * lit = static_cast<ScriptNodeLiteral*>(_node);
		QQuickItem * input = _view->newLeaf(_view->textInputComponent());
		if(input)
		{
			input->setParentItem(this);
			input->setProperty("text", QString::number(lit->numberValue()));
			QFont f = theme->font();
			f.setPixelSize(static_cast<int>(_view->fontPixelSize()));
			input->setProperty("font", f);
			input->setProperty("color", theme->textEnabled());
			addLeaf(input);
			connect(input, SIGNAL(editingFinished()), this, SLOT(onLiteralEditFinished()));
		}
		break;
	}
	case ScriptNode::Type::String:
	{
		auto * lit = static_cast<ScriptNodeLiteral*>(_node);
		QQuickItem * input = _view->newLeaf(_view->textInputComponent());
		if(input)
		{
			input->setParentItem(this);
			input->setProperty("text", QString::fromStdString(lit->stringValue()));
			QFont f = theme->font();
			f.setPixelSize(static_cast<int>(_view->fontPixelSize()));
			input->setProperty("font", f);
			input->setProperty("color", theme->textEnabled());
			addLeaf(input);
			connect(input, SIGNAL(editingFinished()), this, SLOT(onLiteralEditFinished()));
		}
		break;
	}
	case ScriptNode::Type::Boolean:
	{
		auto * lit = static_cast<ScriptNodeLiteral*>(_node);
		QQuickItem * box = _view->newLeaf(_view->checkBoxComponent());
		if(box)
		{
			box->setParentItem(this);
			box->setProperty("checked", lit->boolValue());
			addLeaf(box);
			connect(box, SIGNAL(toggled()), this, SLOT(onBooleanToggled()));
		}
		break;
	}
	case ScriptNode::Type::Column:
	{
		auto * col = static_cast<ScriptNodeColumn*>(_node);

		int actual = 1;
		if(_view->model()->columnTypeProvider())
			actual = _view->model()->columnTypeProvider()->columnType(col->columnName());
		int effective = col->effectiveColumnType(actual);

		// A column whose effective type differs from its dataset type gets the "transformed"
		// icon (the one with the asterisk).
		varIconType iconType = (effective == actual) ? varIconType::DefaultIconType : varIconType::TransformedIconType;
		makeImage(getIconFilename(static_cast<columnType>(effective), iconType));
		makeText(QString::fromStdString(col->columnName()));
		break;
	}
	case ScriptNode::Type::Operator:
	case ScriptNode::Type::OperatorVertical:
	{
		auto * op = static_cast<ScriptNodeOperator*>(_node);
		const ScriptOperatorDef * def = ScriptConstructorRegistry::instance().operatorDef(op->op());

		// Horizontal operators wrap their children in parentheses; vertical (division) does not.
		bool nest = !op->isVertical();

		ScriptDropSpot * leftSpot = makeDropSpot(DropTarget{DropTarget::Kind::OperatorLeft, op, 0, op->dropKeysLeft(), false, nest}, "...");

		if(def && !def->image.empty())
			makeImage(tq(def->image));
		else
			makeText(QString::fromStdString(op->op()), true);

		ScriptDropSpot * rightSpot = makeDropSpot(DropTarget{DropTarget::Kind::OperatorRight, op, 1, op->dropKeysRight(), false, nest}, "...");

		// Parentheses shown when this operator is nested inside another node's drop spot.
		_openParen	= makeParenText("(");
		_closeParen	= makeParenText(")");

		fillSpot(leftSpot, op->leftChild());
		fillSpot(rightSpot, op->rightChild());
		break;
	}
	case ScriptNode::Type::Function:
	{
		auto * func = static_cast<ScriptNodeFunction*>(_node);
		const ScriptFunctionDef * funcDef = ScriptConstructorRegistry::instance().functionDef(func->functionName());

		if(funcDef && !funcDef->image.empty())
			makeImage(tq(funcDef->image));
		else
		{
			QString displayName = (funcDef && !funcDef->friendlyName.empty()) ? tq(funcDef->friendlyName) : QString::fromStdString(func->functionName());
			makeText(displayName);
		}

		// Single-argument (non-abs) functions wrap their child in parentheses.
		bool nest = (func->childCount() == 1 && func->functionName() != "abs");

		for(int i = 0; i < func->childCount(); i++)
		{
			const auto & arg = func->arguments()[i];
			ScriptDropSpot * spot = makeDropSpot(DropTarget{DropTarget::Kind::FunctionArg, func, i, arg.dropKeys, arg.optional, nest}, QString::fromStdString(arg.name));
			fillSpot(spot, arg.value);
		}
		break;
	}
	case ScriptNode::Type::RowFunction:
	{
		auto * rowFunc = static_cast<ScriptNodeRowFunction*>(_node);
		const ScriptFunctionDef * rowDef = ScriptConstructorRegistry::instance().rowFunctionDef(rowFunc->functionName());

		// Row functions with a math-symbol image render as "row" + the symbol (e.g. rowSum -> row Σ).
		// Others render their name as text.
		if(rowDef && !rowDef->image.empty())
		{
			makeText("row");
			makeImage(tq(rowDef->image));
		}
		else
			makeText(QString::fromStdString(rowFunc->functionName()));

		for(int i = 0; i < rowFunc->childCount(); i++)
		{
			ScriptDropSpot * spot = makeDropSpot(DropTarget{DropTarget::Kind::RowFunctionArg, rowFunc, i, {"number"}, true}, "...");
			fillSpot(spot, rowFunc->childAt(i));
		}
		break;
	}
	}

	layout();
}

void ScriptNodeItem::layout()
{
	if(!_node) return;

	qreal block = _view->blockDim();
	qreal spacing = _view->spacing();
	qreal x = 0, maxH = block;

	// Lay out leaves and drop spots left-to-right in creation order.
	auto placeNext = [&](QQuickItem * item)
	{
		if(!item) return;
		qreal w = item->property("implicitWidth").toReal();
		qreal h = item->property("implicitHeight").toReal();
		if(w <= 0) w = item->width();
		if(h <= 0) h = item->height();
		if(h <= 0) h = block;

		item->setX(x);
		item->setY((maxH > h ? (maxH - h) / 2 : 0));
		x += w + spacing;
		maxH = std::max(maxH, h);
	};

	// Interleave leaves and drop spots: for operators/function the drop spots were created
	// between leaves. We simply walk both lists by their visual order stored during rebuild.
	// To keep it simple we lay out leaves first that precede drops; the rebuild order is
	// preserved by construction (leaves and spots appended in visual order is not guaranteed),
	// so we reconstruct order by type.

	// For a robust visual order we re-derive from the node structure:
	ScriptNode::Type t = _node->type();

	if(t == ScriptNode::Type::Operator || t == ScriptNode::Type::OperatorVertical)
	{
		auto * op = static_cast<ScriptNodeOperator*>(_node);
		ScriptDropSpot * left = _dropSpots.size() > 0 ? _dropSpots[0] : nullptr;
		ScriptDropSpot * right = _dropSpots.size() > 1 ? _dropSpots[1] : nullptr;

		bool showParens = _nested && _openParen && _closeParen;
		if(_openParen)	_openParen->setVisible(showParens);
		if(_closeParen)	_closeParen->setVisible(showParens);

		if(showParens) placeNext(_openParen);

		if(left) { left->layout(); placeNext(left); }

		QQuickItem * opVisual = _leaves.isEmpty() ? nullptr : _leaves.first();
		if(opVisual)
		{
			qreal w = opVisual->width() > 0 ? opVisual->width() : opVisual->property("implicitWidth").toReal();
			qreal h = opVisual->height() > 0 ? opVisual->height() : block;
			opVisual->setX(x);
			opVisual->setY((maxH > h ? (maxH - h) / 2 : 0));
			x += w + spacing;
			maxH = std::max(maxH, h);
		}

		if(right) { right->layout(); placeNext(right); }

		if(showParens) placeNext(_closeParen);
		(void)op;
	}
	else if(t == ScriptNode::Type::Function || t == ScriptNode::Type::RowFunction)
	{
		// Lay out all name leaves left-to-right. Row functions with a math symbol render as
		// "row" text + image (e.g. rowSum -> "row" + Σ), so there can be more than one leaf.
		for(QQuickItem * nameVisual : _leaves)
		{
			if(!nameVisual) continue;
			qreal w = nameVisual->property("implicitWidth").toReal();
			if(w <= 0) w = nameVisual->width();
			qreal h = nameVisual->property("implicitHeight").toReal();
			if(h <= 0) h = nameVisual->height();
			if(h <= 0) h = block;

			nameVisual->setX(x);
			nameVisual->setY((maxH > h ? (maxH - h) / 2 : 0));
			x += w;
			maxH = std::max(maxH, h);
		}

		// opening paren
		x += 2;

		for(int i = 0; i < _dropSpots.size(); i++)
		{
			ScriptDropSpot * spot = _dropSpots[i];
			spot->layout();
			placeNext(spot);
		}

		x += 2; // closing paren
	}
	else
	{
		// Leaves (column, number, string, boolean)
		for(QQuickItem * leaf : _leaves)
		{
			if(!leaf) continue;
			qreal w = leaf->property("implicitWidth").toReal();
			if(w <= 0) w = leaf->width();
			qreal h = leaf->property("implicitHeight").toReal();
			if(h <= 0) h = leaf->height();
			if(h <= 0) h = block;

			leaf->setX(x);
			leaf->setY((maxH > h ? (maxH - h) / 2 : 0));
			x += w + spacing;
			maxH = std::max(maxH, h);
		}
	}

	_preferredWidth = x > 0 ? x - spacing : 0;
	_preferredHeight = maxH;
	setImplicitWidth(_preferredWidth);
	setImplicitHeight(_preferredHeight);
	setWidth(_preferredWidth);
	setHeight(_preferredHeight);
}

// -------------------------------------------------------------------------------------
// Mouse handling -> forwards to the view's drag orchestration
// -------------------------------------------------------------------------------------

void ScriptNodeItem::mousePressEvent(QMouseEvent * event)
{
	if(!_acceptsDrops)
	{
		// Palette / operator-bar prototype: spawn a fresh draggable node.
		if(event->button() == Qt::LeftButton)
		{
			grabMouse();
			_view->spawnFromPrototype(_node, event->scenePosition());
			event->accept();
		}
		else
			event->ignore();
		return;
	}

	if(event->button() == Qt::RightButton)
	{
		// Right-click deletes the node (matches old DragGeneric behaviour).
		// Remove the node from the model and rebuild the view so no item keeps a dangling
		// ScriptNode pointer (the model deletes the node subtree immediately).
		_view->model()->removeNode(_node);
		_view->refresh();
		_view->nodeEdited();
		event->accept();
		return;
	}

	if(!shouldDrag(event->position().x(), event->position().y()))
	{
		// Clicking a column's icon cycles its requested type (scale -> ordinal -> nominal -> scale).
		if(_node && _node->type() == ScriptNode::Type::Column)
		{
			auto * col = static_cast<ScriptNodeColumn*>(_node);

			// When the column sits in a restrictive drop slot (e.g. a numeric argument of a
			// function) its type is constrained to what the slot accepts; tell the user instead
			// of silently cycling to a type that would just be reverted.
			const std::vector<int> allowed = _view->model()->allowedColumnTypes(_node);
			if(allowed.size() < 3)
			{
				QStringList names;
				for(int t : allowed)
					names << QColumnUtils::getTypeFriendly(static_cast<columnType>(t));

				MessageForwarder::showWarning(tr("Cannot change column type"),
											  tr("Only %1 allowed in this context.").arg(names.join(tr("/"))));
				event->accept();
				return;
			}

			int cur = col->columnTypeUser();
			int next = (cur < 1 || cur >= 3) ? 1 : cur + 1;

			_view->model()->setColumnTypeUser(col, next);
			_view->refresh();
			_view->nodeEdited();
			event->accept();
			return;
		}

		event->ignore();
		return;
	}

	grabMouse();
	_view->startDragExisting(this, event->scenePosition());
	event->accept();
}

void ScriptNodeItem::mouseMoveEvent(QMouseEvent * event)
{
	if(_view)
		_view->dragMove(event->scenePosition());
	event->accept();
}

void ScriptNodeItem::mouseReleaseEvent(QMouseEvent * event)
{
	ungrabMouse();
	if(_view)
		_view->endDrag(event->scenePosition());
	event->accept();
}

void ScriptNodeItem::mouseDoubleClickEvent(QMouseEvent * event)
{
	event->accept();
}

void ScriptNodeItem::onLiteralEditFinished()
{
	QQuickItem * input = qobject_cast<QQuickItem*>(sender());
	if(!input || !_node) return;

	QString text = input->property("text").toString();

	if(_node->type() == ScriptNode::Type::Number)
	{
		bool ok = false;
		double value = text.toDouble(&ok);

		if(ok)
			_view->model()->setLiteralNumber(static_cast<ScriptNodeLiteral*>(_node), value);
		else
			input->setProperty("text", QString::number(static_cast<ScriptNodeLiteral*>(_node)->numberValue()));
	}
	else if(_node->type() == ScriptNode::Type::String)
	{
		if(!text.isEmpty())
			_view->model()->setLiteralString(static_cast<ScriptNodeLiteral*>(_node), fq(text));
	}

	_view->nodeEdited();
}

void ScriptNodeItem::onBooleanToggled()
{
	QQuickItem * box = qobject_cast<QQuickItem*>(sender());
	if(!box || !_node || _node->type() != ScriptNode::Type::Boolean) return;

	bool checked = box->property("checked").toBool();
	_view->model()->setLiteralBool(static_cast<ScriptNodeLiteral*>(_node), checked);
	_view->nodeEdited();
}
