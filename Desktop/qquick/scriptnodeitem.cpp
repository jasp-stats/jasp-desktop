#include "scriptnodeitem.h"
#include "scriptconstructorview.h"
#include "jasptheme.h"
#include "qutils.h"
#include <QQmlComponent>
#include <QQmlIncubator>
#include <QQmlEngine>
#include <QFontMetricsF>
#include <QMouseEvent>

// =====================================================================================
// ScriptDropSpot
// =====================================================================================

ScriptDropSpot::ScriptDropSpot(ScriptConstructorView * view, QQuickItem * parent)
	: QQuickItem(parent)
	, _view(view)
{
	setImplicitWidth(_view ? _view->blockDim() * 3 : 60);
	setImplicitHeight(_view ? _view->blockDim() : 20);
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
		_placeholder->setProperty("verticalAlignment", 0); // Text.AlignVCenter? set below via anchors
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
		setImplicitWidth(item->preferredWidth());
		setImplicitHeight(item->preferredHeight());
	}
}

void ScriptDropSpot::clearFilled()
{
	_filled = nullptr;
	if(_placeholder) _placeholder->setVisible(_acceptsDrops);
	setImplicitWidth(_view ? _view->blockDim() * 3 : 60);
	setImplicitHeight(_view ? _view->blockDim() : 20);
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
	if(_marker)
	{
		_marker->setWidth(width());
		_marker->setHeight(height());
	}

	if(_filled)
	{
		_filled->layout();
		setImplicitWidth(_filled->preferredWidth());
		setImplicitHeight(_filled->preferredHeight());
	}
	else if(_placeholder)
	{
		_placeholder->setProperty("text", _defaultText);
		qreal w = _placeholder->property("implicitWidth").toReal();
		qreal minW = _acceptsDrops && _view ? _view->blockDim() * 3 : 0;
		setImplicitWidth(std::max(w, minW));
		setImplicitHeight(_view ? _view->blockDim() : 20);
	}
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

ScriptDropSpot * ScriptNodeItem::makeDropSpot(const DropTarget & target, const QString & placeholder)
{
	ScriptDropSpot * spot = new ScriptDropSpot(_view, this);
	spot->setTarget(target);
	spot->setDefaultText(placeholder);
	spot->setAcceptsDrops(_acceptsDrops);
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

		makeImage(getIconFilename(static_cast<columnType>(effective), varIconType::DefaultIconType));
		makeText(QString::fromStdString(col->columnName()));
		break;
	}
	case ScriptNode::Type::Operator:
	case ScriptNode::Type::OperatorVertical:
	{
		auto * op = static_cast<ScriptNodeOperator*>(_node);
		const ScriptOperatorDef * def = ScriptConstructorRegistry::instance().operatorDef(op->op());

		makeDropSpot(DropTarget{op->isVertical() ? DropTarget::Kind::OperatorLeft : DropTarget::Kind::OperatorLeft, op, 0, op->dropKeysLeft()}, "...");

		if(def && !def->image.empty())
			makeImage(tq(def->image));
		else
			makeText(QString::fromStdString(op->op()), true);

		makeDropSpot(DropTarget{DropTarget::Kind::OperatorRight, op, 1, op->dropKeysRight()}, "...");
		break;
	}
	case ScriptNode::Type::Function:
	{
		auto * func = static_cast<ScriptNodeFunction*>(_node);
		makeText(QString::fromStdString(func->functionName()));

		for(int i = 0; i < func->childCount(); i++)
		{
			const auto & arg = func->arguments()[i];
			makeDropSpot(DropTarget{DropTarget::Kind::FunctionArg, func, i, arg.dropKeys}, QString::fromStdString(arg.name));
		}
		break;
	}
	case ScriptNode::Type::RowFunction:
	{
		auto * rowFunc = static_cast<ScriptNodeRowFunction*>(_node);
		makeText(QString::fromStdString(rowFunc->functionName()));

		for(int i = 0; i < rowFunc->childCount(); i++)
			makeDropSpot(DropTarget{DropTarget::Kind::RowFunctionArg, rowFunc, i, {"number"}}, "...");
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
		(void)op;
	}
	else if(t == ScriptNode::Type::Function || t == ScriptNode::Type::RowFunction)
	{
		QQuickItem * nameVisual = _leaves.isEmpty() ? nullptr : _leaves.first();
		if(nameVisual)
		{
			qreal w = nameVisual->property("implicitWidth").toReal();
			qreal h = block;
			nameVisual->setX(x);
			nameVisual->setY(0);
			x += w;
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
		_view->model()->removeNode(_node);
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
			int cur = col->columnTypeUser();
			int next = (cur < 1 || cur >= 3) ? 1 : cur + 1;

			_view->model()->setColumnTypeUser(col, next);
			_view->nodeEdited();
			rebuild();
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
