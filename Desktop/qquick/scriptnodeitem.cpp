#include "scriptnodeitem.h"
#include "scriptconstructorview.h"
#include "jasptheme.h"
#include "qutils.h"
#include "data/columnsmodel.h"
#include "timers.h"
#include "log.h"
#include <QQmlComponent>
#include <QQmlIncubator>
#include <QQmlEngine>
#include <QQmlProperty>
#include <QFontMetricsF>
#include <QMouseEvent>
#include <QWheelEvent>
#include <QHoverEvent>
#include <QToolTip>
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

	_placeholder = _view->newLeaf(_view->textComponent(), "text");
	if(_placeholder)
	{
		_placeholder->setParentItem(this);
		_placeholder->setProperty("verticalAlignment", 128);	// Text.AlignVCenter
		_placeholder->setProperty("horizontalAlignment", 4);	// Text.AlignHCenter
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

	_marker = _view->newLeaf(_view->rectangleComponent(), "rectangle");
	if(_marker)
	{
		_marker->setParentItem(this);
		_marker->setZ(-3);
		_marker->setProperty("color", QColor("transparent"));
		_marker->setProperty("radius", 4.0);
		QQmlProperty(_marker, "border.width").write(2.0);
		QQmlProperty(_marker, "border.color").write(JaspTheme::currentTheme()->blue());
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
		QQmlProperty(m, "border.color").write(accepted ? theme->green() : theme->red());
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
		QQmlProperty(m, "border.color").write(QColor("#BB0000"));
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

	_input = _view->newLeaf(_view->textInputComponent(), "textInput");
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
// ScriptTrashItem
// =====================================================================================

ScriptTrashItem::ScriptTrashItem(ScriptConstructorView * view, QQuickItem * parent)
	: QQuickItem(parent)
	, _view(view)
{
	setAcceptedMouseButtons(Qt::LeftButton);
	setAcceptHoverEvents(true);
	setCursor(Qt::PointingHandCursor); // mirrors the old MouseArea cursorShape
}

void ScriptTrashItem::mousePressEvent(QMouseEvent * event)
{
	// Accept the press so the window treats this item as the press target (and can
	// synthesize the double click on the second press).
	debugPressCount++;
	Log::log() << "DIAG trash mousePress" << std::endl;
	event->accept();
}

void ScriptTrashItem::mouseDoubleClickEvent(QMouseEvent * event)
{
	Log::log() << "DIAG trash mouseDoubleClick" << std::endl;
	debugDoubleClickCount++;
	// Mirrors the old DropTrash: erase the slate AND apply the (now empty) filter, otherwise
	// the surrounding FilterModel keeps the old constructorJson and pushes the erased formula
	// tree straight back into the view on the next filter sync.
	if(event->button() == Qt::LeftButton && _view)
	{
		_view->model()->clear();
		_view->checkAndApply();
	}
	event->accept();
}

void ScriptTrashItem::hoverEnterEvent(QHoverEvent * event)
{
	// Diagnostic for "visible but unclickable": mouse presses are only delivered when EVERY
	// ancestor contains the cursor position, while hover is per-item. Log which ancestor
	// (if any) excludes the point, so the culprit is visible in the log.
#ifdef PROFILE_JASP
	{
		const QPointF scenePos = mapToScene(boundingRect().center());
		QQuickItem * ancestor = parentItem();
		while(ancestor)
		{
			const QPointF local = ancestor->mapFromScene(scenePos);
			const bool contained = ancestor->contains(local);
			Log::log() << "DIAG trash hover ancestor " << ancestor->metaObject()->className()
			           << " name='" << ancestor->objectName() << "' size=" << ancestor->width() << "x" << ancestor->height()
			           << " contains=" << contained << (contained ? "" : "   <== EXCLUDES THE POINT")
			           << std::endl;
			ancestor = ancestor->parentItem();
		}
	}
#endif

	if(!_toolTipText.isEmpty())
		QToolTip::showText(event->globalPosition().toPoint(), _toolTipText, nullptr, QRect(), 15000);

	event->accept();
}

void ScriptTrashItem::hoverLeaveEvent(QHoverEvent * event)
{
	QToolTip::hideText();
	event->accept();
}

// =====================================================================================
// ScriptNodeItem
// =====================================================================================

ScriptNodeItem::ScriptNodeItem(ScriptConstructorView * view, ScriptNode * node, QQuickItem * parent)
	: QQuickItem(parent)
	, _view(view)
	, _node(node)
{
	JASPTIMER_SCOPE(ScriptNodeItem ctor);
	setAcceptedMouseButtons(Qt::LeftButton | Qt::RightButton);

	// Hover shows a native tooltip via QToolTip (see hoverEnterEvent), replacing the old
	// per-item QtQuick ToolTip overlay that required one QML incubation per node item.
	setAcceptHoverEvents(true);
}

void ScriptNodeItem::setToolTip(const QString & toolTip)
{
	if(_toolTip == toolTip)
		return;
	_toolTip = toolTip;
	emit toolTipChanged();
}

ScriptNodeItem::~ScriptNodeItem()
{
	QToolTip::hideText();
	clearLeaves();
}

void ScriptNodeItem::clearLeaves()
{
	JASPTIMER_SCOPE(ScriptNodeItem clearLeaves);
	for(QQuickItem * leaf : _leaves)
		if(leaf)
			leaf->deleteLater();
	_leaves.clear();

	for(QQuickItem * comma : _argumentCommas)
		if(comma)
			comma->deleteLater();
	_argumentCommas.clear();

	for(ScriptDropSpot * spot : _dropSpots)
		if(spot)
			spot->deleteLater();
	_dropSpots.clear();

	if(_openParen)	{ _openParen->deleteLater();	_openParen	= nullptr; }
	if(_closeParen)	{ _closeParen->deleteLater();	_closeParen	= nullptr; }
	if(_overline)	{ _overline->deleteLater();		_overline	= nullptr; }
	if(_fractionBar){ _fractionBar->deleteLater();	_fractionBar = nullptr; }
}

QQuickItem * ScriptNodeItem::makeText(const QString & text, bool bold)
{
	JASPTIMER_SCOPE(ScriptNodeItem makeText total);
	QQuickItem * item = _view->newLeaf(_view->textComponent(), "text");
	if(!item) return nullptr;

	item->setParentItem(this);
	item->setProperty("text", text);

	{
		JASPTIMER_SCOPE(ScriptNodeItem makeText setProps);
		JaspTheme * theme = JaspTheme::currentTheme();
		QFont f = theme->font();
		f.setPixelSize(static_cast<int>(_view->fontPixelSize()));
		f.setBold(bold);
		item->setProperty("font", f);
		item->setProperty("color", theme->textEnabled());
	}

	addLeaf(item);
	return item;
}

QQuickItem * ScriptNodeItem::makeImage(const QString & iconFile)
{
	JASPTIMER_SCOPE(ScriptNodeItem makeImage total);
	QQuickItem * item = _view->newLeaf(_view->imageComponent(), "image");
	if(!item) return nullptr;

	item->setParentItem(this);
	{
		// Suspect #1 for slow init: Image loads synchronously by default, so this
		// property write can trigger a PNG load + decode on the GUI thread.
		JASPTIMER_SCOPE(ScriptNodeItem makeImage setSource);
		item->setProperty("source", JaspTheme::currentTheme()->iconPath() + "/" + iconFile);
	}
	item->setProperty("fillMode", 1); // Image.PreserveAspectFit

	qreal dim = _view->blockDim();
	item->setWidth(dim);
	item->setHeight(dim);
	// Also pin the implicit size, otherwise the Image reports its source image's (huge)
	// intrinsic dimensions and blows up the layout.
	item->setImplicitWidth(dim);
	item->setImplicitHeight(dim);

	addLeaf(item);
	return item;
}

QQuickItem * ScriptNodeItem::makeParenText(const QString & text)
{
	JASPTIMER_SCOPE(ScriptNodeItem makeParenText total);
	QQuickItem * item = _view->newLeaf(_view->textComponent(), "text");
	if(!item) return nullptr;

	item->setParentItem(this);
	item->setProperty("text", text);

	{
		JASPTIMER_SCOPE(ScriptNodeItem makeParenText setProps);
		JaspTheme * theme = JaspTheme::currentTheme();
		QFont f = theme->font();
		f.setPixelSize(static_cast<int>(_view->fontPixelSize()));
		item->setProperty("font", f);
		item->setProperty("color", theme->textEnabled());
		item->setVisible(false); // visibility controlled in layout()
	}

	return item;
}

QQuickItem * ScriptNodeItem::makeComma()
{
	// Argument separator text (", ") — rendered between function/row-function arguments.
	QQuickItem * item = _view->newLeaf(_view->textComponent(), "text");
	if(!item) return nullptr;

	item->setParentItem(this);
	item->setProperty("text", ", ");

	{
		JASPTIMER_SCOPE(ScriptNodeItem makeComma setProps);
		JaspTheme * theme = JaspTheme::currentTheme();
		QFont f = theme->font();
		f.setPixelSize(static_cast<int>(_view->fontPixelSize()));
		item->setProperty("font", f);
		item->setProperty("color", theme->textEnabled());
	}

	_argumentCommas.append(item);
	return item;
}

ScriptDropSpot * ScriptNodeItem::makeDropSpot(const DropTarget & target, const QString & placeholder)
{
	JASPTIMER_SCOPE(ScriptNodeItem makeDropSpot);
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
	JASPTIMER_SCOPE(ScriptNodeItem rebuild);
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
		QQuickItem * input = _view->newLeaf(_view->textInputComponent(), "textInput");
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
		QQuickItem * input = _view->newLeaf(_view->textInputComponent(), "textInput");
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
		QQuickItem * box = _view->newLeaf(_view->checkBoxComponent(), "checkBox");
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
		JASPTIMER_SCOPE(ScriptNodeItem rebuildColumn);
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
		JASPTIMER_SCOPE(ScriptNodeItem rebuildOperator);
		auto * op = static_cast<ScriptNodeOperator*>(_node);
		const ScriptOperatorDef * def = ScriptConstructorRegistry::instance().operatorDef(op->op(), op->isVertical());

		// Horizontal operators wrap their children in parentheses; vertical (division) does not.
		bool nest = !op->isVertical();

		ScriptDropSpot * leftSpot = makeDropSpot(DropTarget{DropTarget::Kind::OperatorLeft, op, 0, op->dropKeysLeft(), false, nest}, "...");

		if(op->isVertical() && _acceptsDrops)
		{
			// Fraction: the horizontal line is drawn in layout(); the ÷ image is only the bar prototype.
			_fractionBar = _view->newLeaf(_view->rectangleComponent(), "rectangle");
			if(_fractionBar)
			{
				_fractionBar->setParentItem(this);
				_fractionBar->setProperty("color", JaspTheme::currentTheme()->textEnabled());
				_fractionBar->setVisible(false);
			}
		}
		else if(def && !def->image.empty())
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
		JASPTIMER_SCOPE(ScriptNodeItem rebuildFunction);
		auto * func = static_cast<ScriptNodeFunction*>(_node);
		const ScriptFunctionDef * funcDef = ScriptConstructorRegistry::instance().functionDef(func->functionName());
		const bool hasImage = funcDef && !funcDef->image.empty();
		const bool isSqrt = func->functionName() == "sqrt";

		if(isSqrt && _acceptsDrops)
		{
			// Radical: a √ head (drawn tall) with an overline layered above the argument in layout().
			QQuickItem * head = _view->newLeaf(_view->imageComponent(), "image");
			if(head)
			{
				head->setParentItem(this);
				head->setProperty("source", theme->iconPath() + "/rootHead.png");
				head->setProperty("fillMode", 0); // Image.Stretch (fill the box so the head's right edge is exact)
				head->setWidth(block);
				head->setHeight(block);
				addLeaf(head);
			}

			_overline = _view->newLeaf(_view->rectangleComponent(), "rectangle");
			if(_overline)
			{
				_overline->setParentItem(this);
				_overline->setProperty("color", theme->textEnabled());
				_overline->setVisible(false);
			}
		}
		else if(isSqrt) // operator bar: plain square-root symbol
			makeImage(QString("sqrtSelector.png"));
		else if(hasImage)
			makeImage(tq(funcDef->image));
		else
		{
			QString displayName = (funcDef && !funcDef->friendlyName.empty()) ? tq(funcDef->friendlyName) : QString::fromStdString(func->functionName());
			makeText(displayName);
		}

		// Single-argument (non-abs) functions wrap their child in parentheses.
		bool nest = (func->childCount() == 1 && func->functionName() != "abs");

		// Functions show parentheses around their arguments unless they are a single-argument
		// math symbol rendered as an image (e.g. sum -> Σ).
		_showParens = (func->childCount() > 1) || !hasImage;

		for(int i = 0; i < func->childCount(); i++)
		{
			const auto & arg = func->arguments()[i];
			ScriptDropSpot * spot = makeDropSpot(DropTarget{DropTarget::Kind::FunctionArg, func, i, arg.dropKeys, arg.optional, nest}, QString::fromStdString(arg.name));
			fillSpot(spot, arg.value);
		}

		_openParen	= makeParenText("(");
		_closeParen	= makeParenText(")");
		for(int i = 0; i + 1 < func->childCount(); i++)
			makeComma();
		break;
	}
	case ScriptNode::Type::RowFunction:
	{
		JASPTIMER_SCOPE(ScriptNodeItem rebuildRowFunction);
		auto * rowFunc = static_cast<ScriptNodeRowFunction*>(_node);
		const ScriptFunctionDef * rowDef = ScriptConstructorRegistry::instance().rowFunctionDef(rowFunc->functionName());
		const bool hasImage = rowDef && !rowDef->image.empty();

		// Row functions with a math-symbol image render as "row" + the symbol (e.g. rowSum -> row Σ).
		// Others render their name as text.
		if(hasImage)
		{
			makeText("row");
			makeImage(tq(rowDef->image));
		}
		else
			makeText(QString::fromStdString(rowFunc->functionName()));

		_showParens = (rowFunc->childCount() > 1) || !hasImage;

		for(int i = 0; i < rowFunc->childCount(); i++)
		{
			ScriptDropSpot * spot = makeDropSpot(DropTarget{DropTarget::Kind::RowFunctionArg, rowFunc, i, {"number"}, true}, "...");
			fillSpot(spot, rowFunc->childAt(i));
		}

		_openParen	= makeParenText("(");
		_closeParen	= makeParenText(")");
		for(int i = 0; i + 1 < rowFunc->childCount(); i++)
			makeComma();
		break;
	}
	}

	// Compute the hover tooltip for this element.
	{
		JASPTIMER_SCOPE(ScriptNodeItem tooltip);
		QString tip;

		switch(_node->type())
		{
		case ScriptNode::Type::Operator:
		case ScriptNode::Type::OperatorVertical:
		{
			const std::string & op = static_cast<ScriptNodeOperator*>(_node)->op();
			if(const ScriptOperatorDef * def = ScriptConstructorRegistry::instance().operatorDef(op, static_cast<ScriptNodeOperator*>(_node)->isVertical()))
				tip = def->toolTipForMode(_view->model()->mode());
			break;
		}
		case ScriptNode::Type::Function:
		{
			const std::string & fn = static_cast<ScriptNodeFunction*>(_node)->functionName();
			if(const ScriptFunctionDef * def = ScriptConstructorRegistry::instance().functionDef(fn))
				tip = def->toolTipForMode(_view->model()->mode());
			break;
		}
		case ScriptNode::Type::RowFunction:
		{
			const std::string & fn = static_cast<ScriptNodeRowFunction*>(_node)->functionName();
			if(const ScriptFunctionDef * def = ScriptConstructorRegistry::instance().rowFunctionDef(fn))
				tip = def->toolTipForMode(_view->model()->mode());
			break;
		}
		case ScriptNode::Type::Column:
		{
			auto * col = static_cast<ScriptNodeColumn*>(_node);

			const int actual		= _view->columnType(col->columnName());
			const int effective		= col->effectiveColumnType(actual);

			QStringList parts;
			parts << tr("Click icon to change column type");

			const QString description = _view->columnDescription(tq(col->columnName()));
			if(!description.isEmpty())
				parts << tr("Column description: ") + description;

			if(effective != actual)
			{
				const QString preview = _view->columnTransformedPreview(tq(col->columnName()), col->columnTypeUser());
				if(!preview.isEmpty())
					parts << preview;
			}

			tip = parts.join("\n\n");
			break;
		}
		default:
			break;
		}

		setToolTip(tip);
	}

	layout();
}

void ScriptNodeItem::layout()
{
	if(!_node) return;

	JASPTIMER_SCOPE(ScriptNodeItem layout);
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

	if(t == ScriptNode::Type::Function && _acceptsDrops
		&& static_cast<ScriptNodeFunction*>(_node)->functionName() == "sqrt")
	{
		// Radical: √ head on the left, an overline above the argument, the argument below it.
		QQuickItem * head = _leaves.isEmpty() ? nullptr : _leaves.first();
		ScriptDropSpot * arg = _dropSpots.isEmpty() ? nullptr : _dropSpots.first();

		const qreal overlineH = std::max(qreal(2.0), block * 0.15);

		qreal argW = 0, argH = block;
		if(arg)
		{
			arg->layout();
			argW = arg->width();
			argH = arg->height();
		}

		const qreal totalH = std::max(block, overlineH + argH);

		if(head)
		{
			head->setX(0);
			head->setY(0);
			head->setWidth(block);
			head->setHeight(totalH);
		}

		if(_overline)
		{
			_overline->setVisible(true);
			_overline->setX(block);
			_overline->setY(0);
			_overline->setWidth(argW);
			_overline->setHeight(overlineH);
		}

		if(arg)
		{
			arg->setX(block);
			arg->setY(overlineH + 1);
		}

		x = block + argW;
		maxH = totalH + 1;
	}
	else if(t == ScriptNode::Type::Operator || t == ScriptNode::Type::OperatorVertical)
	{
		auto * op = static_cast<ScriptNodeOperator*>(_node);
		ScriptDropSpot * left = _dropSpots.size() > 0 ? _dropSpots[0] : nullptr;
		ScriptDropSpot * right = _dropSpots.size() > 1 ? _dropSpots[1] : nullptr;

		// Vertical (fraction) division stacks numerator above a horizontal line above denominator.
		if(op->isVertical() && _acceptsDrops)
		{
			if(left)	left->layout();
			if(right)	right->layout();

			qreal leftW  = left  ? left->width()  : 0;
			qreal leftH  = left  ? left->height() : block;
			qreal rightW = right ? right->width() : 0;
			qreal rightH = right ? right->height() : block;

			const qreal barH = std::max(qreal(2.0), block * 0.1);
			const qreal barW = std::max(std::max(leftW, rightW), block);

			if(left)
			{
				left->setX((barW - leftW) / 2);
				left->setY(0);
			}

			if(_fractionBar)
			{
				_fractionBar->setVisible(true);
				_fractionBar->setX(0);
				_fractionBar->setY(leftH);
				_fractionBar->setWidth(barW);
				_fractionBar->setHeight(barH);
			}

			if(right)
			{
				right->setX((barW - rightW) / 2);
				right->setY(leftH + barH + 1);
			}

			x = barW;
			maxH = leftH + barH + rightH + 1;
		}
		else
		{
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
		}
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

		if(_openParen)	_openParen->setVisible(_showParens);
		if(_closeParen)	_closeParen->setVisible(_showParens);

		if(_showParens) placeNext(_openParen);

		for(int i = 0; i < _dropSpots.size(); i++)
		{
			ScriptDropSpot * spot = _dropSpots[i];
			spot->layout();
			placeNext(spot);

			if(i + 1 < _dropSpots.size() && i < _argumentCommas.size())
				placeNext(_argumentCommas[i]);
		}

		if(_showParens) placeNext(_closeParen);
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

				// Non-modal, transient tooltip near the clicked icon (translatable via tr()).
				const QString message = tr("Only %1 allowed in this context.").arg(names.join(tr("/")));
				QToolTip::showText(event->globalPosition().toPoint(), message, nullptr, QRect(), 30000);

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

void ScriptNodeItem::hoverEnterEvent(QHoverEvent * event)
{
	if(!_toolTip.isEmpty())
		QToolTip::showText(event->globalPosition().toPoint(), _toolTip, nullptr, QRect(), 15000);

	event->accept();
}

void ScriptNodeItem::hoverLeaveEvent(QHoverEvent * event)
{
	QToolTip::hideText();
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
