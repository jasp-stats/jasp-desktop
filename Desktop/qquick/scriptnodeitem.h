#ifndef SCRIPTNODEITEM_H
#define SCRIPTNODEITEM_H

#include <QQuickItem>
#include <QPointer>
#include "scriptconstructormodel.h"

class ScriptConstructorView;
class ScriptNodeItem;
class QQmlComponent;

///
/// A single drop slot in the constructor. Visually a rounded placeholder that can hold one
/// ScriptNodeItem. All drop-validation logic lives in the model; this class only renders state.
class ScriptDropSpot : public QQuickItem
{
	Q_OBJECT

public:
	explicit ScriptDropSpot(ScriptConstructorView * view, QQuickItem * parent = nullptr);

	void				setTarget(const DropTarget & target);
	const DropTarget &	target() const { return _target; }

	void				setFilledItem(ScriptNodeItem * item);
	ScriptNodeItem	*	filledItem() const { return _filled; }
	void				clearFilled();

	void				setHoverState(bool hovered, bool accepted);
	void				setError(bool error);
	void				setAcceptsDrops(bool accepts);
	void				setDefaultText(const QString & text);

	void				layout();

protected:
	void				mousePressEvent(QMouseEvent * event) override;

private slots:
	void				onInputEditingFinished();

private:
	QQuickItem	*	ensurePlaceholder();
	QQuickItem	*	ensureMarker();
	QQuickItem	*	ensureInput();
	void			parseAndCreateLiteral();

	ScriptConstructorView	*	_view		= nullptr;
	DropTarget					_target;
	QPointer<ScriptNodeItem>	_filled;
	QPointer<QQuickItem>		_placeholder;
	QPointer<QQuickItem>		_marker;
	QPointer<QQuickItem>		_input;
	QString						_defaultText	= "...";
	bool						_acceptsDrops	= true;
};

///
/// Visual representation of a single ScriptNode. Creates incubated QML leaves (Text, Image,
/// TextInput, CheckBox) for its content plus ScriptDropSpot children for its slots, and lays
/// them out. Contains no formula logic: it only renders and forwards gestures to the view.
class ScriptNodeItem : public QQuickItem
{
	Q_OBJECT

	Q_PROPERTY(QString toolTip READ toolTip WRITE setToolTip NOTIFY toolTipChanged)

public:
	explicit ScriptNodeItem(ScriptConstructorView * view, ScriptNode * node, QQuickItem * parent = nullptr);
	~ScriptNodeItem() override;

	ScriptNode	*	node() const { return _node; }

	void			rebuild();
	void			layout();

	qreal			preferredWidth() const { return _preferredWidth; }
	qreal			preferredHeight() const { return _preferredHeight; }

	bool			shouldDrag(qreal x, qreal y) const;
	QList<ScriptDropSpot*> dropSpots() const { return _dropSpots; }

	void			setAcceptsDrops(bool accepts);
	bool			acceptsDrops() const { return _acceptsDrops; }

	void			setNested(bool nested);

	QString			toolTip() const { return _toolTip; }
	void			setToolTip(const QString & toolTip);

signals:
	void			toolTipChanged();

protected:
	void			mousePressEvent(QMouseEvent * event) override;
	void			mouseMoveEvent(QMouseEvent * event) override;
	void			mouseReleaseEvent(QMouseEvent * event) override;
	void			mouseDoubleClickEvent(QMouseEvent * event) override;

private slots:
	void			onLiteralEditFinished();
	void			onBooleanToggled();

private:
	QQuickItem	*	makeText(const QString & text, bool bold = false);
	QQuickItem	*	makeImage(const QString & iconFile);
	QQuickItem	*	makeParenText(const QString & text);
	QQuickItem	*	makeComma();
	ScriptDropSpot* makeDropSpot(const DropTarget & target, const QString & placeholder);
	void			clearLeaves();
	void			addLeaf(QQuickItem * leaf);

	qreal			textWidth(QQuickItem * textItem) const;

	ScriptConstructorView		*	_view = nullptr;
	ScriptNode				*	_node = nullptr;
	QList<QQuickItem*>			_leaves;
	QList<QQuickItem*>			_argumentCommas;
	QList<ScriptDropSpot*>		_dropSpots;
	QPointer<QQuickItem>		_openParen,
								_closeParen,
								_overline,
								_fractionBar;
	qreal						_preferredWidth		= 0,
								_preferredHeight	= 0;
	bool						_acceptsDrops		= true,
								_nested				= false,
								_showParens			= false;
	QString						_toolTip;
};

///
/// Scrollable container for palette items (columns/functions/operators).
/// Supports mouse-wheel scrolling and drag-to-scroll on empty background areas.
class ScriptPalette : public QQuickItem
{
	Q_OBJECT

public:
	explicit ScriptPalette(QQuickItem * parent = nullptr);

	QQuickItem	*	content() const { return _content; }
	void			setContentHeight(qreal height);

protected:
	void			wheelEvent(QWheelEvent * event) override;
	void			mousePressEvent(QMouseEvent * event) override;
	void			mouseMoveEvent(QMouseEvent * event) override;
	void			mouseReleaseEvent(QMouseEvent * event) override;
	void			geometryChange(const QRectF & newGeometry, const QRectF & oldGeometry) override;

private:
	void			clampScroll();

	QQuickItem	*	_content = nullptr;
	qreal			_scrollY = 0;
	bool			_dragScrolling = false;
	qreal			_dragStartY = 0,
					_dragStartScroll = 0;
};

#endif // SCRIPTNODEITEM_H
