#ifndef SCRIPTCONSTRUCTORVIEW_H
#define SCRIPTCONSTRUCTORVIEW_H

#include <QQuickItem>
#include <QPointer>
#include <map>
#include "scriptconstructormodel.h"

class ScriptNodeItem;
class ScriptDropSpot;
class QQmlComponent;
class QAbstractItemModel;

///
/// C++ replacement for the old QML FilterConstructor / ComputedColumnsConstructor.
///
/// Owns a ScriptConstructorModel (the single source of truth for the formula tree, JSON and R
/// code) and renders it as a tree of QQuickItems. All formula logic lives in the model; this
/// class only renders, lays out, and forwards user gestures (drag/drop, inline editing, column
/// type changes) to the model.
class ScriptConstructorView : public QQuickItem
{
	Q_OBJECT

	Q_PROPERTY( int					mode					READ modeInt			WRITE setModeInt			NOTIFY modeChanged				)
	Q_PROPERTY( QString				constructorJson			READ constructorJson	WRITE setConstructorJson	NOTIFY constructorJsonChanged	)
	Q_PROPERTY( QString				rCode					READ rCode											NOTIFY rCodeChanged				)
	Q_PROPERTY( bool				somethingChanged		READ somethingChanged	WRITE setSomethingChanged	NOTIFY somethingChangedChanged	)
	Q_PROPERTY( bool				lastCheckPassed			READ lastCheckPassed								NOTIFY lastCheckPassedChanged	)
	Q_PROPERTY( bool				isColumnConstructor		READ isColumnConstructor							NOTIFY modeChanged				)
	Q_PROPERTY( bool				showGeneratedRCode		READ showGeneratedRCode	WRITE setShowGeneratedRCode	NOTIFY showGeneratedRCodeChanged)
	Q_PROPERTY( QAbstractItemModel* columnsModel			READ columnsModel		WRITE setColumnsModel		NOTIFY columnsModelChanged		)

public:
	enum Mode { Filter = 0, ComputedColumn = 1, ComputedDataSet = 2 };
	Q_ENUM(Mode)

	explicit ScriptConstructorView(QQuickItem * parent = nullptr);
	~ScriptConstructorView() override;

	ScriptConstructorModel	*	model() { return &_model; }

	int					modeInt() const { return static_cast<int>(_model.mode()); }
	void				setModeInt(int m);

	QString				constructorJson() const;
	void				setConstructorJson(const QString & json);

	QString				rCode() const;

	bool				somethingChanged() const { return _somethingChanged; }
	void				setSomethingChanged(bool v);

	bool				lastCheckPassed() const { return _lastCheckPassed; }
	bool				isColumnConstructor() const { return _model.mode() != ScriptConstructorMode::Filter; }

	bool				showGeneratedRCode() const { return _showGeneratedRCode; }
	void				setShowGeneratedRCode(bool v);

	QAbstractItemModel*	columnsModel() const { return _columnsModel; }
	void				setColumnsModel(QAbstractItemModel * m);

	void				setColumnTypeProvider(const ScriptColumnTypeProvider * p) { _model.setColumnTypeProvider(p); }
	void				setUndoStack(QUndoStack * s) { _model.setUndoStack(s); }

	// --- QML-callable API mirroring the old constructors ---
	Q_INVOKABLE bool	checkAndApply();
	Q_INVOKABLE void	initializeFromJSON(const QString & json = QString());
	Q_INVOKABLE bool	jsonChanged() const;
	Q_INVOKABLE QString returnFilterJSON() const;

	// --- used by ScriptNodeItem / ScriptDropSpot ---
	QQmlComponent	*	textComponent();
	QQmlComponent	*	imageComponent();
	QQmlComponent	*	textInputComponent();
	QQmlComponent	*	checkBoxComponent();
	QQmlComponent	*	rectangleComponent();

	qreal				blockDim() const;
	qreal				fontPixelSize() const;
	qreal				spacing() const;

	QQuickItem		*	scriptArea() const { return _scriptArea; }
	QQuickItem		*	newLeaf(QQmlComponent * comp);

	void				nodeEdited();

	// --- drag & drop orchestration (called by ScriptNodeItem / palette items) ---
	void				startDragExisting(ScriptNodeItem * item, const QPointF & scenePos);
	void				startDragNew(ScriptNode * newNode, const QPointF & scenePos);
	void				spawnFromPrototype(ScriptNode * proto, const QPointF & scenePos);
	void				dragMove(const QPointF & scenePos);
	void				endDrag(const QPointF & scenePos);
	ScriptDropSpot	*	dropSpotAt(const QPointF & scenePos) const;
	void				collectDropSpots(QList<ScriptDropSpot*> & out) const;

signals:
	void				modeChanged();
	void				constructorJsonChanged();
	void				rCodeChanged(QString rScript);
	void				somethingChangedChanged();
	void				lastCheckPassedChanged();
	void				showGeneratedRCodeChanged();
	void				columnsModelChanged();

	/// Emitted when the user applies a valid formula. The surrounding window persists it
	/// (FilterModel::applyConstructorJson or Column::setConstructorJson/setRCode).
	void				applyRequested(QString json, QString rCode);

protected:
	void				componentComplete() override;
	void				geometryChange(const QRectF & newGeometry, const QRectF & oldGeometry) override;
	QSGNode			*	updatePaintNode(QSGNode * oldNode, UpdatePaintNodeData *) override;

private:
	void				buildChrome();
	void				buildOperatorBar();
	void				buildColumnPalette();
	void				buildFunctionPalette();
	void				rebuildFormulaItems();
	void				clearFormulaItems();
	void				layoutAll();
	void				layoutScriptArea();
	void				refreshHint();

	ScriptNodeItem	*	makeNodeItem(ScriptNode * node, QQuickItem * parent);
	void				clearHover();

	ScriptConstructorModel					_model;

	QPointer<QQuickItem>					_background,
											_operatorBar,
											_columnPalette,
											_functionPalette,
											_scriptArea,
											_scriptColumn,
											_trash,
											_hint;
	std::map<ScriptNode*, ScriptNodeItem*>	_nodeItems;
	QList<ScriptNodeItem*>					_rootItems;

	QPointer<QQmlComponent>					_textComp,
											_imageComp,
											_textInputComp,
											_checkBoxComp,
											_rectComp;

	QAbstractItemModel				*		_columnsModel = nullptr;

	// drag state
	QPointer<ScriptNodeItem>				_draggedItem;
	ScriptNode						*		_draggedNewNode = nullptr;
	QPointF									_dragOffset;
	QPointer<ScriptDropSpot>				_hoveredSpot;
	bool									_dragIsNew = false;

	bool									_somethingChanged	= false,
											_lastCheckPassed		= true,
											_showGeneratedRCode	= false,
											_chromeBuilt			= false;
	QString									_lastAppliedJson;
};

#endif // SCRIPTCONSTRUCTORVIEW_H
