#ifndef SCRIPTCONSTRUCTORMODEL_H
#define SCRIPTCONSTRUCTORMODEL_H

#include <QObject>
#include <QUndoCommand>
#include <json/json.h>
#include <vector>
#include "scriptnode.h"
#include "scriptconstructorregistry.h"

class QUndoStack;

/// Describes a place where a ScriptNode can be (or is requested to be) inserted.
struct DropTarget
{
	enum class Kind { None, Root, OperatorLeft, OperatorRight, FunctionArg, RowFunctionArg };

	Kind			kind		= Kind::None;
	ScriptNode	*	parent		= nullptr;	///< Node owning the slot (nullptr for Root)
	int				index		= -1;		///< Formula index for Root, argument index for Function/RowFunction
	stringvec		dropKeys;				///< Keys accepted at this spot

	bool isValid() const { return kind != Kind::None; }
	bool isRoot() const { return kind == Kind::Root; }
	bool accepts(ScriptNode * node) const;

	static DropTarget none() { return {}; }
	static DropTarget root(int formulaIndex = -1) { DropTarget t; t.kind = Kind::Root; t.index = formulaIndex; return t; }
};

///
/// Owns the tree of ScriptNodes that make up a drag-and-drop filter / computed column formula.
/// All mutations go through this class so that undo and change-notification have a single source.
/// It is UI-agnostic: the QQuickItem view layer renders the tree and forwards user gestures here.
class ScriptConstructorModel : public QObject
{
	Q_OBJECT

public:
	explicit ScriptConstructorModel(QObject * parent = nullptr);
	~ScriptConstructorModel() override;

	// (Re)build the whole tree from stored JSON. Does not push undo.
	void			fromJson(const std::string & json);
	void			fromJson(const Json::Value & json);

	std::string		toString() const;					///< {"formulas":[...]}
	Json::Value		toJson() const;
	std::string		toR() const;						///< R code for all formulas

	bool			checkCompleteness() const;			///< true when every required slot is filled
	bool			allBoolean() const;					///< true when every root formula returns boolean
	int				formulaCount() const { return static_cast<int>(_formulas.size()); }
	ScriptNode	*	formulaAt(int index) const;
	const std::vector<ScriptNode*> & formulas() const { return _formulas; }

	ScriptConstructorMode mode() const { return _mode; }
	void			setMode(ScriptConstructorMode mode) { _mode = mode; }

	void			setColumnTypeProvider(const ScriptColumnTypeProvider * provider) { _typeProvider = provider; }
	const ScriptColumnTypeProvider * columnTypeProvider() const { return _typeProvider; }

	void			setUndoStack(QUndoStack * stack) { _undoStack = stack; }

	// --- editing operations (each pushes an undo command when an undo stack is set) ---
	void			insertNode(ScriptNode * node, DropTarget target);	///< takes ownership of node
	void			removeNode(ScriptNode * node);						///< deletes the node subtree
	void			moveNode(ScriptNode * node, DropTarget target);
	void			setColumnTypeUser(ScriptNodeColumn * node, int columnType);
	void			setLiteralNumber(ScriptNodeLiteral * node, double value);
	void			setLiteralBool(ScriptNodeLiteral * node, bool value);
	void			setLiteralString(ScriptNodeLiteral * node, const std::string & value);
	void			clear();

	/// Resolves where a freshly created node should go when the user did not drop it anywhere specific.
	DropTarget		findReasonableInsertionSpot(ScriptNode * node) const;

	/// Returns the drop keys accepted at a given target (used by the view for hover feedback).
	static bool		keysOverlap(const stringvec & a, const stringvec & b);

signals:
	void			changed();		///< tree contents changed (any edit)
	void			reset();		///< whole tree rebuilt (fromJson/clear/undo); view must rebuild items

private:
	void			deleteAllFormulas();
	void			detachFromParent(ScriptNode * node);
	void			placeAt(ScriptNode * node, const DropTarget & target);
	ScriptNode	*	rootFormulaOf(ScriptNode * node) const;
	int				rootIndexOf(ScriptNode * node) const;
	bool			isAncestor(ScriptNode * ancestor, ScriptNode * descendant) const;
	DropTarget		resolveInsertionTarget(ScriptNode * node, DropTarget requested);
	bool			tryGobbleLeft(ScriptNode * node);

	void			beginEdit();
	void			endEdit(const QString & description);

	std::vector<ScriptNode*>			_formulas;
	ScriptConstructorMode				_mode			= ScriptConstructorMode::Filter;
	const ScriptColumnTypeProvider	*	_typeProvider	= nullptr;
	QUndoStack						*	_undoStack		= nullptr;
	std::string							_editBeforeJson;
};

/// Undo command that snapshots the constructor JSON before and after an edit.
class ScriptConstructorEditCommand : public QUndoCommand
{
public:
	ScriptConstructorEditCommand(ScriptConstructorModel * model, const std::string & beforeJson, const std::string & afterJson, const QString & description);

	void undo() override;
	void redo() override;

private:
	ScriptConstructorModel	* _model;
	std::string				_beforeJson,
							_afterJson;
	bool					_firstRedo = true;
};

#endif // SCRIPTCONSTRUCTORMODEL_H
