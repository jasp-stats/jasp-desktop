#include "scriptconstructormodel.h"
#include "columntype.h"
#include "timers.h"
#include <QUndoStack>

// --- DropTarget ---

bool DropTarget::accepts(ScriptNode * node) const
{
	if(!node) return false;
	return ScriptConstructorModel::keysOverlap(node->dragKeys(), dropKeys);
}

bool ScriptConstructorModel::keysOverlap(const stringvec & a, const stringvec & b)
{
	for(const std::string & ka : a)
		for(const std::string & kb : b)
			if(ka == kb)
				return true;
	return false;
}

// --- ScriptConstructorModel ---

ScriptConstructorModel::ScriptConstructorModel(QObject * parent)
	: QObject(parent)
{
}

ScriptConstructorModel::~ScriptConstructorModel()
{
	deleteAllFormulas();
}

void ScriptConstructorModel::deleteAllFormulas()
{
	for(ScriptNode * node : _formulas)
		ScriptNode::deleteTree(node);
	_formulas.clear();
}

void ScriptConstructorModel::fromJson(const std::string & json)
{
	JASPTIMER_SCOPE(ScriptConstructorModel fromJson);
	Json::Value root;
	Json::Reader().parse(json, root);
	fromJson(root);
}

void ScriptConstructorModel::fromJson(const Json::Value & json)
{
	deleteAllFormulas();

	const Json::Value & formulas = json.get("formulas", Json::arrayValue);
	for(Json::ArrayIndex i = 0; i < formulas.size(); i++)
	{
		ScriptNode * node = ScriptNode::fromJson(formulas[i], nullptr);
		if(node)
			_formulas.push_back(node);
	}

	emit reset();
	emit changed();
}

Json::Value ScriptConstructorModel::toJson() const
{
	Json::Value json;
	json["formulas"] = Json::arrayValue;

	for(ScriptNode * node : _formulas)
		json["formulas"].append(node->toJson());

	return json;
}

std::string ScriptConstructorModel::toString() const
{
	JASPTIMER_SCOPE(ScriptConstructorModel toString);
	Json::StreamWriterBuilder builder;
	builder["indentation"] = "";
	std::string out = Json::writeString(builder, toJson());

	while(!out.empty() && (out.back() == '\n' || out.back() == '\r' || out.back() == ' '))
		out.pop_back();

	return out;
}

std::string ScriptConstructorModel::toR() const
{
	JASPTIMER_SCOPE(ScriptConstructorModel toR);
	std::string out;

	for(int i = 0; i < static_cast<int>(_formulas.size()); i++)
	{
		if(i > 0) out += "& ";
		out += _formulas[i]->toR(_typeProvider);

		if(_mode == ScriptConstructorMode::Filter)
			out += "\n";
		else if(i < static_cast<int>(_formulas.size()) - 1)
			out += "\n";
	}

	return out;
}

bool ScriptConstructorModel::checkCompleteness() const
{
	for(ScriptNode * node : _formulas)
		if(!node->isComplete())
			return false;
	return true;
}

bool ScriptConstructorModel::allBoolean() const
{
	for(ScriptNode * node : _formulas)
	{
		bool isBool = false;
		for(const std::string & key : node->dragKeys())
			if(key == "boolean")
				isBool = true;

		if(!isBool)
			return false;
	}
	return true;
}

ScriptNode * ScriptConstructorModel::formulaAt(int index) const
{
	if(index < 0 || index >= static_cast<int>(_formulas.size()))
		return nullptr;
	return _formulas[index];
}

int ScriptConstructorModel::rootIndexOf(ScriptNode * node) const
{
	for(int i = 0; i < static_cast<int>(_formulas.size()); i++)
		if(_formulas[i] == node)
			return i;
	return -1;
}

ScriptNode * ScriptConstructorModel::rootFormulaOf(ScriptNode * node) const
{
	ScriptNode * cur = node;
	while(cur && cur->parent())
		cur = cur->parent();
	return cur;
}

bool ScriptConstructorModel::isAncestor(ScriptNode * ancestor, ScriptNode * descendant) const
{
	ScriptNode * cur = descendant;
	while(cur)
	{
		if(cur == ancestor) return true;
		cur = cur->parent();
	}
	return false;
}

void ScriptConstructorModel::detachFromParent(ScriptNode * node)
{
	if(!node) return;

	ScriptNode * par = node->parent();
	if(!par)
	{
		int idx = rootIndexOf(node);
		if(idx >= 0)
			_formulas.erase(_formulas.begin() + idx);
		return;
	}

	if(auto * op = dynamic_cast<ScriptNodeOperator*>(par))
	{
		if(op->leftChild() == node)	op->setLeft(nullptr);
		else							op->setRight(nullptr);
	}
	else if(auto * func = dynamic_cast<ScriptNodeFunction*>(par))
	{
		for(int i = 0; i < func->childCount(); i++)
			if(func->childAt(i) == node)
			{
				func->setArgumentValue(i, nullptr);
				break;
			}
	}
	else if(auto * rowFunc = dynamic_cast<ScriptNodeRowFunction*>(par))
	{
		for(int i = 0; i < rowFunc->childCount(); i++)
			if(rowFunc->childAt(i) == node)
			{
				rowFunc->setChild(i, nullptr);
				break;
			}
	}

	node->setParent(nullptr);
}

void ScriptConstructorModel::placeAt(ScriptNode * node, const DropTarget & target)
{
	switch(target.kind)
	{
	case DropTarget::Kind::Root:
	{
		int idx = target.index;
		if(idx < 0 || idx > static_cast<int>(_formulas.size()))
			idx = static_cast<int>(_formulas.size());
		_formulas.insert(_formulas.begin() + idx, node);
		node->setParent(nullptr);
		break;
	}
	case DropTarget::Kind::OperatorLeft:
		if(auto * op = dynamic_cast<ScriptNodeOperator*>(target.parent))
			op->setLeft(node);
		break;
	case DropTarget::Kind::OperatorRight:
		if(auto * op = dynamic_cast<ScriptNodeOperator*>(target.parent))
			op->setRight(node);
		break;
	case DropTarget::Kind::FunctionArg:
		if(auto * func = dynamic_cast<ScriptNodeFunction*>(target.parent))
			func->setArgumentValue(target.index, node);
		break;
	case DropTarget::Kind::RowFunctionArg:
		if(auto * rowFunc = dynamic_cast<ScriptNodeRowFunction*>(target.parent))
		{
			rowFunc->setChild(target.index, node);
			rowFunc->ensureTrailingEmptySlot();
		}
		break;
	case DropTarget::Kind::None:
		break;
	}

	if(auto * col = dynamic_cast<ScriptNodeColumn*>(node))
		resolveColumnTypeDrop(col, target.dropKeys);
}

void ScriptConstructorModel::resolveColumnTypeDrop(ScriptNodeColumn * col, const stringvec & slotKeys)
{
	// Mirrors the old JASPColumn.qml dropHandler.onWasDroppedOn():
	// prefer the user-selected type if the slot accepts it, then the actual
	// column type, then scale/ordinal/nominal in order.
	col->setColumnTypeDrop(-1);

	if(slotKeys.empty())
		return;

	auto accepts = [&slotKeys](int colType)
	{
		return keysOverlap(ScriptConstructorRegistry::dropKeysForColumnType(colType), slotKeys);
	};

	int userType = col->columnTypeUser();
	if(userType != -1 && accepts(userType))
	{
		col->setColumnTypeDrop(userType);
		return;
	}

	int actualType = _typeProvider ? _typeProvider->columnType(col->columnName()) : 1;
	if(accepts(actualType))
	{
		col->setColumnTypeDrop(actualType);
		return;
	}

	for(int t : {int(columnType::scale), int(columnType::ordinal), int(columnType::nominal)})
	{
		if(accepts(t))
		{
			col->setColumnTypeDrop(t);
			return;
		}
	}
}

// --- right-most empty / filled drop spot helpers ---

static stringvec containingSlotKeys(ScriptNode * node)
{
	ScriptNode * par = node ? node->parent() : nullptr;
	if(!par) return {};

	if(auto * op = dynamic_cast<ScriptNodeOperator*>(par))
		return op->leftChild() == node ? op->dropKeysLeft() : op->dropKeysRight();

	if(auto * func = dynamic_cast<ScriptNodeFunction*>(par))
	{
		for(int i = 0; i < func->childCount(); i++)
			if(func->childAt(i) == node)
				return func->arguments()[i].dropKeys;
		return {};
	}

	if(dynamic_cast<ScriptNodeRowFunction*>(par))
		return {"number"};

	return {};
}

static DropTarget makeSlotTarget(ScriptNode * parent, DropTarget::Kind kind, int index, const stringvec & keys)
{
	DropTarget t;
	t.kind = kind;
	t.parent = parent;
	t.index = index;
	t.dropKeys = keys;
	return t;
}

static DropTarget leftMostEmptyDropSpotRec(ScriptNode * node, const stringvec & dragKeys)
{
	// In-order leftmost empty slot that accepts the dragged node's keys: for operators the
	// left subtree/slot before the right one, for functions and row functions the arguments
	// in ascending index (descending into filled arguments to find nested empty slots further
	// left). Non-accepting empty slots are skipped, search continues to their right.
	if(!node) return DropTarget::none();

	if(auto * op = dynamic_cast<ScriptNodeOperator*>(node))
	{
		if(op->leftChild())
		{
			DropTarget sub = leftMostEmptyDropSpotRec(op->leftChild(), dragKeys);
			if(sub.isValid())
				return sub;
		}
		else if(ScriptConstructorModel::keysOverlap(dragKeys, op->dropKeysLeft()))
			return makeSlotTarget(op, DropTarget::Kind::OperatorLeft, 0, op->dropKeysLeft());

		if(op->rightChild())
		{
			DropTarget sub = leftMostEmptyDropSpotRec(op->rightChild(), dragKeys);
			if(sub.isValid())
				return sub;
		}
		else if(ScriptConstructorModel::keysOverlap(dragKeys, op->dropKeysRight()))
			return makeSlotTarget(op, DropTarget::Kind::OperatorRight, 1, op->dropKeysRight());

		return DropTarget::none();
	}

	if(auto * func = dynamic_cast<ScriptNodeFunction*>(node))
	{
		for(int i = 0; i < func->childCount(); i++)
		{
			const auto & arg = func->arguments()[i];
			if(arg.value)
			{
				DropTarget sub = leftMostEmptyDropSpotRec(arg.value, dragKeys);
				if(sub.isValid())
					return sub;
			}
			else if(ScriptConstructorModel::keysOverlap(dragKeys, arg.dropKeys))
				return makeSlotTarget(func, DropTarget::Kind::FunctionArg, i, arg.dropKeys);
		}
		return DropTarget::none();
	}

	if(auto * rowFunc = dynamic_cast<ScriptNodeRowFunction*>(node))
	{
		for(int i = 0; i < rowFunc->childCount(); i++)
		{
			if(rowFunc->childAt(i))
			{
				DropTarget sub = leftMostEmptyDropSpotRec(rowFunc->childAt(i), dragKeys);
				if(sub.isValid())
					return sub;
			}
			else if(ScriptConstructorModel::keysOverlap(dragKeys, {"number"}))
				return makeSlotTarget(rowFunc, DropTarget::Kind::RowFunctionArg, i, {"number"});
		}
		return DropTarget::none();
	}

	return DropTarget::none();
}

static DropTarget rightMostFilledDropSpotRec(ScriptNode * node)
{
	if(!node) return DropTarget::none();

	if(auto * op = dynamic_cast<ScriptNodeOperator*>(node))
	{
		if(op->rightChild())
			return makeSlotTarget(op, DropTarget::Kind::OperatorRight, 1, op->dropKeysRight());
		return DropTarget::none();
	}

	if(auto * func = dynamic_cast<ScriptNodeFunction*>(node))
	{
		DropTarget last;
		for(int i = 0; i < func->childCount(); i++)
		{
			if(!func->childAt(i))
				return last.isValid() ? last : DropTarget::none();
			last = makeSlotTarget(func, DropTarget::Kind::FunctionArg, i, func->arguments()[i].dropKeys);
		}
		return last.isValid() ? last : DropTarget::none();
	}

	if(auto * rowFunc = dynamic_cast<ScriptNodeRowFunction*>(node))
	{
		DropTarget last;
		for(int i = 0; i < rowFunc->childCount(); i++)
		{
			if(!rowFunc->childAt(i))
				return last.isValid() ? last : DropTarget::none();
			last = makeSlotTarget(rowFunc, DropTarget::Kind::RowFunctionArg, i, {"number"});
		}
		return last.isValid() ? last : DropTarget::none();
	}

	return DropTarget::none();
}

DropTarget ScriptConstructorModel::findReasonableInsertionSpot(ScriptNode * node) const
{
	// "Best spot" for a node dropped without an explicit target: scan the root formulas in
	// order (they are laid out top-to-bottom) and take the first formula whose leftmost empty
	// slot accepts the node — consecutive drops fill the constructor left-to-right,
	// top-to-bottom. Returning nothing lets the caller fall back to gobble-left absorption.
	for(ScriptNode * formula : _formulas)
	{
		if(formula == node)
			continue;

		DropTarget spot = leftMostEmptyDropSpotRec(formula, node->dragKeys());
		if(spot.isValid())
			return spot;
	}

	return DropTarget::none();
}

std::vector<int> ScriptConstructorModel::allowedColumnTypes(ScriptNode * node) const
{
	if(!node || !node->parent())
		return {int(columnType::scale), int(columnType::ordinal), int(columnType::nominal)}; // root: unconstrained

	const stringvec keys = containingSlotKeys(node);

	std::vector<int> out;
	for(int t : {int(columnType::scale), int(columnType::ordinal), int(columnType::nominal)})
		if(keysOverlap(ScriptConstructorRegistry::dropKeysForColumnType(t), keys))
			out.push_back(t);
	return out;
}

// --- editing operations ---

void ScriptConstructorModel::beginEdit()
{
	_editBeforeJson = toString();
}

void ScriptConstructorModel::endEdit(const QString & description)
{
	emit changed();

	if(!_undoStack)
		return;

	std::string afterJson = toString();
	if(afterJson == _editBeforeJson)
		return;

	_undoStack->push(new ScriptConstructorEditCommand(this, _editBeforeJson, afterJson, description));
}

void ScriptConstructorModel::insertNode(ScriptNode * node, DropTarget target)
{
	if(!node) return;

	beginEdit();

	if(target.isValid())
	{
		placeAt(node, target);
	}
	else
	{
		_formulas.push_back(node);
		node->setParent(nullptr);

		DropTarget spot = findReasonableInsertionSpot(node);
		if(spot.isValid() && keysOverlap(node->dragKeys(), spot.dropKeys))
		{
			detachFromParent(node);
			placeAt(node, spot);
		}
		else
			tryGobbleLeft(node);
	}

	endEdit(tr("Insert element"));
}

void ScriptConstructorModel::removeNode(ScriptNode * node)
{
	if(!node) return;

	beginEdit();
	detachFromParent(node);
	ScriptNode::deleteTree(node);
	endEdit(tr("Remove element"));
}

void ScriptConstructorModel::moveNode(ScriptNode * node, DropTarget target)
{
	if(!node) return;

	if(target.isValid() && isAncestor(node, target.parent))
		return;

	beginEdit();
	detachFromParent(node);

	if(target.isValid())
		placeAt(node, target);
	else
	{
		_formulas.push_back(node);
		node->setParent(nullptr);
	}

	endEdit(tr("Move element"));
}

void ScriptConstructorModel::setColumnTypeUser(ScriptNodeColumn * node, int columnType)
{
	if(!node) return;
	beginEdit();
	node->setColumnTypeUser(columnType);
	// Re-resolve the drop-time type, like the old JASPColumn.qml did when the
	// user clicked the type icon while the column was inside a drop spot.
	resolveColumnTypeDrop(node, containingSlotKeys(node));
	endEdit(tr("Change column type"));
}

void ScriptConstructorModel::setLiteralNumber(ScriptNodeLiteral * node, double value)
{
	if(!node) return;
	beginEdit();
	node->setNumberValue(value);
	endEdit(tr("Edit number"));
}

void ScriptConstructorModel::setLiteralBool(ScriptNodeLiteral * node, bool value)
{
	if(!node) return;
	beginEdit();
	node->setBoolValue(value);
	endEdit(tr("Edit logical"));
}

void ScriptConstructorModel::setLiteralString(ScriptNodeLiteral * node, const std::string & value)
{
	if(!node) return;
	beginEdit();
	node->setStringValue(value);
	endEdit(tr("Edit text"));
}

void ScriptConstructorModel::clear()
{
	beginEdit();
	deleteAllFormulas();
	endEdit(tr("Clear all"));
	emit reset();
}

bool ScriptConstructorModel::tryGobbleLeft(ScriptNode * node)
{
	auto * op = dynamic_cast<ScriptNodeOperator*>(node);
	if(!op || op->leftChild() != nullptr)
		return false;

	if(_formulas.size() <= 1)
		return false;

	stringvec leftKeys = op->dropKeysLeft();

	for(int i = static_cast<int>(_formulas.size()) - 1; i >= 0; i--)
	{
		if(_formulas[i] == node)
			continue;

		ScriptNode * gobbleMeUp	= _formulas[i];
		DropTarget putResultHere = DropTarget::root(i);
		bool putIsRoot = true;

		while(gobbleMeUp)
		{
			if(keysOverlap(gobbleMeUp->dragKeys(), leftKeys))
			{
				bool iFitHere = putIsRoot || keysOverlap(node->dragKeys(), putResultHere.dropKeys);
				if(iFitHere)
				{
					int gobbleRootIndex = rootIndexOf(gobbleMeUp);

					detachFromParent(gobbleMeUp);
					op->setLeft(gobbleMeUp);

					detachFromParent(node);
					if(putIsRoot)
					{
						int insertAt = gobbleRootIndex >= 0 ? gobbleRootIndex : static_cast<int>(_formulas.size());
						if(insertAt > static_cast<int>(_formulas.size())) insertAt = static_cast<int>(_formulas.size());
						_formulas.insert(_formulas.begin() + insertAt, node);
						node->setParent(nullptr);
					}
					else
						placeAt(node, putResultHere);

					return true;
				}
			}

			DropTarget filled = rightMostFilledDropSpotRec(gobbleMeUp);
			if(!filled.isValid())
				return false;

			gobbleMeUp		= filled.parent ? filled.parent->childAt(filled.index) : nullptr;
			putResultHere	= filled;
			putIsRoot		= false;
		}

		return false;
	}

	return false;
}

// --- ScriptConstructorEditCommand ---

ScriptConstructorEditCommand::ScriptConstructorEditCommand(ScriptConstructorModel * model, const std::string & beforeJson, const std::string & afterJson, const QString & description)
	: QUndoCommand(description)
	, _model(model)
	, _beforeJson(beforeJson)
	, _afterJson(afterJson)
{
}

void ScriptConstructorEditCommand::undo()
{
	if(!_model) return;
	_model->fromJson(_beforeJson);
}

void ScriptConstructorEditCommand::redo()
{
	if(!_model) return;

	if(_firstRedo)
	{
		_firstRedo = false;
		return;
	}

	_model->fromJson(_afterJson);
}
