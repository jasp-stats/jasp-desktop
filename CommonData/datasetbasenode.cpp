#include "log.h"
#include <QThread>
#include <QPointer>
#include "dataenums.h"
#include "datasetbasenode.h"
#include <QGuiApplication>

DataSetBaseNode::DataSetBaseNode(dataSetBaseNodeType typeNode, QObject * parent) 
	: QAbstractTableModel(nullptr), _type(typeNode), _nodeAbove(dynamic_cast<DataSetBaseNode *>(parent))
{
	//Nodes are built by the importers on the AsyncLoader (and QThreadPool) threads but must live on
	//the GUI thread, because they are QAbstractItemModels that QML binds to.
	QThread * target =	QGuiApplication::instance()	? QGuiApplication::instance()->thread()
						:	parent						? parent->thread()
						:							  thread();

	if(thread() != target)
		moveToThread(target);

	//QObject::setParent() sends a ChildAdded event to the parent, and QCoreApplication::sendEvent()
	//asserts (in debug builds) when called from a thread other than the one owning the receiver.
	//The moveToThread() above already made our affinity match the parent's, so Qt's own "new parent
	//is in a different thread" guard no longer catches this and the assert fires for every
	//Column/Label/DataSet created during an import. Hand the parenting to the parent's thread instead.
	//Only the QObject ownership is deferred; the node tree (_nodeAbove/registerNode below) is still
	//wired up synchronously, and DataSetBaseNode::parent() returns _nodeAbove anyway.
	if(parent)
	{
		if(QThread::currentThread() == target)
			setParent(parent);
		else
		{
			QPointer<DataSetBaseNode> self(this); //We might be destroyed again before the GUI thread gets round to this
			QMetaObject::invokeMethod(parent, [self, parent]() { if(self) self->setParent(parent); }, Qt::QueuedConnection);
		}
	}
	
	if(_nodeAbove)
		_nodeAbove->registerNode(this);
	
	connect(this, &QAbstractItemModel::modelReset,		this,	&DataSetBaseNode::rowCountChanged);
	connect(this, &QAbstractItemModel::rowsInserted,	this,	&DataSetBaseNode::rowCountChanged);
	connect(this, &QAbstractItemModel::rowsRemoved,		this,	&DataSetBaseNode::rowCountChanged);

	connect(this, &QAbstractItemModel::modelReset,		this,	&DataSetBaseNode::columnCountChanged);	
	connect(this, &QAbstractItemModel::columnsInserted,	this,	&DataSetBaseNode::columnCountChanged);
	connect(this, &QAbstractItemModel::columnsRemoved,	this,	&DataSetBaseNode::columnCountChanged);
}

DataSetBaseNode::~DataSetBaseNode()
{
	if(_nodeAbove)
		_nodeAbove->unregisterNode(this);
	 
	_nodeAbove = nullptr;
}

void DataSetBaseNode::registerNode(DataSetBaseNode *child)
{
	_nodesBelow.insert(child);
}

void DataSetBaseNode::unregisterNode(DataSetBaseNode *child)
{
	child->_nodeAbove = nullptr;
	_nodesBelow.erase(child);
}

bool DataSetBaseNode::nodeStillExists(DataSetBaseNode *node) const
{
	if(node == this)
		return true;
	
	for(DataSetBaseNode * child : _nodesBelow)
		if(child->nodeStillExists(node))
			return true;

	return false;
}

void DataSetBaseNode::incRevision()
{
	_revision++;
	checkForChanges();
}

int DataSetBaseNode::nestedRevision()
{
	int rev = _revision;
	
	//Sum (not product) of child revisions: a multiplier is fragile as a change detector because any
	//child whose revision is 0 forces the whole product to 0, masking parent-only changes. A
	//monotonic sum strictly increases whenever the node or any descendant's revision is incremented.
	for(DataSetBaseNode * child : _nodesBelow)
		rev += child->nestedRevision();
	
	return rev;
}

void DataSetBaseNode::checkForChanges()
{
	if(_nodeAbove)
		_nodeAbove->checkForChanges();
	else
	{
		int nested = nestedRevision();
		
		if(nested != _previousNestedRevision)
			emit somethingModified();
		
		_previousNestedRevision = nested;
	}
}

QHash<int, QByteArray> DataSetBaseNode::roleNames() const
{
	static bool						set = false;
	static QHash<int, QByteArray> roles = QAbstractTableModel::roleNames();

	if(!set)
	{
		for(const auto & enumString : dataPkgRolesToStringMap())
			roles[int(enumString.first)] = QString::fromStdString(enumString.second).toUtf8();

		set = true;
	}

	return roles;
}
