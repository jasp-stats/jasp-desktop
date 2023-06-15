#define ENUM_DECLARATION_CPP
#include "datasetbasenode.h"

DataSetBaseNode::DataSetBaseNode(dataSetBaseNodeType typeNode, DataSetBaseNode * parent) 
	: _type(typeNode), _parent(parent)
{
	if(_parent)
		_parent->registerChild(this);
}

DataSetBaseNode::~DataSetBaseNode()
{
	if(_parent)
		_parent->unregisterChild(this);
	_parent = nullptr;
}

void DataSetBaseNode::registerChild(DataSetBaseNode *child)
{
	_children.insert(child);
}

void DataSetBaseNode::unregisterChild(DataSetBaseNode *child)
{
	_children.erase(child);
}

bool DataSetBaseNode::nodeStillExists(DataSetBaseNode *node) const
{
	if(node == this)
		return true;
	
	for(DataSetBaseNode * child : _children)
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
	
	for(DataSetBaseNode * child : _children)
		rev *= child->nestedRevision();
	
	return rev;
}

void DataSetBaseNode::setModifiedCallback(std::function<void ()> callback)
{
	_somethingModifiedCallback = callback;
}

void DataSetBaseNode::checkForChanges()
{
	if(_parent)
		_parent->checkForChanges();
	
	else if(_somethingModifiedCallback) //we have a callback so use it
	{
		int nested = nestedRevision();
		
		if(nested != _previousNestedRevision)
			_somethingModifiedCallback();
		
		_previousNestedRevision = nested;
	}
}
