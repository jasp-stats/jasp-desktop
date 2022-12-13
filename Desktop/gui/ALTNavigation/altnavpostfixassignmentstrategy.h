#ifndef ALTNAVPOSTFIXASSIGNMENTSTRATEGY_H
#define ALTNAVPOSTFIXASSIGNMENTSTRATEGY_H

#include <QObject>

class ALTNavScope;

class ALTNavPostfixAssignmentStrategy
{
	Q_GADGET

public:
	virtual ~ALTNavPostfixAssignmentStrategy() {};
	virtual void assignPostfixes(QObjectList const&  children, QString prefix) = 0;

	enum AssignmentStrategy { PASSTHROUGH, INDEXED, PRIORITY, UNKNOWN };
	Q_ENUM(AssignmentStrategy)


	//factory method
	static ALTNavPostfixAssignmentStrategy* createStrategy(AssignmentStrategy strategy);



};

class PriorityStrategy : public ALTNavPostfixAssignmentStrategy
{
public:
	PriorityStrategy(){};
	~PriorityStrategy(){};
	void assignPostfixes(QObjectList const&  children, QString prefix);

};

class IndexedStrategy : public ALTNavPostfixAssignmentStrategy
{
public:
	IndexedStrategy(){};
	~IndexedStrategy(){};
	void assignPostfixes(QObjectList const&  children, QString prefix);
};

class PassthroughStrategy : public ALTNavPostfixAssignmentStrategy
{
public:
	PassthroughStrategy(){};
	~PassthroughStrategy(){};
	void assignPostfixes(QObjectList const&  children, QString prefix);
};

#endif // ALTNAVPOSTFIXASSIGNMENTSTRATEGY_H
