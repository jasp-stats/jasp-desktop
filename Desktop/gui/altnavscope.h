#ifndef ALTNAVSCOPE_H
#define ALTNAVSCOPE_H

#include <QObject>
#include <QQuickItem>

#include "altnavtag.h"
#include "altnavpostfixassignmentstrategy.h"

using AssignmentStrategy = ALTNavPostfixAssignmentStrategy::AssignmentStrategy;

class ALTNavScope : public QObject
{
	Q_OBJECT

	//ALTNavigation Interface
	Q_PROPERTY( bool						enabled						MEMBER		enabled					WRITE	setEnabled			NOTIFY	enabledChanged													);
	Q_PROPERTY( QObject*					parentScope					MEMBER		parentScopeAttachee		WRITE	setParentAttachee	NOTIFY	parentScopeChanged												);
	Q_PROPERTY( bool						scopeOnly					MEMBER		scopeOnly				WRITE	setScopeOnly		NOTIFY	scopeOnlyChanged												);
	Q_PROPERTY( AssignmentStrategy			strategy					MEMBER		currentStrategy			WRITE	setStrategy			NOTIFY	postfixAssignmentStrategyChanged								);
	Q_PROPERTY( QString						requestedPostfix			READ		getRequestedPostfix		WRITE	setRequestedPostfix	NOTIFY	requestedPostfixChanged											);
	Q_PROPERTY( int							scopePriority				READ		getScopePriority		WRITE	setScopePriority	NOTIFY	scopePriorityChanged											);
	Q_PROPERTY( int							index						READ		getIndex				WRITE	setIndex			NOTIFY	indexChanged													);

signals:
	void enabledChanged();
	void tagMatch();
	void postfixAssignmentStrategyChanged();
	void scopeOnlyChanged();
	void requestedPostfixChanged();
	void scopePriorityChanged();
	void globalPriorityChanged();
	void indexChanged();
	void parentScopeChanged();

public:
	explicit ALTNavScope(QObject* _attachee);
	~ALTNavScope();

	void traverse(QString input);
	void match();
	void setStrategy(ALTNavPostfixAssignmentStrategy* strategy);
	void setPrefix(QString prefix);
	void setScopeActive(bool value);
	void setChildrenPrefix(QString prefix);
	void setChildrenActive(bool value);

	QString getRequestedPostfix();
	int getScopePriority();
	int getIndex();

public slots:
	void registerWithParent();


protected:
	void childEvent(QChildEvent *event) override;
	void setEnabled(bool value);
	void setStrategy(AssignmentStrategy strategy);
	void setScopeOnly(bool value);
	void setParentAttachee(QObject* _parent);
	void setRequestedPostfix(QString postfix);
	void setScopePriority(int priority);
	void setIndex(int index);

	bool scopeActive = false;
	QString prefix;


private:
	//ALTNavigation Interface
	bool enabled = false;
	bool scopeOnly = false;
	QString requestedPostfix = "";
	int index = -1;
	int scopePriority = 0;
	AssignmentStrategy currentStrategy = AssignmentStrategy::PRIORITY;

	QQuickItem* attachee;
	ALTNavTag* attachedTag = nullptr;
	QQuickItem* parentScopeAttachee = nullptr;
	int treeDepth;
	bool parentOverride = false;


	ALTNavPostfixAssignmentStrategy* postfixBroker = nullptr;

	friend class ALTNavPostfixAssignmentStrategy;

};

#endif // ALTNAVSCOPE_H
