#ifndef ALTNAVSCOPE_H
#define ALTNAVSCOPE_H

#include <QObject>
#include <QQuickItem>

#include "altnavtag.h"

class ALTNavPostfixAssignmentStrategy;

class ALTNavScope : public QObject
{
	Q_OBJECT

	//ALTNavigation Interface
	Q_PROPERTY( bool						enabled						MEMBER		enabled					WRITE	setEnabled			NOTIFY	enabledChanged													);
	Q_PROPERTY( QObject*					parentScope					MEMBER		parentScopeAttachee		WRITE	setParentAttachee	NOTIFY	parentScopeChanged												);
	Q_PROPERTY( bool						scopeOnly					MEMBER		scopeOnly				WRITE	setScopeOnly		NOTIFY	scopeOnlyChanged												);
	Q_PROPERTY( AssignmentStrategy			postfixAssignmentStrategy	MEMBER		currentStrategy			WRITE	setStrategy			NOTIFY	postfixAssignmentStrategyChanged								);
	Q_PROPERTY( QString						requestedPostfix			MEMBER		requestedPostfix		WRITE	setRequestedPostfix	NOTIFY	requestedPostfixChanged											);
	Q_PROPERTY( int							scopePriority				MEMBER		scopePriority			WRITE	setScopePriority	NOTIFY	scopePriorityChanged											);
	Q_PROPERTY( int							index						MEMBER		index					WRITE	setIndex			NOTIFY	indexChanged													);

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

	enum AssignmentStrategy { PASSTHROUGH, INDEXED, PRIORITY, UNKNOWN };
	Q_ENUM(AssignmentStrategy)

	void traverse(QString input);
	void setStrategy(ALTNavPostfixAssignmentStrategy* strategy);
	void setPrefix(QString prefix);
	void setScopeActive(bool value);
	void setChildrenPrefix(QString tag);
	void setChildrenActive(bool value);

public slots:
	void registerWithParent();


protected:
	void childEvent(QChildEvent *event) override;

	bool scopeActive = false;
	QString prefix;

private:
	void setEnabled(bool value);
	void setStrategy(AssignmentStrategy strategy);
	void setScopeOnly(bool value);
	void setParentAttachee(QObject* _parent);
	void setRequestedPostfix(QString postfix);
	void setScopePriority(int priority);
	void setIndex(int index);


private:
	//ALTNavigation Interface
	bool enabled = false;
	bool scopeOnly = false;
	QString requestedPostfix = "";
	int index = -1;
	int scopePriority = 0;
	AssignmentStrategy currentStrategy = PRIORITY;

	QQuickItem* attachee;
	ALTNavTag* attachedTag = nullptr;
	QQuickItem* parentScopeAttachee = nullptr;
	int treeDepth;
	bool parentOverride = false;


	ALTNavPostfixAssignmentStrategy* postfixBroker = nullptr;

};

#endif // ALTNAVSCOPE_H
