#ifndef ALTNAVSCOPE_H
#define ALTNAVSCOPE_H

#include <QObject>
#include <QQuickItem>

#include "altnavtag.h"
#include "altnavpostfixassignmentstrategy.h"

using AssignmentStrategy = ALTNavPostfixAssignmentStrategy::AssignmentStrategy;

/*! \brief Attaching object type providing the properties and signals along with most of the tree construction and traversal logic.
 *
 */
class ALTNavScope : public QObject
{
	Q_OBJECT

	//ALTNavigation Interface
	//! When set to false an ALTNavscope unparent itself until enabled is true once again
	Q_PROPERTY( bool						enabled						MEMBER		enabled					WRITE	setEnabled			NOTIFY	enabledChanged													);
	//! Defines a scope as a root
	Q_PROPERTY( bool						root						MEMBER		root					WRITE	setRoot				NOTIFY	rootChanged														);
	//! Overrules the parent discovered by the normal tree forming parent search method. If null the default root is set as parent
	Q_PROPERTY( QObject*					parent						MEMBER		parentScopeAttachee		WRITE	setParentAttachee	NOTIFY	parentScopeChanged												);
	//! Set if their should be no visual representation
	Q_PROPERTY( bool						scopeOnly					MEMBER		scopeOnly				WRITE	setScopeOnly		NOTIFY	scopeOnlyChanged												);
	//! Children of this scope will also be visible when this node is.
	Q_PROPERTY( bool						showChildren				MEMBER		propagateActivity																											);
	//! Define postfix allocation strategy
	Q_PROPERTY( AssignmentStrategy			strategy					MEMBER		currentStrategy			WRITE	setStrategy			NOTIFY	postfixAssignmentStrategyChanged								);
	//! Defines the x position of the visual tag relative to the attachee
	Q_PROPERTY( int							x							MEMBER		x						WRITE	setX																						);
	//! Defines the y position of the visual tag relative to the attachee
	Q_PROPERTY( int							y							MEMBER		y						WRITE	setY																						);
	//! This property may be set to true if the scope should be forcefully set to the current node (e.g. submenu is open)
	Q_PROPERTY( bool						foreground					READ		foreground				WRITE	setForeground		NOTIFY	foregroundChanged												);
	//! Defines a postfix preference that the current strategy may use
	Q_PROPERTY( QString						requestedPostfix			READ		getRequestedPostfix		WRITE	setRequestedPostfix	NOTIFY	requestedPostfixChanged											);
	//! Defines a priority for the prefix preference that the strategy may use
	Q_PROPERTY( int							scopePriority				READ		getScopePriority		WRITE	setScopePriority	NOTIFY	scopePriorityChanged											);
	//! Defines a index that the current strategy may use to make ordered postfixes (e.g. items in a list)
	Q_PROPERTY( int							index						READ		getIndex				WRITE	setIndex			NOTIFY	indexChanged													);


signals:
	//! Signal is emitted upon tag match
	void tagMatch();
	void enabledChanged();
	void postfixAssignmentStrategyChanged();
	void scopeOnlyChanged();
	void requestedPostfixChanged();
	void scopePriorityChanged();
	void indexChanged();
	void parentScopeChanged();
	void foregroundChanged();
	void rootChanged();

public:
	explicit ALTNavScope(QObject* _attachee);
	~ALTNavScope();

	/*!
	 * \brief Traverses scope tree by current input. Disables mode once no match on future input is possible.
	 * \param input
	 */
	void traverse(QString input);
	/*!
	 * \brief Sets scope prefix and recomputes postfixes for children. Updates ALTNavTag
	 * \param prefix
	 */
	void setPrefix(QString prefix);
	/*!
	 * \brief Sets scope activity. Propagates state to ALTNavTag and to children if scope or showchildren are true
	 * \param value
	 */
	void setScopeActive(bool value);
	/*!
	 * \brief Recomputes postfixes and sets prefixes for children
	 * \param prefix
	 */
	void setChildrenPrefix();
	/*!
	 * \brief Sets child scope activities.
	 * \param value
	 */
	void setChildrenActive(bool value);
	void match();

	QString getRequestedPostfix();
	int getScopePriority();
	int getIndex();
	bool foreground();
	QString prefix();


	void setEnabled(bool value);
	void setStrategy(ALTNavPostfixAssignmentStrategy* strategy);
	void setStrategy(AssignmentStrategy strategy);
	void setRoot(bool value);
	void setScopeOnly(bool value);


protected:
	void childEvent(QChildEvent *event) override;
	void setParentAttachee(QObject* _parent);
	void setRequestedPostfix(QString postfix);
	void setScopePriority(int priority);
	void setIndex(int index);
	void setForeground(bool onForeground);
	void setX(qreal x);
	void setY(qreal y);


private slots:
	void init();

	/*!
	 * \brief Attempts to find a valid parent in the attachee ancestor chain.
	 *		  Registers with default root otherwise. May be overruled by setting parent property
	 */
	void registerWithParent();


protected:
	bool scopeActive = false;
	QString _prefix = "";

private:
	//ALTNavigation Interface
	bool enabled = false;
	bool root = false;
	bool _foreground = false;
	bool scopeOnly = false;
	bool propagateActivity = false;
	QString requestedPostfix = "";
	int index = -1;
	int scopePriority = 0;
	AssignmentStrategy currentStrategy = AssignmentStrategy::PRIORITY;
	qreal x, y;

	QQuickItem* attachee;
	ALTNavTagBase* attachedTag = nullptr;
	QQuickItem* parentScopeAttachee = nullptr;
	bool parentOverride = false;
	bool initialized = false;


	ALTNavPostfixAssignmentStrategy* postfixBroker = nullptr;

	friend class ALTNavPostfixAssignmentStrategy;

};

#endif // ALTNAVSCOPE_H
