/*! @file altnavigation.h
 *  @brief Defines the attaching type for the ALTNavigation attached property.
 *
 *  The ALTNavigation subsystem implements an Attached Property which allows QQuickItems to register themselves.
 *  Upon registration they are assigned a textual tag shown when the ALT navigation mode is activated.
 *  The registered item may connect a handler to the attached onTagMatch signal to provide functionality/navigation.
 *
 *  See https://doc.qt.io/qt-6/qtqml-cppintegration-definetypes.html#providing-attached-properties for information on attached properties.
 *  This file provides the attaching type which creates the attached object type, ALTNavScope, to be attached to the attachee.
 *  ALTNavScope will create an ALTNavTag visual item. The lifetime of the ALTNavScope and visual ALTNavTag are tied to the attachee.
 *
 *	ALTNavScopes will arrange themselves into Tree(s) by traversing upwards through the attachee ancestors
 *  until a registered parent is discovered. If none exist it is set as a child of the default root.
 *  This Process is facilitated by ALTNavControl.
 *  Tags are formed by combining parent prefixes with unique postfixes for all children.
 *  The postfixes are determined by the ALTNavPostfixAssignmentStrategy of the parent node.
 *
 *  ALTNavScopes may generate a visual tag or they may be just parent scopes without visual representation to bundle other tags.
 *  e.g the items in a list share a scope-only parent node.
 *
 *  ALTNavControl collects user input through which the ALTNavScope tree is traversed and the mode is (de)activated.
 *  It keeps track of the current node, current root, registered QQuickItems and their attached ALTNavScopes.
 *  The children of the the current node will be visible.
 *
 *  ALTNavScopes may define a rule that  defines them as being on the foreground.
 *  They will be set as the current node when true.
 *
 *  ALTNavScopes may define themselves as a root. A forest of ALTNavscrope trees will result.
 *  Only one root can be the current root. This is determined by the foreground property of the root scopes.
 *
 *
 *  The following properties are provided through ALTNavigation:
 *
 *  - enabled			When set to false an ALTNavscope unparent itself until enabled is true once again
 *  - root				Defines a scope as a root
 *  - parent			Overrules the parent discovered by the normal tree forming parent search method. If set to null default root is set as parent.
 *  - scopeOnly			Scope has no visual representation
 *  - showChildren		Children of this scope will also be visible when this node is
 *  - strategy			Define postfix allocation strategy
 *  - foreground		This property may be set to true if the scope should be forcefully set to the current node (e.g. submenu is open)
 *  - requestedPostfix  Defines a postfix preference that the current strategy may use
 *  - scopePriority		Defines a priority for the preference that the strategy may use
 *  - index				Defines a index that the current strategy may use to make ordered postfixes (e.g. items in a list)
 *  - x,y				Defines the position of the visual tag relative to the attachee
 *
 *	The following signals are provided:
 *	- tagMatch			triggers when current user input matches the tag of a scope
 *
 *
 *	Examples of basic usage:
 *	SomeItem
 *	{
 *		ALTNavigation.enabled:		true
 *		ALTNavigation.onTagMatch:	{ forceActiveFocus(); }
 *	}
 *
 *	List
 *	{
 *		ALTNavigation.enabled:		true
 *		ALTNavigation.scopeOnly:	true
 *		ALTNavigation.strategy:     AssignmentStrategy.INDEXED
 *
 *		delegate: SomeItem
 *		{
 *			ALTNavigation.enabled:		true
 *			ALTNavigation.index:		index
 *			ALTNavigation.onTagMatch:	{ open(); }
 *		}
 *  }
 *
 *  @author Rens Dofferhoff
 */


#ifndef ALTNAVIGATION_H
#define ALTNAVIGATION_H

#include <QObject>
#include <QQmlEngine>

#include "altnavscope.h"

//! Attaching type ALTNavigation
class ALTNavigation : public QObject
{
	Q_OBJECT
	QML_ATTACHED(ALTNavScope)

public:
	/*!
	 * \brief Creates and registers attached object type ALTNavScope for attachee
	 * \param object Attachee object
	 * \return Attaching ALNavScope
	 */
	static ALTNavScope* qmlAttachedProperties(QObject *object);

	/*!
	 * \brief registers the QML types related to this subsystem under JASP
	 */
	static void registerQMLTypes(QString uri);

};

#endif // ALTNAVIGATION_H
