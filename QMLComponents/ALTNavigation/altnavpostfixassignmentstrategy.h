#ifndef ALTNAVPOSTFIXASSIGNMENTSTRATEGY_H
#define ALTNAVPOSTFIXASSIGNMENTSTRATEGY_H

#include <QObject>

class ALTNavScope;

/*!
 * \brief The ALTNavPostfixAssignmentStrategy class defines the interface which concrete strategies must define
 */
class ALTNavPostfixAssignmentStrategy : public QObject
{
	Q_OBJECT

public:
	virtual ~ALTNavPostfixAssignmentStrategy() {};

	/*!
	 * \brief Compute postfixes and set resulting prefixes for children
	 * \param children
	 * \param prefix parent prefix
	 */
	virtual void assignPostfixes(QList<ALTNavScope*>&  children, QString prefix) = 0;

	/*!
	 * \brief The AssignmentStrategy enum defines all strategies
	 */
	enum AssignmentStrategy { PASS_THROUGH, INDEXED, PRIORITIZED, UNKNOWN };
	Q_ENUM(AssignmentStrategy)


	/*!
	 * \brief Factory method that creates the desired strategy type
	 * \param strategy
	 * \return Specified strategy type
	 */
	static ALTNavPostfixAssignmentStrategy* createStrategy(AssignmentStrategy strategy);



};

/*!
 * \brief The PriorityStrategy class assigns postfixes A-Z while taking request and priorities into account
 *		  If the number of children is larger than 26 it finds a suitable sacrifice X and assigns postfixes X(A-Z)
 */
class PriorityStrategy : public ALTNavPostfixAssignmentStrategy
{
public:
	PriorityStrategy(){};
	~PriorityStrategy(){};
	void assignPostfixes(QList<ALTNavScope*>&  children, QString prefix);

};

/*!
 * \brief The IndexedStrategy class assigns prefixes 1-9 dependend on child index properties
 *		  If the number of children > 9 it will assign prefixes 0(1-9) + 10-99.
 */
class IndexedStrategy : public ALTNavPostfixAssignmentStrategy
{
public:
	IndexedStrategy(){};
	~IndexedStrategy(){};
	void assignPostfixes(QList<ALTNavScope*>&  children, QString prefix);
};

/*!
 * \brief The PassthroughStrategy class copies the parent prefix. Only use with one child.
 */
class PassthroughStrategy : public ALTNavPostfixAssignmentStrategy
{
public:
	PassthroughStrategy(){};
	~PassthroughStrategy(){};
	void assignPostfixes(QList<ALTNavScope*>&  children, QString prefix);
};

#endif // ALTNAVPOSTFIXASSIGNMENTSTRATEGY_H
