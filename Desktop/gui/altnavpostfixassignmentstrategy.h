#ifndef ALTNAVPOSTFIXASSIGNMENTSTRATEGY_H
#define ALTNAVPOSTFIXASSIGNMENTSTRATEGY_H

#include <QSet>

#include "altnavscope.h"


class ALTNavPostfixAssignmentStrategy
{
public:
	virtual ~ALTNavPostfixAssignmentStrategy() {};
	virtual void assignPostfixes(QObjectList const&  children, QString tag) = 0;

	//factory method
	static ALTNavPostfixAssignmentStrategy* createStrategy(ALTNavScope::AssignmentStrategy strategy);

};

class TMPSTRAT : public ALTNavPostfixAssignmentStrategy
{
public:
	TMPSTRAT(){};
	~TMPSTRAT(){};
	void assignPostfixes(QObjectList const&  children, QString tag) {for(auto child : children) qobject_cast<ALTNavScope*>(child)->setPrefix(tag + "a");}
};


#endif // ALTNAVPOSTFIXASSIGNMENTSTRATEGY_H
