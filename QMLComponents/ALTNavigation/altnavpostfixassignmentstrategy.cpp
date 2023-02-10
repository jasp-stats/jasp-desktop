#include "altnavpostfixassignmentstrategy.h"
#include "altnavscope.h"
#include <cmath>

std::string capitalLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

ALTNavPostfixAssignmentStrategy* ALTNavPostfixAssignmentStrategy::createStrategy(AssignmentStrategy strategy)
{
	switch (strategy) {
		case PRIORITIZED:
			return new PriorityStrategy();
		case INDEXED:
			return new IndexedStrategy();
		case PASS_THROUGH:
			return new PassthroughStrategy();
		case UNKNOWN:
		default:
			return nullptr;
	}
};

void PriorityStrategy::assignPostfixes(QList<ALTNavScope*>& children, QString prefix)
{

	//handle postfix requests
	//postfix request must be unique on first character and priority is taken into account
	QList<ALTNavScope*> unassigned;
	QSet<char> assigned;
	ALTNavScope* assignments[26] = { 0 };
	for (auto it = children.rbegin(); it != children.rend() ;it++)
	{
		ALTNavScope* scope = *it;
		QString request = scope->getRequestedPostfix();
		if (request != "" && request[0].isLetter() && request[0].isUpper())
		{
			ALTNavScope* current = assignments[request[0].toLatin1() - 'A'];
			if(current) //check if already assigned if so compare priority
			{
				if(current->getScopePriority() >= scope->getScopePriority())
				{
					unassigned.push_back(scope);
					continue;
				}
				else
					unassigned.push_back(current);
			}
			assignments[request[0].toLatin1() - 'A'] = scope;
			assigned.insert(request[0].toLatin1());
			scope->setPrefix(prefix + scope->getRequestedPostfix());
		}
		else
			unassigned.push_back(scope);
	}

	//assign the leftover postfixes
	std::string preferredSacrifices = "";
	for (char option : capitalLetters)
	{
		if(unassigned.empty())
			return;
		else if (assigned.contains(option))
			continue;
		else
		{
			preferredSacrifices += option;
			assigned.insert(option);
			assignments[option - 'A'] = unassigned.last();
			unassigned.last()->setPrefix(prefix + option);
			unassigned.pop_back();
		}
	}


	//We assigned 26 postfixes so now we need to get creative and we start assigning postfixes of lenght >= 2, like ZA,ZB...,ZZ
	QString layerPrefix = "";
	QString nextLayerPrefix = "";
	//no spots left so we need a sacrifice. If no candidates is available we use Z. Who uses the letter Z anyways
	if (assigned.size() == 26 && unassigned.size() > 0) //26 letters in alphabet
	{
		char sacrifice = preferredSacrifices.length() == 0 ? 'Z' : preferredSacrifices.back();
		assigned.remove(sacrifice);
		unassigned.push_back(assignments[sacrifice - 'A']);
		nextLayerPrefix = layerPrefix += sacrifice;
	}


	//assign the rest, move to next layer if more than 26 left to assign
	while (unassigned.size())
	{
		bool nextLayer = unassigned.size() > 26;
		for (char option : capitalLetters)
		{
			if (unassigned.empty())
				return;
			else if (nextLayer)
			{
				nextLayerPrefix += option;
				nextLayer = false;
			}
			else
			{
				unassigned.last()->setPrefix(prefix + layerPrefix + option);
				unassigned.pop_back();
			}
		}
		layerPrefix = nextLayerPrefix;
	}
}

void IndexedStrategy::assignPostfixes(QList<ALTNavScope*>& children, QString prefix)
{
	int maxLeadingZeros = std::log10((double)children.size()); //if needed I can write a fast integer version that never fails but I doubt it will really matter for reasonable input sizes
	for (ALTNavScope* scope : children)
	{
		int n = scope->getIndex() + 1; //get rid of 0 index
		int numLeadingZeros = maxLeadingZeros - (int)std::log10((double)n);
		QString tag = QString("0").repeated(numLeadingZeros) + QString::number(n);
		scope->setPrefix(prefix + tag);
	}
}

void PassthroughStrategy::assignPostfixes(QList<ALTNavScope*>& children, QString prefix)
{
	assert(children.length() <= 1);
	for (ALTNavScope* scope : children)
	{
		scope->setPrefix(prefix);
	}
}
