#ifndef ALTNAVROOT_H
#define ALTNAVROOT_H

#include <QHash>
#include "altnavscope.h"

class ALTNavRoot : public ALTNavScope
{
public:
	explicit ALTNavRoot(QObject *parent = nullptr);


	void registerTag(ALTNavTag* tagObject);
	void removeTag(ALTNavTag* tagObject);
	void updateTag(ALTNavTag* tagObject);

	ALTNavScope* getAttachedScope(QObject* obj) { auto it = attachedScopeMap.find(obj); if (it != attachedScopeMap.end()) return it.value(); return nullptr;};
	void registerScope(ALTNavScope* scope, QObject* obj);
	void removeScope(QObject* obj);

	bool eventFilter(QObject* object, QEvent* event);

	void resetAltNavInput();
	void updateAltNavInput(QString entry);
	void setAltNavEnabled(bool value);

	//singleton stuff
	static ALTNavRoot* getInstance();
	ALTNavRoot(ALTNavRoot& other) = delete;
	void operator=(const ALTNavRoot&) = delete;

private:
	static ALTNavRoot* instance;
	QHash<QObject*, ALTNavScope*> attachedScopeMap;
	QSet<ALTNavTag*> tags;

	ALTNavScope* activeScope = this;

	bool altNavEnabled = false;
	QString currenAltNavInput = "";

};

#endif // ALTNAVROOT_H
