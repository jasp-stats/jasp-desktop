#ifndef ALTNAVROOT_H
#define ALTNAVROOT_H

#include <QHash>
#include "altnavscope.h"

class ALTNavRoot : public ALTNavScope
{
	Q_OBJECT
public:
	explicit ALTNavRoot(QObject *parent = nullptr);

	ALTNavScope* getAttachedScope(QObject* obj);
	void registerScope(ALTNavScope* scope, QObject* obj);
	void removeScope(QObject* obj);

	bool eventFilter(QObject* object, QEvent* event);

	void resetAltNavInput();
	void updateAltNavInput(QString entry);
	void setAltNavEnabled(bool value);

	void setActiveNode(ALTNavScope *scope, bool setActive = false);
	ALTNavScope* getActiveNode();

	QString getCurrentALTNavInput();
	bool dynamicTreeUpdate();

	//singleton stuff
	static ALTNavRoot* getInstance();
	ALTNavRoot(ALTNavRoot& other) = delete;
	void operator=(const ALTNavRoot&) = delete;

signals:
	void altNavInputChanged();

private:
	static ALTNavRoot* instance;
	QHash<QObject*, ALTNavScope*> attachedScopeMap;

	ALTNavScope* activeNode = this;

	bool altNavEnabled = false;
	bool _dynamicTreeUpdate = false;
	QString currenAltNavInput = "";

};

#endif // ALTNAVROOT_H
