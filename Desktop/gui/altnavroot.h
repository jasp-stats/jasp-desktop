#ifndef ALTNAVROOT_H
#define ALTNAVROOT_H

#include <QHash>
#include "altnavscope.h"

class ALTNavRegistry : public QObject
{
	Q_OBJECT
public:
	explicit ALTNavRegistry(QObject *parent = nullptr);

	ALTNavScope* getAttachedScope(QObject* obj);
	void registerScope(ALTNavScope* scope, QObject* obj);
	void removeScope(QObject* obj);

	bool eventFilter(QObject* object, QEvent* event);

	void resetAltNavInput();
	void setAltNavInput(QString input);
	void updateAltNavInput(QString entry);
	void setAltNavEnabled(bool value);

	void setCurrentNode(ALTNavScope* scope);
	void setCurrentRoot(ALTNavScope* root);
	ALTNavScope* getCurrentNode();
	ALTNavScope* getCurrentRoot();
	ALTNavScope* getDefaultRoot();

	QString getCurrentALTNavInput();
	bool dynamicTreeUpdate();

	//singleton stuff
	static ALTNavRegistry* getInstance();
	ALTNavRegistry(ALTNavRegistry& other) = delete;
	void operator=(const ALTNavRegistry&) = delete;

signals:
	void altNavInputChanged();
	void altNavEnabledChanged();

private:
	static ALTNavRegistry* instance;
	QHash<QObject*, ALTNavScope*> attachedScopeMap;

	ALTNavScope* currentNode = nullptr;
	ALTNavScope* currentRoot = nullptr;
	ALTNavScope* defaultRoot = nullptr;

	bool altNavEnabled = false;
	bool _dynamicTreeUpdate = false;
	QString currenAltNavInput = "";

};

#endif // ALTNAVROOT_H
