#ifndef ALTNAVROOT_H
#define ALTNAVROOT_H

#include <QHash>
#include "altnavscope.h"

class ALTNavControl : public QObject
{
	Q_OBJECT
public:
	explicit ALTNavControl(QObject *parent = nullptr);
	~ALTNavControl();

	ALTNavScope* getAttachedScope(QObject* obj);
	void registerScope(ALTNavScope* scope, QObject* obj);
	void removeScope(QObject* obj);

	bool eventFilter(QObject* object, QEvent* event);

	void resetAltNavInput();
	void setAltNavInput(QString input);
	void updateAltNavInput(QString entry);
	QString getCurrentALTNavInput();

	bool dynamicTreeUpdate();

	void setAltNavEnabled(bool value);
	bool AltNavEnabled();


	void setCurrentNode(ALTNavScope* scope);
	void setCurrentRoot(ALTNavScope* root);
	ALTNavScope* getCurrentNode();
	ALTNavScope* getCurrentRoot();
	ALTNavScope* getDefaultRoot();

	//singleton stuff
	static ALTNavControl* getInstance();
	ALTNavControl(ALTNavControl& other) = delete;
	void operator=(const ALTNavControl&) = delete;

signals:
	void altNavInputChanged();
	void altNavEnabledChanged();

private:
	static ALTNavControl* instance;
	QHash<QObject*, ALTNavScope*> attachedScopeMap;

	ALTNavScope* currentNode = nullptr;
	ALTNavScope* currentRoot = nullptr;
	ALTNavScope* defaultRoot = nullptr;

	bool altNavEnabled = false;
	bool _dynamicTreeUpdate = false;
	QString currenAltNavInput = "";

};

#endif // ALTNAVROOT_H
