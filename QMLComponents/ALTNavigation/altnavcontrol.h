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

	/*!
	 * \brief Returns ALTNavScope attached to attachee object
	 * \param obj attachee object
	 * \return Attached scope
	 */
	ALTNavScope* getAttachedScope(QObject* obj);
	/*!
	 * \brief Registers connection between attachee and attached scope
	 * \param scope
	 * \param obj
	 */
	void registrate(ALTNavScope* scope, QObject* obj);
	/*!
	 * \brief Undo registration
	 * \param obj attachee object
	 */
	void unregister(QObject* obj);

	/*!
	 * \brief This global event filter captures user input for the ALTNavigation subsystem.
	 *
	 * ALT keypress captured when mode is off. Turns mode on when captured.
	 * Captures 0-9 + A-Z + ALT + ESC when mode is on. Disables mode for ALT + ESC press or uncaptured keystrokes.
	 *
	 * \param object object for which the event is filtered
	 * \param event
	 * \return true when event is filtered out. false when the event is not handeled.
	 */
	bool eventFilter(QObject* object, QEvent* event);

	void resetAltNavInput();
	void setAltNavInput(QString input);
	/*!
	 * \brief Appends the given entry to the current user input.
	 * \param entry
	 */
	void updateAltNavInput(QString entry);
	QString getCurrentALTNavInput();

	/*!
	 * \brief Returns whether trees should dynamically update upon child events
	 */
	bool dynamicTreeUpdate();

	/*!
	 * \brief Turns the ALT navigation mode on or off.
	 * \param value
	 */
	void setAltNavActive(bool value);
	/*!
	 * \brief Returns whether ALT navigation mode is on or off
	 * \return
	 */
	bool AltNavActive();


	void setCurrentNode(ALTNavScope* scope);
	void setCurrentRoot(ALTNavScope* root);
	ALTNavScope* getCurrentNode();
	ALTNavScope* getCurrentRoot();
	ALTNavScope* getDefaultRoot();

	//singleton stuff
	/*!
	 * \brief returns the ALTNavControl singleton
	 */
	static ALTNavControl* ctrl();
	ALTNavControl(ALTNavControl& other) = delete;
	void operator=(const ALTNavControl&) = delete;

public slots:
	void enableAlTNavigation(bool state);

signals:
	void altNavInputChanged();
	void altNavActiveChanged();

private:
	static ALTNavControl* _instance;
	QHash<QObject*, ALTNavScope*> _attachedScopeMap;

	ALTNavScope* _currentNode = nullptr;
	ALTNavScope* _currentRoot = nullptr;
	ALTNavScope* _defaultRoot = nullptr;

	bool _altNavEnabled = false;
	bool _altNavActive = false;
	bool _dynamicTreeUpdate = false;
	QString _currenAltNavInput = "";

};

#endif // ALTNAVROOT_H
