#ifndef ENTRYBASE_H
#define ENTRYBASE_H

#include <QQuickItem>
#include "qutils.h"
#include "description.h"
#include "descriptionchildbase.h"

namespace Modules
{

class AnalysisEntry;
struct EntryError  : public std::runtime_error
{
	EntryError(QString problem);
	const char* what() const noexcept override;
};

/// Is used by MAKE_ENTRY_CLASS to generate the subitems for Description{}
/// All functionality is already present here, the only thing the subclasses add is their type
/// This class must be merged with analysisentry somehow
class EntryBase : public DescriptionChildBase
{
	Q_OBJECT

	Q_PROPERTY(QString		menu			READ menu			WRITE setMenu			NOTIFY menuChanged			)
	Q_PROPERTY(QString		title			READ title			WRITE setTitle			NOTIFY titleChanged			)
	Q_PROPERTY(QString		func			READ function		WRITE setFunction		NOTIFY functionChanged		)
	Q_PROPERTY(QString		icon			READ icon			WRITE setIcon			NOTIFY iconChanged			)
	Q_PROPERTY(QString		qml				READ qml			WRITE setQml			NOTIFY qmlChanged			)
	Q_PROPERTY(EntryType	entryType		READ entryType								NOTIFY entryTypeChanged		) //Entry type can only be set in constructor, to keep things manageable
	Q_PROPERTY(bool			requiresData	READ requiresData	WRITE setRequiresData	NOTIFY requiresDataChanged	)
	Q_PROPERTY(bool			enabled			READ enabled		WRITE setEnabled		NOTIFY enabledChanged		) //Hmm, this already exists in QQuickItem, maybe a problem?
	Q_PROPERTY(bool			debug			READ debug			WRITE setDebug			NOTIFY debugChanged			)

public:
	enum class EntryType {unknown, separator, groupTitle, analysis};
	Q_ENUM(EntryType);

	EntryBase(EntryType entryType);

	QString		menu()			const { return _menu;			}
	QString		title()			const { return _title;			}
	QString		function()		const { return _function;		}
	QString		icon()			const { return _icon;			}
	EntryType	entryType()		const { return _entryType;		}
	bool		requiresData()	const { return _requiresData;	}
	bool		enabled()		const { return _enabled;		}
	QString		qml()			const { return _qml;			}
	bool		debug()			const { return _debug;			}
	QString		toString()		const;
	bool		shouldBeAdded()	const;

	///This function is a stopgap and these two classes must be merged together later
	AnalysisEntry * convertToAnalysisEntry(bool requiresDataDefault) const;



public slots:
	void setMenu(			QString menu);
	void setTitle(			QString title);
	void setFunction(		QString function);
	void setIcon(			QString icon);
	void setQml(			QString qml);
	void setRequiresData(	bool	requiresData);
	void setEnabled(		bool	enabled);
	void setDebug(			bool	debug);
	void devModeChanged(	bool	devMode);

signals:
	void menuChanged();
	void titleChanged();
	void functionChanged();
	void iconChanged();
	void entryTypeChanged();
	void requiresDataChanged();
	void enabledChanged();
	void qmlChanged();
	void debugChanged();

private:
	QString			_menu					= "",
					_title					= "???",
					_function				= "",
					_icon					= "",
					_qml					= "";
	EntryType		_entryType				= EntryType::unknown;
	bool			_requiresData			= false,
					_useDefaultRequiresData = true, //will be set to false whenever a value is set through setRequiresData
					_enabled				= true,
					_debug					= false;
};

#define MAKE_ENTRY_CLASS(CLASS_NAME, ENTRYTYPENAME) class CLASS_NAME : public EntryBase \
{\
public: \
	CLASS_NAME() \
	: EntryBase(EntryType::ENTRYTYPENAME) \
		{}\
}

MAKE_ENTRY_CLASS(Separator,		separator);
MAKE_ENTRY_CLASS(AnalysisItem,	analysis);
MAKE_ENTRY_CLASS(GroupTitle,	groupTitle);
}
#endif // ENTRYBASE_H
