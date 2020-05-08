#ifndef ENTRYBASE_H
#define ENTRYBASE_H

#include <QQuickItem>
#include "utilities/qutils.h"
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

// This class must be merged with analysisentry somehow
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
	Q_PROPERTY(bool			enabled			READ enabled		WRITE setEnabled		NOTIFY enabledChanged		)

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

	QString		toString()		const;

	///This function is a stopgap and these two classes must be merged together later
	AnalysisEntry * convertToAnalysisEntry(bool requiresDataDefault) const;

public slots:
	void setMenu(			QString menu);
	void setTitle(			QString title);
	void setFunction(		QString function);
	void setIcon(			QString icon);
	void setRequiresData(	bool	requiresData);
	void setEnabled(		bool	enabled);
	void setQml(			QString qml);

signals:
	void menuChanged(			QString		menu);
	void titleChanged(			QString		title);
	void functionChanged(		QString		function);
	void iconChanged(			QString		icon);
	void entryTypeChanged(		EntryType	entryType);
	void requiresDataChanged(	bool		requiresData);
	void enabledChanged(		bool		enabled);

	void qmlChanged(QString qml);

private:
	QString			_menu					= "",
					_title					= "???",
					_function				= "",
					_icon					= "",
					_qml					= "";
	EntryType		_entryType				= EntryType::unknown;
	bool			_requiresData			= false,
					_useDefaultRequiresData = true, //will be set to false whenever a value is set through setRequiresData
					_enabled				= true;

};

#define MAKE_ENTRY_CLASS(CLASS_NAME, ENTRYTYPENAME) class CLASS_NAME : public EntryBase \
{\
public: \
	CLASS_NAME() \
	: EntryBase(EntryType::ENTRYTYPENAME) \
		{}\
}

MAKE_ENTRY_CLASS(Separator,		separator);
MAKE_ENTRY_CLASS(Analysis,		analysis);
MAKE_ENTRY_CLASS(GroupTitle,	groupTitle);
}
#endif // ENTRYBASE_H
