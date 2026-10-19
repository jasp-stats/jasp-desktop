#ifndef DESCRIPTION_H
#define DESCRIPTION_H

#include <QUrl>
#include <QTimer>
#include "version.h"
#include <QQuickItem>
#include <json/json.h>
#include "qutils.h"
#include "modules/dynamicmodule.h"

namespace Modules
{

class DescriptionChildBase;
class EntryBase;
class RequiredPackage;
class RequiredModule;
class AnalysisEntry;

///
/// Backend for Description{} in Description.qml in each module
/// Most of the properties defined here should actually be taken from DESCRIPTION though
class Description : public QQuickItem
{
	Q_OBJECT
	Q_PROPERTY(QString					name			READ name				WRITE setDummy				NOTIFY nameChanged				)
	Q_PROPERTY(QString					title			READ title				WRITE setTitle				NOTIFY titleChanged				)
	Q_PROPERTY(QString					icon			READ icon				WRITE setIcon				NOTIFY iconChanged				)
	Q_PROPERTY(QString					description		READ description		WRITE setDescription		NOTIFY descriptionChanged		)
	Q_PROPERTY(QString					version			READ versionStr			WRITE setDummy				NOTIFY versionChanged			)
	Q_PROPERTY(QString					author			READ author				WRITE setDummy				NOTIFY authorChanged			)
	Q_PROPERTY(QString					maintainer		READ maintainer			WRITE setDummy				NOTIFY maintainerChanged		)
	Q_PROPERTY(QUrl						website			READ website			WRITE setDummyUrl			NOTIFY websiteChanged			)
	Q_PROPERTY(QString					license			READ license			WRITE setDummy				NOTIFY licenseChanged			)
	///requiresData should really be called defaultRequiresData or something. Because that is what it does. But it would be a lot of work to change all the qmls...
	Q_PROPERTY(bool						requiresData	READ requiresDataDef	WRITE setRequiresDataDef	NOTIFY requiresDataDefChanged	)
	Q_PROPERTY(bool						preloadData		READ preloadData		WRITE setPreloadData		NOTIFY preloadDataChanged		)
	Q_PROPERTY(Modules::DynamicModule *	dynMod			READ dynMod				WRITE setDynMod				NOTIFY dynModChanged			)
	Q_PROPERTY(bool						hasWrappers		READ hasWrappers		WRITE setHasWrappers		NOTIFY hasWrappersChanged		)
	Q_PROPERTY(bool						alwaysSaveState	READ alwaysSaveState	WRITE setAlwaysSaveState	NOTIFY alwaysSaveStateChanged	)
	Q_PROPERTY(bool						neverSaveState	READ neverSaveState		WRITE setNeverSaveState		NOTIFY neverSaveStateChanged	)
	Q_PROPERTY(bool						useSubMenus		READ useSubMenus		WRITE setUseSubMenus		NOTIFY useSubMenusChanged		)

public:
	Description(QQuickItem *parent = nullptr);
	~Description();

	void			setDummy(QString ) {}; // Temporary placeholder to make existing entries in Description.qml not stop it from loading, can be removed once all modules are updated
	void			setDummyUrl(QUrl ) {}; // Temporary placeholder to make existing entries in Description.qml not stop it from loading, can be removed once all modules are updated
	
	const QString &	name()				const { return _name;						}
	const QString & title()				const { return _title;						}
	const QString & icon()				const { return _icon;						}
	const QString & description()		const { return _description;				}
	const Version & version()			const { return _version;					}
	QString versionStr()				const { return tq(_version.asString());		}
	const QString & author()			const { return _author;						}
	const QString & maintainer()		const { return _maintainer;					}
	const QUrl	  & website()			const { return _website;					}
	const QString & license()			const { return _license;					}
	bool			requiresDataDef()	const { return _requiresDataDef;			}
	bool			preloadData()		const;
	DynamicModule * dynMod()			const { return _dynMod;						}
	bool			hasWrappers()		const { return _hasWrappers;				}
	bool			alwaysSaveState()	const;	
	bool			neverSaveState()	const;
	bool			useSubMenus()		const { return _useSubMenus;				}

	void	addChild(	DescriptionChildBase * child);
	void	removeChild(DescriptionChildBase * child);

	std::vector<AnalysisEntry *>	menuEntries()		const;
	
public slots:
	void setName(					QString						name			);
	void setTitle(					QString						title			);
	void setIcon(					QString						icon			);
	void setDescription(			QString						description		);
	void setVersion(				QString						version			);
	void setAuthor(					QString						author			);
	void setMaintainer(				QString						maintainer		);
	void setWebsite(				QUrl						website			);
	void setLicense(				QString						license			);
	void setRequiresDataDef(		bool						defRequiresData	);
	void setPreloadData(			bool						newPreloadData	);
	void setDynMod(					Modules::DynamicModule	*	dynMod			);
	void delayedUpdate();
	void setHasWrappers(			bool						hasWrappers		);
	void setNeverSaveState(			bool						newNeverSaveState);
	void setAlwaysSaveState(		bool						newAlwaysSaveState);
	void setUseSubMenus(			bool						useSubMenus);

signals:
	void titleChanged(				QString						title			);
	void iconChanged(				QString						icon			);
	void descriptionChanged(		QString						description		);
	void versionChanged();
	void authorChanged(				QString						author			);
	void maintainerChanged(			QString						maintainer		);
	void websiteChanged(			QUrl						website			);
	void licenseChanged(			QString						license			);
	void nameChanged(				QString						name			);
	void requiresDataDefChanged(	bool						defRequiresData	);
	void preloadDataChanged();
	void hasWrappersChanged(		bool						hasWrappers		);
	void dynModChanged(				Modules::DynamicModule	*	dynMod			);
	void iShouldBeUpdated(			Modules::Description	*	desc			);
	void childChanged();
	void alwaysSaveStateChanged();
	void neverSaveStateChanged();
	void useSubMenusChanged();

private:
	void					setUpDelayedUpdate();
	void					connectChangesToDelay();

	QString					_name,
							_title,
							_icon,
							_description,
							_author,
							_maintainer,
							_license;
	QUrl					_website;
	Version					_version;
	bool					_requiresDataDef	= true,
							_hasWrappers		= false,
							_preloadData		= true,
							_alwaysSaveState	= false,
							_neverSaveState		= false,
							_useSubMenus		= false;
	DynamicModule		*	_dynMod				= nullptr;
	QList<EntryBase*>		_entries;
	QTimer					_timer;
};

}
#endif // DESCRIPTION_H
