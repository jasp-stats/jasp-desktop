#ifndef DESCRIPTION_H
#define DESCRIPTION_H

#include <QQuickItem>
#include <QTimer>
#include <QUrl>

#include "version.h"
#include "utilities/qutils.h"
#include "jsonredirect.h"

namespace Modules
{

class DescriptionChildBase;
class EntryBase;
class RequiredPackage;
class RequiredModule;
class AnalysisEntry;
class DynamicModule;

///
/// Backend for Description{} in Description.qml in each module
/// Most of the properties defined here should actually be taken from DESCRIPTION though
class Description : public QQuickItem
{
	Q_OBJECT
	Q_PROPERTY(QString					name			READ name				WRITE setName				NOTIFY nameChanged				)
	Q_PROPERTY(QString					title			READ title				WRITE setTitle				NOTIFY titleChanged				)
	Q_PROPERTY(QString					icon			READ icon				WRITE setIcon				NOTIFY iconChanged				)
	Q_PROPERTY(QString					description		READ description		WRITE setDescription		NOTIFY descriptionChanged		)
	Q_PROPERTY(QString					version			READ version			WRITE setVersion			NOTIFY versionChanged			)
	Q_PROPERTY(QString					author			READ author				WRITE setAuthor				NOTIFY authorChanged			)
	Q_PROPERTY(QString					maintainer		READ maintainer			WRITE setMaintainer			NOTIFY maintainerChanged		)
	Q_PROPERTY(QUrl						website			READ website			WRITE setWebsite			NOTIFY websiteChanged			)
	Q_PROPERTY(QString					license			READ license			WRITE setLicense			NOTIFY licenseChanged			)
	//requiresData should really be called defaultRequiresData or something. Because that is what it does. But it would be a lot of work to change all the qmls...
	Q_PROPERTY(bool						requiresData	READ requiresDataDef	WRITE setRequiresDataDef	NOTIFY requiresDataDefChanged	)
	Q_PROPERTY(Modules::DynamicModule *	dynMod			READ dynMod				WRITE setDynMod				NOTIFY dynModChanged			)

public:
	Description(QQuickItem *parent = nullptr);
	~Description();

	QString			name()				const { return _name;						}
	QString			title()				const { return _title;						}
	QString			icon()				const { return _icon;						}
	QString			description()		const { return _description;				}
	QString			version()			const { return tq(_version.asString(3));	}
	QString			author()			const { return _author;						}
	QString			maintainer()		const { return _maintainer;					}
	QUrl			website()			const { return _website;					}
	QString			license()			const { return _license;					}
	bool			requiresDataDef()	const { return _requiresDataDef;			}
	DynamicModule * dynMod()			const { return _dynMod;						}

	void	addChild(	DescriptionChildBase * child);
	void	removeChild(DescriptionChildBase * child);

	std::vector<AnalysisEntry *>	menuEntries()		const;
	std::set<std::string>			requiredModules()	const;

public slots:
	void setName(				QString			name		);
	void setTitle(				QString			title		);
	void setIcon(				QString			icon		);
	void setDescription(		QString			description	);
	void setVersion(			QString			version		);
	void setAuthor(				QString			author		);
	void setMaintainer(			QString			maintainer	);
	void setWebsite(			QUrl			website		);
	void setLicense(			QString			license		);
	void setRequiresDataDef(	bool			defRequiresData);
	void setDynMod(				DynamicModule * dynMod		);
	void delayedUpdate();

signals:
	void titleChanged(				QString			title		);
	void iconChanged(				QString			icon		);
	void descriptionChanged(		QString			description	);
	void versionChanged();
	void authorChanged(				QString			author		);
	void maintainerChanged(			QString			maintainer	);
	void websiteChanged(			QUrl			website		);
	void licenseChanged(			QString			license		);
	void nameChanged(				QString			name		);
	void requiresDataDefChanged(	bool			defRequiresData);
	void dynModChanged(				DynamicModule * dynMod		);
	void iShouldBeUpdated(			Description	  *	desc		);
	void childChanged();

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
	bool					_requiresDataDef	= true;
	DynamicModule		*	_dynMod				= nullptr;
	QList<EntryBase*>		_entries;
	QTimer					_timer;
};

}
#endif // DESCRIPTION_H
