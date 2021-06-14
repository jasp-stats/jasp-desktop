#include "description.h"
#include "log.h"
#include "descriptionchildbase.h"
#include "entrybase.h"
#include "../analysisentry.h"
#include "../dynamicmodule.h"
#include "utilities/languagemodel.h"

namespace Modules
{

Description::Description(QQuickItem *parent) : QQuickItem(parent)
{
	//Log::log() << "Description created!" << std::endl;
	setVisible(false);

	setUpDelayedUpdate();
}

Description::~Description()
{
	for(EntryBase * entry : _entries)
		delete entry;

	_entries.clear();
}

void Description::setUpDelayedUpdate()
{
	connectChangesToDelay();

	//Some setting up for delayedUpdate;
	Description * desc = this;

	_timer.setSingleShot(true);
	_timer.setInterval(300); //Some interval? Could be shorter too. Lets see how it works out
	_timer.callOnTimeout([=]()
	{
		Log::log() << "Description delay timer done, update!" << std::endl;
		desc->iShouldBeUpdated(desc);
	});
}

void Description::delayedUpdate()
{
	//This slot will be fired whenever anything in any of the children changes. If a translation has just happened this might be quite a lot.
	//So instead of having them all trigger a reload of the description in the menu (because we are not loading that directly from QML, which could be an improvement)
	// we will simply set a little timer, and whenever we get a new "change" we reset this timer. This should minimize the events going off nicely.

	_timer.start(); //Will restart the timer if already running

	//Log::log() << "Description::delayedUpdate() called and timer (re)started." << std::endl;
}

void Description::connectChangesToDelay()
{
	// Ideally this function (and delayedUpdate) will be gone at some point and all of these properties simply displayed directly.
	// But this glorious future is not when we are right now.

	connect(this, &Description::titleChanged,			this, &Description::delayedUpdate);
	connect(this, &Description::iconChanged,			this, &Description::delayedUpdate);
	connect(this, &Description::descriptionChanged,		this, &Description::delayedUpdate);
	connect(this, &Description::authorChanged,			this, &Description::delayedUpdate);
	connect(this, &Description::maintainerChanged,		this, &Description::delayedUpdate);
	connect(this, &Description::websiteChanged,			this, &Description::delayedUpdate);
	connect(this, &Description::licenseChanged,			this, &Description::delayedUpdate);
	connect(this, &Description::nameChanged,			this, &Description::delayedUpdate);
	connect(this, &Description::requiresDataDefChanged,	this, &Description::delayedUpdate);
	connect(this, &Description::dynModChanged,			this, &Description::delayedUpdate);
	connect(LanguageModel::lang(), &LanguageModel::languageChanged, this, &Description::delayedUpdate);
}

void Description::addChild(DescriptionChildBase * child)
{
	assert(child);

	if(qobject_cast<EntryBase*>(child))			_entries.push_back(dynamic_cast<EntryBase*>			(child));
	
	connect(child, &DescriptionChildBase::somethingChanged, this, &Description::childChanged, Qt::UniqueConnection);
}


void Description::removeChild(DescriptionChildBase * child)
{
	assert(child);

	if(qobject_cast<EntryBase*>(child))			_entries.removeAll(dynamic_cast<EntryBase*>			(child));

	disconnect(child, &DescriptionChildBase::somethingChanged, this, &Description::childChanged);
}

void Description::setName(QString name)
{
	if (_name == name)
		return;

	_name = name;
	emit nameChanged(_name);
}

void Description::setTitle(QString title)
{
	if (_title == title)
		return;

	_title = title;
	emit titleChanged(_title);

	//Log::log() << "Title of Description '" << _name << "' changes to '" << _title << "'!" << std::endl;
}

void Description::setIcon(QString icon)
{
	if (_icon == icon)
		return;

	_icon = icon;
	emit iconChanged(_icon);
}

void Description::setDescription(QString description)
{
	if (_description == description)
		return;

	_description = description;
	emit descriptionChanged(_description);
}

void Description::setVersion(QString version)
{
	Version ver = fq(version);

	if (_version == ver)
		return;

	_version = ver;
	emit versionChanged();
}

void Description::setAuthor(QString author)
{
	if (_author == author)
		return;

	_author = author;
	emit authorChanged(_author);
}

void Description::setMaintainer(QString maintainer)
{
	if (_maintainer == maintainer)
		return;

	//We could check the maintainer here, if it is according to CRAN or not "name <email@server.code>" but otherwise it will complain quickly enough anyway

	_maintainer = maintainer;
	emit maintainerChanged(_maintainer);
}

void Description::setWebsite(QUrl website)
{
	if (_website == website)
		return;

	_website = website;
	emit websiteChanged(_website);
}

void Description::setLicense(QString license)
{
	if (_license == license)
		return;

	_license = license;
	emit licenseChanged(_license);
}

void Description::setRequiresDataDef(bool requiresData)
{
	if (_requiresDataDef == requiresData)
		return;

	_requiresDataDef = requiresData;
	emit requiresDataDefChanged(_requiresDataDef);
}

void Description::setDynMod(DynamicModule * dynMod)
{
	if (_dynMod == dynMod)
		return;

	if(_dynMod)	disconnect(	this, &Description::iShouldBeUpdated, _dynMod, &DynamicModule::loadInfoFromDescriptionItem);
	if(dynMod)	connect(	this, &Description::iShouldBeUpdated,  dynMod, &DynamicModule::loadInfoFromDescriptionItem);

	_dynMod = dynMod;

	emit dynModChanged(_dynMod);
}

std::vector<AnalysisEntry*> Description::menuEntries() const
{
	std::vector<AnalysisEntry*> entries;
	AnalysisEntry *previousEntry = nullptr;

	for(EntryBase * entry : _entries)
	{
		if(entry->shouldBeAdded())
		{
			AnalysisEntry *analysisEntry = entry->convertToAnalysisEntry(requiresDataDef());
			if (analysisEntry != nullptr)
			{
				if (analysisEntry->isGroupTitle() && previousEntry != nullptr && !previousEntry->isSeparator())
					entries.push_back(new AnalysisEntry()); // Add a separator
				entries.push_back(analysisEntry);
				previousEntry = analysisEntry;
			}
		}
	}

	return entries;
}

}
