#include "json/json.h"
#include "data/fileevent.h"
#include "utilities/qutils.h"
#include "utilities/appdirs.h"
#include "autosavefilesystem.h"
#include "gui/preferencesmodel.h"
#include "utilities/extractarchive.h"

AutoSaveFileSystem::AutoSaveFileSystem(QObject *parent)
	: FileSystem{parent}
{
	AutoSaveFileSystem::refresh();
}

void AutoSaveFileSystem::refresh()
{
	_entries.clear();

	QDir autoSave = AppDirs::autoSaveDir();

	Json::Reader reader;

	QDateTime now = QDateTime::currentDateTime();

	for(const QFileInfo & asf : autoSave.entryInfoList(QDir::Filter::Files | QDir::Filter::NoDotAndDotDot | QDir::Filter::NoSymLinks, QDir::SortFlag::Time))
	{
		int daysAgo = asf.lastModified().daysTo(now),
			secsAgo = asf.lastModified().secsTo(now);

		if(daysAgo > 30)
		{
			if(asf.exists())
			{
				QFile asfF(asf.absoluteFilePath());

				if(!asfF.moveToTrash())
					asfF.remove();
			}
		}
		else if(asf.absoluteFilePath() != FileEvent::pathTmp())
		{
			//Its not our autosave file and also a file that isnt under active autosave by another jasp instance, so we can show it
			std::string json	= ExtractArchive::extractSingleTextFileFromArchive(fq(asf.absoluteFilePath()), "analyses.json");
			Json::Value jObj;
			bool		alive	= PreferencesModel::prefs()->autoSaveAtAll() && secsAgo <= 60 + PreferencesModel::prefs()->autoSaveIntervalSec();
			QString		savedAt = !alive ? tr("Saved at %1").arg(asf.lastModified().toString()) : tr("Saved %1 seconds ago, might still be loaded in another JASP.").arg(secsAgo);
			bool		parsed	= reader.parse(json, jObj) && jObj.isMember("autoSaveDescription") && jObj.isMember("autoSaveFileName");

			_entries.push_back(parsed	? createEntry(asf.absoluteFilePath(), tq(jObj.get("autoSaveFileName", "unknown").asString()),	tq(jObj.get("autoSaveDescription", "").asString()) +"<br>" + savedAt,	FileSystemEntry::JASP)
										: createEntry(asf.absoluteFilePath(), asf.fileName(),											savedAt,																FileSystemEntry::JASP));
		}
	}

	emit entriesChanged();
}
