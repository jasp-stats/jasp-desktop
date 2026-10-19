#include "qutils.h"
#include "json/json.h"
#include "data/fileevent.h"
#include "utilities/appdirs.h"
#include "autosavefilesystem.h"
#include "gui/preferencesmodel.h"
#include "utilities/extractarchive.h"
#include "data/jaspencrypt.h"
#include "log.h"

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
		try
		{
			int daysAgo = asf.lastModified().daysTo(now),
				secsAgo = asf.lastModified().secsTo(now);

			QString path = asf.absoluteFilePath();

			if(daysAgo > 30)
			{
				if(asf.exists())
				{
					QFile asfF(path);

					if(!asfF.moveToTrash())
						asfF.remove();
				}
			}
			else if(path != FileEvent::pathTmp())
			{
				//Its not our autosave file and also a file that isnt under active autosave by another jasp instance, so we can show it
				bool parsed = false;
				bool encrypted = JASPEncrypt::detectEncryptedJASPFile(fq(path));
				Json::Value jObj;
				if (!encrypted)
				{
					std::string json	= ExtractArchive::extractSingleTextFileFromArchive(fq(path), "analyses.json");
					parsed	= reader.parse(json, jObj) && jObj.isMember("autoSaveDescription") && jObj.isMember("autoSaveFileName");
				}
				bool		alive	= PreferencesModel::prefs()->autoSaveAtAll() && secsAgo <= 60 + PreferencesModel::prefs()->autoSaveIntervalSec();
				QString		savedAt = !alive ? tr("Saved at %1").arg(asf.lastModified().toString()) : tr("Saved %1 seconds ago, might still be loaded in another JASP.").arg(secsAgo);

				_entries.push_back(parsed	? createEntry(path, tq(jObj.get("autoSaveFileName", "unknown").asString()),	tq(jObj.get("autoSaveDescription", "").asString()) +"<br>" + savedAt,	FileSystemEntry::JASP)
											: createEntry(path, asf.fileName() + (encrypted ? " (" + tr("encrypted") + ")" : ""),		savedAt,																FileSystemEntry::JASP));
			}
		}
		catch(std::exception & e)
		{
			Log::log() << "Error when reading autosave files: " << e.what() << std::endl;
		}
		catch(...)
		{
			Log::log() << "Unknown error when reading autosave files" << std::endl;
		}
	}

	emit entriesChanged();
}
