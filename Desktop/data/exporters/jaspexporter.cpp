//
// Copyright (C) 2018-2026 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "jaspexporter.h"

#include <sys/stat.h>

#include <ios>
#include <archive.h>
#include <archive_entry.h>
#include <json/json.h>
#include <fstream>
#include "data/jaspencryptiondata.h"
#include "data/jaspencrypt.h"
#include "version.h"
#include "tempfiles.h"
#include "log.h"
#include "utilenums.h"
#include "utilities/qutils.h"
#include <fstream>
#include "appinfo.h"
#include "gui/preferencesmodel.h"
#include "data/asyncloader.h"

#include <utilities/desktopcommunicator.h>
#include <chrono>


time_t JASPExporter::_now;
const Version JASPExporter::jaspArchiveVersion = Version("6.0.0");
std::queue<std::string> JASPExporter::_snapshotQueue;
std::mutex JASPExporter::_snapshotMutex;

JASPExporter::JASPExporter()
{
	_defaultFileType = Utils::FileType::jasp;
	_allowedFileTypes.push_back(Utils::FileType::jasp);
}

bool JASPExporter::createSnapshot(const std::string &snapshotPrefix, std::string *errorOut)
{
	auto fail = [errorOut](const std::string &error)
	{
		Log::log() << "JASP Export: " << error << std::endl;
		if (errorOut != nullptr)
			*errorOut = error;
		return false;
	};

	auto now = std::chrono::system_clock::now().time_since_epoch();
	auto sec = std::chrono::duration_cast<std::chrono::seconds>(now).count();
	auto ns  = std::chrono::duration_cast<std::chrono::nanoseconds>(now).count() % 1'000'000'000;
	std::string timestamp = std::to_string(sec) + "_" + std::to_string(ns);

	std::filesystem::path fullSnapshotPath = std::filesystem::path(QDir::tempPath().toStdString()) / (snapshotPrefix + timestamp);

	std::error_code ec;
	std::filesystem::create_directories(fullSnapshotPath, ec);

	if (ec)
		return fail("Failed to create snapshot directory (" + ec.message() + ")");

	std::string sessionDir = TempFiles::sessionDirName();
	if (!sessionDir.empty())
	{
		//Stream every file instead of using std::filesystem::copy (which is CopyFile-based): the session directory can
		//live in a tree Windows EFS-encrypts (MSIX "Application Protected", see jasp-issues#4566) and CopyFile then fails
		//with ERROR_ENCRYPTION_FAILED while plain reads still decrypt transparently. A .jasp without the database is no
		//save at all, so failing to copy that one file is fatal; other files are skipped and logged, just like the release
		//build treats missing files when zipping.
		const std::string		dbFileName	= DatabaseInterface::singleton()->dbFile(true);
		std::filesystem::path	sessionPath(sessionDir);

		std::error_code walkEc;
		std::filesystem::recursive_directory_iterator dirIt(sessionPath, walkEc);

		if (walkEc)
		{
			std::filesystem::remove_all(fullSnapshotPath, ec);
			return fail("Failed to open session directory '" + sessionDir + "' (" + walkEc.message() + ")");
		}

		bool dbCopied = false;

		while (dirIt != std::filesystem::recursive_directory_iterator())
		{
			const std::filesystem::directory_entry entry = *dirIt;

			std::error_code entryEc;
			if (!entry.is_directory(entryEc) && entry.is_regular_file(entryEc))
			{
				std::error_code relEc;
				std::filesystem::path relativePath = std::filesystem::relative(entry.path(), sessionPath, relEc);

				if (relEc)
				{
					Log::log() << "JASP Export: Skipping '" << entry.path().string() << "' (" << relEc.message() << ")" << std::endl;
				}
				else
				{
					const bool			isDb		= relativePath.filename().string() == dbFileName;
					std::filesystem::path destination	= fullSnapshotPath / relativePath;

					std::error_code dirEc;
					std::filesystem::create_directories(destination.parent_path(), dirEc);

					std::string copyError;
					if (!dirEc && Utils::copyFileStreamed(entry.path().string(), destination.string(), copyError))
					{
						if (isDb)
							dbCopied = true;
					}
					else
					{
						std::string reason = dirEc ? dirEc.message() : copyError;

						if (isDb)
						{
							std::filesystem::remove_all(fullSnapshotPath, ec);
							return fail("Could not copy the database file '" + relativePath.string() + "' (" + reason + ")");
						}

						Log::log() << "JASP Export: Skipping '" << relativePath.string() << "': " << reason << std::endl;
					}
				}
			}

			dirIt.increment(walkEc);
			if (walkEc)
			{
				std::filesystem::remove_all(fullSnapshotPath, ec);
				return fail("Failed to enumerate session directory '" + sessionDir + "' (" + walkEc.message() + ")");
			}
		}

		if (!dbCopied)
			Log::log() << "JASP Export: No database file '" << dbFileName << "' found in the session directory" << std::endl;

		Log::log() << "JASP Export: Created snapshot at " << fullSnapshotPath << std::endl;
		printSnapshotContents(fullSnapshotPath.string());
	}
	else
	{
		Log::log() << "JASP Export: Session directory was empty, snapshot created but empty" << std::endl;
	}

	std::lock_guard<std::mutex> lock(_snapshotMutex);
	_snapshotQueue.push(fullSnapshotPath.string());

	return true;
}

bool JASPExporter::isSaveInProgress()
{
	std::lock_guard<std::mutex> lock(_snapshotMutex);
	return !_snapshotQueue.empty();
}

void JASPExporter::cleanupSnapshot(const std::string &snapshotPath)
{
	if (snapshotPath.empty())
		return;

	std::error_code ec;
	std::filesystem::path path(snapshotPath);

	std::filesystem::remove_all(path, ec);
	if (!ec)
		Log::log() << "JASP Export: Cleaned up snapshot at " << snapshotPath << std::endl;
}

void JASPExporter::printSnapshotContents(const std::string &snapshotPath)
{
	std::error_code ec;

	Log::log() << "JASP Export: Snapshot contents:" << std::endl;

	if (!std::filesystem::exists(snapshotPath, ec) || ec)
	{
		Log::log() << "  [Error accessing snapshot directory: " << ec.message() << "]" << std::endl;
		return;
	}

	for (auto entry : std::filesystem::directory_iterator(snapshotPath, ec))
	{
		std::string name = entry.path().filename().string();
		std::string relativePath = name;
		if (entry.path().is_absolute() && entry.path().parent_path() != std::filesystem::path(snapshotPath))
			relativePath = entry.path().relative_path().string();

		if (entry.is_regular_file())
		{
			std::error_code sizeEc;
			auto size = std::filesystem::file_size(entry.path(), sizeEc);
			if (!sizeEc)
				Log::log() << "  File: " << relativePath << " (" << (size / 1024) << " KB)" << std::endl;
		}
		else if (entry.is_directory())
		{
			Log::log() << "  Dir: " << relativePath << "/" << std::endl;
		}
	}
}

void JASPExporter::saveDataSet(const std::string &path, std::function<void(int)> progressCallback)
{
	struct archive *a;

	_now = time(nullptr); //Give all files same timestamp

	std::string sourceDir;
	{
		std::lock_guard<std::mutex> lock(_snapshotMutex);
		if (_snapshotQueue.empty())
			throw LoaderException("JASP Export: No snapshot available for saving.");
		sourceDir = _snapshotQueue.front();
		_snapshotQueue.pop();
	}

	//The snapshot was popped, so from here on we own it: clean it up when leaving this function,
	//including on exceptions, otherwise the popped directory leaks in the temp dir forever.
	struct ScopedSnapshotCleanup { std::string dir; ~ScopedSnapshotCleanup() { JASPExporter::cleanupSnapshot(dir); } } snapshotCleanup{sourceDir};

	std::filesystem::path tmpPath = path;
	bool encrypt = JaspEncryptionData::getInstance()->encryptionActive();
	if(encrypt) {
		if(!JaspEncryptionData::getInstance()->paramsSet())
		{
			if (!DesktopCommunicator::singleton()->queryEncryptionSettings())
				throw LoaderException("Query encryption settings is cancelled", true); // Cancelled
		}

		if(!JaspEncryptionData::getInstance()->paramsSet())
			throw LoaderException(DesktopCommunicator::tr("No password given!").toStdString());

		tmpPath = std::filesystem::temp_directory_path() / ("_tmp_unlock_" + std::filesystem::path(path).filename().generic_string());
	}

	a = archive_write_new();
	archive_write_set_format_zip(a);


#ifdef _WIN32
	if (archive_write_open_filename_w(a, QString(tmpPath.c_str()).toStdWString().c_str()) != ARCHIVE_OK)
#else
	if (archive_write_open_filename(a, tmpPath.c_str()) != ARCHIVE_OK)
#endif
		throw LoaderException(std::string("File could not be opened because of ") + archive_error_string(a));

	saveManifest(a);              progressCallback(10);
	saveAnalyses(a, sourceDir);   progressCallback(30);
	saveResults(a);               progressCallback(70);
	saveDatabase(a, sourceDir);   progressCallback(100);

	if (archive_write_close(a) != ARCHIVE_OK)
		throw LoaderException("File could not be closed.");

	archive_write_free(a);
	if(encrypt) {
		Json::Value root;
		try {
			auto privKey = JaspEncryptionData::getInstance()->getPrivatekey();
			if(privKey.length()) //check if user want to use privkey or password to encrypt
				JASPEncrypt::encrypt(tmpPath, path, privKey, root, JaspEncryptionData::getInstance()->getPublicKeyResponse(), JaspEncryptionData::getInstance()->getPasswordSaltResponse(), true);
			else
				JASPEncrypt::encrypt(tmpPath, path, JaspEncryptionData::getInstance()->getPassword(), root, JaspEncryptionData::getInstance()->getPublicKeyResponse());
		} catch (std::exception& e) {
			Log::log() << "Encryption failed: " << e.what() << std::endl;
			throw LoaderException("Encryption failed. Click 'Save As' and save as normal Jasp File. \n\n" + std::string(" Technical Reason: ") + std::string(e.what()));
		}
		std::filesystem::remove(tmpPath);
	}

	DataSetPackage::pkg()->setLoaded(true);
}

void JASPExporter::saveManifest(archive * a)
{

	Json::Value manifest = Json::objectValue;

	manifest["jaspArchiveVersion"]	= jaspArchiveVersion.asString();
	manifest["jaspVersion"]			= AppInfo::version.asString();

	makeEntry(a, "manifest.json", manifest.toStyledString());
}

void JASPExporter::saveResults(archive * a)
{

	DataSetPackage::pkg()->waitForExportResultsReady();

	makeEntry(a, "index.html", fq(DataSetPackage::pkg()->analysesHTML()));
}

void JASPExporter::saveSnapshotFile(archive *a, const std::string & fileName, const std::string &sourceDir)
{

	std::string fullPath = sourceDir + "/" + fileName;
	std::ifstream readTempFile(fullPath, std::ios::ate | std::ios::binary);
	char fileBuff[1 << 16]; //64KB: big files (300MB+ .jasp) go through here, keep the syscall count down

	if (readTempFile.is_open())
	{
		archive_entry * entry = archive_entry_new();

		archive_entry_set_pathname( entry,  fileName.c_str());
		archive_entry_set_size(     entry,  readTempFile.tellg());
		archive_entry_set_filetype( entry,  AE_IFREG);
		archive_entry_set_birthtime(entry,  _now, 0);
		archive_entry_set_ctime(    entry,  _now, 0);
		archive_entry_set_mtime(    entry,  _now, 0);
		archive_entry_set_atime(    entry,  _now, 0);
		archive_entry_set_perm(     entry,  0644);

		archive_write_header(a, entry);

		readTempFile.seekg(0, std::ios::beg);
		while (!readTempFile.eof())
		{
			readTempFile.read(fileBuff, sizeof(fileBuff));
			archive_write_data(a, fileBuff, readTempFile.gcount());
		}
		archive_entry_free(entry);
	}
	else
	{
		Log::log() << "JASP Export: cannot find/open file " << fileName << " in " << sourceDir << std::endl;
#ifdef JASP_DEBUG
		throw LoaderException("JASP Export: cannot find/open file " + fileName);
#endif
	}
	readTempFile.close();
}

void JASPExporter::saveAnalyses(archive *a, const std::string &sourceDir)
{

	const Json::Value analysesJson = DataSetPackage::pkg()->analysesData();

	makeEntry(a, "analyses.json", analysesJson.toStyledString());

	const Json::Value & analysesDataList = analysesJson.isArray() ? analysesJson : analysesJson["analyses"];

	for (const Json::Value & analysisJson : analysesDataList)
		for (const std::string & path : TempFiles::retrieveList(analysisJson["id"].asInt(), sourceDir))
			if(!stringUtils::endsWith(path, "/state") || (PreferencesModel::prefs()->storeStateEtc() && analysisJson.get("saveState", "default").asString() != "never") || analysisJson.get("saveState", "default").asString() == "always")
				saveSnapshotFile(a, path, sourceDir);
}

void JASPExporter::saveDatabase(archive * a, const std::string &sourceDir)
{
	saveSnapshotFile(a, DatabaseInterface::singleton()->dbFile(true), sourceDir);
}

void JASPExporter::makeEntry(archive * a, const std::string & filename, const std::string & data)
{
	archive_entry *entry = archive_entry_new();

	archive_entry_set_pathname( entry,	filename.c_str());
	archive_entry_set_size(     entry,  int(data.size()));
	archive_entry_set_birthtime(entry,  _now, 0);
	archive_entry_set_ctime(    entry,  _now, 0);
	archive_entry_set_mtime(    entry,  _now, 0);
	archive_entry_set_atime(    entry,  _now, 0);
	archive_entry_set_filetype( entry,  AE_IFREG);
	archive_entry_set_perm(     entry,  0644);

	archive_write_header(               a, entry);
	size_t written = archive_write_data(a,  data.c_str(), data.size());

	if (written != data.size())
		throw LoaderException("File could not be written.");

	archive_entry_free(entry);
}
