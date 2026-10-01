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

#ifndef JASPEXPORTER_H
#define JASPEXPORTER_H

#include "exporter.h"
#include <archive.h>
#include <time.h>
#include <queue>
#include <mutex>
#include "version.h"
#include "../datasetpackage.h"

///
/// To export to *.JASP files
/// Those are basically zips with some json files in there btw
class JASPExporter: public Exporter
{
public:
	JASPExporter();
	void saveDataSet(const std::string &path, std::function<void (int)> progressCallback) override;

	static const Version jaspArchiveVersion;

	static time_t _now;

// Snapshot management functions
public:
	static bool createSnapshot(const std::string &snapshotPrefix = "jasp_snapshot_", std::string *errorOut = nullptr);
	static void cleanupSnapshot(const std::string &snapshotPath);
	static void printSnapshotContents(const std::string &snapshotPath);
	static bool isSaveInProgress();

private:
	static void saveManifest(archive * a);
	static void saveResults(archive * a);
	static void saveAnalyses(archive * a, const std::string &sourceDir);
	static void saveDatabase(archive * a, const std::string &sourceDir);
	static void saveSnapshotFile(archive * a, const std::string &fileName, const std::string &sourceDir);
	static void makeEntry(archive * a, const std::string & filename, const std::string & data);

	static std::queue<std::string> _snapshotQueue;
	static std::mutex _snapshotMutex;
};

#endif // JASPEXPORTER_H