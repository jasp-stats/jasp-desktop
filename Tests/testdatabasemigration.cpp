//
// Copyright (C) 2013-2026 University of Amsterdam
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
#include "testdatabasemigration.h"
#include "databaseinterface.h"
#include "version.h"

namespace {

// DatabaseInterface(create=true, inMemory=true) builds the *current* schema in :memory:.
// To test the migration we revert just the tables it touches back to their pre-0.99 shape and
// seed a representative row, so upgradeDBFromVersion actually has work to do.
void revertToPre099Schema(DatabaseInterface & db)
{
	db.runStatements("PRAGMA foreign_keys=OFF;");			// so we can drop/recreate freely in the test
	db.runStatements("DROP TABLE IF EXISTS Workspace;");
	db.runStatements("DROP TABLE IF EXISTS Filters;");
	db.runStatements("DROP TABLE IF EXISTS DataSets;");

	db.runStatements(
		"CREATE TABLE DataSets ("
		" id INTEGER PRIMARY KEY, dataFilePath TEXT, dataFileTimestamp INT DEFAULT 0,"
		" description TEXT, databaseJson TEXT, emptyValuesJson TEXT, revision INT DEFAULT 0,"
		" dataFileSynch INT, showRSyntax INT DEFAULT 0, csvDelimiter INT DEFAULT 0"
		");");

	db.runStatements(
		"CREATE TABLE Filters ("
		" id INTEGER PRIMARY KEY, dataSet INT, rFilter TEXT, name TEXT, generatedFilter TEXT,"
		" constructorJson TEXT, constructorR TEXT, errorMsg TEXT, revision INT DEFAULT 0"
		");");

	// representative data: one dataset with R-syntax shown, one named filter on it
	db.runStatements("INSERT INTO DataSets (id, description, showRSyntax) VALUES (1, 'my data', 1);");
	db.runStatements("INSERT INTO Filters  (id, dataSet, name, rFilter)   VALUES (1, 1, 'Filter 1', 'TRUE');");
}

} // namespace


void TestDatabaseMigration::migratePre099_movesShowRSyntaxAndAddsColumns()
{
	DatabaseInterface db(/*create=*/true, /*inMemory=*/true);	// ctor clears _singleton on destruction
	revertToPre099Schema(db);

	// sanity: we really are on the old shape
	QVERIFY ( db.tableHasColumn("DataSets", "showRSyntax"));
	QVERIFY (!db.tableExists   ("Workspace"));
	QVERIFY (!db.tableHasColumn("Filters",  "invalidated"));

	db.upgradeDBFromVersion(Version("0.98.1"));		// a pre-0.99 file

	// the new multi-dataset schema is present
	QVERIFY ( db.tableExists   ("Workspace"));
	QVERIFY ( db.tableHasColumn("DataSets", "title"));
	QVERIFY ( db.tableHasColumn("DataSets", "codeType"));
	QVERIFY ( db.tableHasColumn("DataSets", "rCode"));
	QVERIFY ( db.tableHasColumn("DataSets", "defaultInputFilter"));
	QVERIFY ( db.tableHasColumn("Filters",  "invalidated"));
	QVERIFY (!db.tableHasColumn("DataSets", "showRSyntax"));		// moved out & dropped

	// showRSyntax value preserved into the new Workspace row
	QCOMPARE( db.runStatementsId("SELECT showRSyntax FROM Workspace LIMIT 1;"), 1);

	// user data survived the migration
	QCOMPARE( db.runStatementsId("SELECT id      FROM DataSets WHERE description = 'my data';"), 1);
	QCOMPARE( db.runStatementsId("SELECT dataSet  FROM Filters  WHERE name = 'Filter 1';"),      1);
}

void TestDatabaseMigration::migratePre099_isIdempotent()
{
	DatabaseInterface db(/*create=*/true, /*inMemory=*/true);
	revertToPre099Schema(db);

	db.upgradeDBFromVersion(Version("0.98.1"));
	// running the same migration again must not throw nor change the preserved value
	db.upgradeDBFromVersion(Version("0.98.1"));

	QVERIFY ( db.tableExists("Workspace"));
	QVERIFY (!db.tableHasColumn("DataSets", "showRSyntax"));
	QCOMPARE( db.runStatementsId("SELECT showRSyntax FROM Workspace LIMIT 1;"), 1);
}


QTEST_MAIN(TestDatabaseMigration)
