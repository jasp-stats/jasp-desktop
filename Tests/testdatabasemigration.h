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
#pragma once
#include <QtTest>

/// Unit tests for DatabaseInterface::upgradeDBFromVersion, i.e. the migration that older jasp-files
/// go through when loaded. Uses an in-memory sqlite database (no session dir / file I/O) so the
/// schema mechanics can be checked in isolation.
class TestDatabaseMigration : public QObject
{
	Q_OBJECT
private slots:
	void migratePre099_movesShowRSyntaxAndAddsColumns();
	void migratePre099_isIdempotent();
};
