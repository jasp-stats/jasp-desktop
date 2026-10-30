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
#ifndef QMLGCRESCAN_H
#define QMLGCRESCAN_H

class QQmlEngine;

///
/// Works around a bug in Qt's incremental QML garbage collector (Qt >= 6.8, still in 6.12).
/// The collector decides only once per cycle, early on, which QObjectWrappers are kept alive by their QObject (C++ ownership or parent chain).
/// A QObject that gets parented or rooted after that pass, as happens while big forms are built, keeps a wrapper the collector never marks.
/// That wrapper and its property/method storage are freed while still in use, and JASP later crashes inside the collector.
/// installQmlGcRescan repeats that pass right before marking ends (one short atomic step), through Qt's private GC state machine.
///
void installQmlGcRescan(QQmlEngine * engine);

#endif // QMLGCRESCAN_H
