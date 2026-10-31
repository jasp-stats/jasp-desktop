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
/// Works around a bug in Qt's incremental QML garbage collector (Qt 6.8 through at least 6.12 and dev, not fixed upstream yet).
/// The collector decides only once per cycle, early on, which QObjectWrappers are kept alive by their QObject (C++ ownership or parent chain).
/// A QObject that gets parented or rooted after that pass, as happens while big forms are built, keeps a wrapper the collector never marks.
/// That wrapper and its property/method storage are freed while still in use, and JASP later crashes inside the collector (QV4::MarkStack::drain).
///
/// installQmlGcRescan repeats Qt's own pass, to completion, right before marking ends (the MarkReady step of Qt's private GC state machine),
/// then starts a fresh time slice so that Qt rescans the JS stack and drains (InitCallDestroyObjects) before any JS runs again.
/// Not covered: JS that runs during the later, interruptible steps of the same cycle (destroying objects, sweeping).
/// Cost: one blocking step per GC cycle that grows with the number of QML objects, about 6 ms per open analysis form
/// (under 0.1 ms in a fresh session, 144 ms with 25 Parametric Survival analyses; Qt 6.11.1, Windows).
/// It must be called for every QQmlEngine that builds analysis forms; only MainWindow's engine is hooked (not SyntaxInterface's).
/// Uses Qt's private API (Qt::QmlPrivate): compiled only for Qt >= 6.8, and installed only when the Qt at run time is the version it was built with.
/// Tested with Qt 6.11.1; the Qt logic it relies on is equivalent from 6.8.0 through 6.12 and dev.
/// Remove it once Qt repeats the weak-value pass before sweeping. Check with Qt >= 6.11, which has the validator: with QV4_MM_CROSS_VALIDATE_INCREMENTAL_GC=1,
/// QV4_GC_TIMELIMIT=1 and the installQmlGcRescan call removed, creating a few Parametric Survival analyses then logs no "Cross Validation Error".
///
void installQmlGcRescan(QQmlEngine * engine);

#endif // QMLGCRESCAN_H
