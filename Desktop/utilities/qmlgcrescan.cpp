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
#include "qmlgcrescan.h"

#include <QQmlEngine>

//Qt's incremental GC, and with it the bug, starts with Qt 6.8. The GC state names are spelled as in 6.8.0 (directly in QV4), which from 6.8.1 on are aliases.
#if QT_VERSION >= QT_VERSION_CHECK(6, 8, 0)

#include <QElapsedTimer>
#include <QLoggingCategory>

#include <private/qv4engine_p.h>
#include <private/qv4mm_p.h>

//Off by default, enable with QT_LOGGING_RULES="jasp.qml.gc.rescan.debug=true" to see how long each rescan takes
Q_LOGGING_CATEGORY(lcQmlGcRescan, "jasp.qml.gc.rescan", QtWarningMsg)

namespace
{
	using QV4::GCState;
	using QV4::GCStateMachine;
	using ExtraData		= QV4::GCStateInfo::ExtraData;
	using StateFunction = GCState (*)(GCStateMachine *, ExtraData &);

	StateFunction originalMarkReady = nullptr;

	//Runs Qt's own weak-value pass (QObjectWrappers kept alive by their QObject or its parent chain) once more, to completion.
	//The pass leaves its marks on the mark stack; Qt's InitCallDestroyObjects rescans the JS stack and drains it before anything is destroyed or swept.
	GCState rescanThenMarkReady(GCStateMachine * gc, ExtraData & data)
	{
		QV4::MemoryManager * mm = gc->mm;

		if(gc->timeLimit.count() > 0 && !mm->aggressiveGC && mm->markStack() && mm->m_weakValues) //Only incremental runs can miss anything (the same test as Qt's markReady from 6.11 on)
		{
			QElapsedTimer timer;
			timer.start();

			gc->deadline = QDeadlineTimer(QDeadlineTimer::Forever);

			gc->stateInfoMap[GCState::InitMarkWeakValues].execute(gc, data);
			while(gc->stateInfoMap[GCState::MarkWeakValues].execute(gc, data) == GCState::MarkWeakValues) {}

			qCDebug(lcQmlGcRescan).nospace() << "QML GC rescan took " << timer.nsecsElapsed() / 1000 << " us";

			gc->deadline = QDeadlineTimer(gc->timeLimit); //A fresh slice, so Qt's InitCallDestroyObjects (which redrains) runs before any JS can parent more objects
		}

		return originalMarkReady(gc, data);
	}
}

#endif

void installQmlGcRescan(QQmlEngine * engine)
{
#if QT_VERSION >= QT_VERSION_CHECK(6, 8, 0)
	//The hook relies on Qt-private struct layouts, which may change in any Qt release
	if(qstrcmp(qVersion(), QT_VERSION_STR) != 0)
	{
		qCWarning(lcQmlGcRescan).nospace() << "QML GC rescan disabled: built for Qt " << QT_VERSION_STR << ", running " << qVersion();
		return;
	}

	GCStateMachine	* gc		= engine->handle()->memoryManager->gcStateMachine.get();
	StateFunction	& markReady	= gc->stateInfoMap[GCState::MarkReady].execute;

	if(markReady == rescanThenMarkReady)
		return;

	originalMarkReady	= markReady;
	markReady			= rescanThenMarkReady;
#else
	Q_UNUSED(engine);
#endif
}
