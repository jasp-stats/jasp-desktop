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
#include <QElapsedTimer>
#include <QLoggingCategory>

#include <private/qv4engine_p.h>
#include <private/qv4mm_p.h>

//Off by default, enable with QT_LOGGING_RULES="jasp.qml.gc.rescan.debug=true" to see how long each rescan takes
Q_LOGGING_CATEGORY(lcQmlGcRescan, "jasp.qml.gc.rescan", QtWarningMsg)

namespace
{
	using QV4::GCStateMachine;
	using StateFunction = GCStateMachine::GCState (*)(GCStateMachine *, GCStateMachine::ExtraData &);

	StateFunction originalMarkReady = nullptr;

	//Runs Qt's own weak-value pass (QObjectWrappers kept alive by their QObject or its parent chain) once more, to completion,
	//then rescans the JS stack and drains, as Qt does before sweeping. The pass keeps its iterator in the state data, as in Qt.
	GCStateMachine::GCState rescanThenMarkReady(GCStateMachine * gc, GCStateMachine::ExtraData & data)
	{
		QV4::MemoryManager * mm = gc->mm;

		if(gc->timeLimit.count() > 0 && mm->markStack() && mm->m_weakValues) //Only incremental runs can miss anything
		{
			QElapsedTimer timer;
			timer.start();

			auto oldDeadline = std::exchange(gc->deadline, QDeadlineTimer(QDeadlineTimer::Forever));

			gc->stateInfoMap[GCStateMachine::InitMarkWeakValues].execute(gc, data);
			while(gc->stateInfoMap[GCStateMachine::MarkWeakValues].execute(gc, data) == GCStateMachine::MarkWeakValues) {}

			gc->deadline = oldDeadline;

			mm->collectFromJSStack(mm->markStack());
			mm->markStack()->drain();

			qCDebug(lcQmlGcRescan).nospace() << "QML GC rescan took " << timer.nsecsElapsed() / 1000 << " us";
		}

		return originalMarkReady(gc, data);
	}
}

void installQmlGcRescan(QQmlEngine * engine)
{
	GCStateMachine	* gc		= engine->handle()->memoryManager->gcStateMachine.get();
	StateFunction	& markReady	= gc->stateInfoMap[GCStateMachine::MarkReady].execute;

	if(markReady == rescanThenMarkReady)
		return;

	originalMarkReady	= markReady;
	markReady			= rescanThenMarkReady;
}
