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
#include <QStringList>

#include <private/qv4engine_p.h>
#include <private/qv4identifierhashdata_p.h>
#include <private/qv4identifiertable_p.h>
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
	StateFunction originalDoSweep	= nullptr;

	bool isIncrementalRun(GCStateMachine * gc)
	{
		return gc->timeLimit.count() > 0 && !gc->mm->aggressiveGC && gc->mm->markStack(); //Only incremental runs can miss anything (the same test as Qt's markReady from 6.11 on)
	}

	//The not yet marked identifiers of all identifier hashes, for the debug log
	QStringList unmarkedIdentifiers(const QV4::IdentifierTable * table)
	{
		QStringList names;
		for(const QV4::IdentifierHashData * hash : table->idHashes)
			for(const QV4::IdentifierHashEntry * e = hash->entries; e < hash->entries + hash->alloc; ++e)
				if(QV4::Heap::StringOrSymbol * id = e->identifier.asStringOrSymbol(); id && !id->isMarked())
					names << id->toQString();
		return names;
	}

	//Marks the identifiers of all identifier hashes (such as the property names of every QQmlContext) once more; the marks go on the mark stack.
	//Qt marks them only at the start of a cycle (ExecutionEngine::markObjects), and an identifier that already exists unmarked in the identifier table is not marked when a hash takes it
	//(IdentifierTable::resolveStringEntry, IdentifierHash::addEntry). JASP's controls intern "isDynamic" through a failed contextProperty lookup, then RowControls gives it to setContextProperty for every row:
	//when that happens during a cycle, the string is swept while the row contexts still hold it, and the next cycle marks the freed string and crashes in QV4::MarkStack::drain.
	void remarkIdentifiers(GCStateMachine * gc, const char * step)
	{
		QElapsedTimer timer;
		timer.start();

		QV4::IdentifierTable * table = gc->mm->engine->identifierTable;
		const QStringList unmarked = lcQmlGcRescan().isDebugEnabled() ? unmarkedIdentifiers(table) : QStringList();

		table->markObjects(gc->mm->markStack());

		qCDebug(lcQmlGcRescan).nospace().noquote() << "QML GC identifier remark (" << step << ") took " << timer.nsecsElapsed() / 1000 << " us"
												   << (unmarked.isEmpty() ? QString() : ", kept unmarked: " + unmarked.join(", "));
	}

	//Runs Qt's own weak-value pass (QObjectWrappers kept alive by their QObject or its parent chain) once more, to completion, and marks the identifiers again.
	//The passes leave their marks on the mark stack; Qt's InitCallDestroyObjects rescans the JS stack and drains it before anything is destroyed or swept.
	GCState rescanThenMarkReady(GCStateMachine * gc, ExtraData & data)
	{
		QV4::MemoryManager * mm = gc->mm;

		if(isIncrementalRun(gc))
		{
			if(mm->m_weakValues)
			{
				QElapsedTimer timer;
				timer.start();

				gc->deadline = QDeadlineTimer(QDeadlineTimer::Forever);

				gc->stateInfoMap[GCState::InitMarkWeakValues].execute(gc, data);
				while(gc->stateInfoMap[GCState::MarkWeakValues].execute(gc, data) == GCState::MarkWeakValues) {}

				qCDebug(lcQmlGcRescan).nospace() << "QML GC rescan took " << timer.nsecsElapsed() / 1000 << " us";
			}

			remarkIdentifiers(gc, "MarkReady");

			gc->deadline = QDeadlineTimer(gc->timeLimit); //A fresh slice, so Qt's InitCallDestroyObjects (which redrains) runs before any JS can parent more objects
		}

		return originalMarkReady(gc, data);
	}

	//JS runs between the later steps of a cycle and in destruction handlers (also in a non-incremental cycle) and can give contexts more identifiers,
	//so mark them once more right before Qt sweeps the identifier table and the heap.
	GCState remarkIdentifiersThenSweep(GCStateMachine * gc, ExtraData & data)
	{
		if(gc->mm->markStack())
		{
			remarkIdentifiers(gc, "DoSweep");
			gc->mm->markStack()->drain(); //Marks the identifiers and the strings they refer to, before the sweep
		}

		return originalDoSweep(gc, data);
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
	StateFunction	& doSweep	= gc->stateInfoMap[GCState::DoSweep].execute;

	if(markReady == rescanThenMarkReady)
		return;

	originalMarkReady	= markReady;
	markReady			= rescanThenMarkReady;
	originalDoSweep		= doSweep;
	doSweep				= remarkIdentifiersThenSweep;
#else
	Q_UNUSED(engine);
#endif
}
