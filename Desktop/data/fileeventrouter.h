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
#ifndef FILEEVENTROUTER_H
#define FILEEVENTROUTER_H

#include <QObject>

/**
 * @brief The UI side of the FileEvent pipeline.
 *
 * A FileEvent that is driven by the user interface routes its started/completed signals through
 * whichever FileEventRouter is registered at the time the event is constructed. That used to be a
 * FileMenu::singleton() lookup inside FileEvent itself, which coupled the event (data layer) to a
 * specific widget class; through this interface any QObject can run the pipeline instead, and the
 * data-syncer path can opt out entirely (FileEvent's routeThroughFileMenu == false).
 *
 * The router clears itself on destruction, so events constructed afterwards simply log that no UI
 * handler is available instead of connecting into freed memory.
 */
class FileEventRouter : public QObject
{
	Q_OBJECT

public:
	explicit FileEventRouter(QObject * parent) : QObject(parent)
	{
		_current = this;
	}

	~FileEventRouter() override
	{
		if(_current == this)
			_current = nullptr;
	}

	static FileEventRouter * current() { return _current; }

	virtual void startFileEvent()	= 0; ///< called when an event starts; runs the request side of the pipeline
	virtual void finalizeFileEvent() = 0; ///< called when an event completes; runs the finalize side and cleans the event up

private:
	static FileEventRouter * _current;
};

#endif // FILEEVENTROUTER_H
