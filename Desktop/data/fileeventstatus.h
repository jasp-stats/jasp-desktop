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
#ifndef FILEEVENTSTATUS_H
#define FILEEVENTSTATUS_H
#include "enumutilities.h"

///Where a FileEvent is in its lifecycle: it is initialized, then started once, and completed once.
DECLARE_ENUM(FileEventStatus, Initialized, Started, Completed);

#endif // FILEEVENTSTATUS_H
