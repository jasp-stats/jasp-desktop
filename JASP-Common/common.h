//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#ifndef COMMON_H
#define COMMON_H

#if __GNUC__ == 4 && __GNUC_MINOR__ == 6
#define OVERRIDE
#else
#define OVERRIDE override
#endif

#if _WIN64 || __amd64__
#define ARCH_64
#else
#define ARCH_32
#endif

typedef unsigned int uint;

#endif // COMMON_H
