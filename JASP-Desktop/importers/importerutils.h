/*
	Copyright (C) Copyright (C) 2013-2017 University of Amsterdam

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 2 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.


	File created by patrick, on 27-01-2017
	Original file name was
*/

#ifndef IMPORTERUTILS_H_
#define IMPORTERUTILS_H_ 1

#ifndef QT_NO_DEBUG

#include <iostream>

#define DEBUG_COUT1(z) do { cout << z << endl; } while (false)
#define DEBUG_COUT2(y,z) do { cout << y << z << endl; } while (false)
#define DEBUG_COUT3(x, y, z) do { cout << x << y << z << endl; } while (false)
#define DEBUG_COUT4(w, x, y, z) do { cout << w << x << y << z << endl; } while (false)
#define DEBUG_COUT5(v, w, x, y, z) do { cout << v << w << x << y << z << endl; } while (false)
#define DEBUG_COUT6(u, v, w, x, y, z) do { cout << u << v << w << x << y << z << endl; } while (false)
#define DEBUG_COUT7(t, u, v, w, x, y, z) do { cout << t << u << v << w << x << y << z << endl; } while (false)
#define DEBUG_COUT8(s, t, u, v, w, x, y, z) do { cout << s << t << u << v << w << x << y << z << endl; } while (false)
#define DEBUG_COUT9(r, s, t, u, v, w, x, y, z) do { cout << r << s << t << u << v << w << x << y << z << endl; } while (false)


#else
#define DEBUG_COUT1
#define DEBUG_COUT2
#define DEBUG_COUT3
#define DEBUG_COUT4
#define DEBUG_COUT5
#define DEBUG_COUT6
#define DEBUG_COUT7
#define DEBUG_COUT8
#define DEBUG_COUT9

#endif // ndef QT_N_DEBUG

#endif // IMPORTERUTILS_H

