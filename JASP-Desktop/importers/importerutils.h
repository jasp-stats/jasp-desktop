//
// Copyright (C) 2015-2017 University of Amsterdam
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

#ifndef IMPORTERUTILS_H
#define IMPORTERUTILS_H 1

/*
 * Macros to build attriutes of a class.
 */
#define _ATT_VALUE(type, name)		\
private:							\
	type _##name;					\

#define READ_ATTR(type, name)		\
	_ATT_VALUE(type, name)			\
public:								\
	const type & name() const		\
		{ return _##name; }			\

#define WRITE_ATTR(type, name)		\
	_ATT_VALUE(type, name)			\
public:								\
	void name(const type &value)	\
		{ _##name = value; }		\

#define RW_ATTR(type, name)			\
	READ_ATTR(type, name)			\
public:								\
	void name(const type &value)	\
		{ _##name = value; }		\

/*
 * Defines a series of DEBUG_COUT macros,
 * These compile to nothing in release builds.
 * usage: e.g. DEBUG_COUT1("Got here!");
 */
#ifndef QT_NO_DEBUG

#include <iostream>

#define DEBUG_COUT1(z)	std::cout << z << std::endl; std::cout.flush()
#define DEBUG_COUT2(y, z)	std::cout << y << z << std::endl; std::cout.flush()
#define DEBUG_COUT3(x, y, z) std::cout << x << y << z << std::endl; std::cout.flush()
#define DEBUG_COUT4(w, x, y, z) std::cout << w << x << y << z << std::endl; std::cout.flush()
#define DEBUG_COUT5(v, w, x, y, z) std::cout << v << w << x << y << z << std::endl; std::cout.flush()
#define DEBUG_COUT6(u, v, w, x, y, z) std::cout << u << v << w << x << y << z << std::endl; std::cout.flush()
#define DEBUG_COUT7(t, u, v, w, x, y, z) std::cout << t << u << v << w << x << y << z << std::endl; std::cout.flush()
#define DEBUG_COUT8(s, t, u, v, w, x, y, z) std::cout << s << t << u << v << w << x << y << z << std::endl; std::cout.flush()
#define DEBUG_COUT9(r, s, t, u, v, w, x, y, z) std::cout << r << s << t << u << v << w << x << y << z << std::endl; std::cout.flush()
#define DEBUG_COUT10(q, r, s, t, u, v, w, x, y, z) std::cout << q << r << s << t << u << v << w << x << y << z << std::endl; std::cout.flush()
#define DEBUG_COUT11(p, q, r, s, t, u, v, w, x, y, z) std::cout << p << q << r << s << t << u << v << w << x << y << z << std::endl; std::cout.flush()

#else

#define DEBUG_COUT1(z)
#define DEBUG_COUT2(y, z)
#define DEBUG_COUT3(x, y, z)
#define DEBUG_COUT4(w, x, y, z)
#define DEBUG_COUT5(v, w, x, y, z)
#define DEBUG_COUT6(u, v, w, x, y, z)
#define DEBUG_COUT7(t, u, v, w, x, y, z)
#define DEBUG_COUT8(s, t, u, v, w, x, y, z)
#define DEBUG_COUT9(r, s, t, u, v, w, x, y, z)
#define DEBUG_COUT10(q, r, s, t, u, v, w, x, y, z)
#define DEBUG_COUT11(p, q, r, s, t, u, v, w, x, y, z)
#endif

#endif // DEBUG_COUT_H
