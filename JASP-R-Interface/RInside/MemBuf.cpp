// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
//
// MemBuf.cpp: R/C++ interface class library -- Easier R embedding into C++
//
// Copyright (C) 2009         Dirk Eddelbuettel
// Copyright (C) 2010 - 2012  Dirk Eddelbuettel and Romain Francois
//
// This file is part of RInside.
//
// RInside is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// RInside is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with RInside.  If not, see <http://www.gnu.org/licenses/>.

#include <iostream>
#include <cstdlib>
#include <string>

#include "MemBuf.h"

extern bool verbose;
extern const char *programName;

MemBuf::~MemBuf() {}

MemBuf::MemBuf(int sizebytes) : buffer() {
    buffer.reserve(sizebytes) ;
}

void MemBuf::resize() {		// Use power of 2 resizing 
    buffer.reserve( 2*buffer.capacity() ) ;
}

void MemBuf::rewind(){
    buffer.clear() ;
}

void MemBuf::add(const std::string& buf){
    int buflen = buf.size() ;
    while ( ( buflen + buffer.size() ) >= buffer.capacity() ) {
	resize();
    }
    buffer += buf ;
}

