// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; tab-width: 8 -*-
//
// MemBuf.h: R/C++ interface class library -- Easier R embedding into C++
//
// Copyright (C) 2009 Dirk Eddelbuettel
// Copyright (C) 2010 Dirk Eddelbuettel and Romain Francois
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

class MemBuf {			// simple C++-ification of littler's membuf
private:
    std::string buffer ;
    
public:    
    MemBuf(int sizebytes=1024);
    ~MemBuf();
    void resize();
    void rewind();
    void add(const std::string& );
    inline const char* getBufPtr() { return buffer.c_str() ; };
};
