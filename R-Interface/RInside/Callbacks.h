// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; tab-width: 8 -*-
//
// Callbacks.h: R/C++ interface class library -- Easier R embedding into C++
//
// Copyright (C) 2010        Dirk Eddelbuettel and Romain Francois
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

#ifndef RINSIDE_CALLBACKS_H
#define RINSIDE_CALLBACKS_H

#include <RInsideCommon.h>

#ifdef RINSIDE_CALLBACKS

class Callbacks {
public:
	
	Callbacks() : R_is_busy(false), buffer() {} ;
	virtual ~Callbacks(){} ;
	
	virtual void ShowMessage(const char* message) {} ;
	virtual void Suicide(const char* message) {};
	virtual std::string ReadConsole( const char* prompt, bool addtohistory ) { return ""; };
	virtual void WriteConsole( const std::string& line, int type ) {};
	virtual void FlushConsole() {};
	virtual void ResetConsole() {};
	virtual void CleanerrConsole(){} ;
	virtual void Busy( bool is_busy ) {} ;
	
	void Busy_( int which ) ;
	int ReadConsole_( const char* prompt, unsigned char* buf, int len, int addtohistory ) ;
	void WriteConsole_( const char* buf, int len, int oType ) ;
	
	// TODO: ShowFiles
	// TODO: ChooseFile
	// TODO: loadHistory
	// TODO: SaveHistory                                                                                      
	
	virtual bool has_ShowMessage() { return false ; } ;
	virtual bool has_Suicide() { return false ; } ;
	virtual bool has_ReadConsole() { return false ; } ;
	virtual bool has_WriteConsole() { return false ; } ;
	virtual bool has_ResetConsole() { return false ; } ;
	virtual bool has_CleanerrConsole() { return false ; } ;
	virtual bool has_Busy() { return false ; } ;
	virtual bool has_FlushConsole(){ return false; } ;
	
private:
	bool R_is_busy ;
	std::string buffer ;
	
} ;                                       

#endif

#endif
