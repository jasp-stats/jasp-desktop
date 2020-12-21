// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; tab-width: 4 -*-
//
// RInside.h: R/C++ interface class library -- Easier R embedding into C++
//
// Copyright (C) 2009         Dirk Eddelbuettel
// Copyright (C) 2010 - 2017  Dirk Eddelbuettel and Romain Francois
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

#ifndef RINSIDE_RINSIDE_H
#define RINSIDE_RINSIDE_H

#include <RInsideCommon.h>
#include <Callbacks.h>

class RInside {
private:
    MemBuf mb_m;
    Rcpp::Environment* global_env_m;
    
    bool verbose_m;							// switch toggled by constructor, or setter
	bool interactive_m;						// switch set by constructor only

    void init_tempdir(void);
    void init_rand(void);
    void autoloads(void);
    
    void initialize(const int argc, const char* const argv[], 
					const bool loadRcpp, const bool verbose, const bool interactive);

    static RInside* instance_m ;

#ifdef RINSIDE_CALLBACKS
    Callbacks* callbacks ;
    friend void RInside_ShowMessage( const char* message);
    friend void RInside_WriteConsoleEx( const char* message, int len, int oType );
    friend int RInside_ReadConsole(const char *prompt, unsigned char *buf, int len, int addtohistory);
    friend void RInside_ResetConsole();
    friend void RInside_FlushConsole();
    friend void RInside_ClearerrConsole();
    friend void RInside_Busy(int which);
#endif 

public:

    class Proxy {
	public:
	    Proxy(SEXP xx): x(xx) { };
	
	    template <typename T>
	    operator T() {
			return ::Rcpp::as<T>(x);
	    }
	private:
	    Rcpp::RObject x;
	};

    int  parseEval(const std::string &line, SEXP &ans); // parse line, return in ans; error code rc
    void parseEvalQ(const std::string &line);			// parse line, no return (throws on error)
    void parseEvalQNT(const std::string &line);			// parse line, no return (no throw)
    Proxy parseEval(const std::string &line);		 	// parse line, return SEXP (throws on error)
    Proxy parseEvalNT(const std::string &line);			// parse line, return SEXP (no throw)

    template <typename T> 
    void assign(const T& object, const std::string& nam) {
		global_env_m->assign( nam, object ) ;
    }
    
    RInside() ;
    RInside(const int argc, const char* const argv[], 
			const bool loadRcpp=true, 					// overridden in code, cannot be set to false 
			const bool verbose=false, const bool interactive=false);
    ~RInside();

	void setVerbose(const bool verbose) 	{ verbose_m = verbose; }

    Rcpp::Environment::Binding operator[]( const std::string& name );
    
    static RInside& instance();
    static RInside* instancePtr();

	void repl();
    
#ifdef RINSIDE_CALLBACKS
    void set_callbacks(Callbacks* callbacks_) ;
#endif

};

#endif
