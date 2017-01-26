//
// Copyright (C) 2013-2017 University of Amsterdam
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

#ifndef RBRIDGE_H
#define RBRIDGE_H

#include <RInside.h>
#include <Rcpp.h>

#ifdef __WIN32__

#undef Realloc
#undef Free

#endif

#include <string>
#include <map>
#include <boost/function.hpp>

#include "../JASP-Common/dataset.h"

/* The R Bridge provides functions to the R analyses;
 * i.e. functions to read the data set from shared memory
 * Similarly, it provides functions to C++ code for
 * launching analyses written in R.
 * In this way, it functions as the bridge between the C++
 * application, and the R analyses
 */

	typedef boost::function<std::string (const std::string &)> RCallback;

	void rbridge_init();
	void rbridge_setFileNameSource(boost::function<void(const std::string &, std::string &, std::string &)> source);
	void rbridge_setStateFileSource(boost::function<void(std::string &, std::string &)> source);
	void rbridge_setDataSetSource(boost::function<DataSet *()> source);
	std::string rbridge_run(const std::string &name, const std::string &options, const std::string &perform = "run", int ppi = 96, RCallback callback = NULL);
	std::string rbridge_saveImage(const std::string &name, const std::string &type);
	//std::string rbridge_saveImage(const std::string &name, const std::string &type, const std::string &height, const std::string &width);
	std::string rbridge_check();


#endif // RBRIDGE_H
