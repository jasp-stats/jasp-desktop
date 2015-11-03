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
	void rbridge_setDataSet(DataSet *dataSet);
	std::string rbridge_run(const std::string &name, const std::string &options, const std::string &perform = "run", int ppi = 96, RCallback callback = NULL);
	std::string rbridge_check();


#endif // RBRIDGE_H
