#ifndef RBRIDGE_H
#define RBRIDGE_H

#include <RInside.h>
#include <Rcpp.h>

#include <string>
#include <map>
#include <boost/function.hpp>

#include "../JASP-Common/dataset.h"


#ifdef __WIN32__

#undef Realloc
#undef Free

#endif

	//typedef int (*RCallback)(std::string value);
	typedef boost::function<int(const std::string &)> RCallback;

	void rbridge_init();
	void rbridge_setDataSet(DataSet *dataSet);
	std::string rbridge_run(const std::string &name, const std::string &options, const std::string &perform = "run", RCallback callback = NULL);


#endif // RBRIDGE_H
