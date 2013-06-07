#ifndef ANALYSISPART_H
#define ANALYSISPART_H

#include "boost/signals2.hpp"
#include "lib_json/json.h"

#include <string>

class Analysis;

class AnalysisPart
{
public:
	friend class Analysis;

	AnalysisPart(Analysis *analysis, std::string partName, Json::Value results);

	//enum Type { AnalysisNative, AnalysisR };

	Json::Value init();

	boost::signals2::signal<void (AnalysisPart* source)> revised;
	boost::signals2::signal<void (AnalysisPart* source)> completed;
	boost::signals2::signal<void (AnalysisPart* source)> aborted;

	bool isCompleted() const;
	void setCompleted(bool value);

	std::string name();

	//Type analysisType();

	Json::Value asJSON();

	Json::Value results() const;
	void setResults(const Json::Value results);

	int revision();

private:

	Analysis *_parent;
	std::string _name;
	Json::Value _results;

	int _revision;
	bool _completed;



};

#endif // ANALYSISPART_H
