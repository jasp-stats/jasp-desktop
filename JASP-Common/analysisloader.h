#ifndef ANALYSISLOADER_H
#define ANALYSISLOADER_H

#include "analysis.h"

class AnalysisLoader
{
public:

	static Analysis *load(int id, std::string analysisName, Json::Value* data = NULL, Analysis::Status status = Analysis::Status::Empty);

};

#endif // ANALYSISLOADER_H
