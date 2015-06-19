#ifndef ANALYSISLOADER_H
#define ANALYSISLOADER_H

#include "analysis.h"

class AnalysisLoader
{
public:

	static Analysis *load(int id, std::string analysisName, Json::Value* data = NULL);

};

#endif // ANALYSISLOADER_H
