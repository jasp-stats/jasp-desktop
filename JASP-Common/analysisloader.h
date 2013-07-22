#ifndef ANALYSISLOADER_H
#define ANALYSISLOADER_H

#include "analysis.h"

using namespace std;

class AnalysisLoader
{
public:

	static Analysis *load(int id, string analysisName, Options* options = NULL);

};

#endif // ANALYSISLOADER_H
