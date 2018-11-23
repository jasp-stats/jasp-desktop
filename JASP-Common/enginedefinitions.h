#ifndef ENGINEDEFINITIONS_H
#define ENGINEDEFINITIONS_H

#include <stdexcept>
#include <string>

enum class engineState	{ idle, analysis, filter, rCode, computeColumn, paused, resuming };
std::string engineStateToString(engineState e);
engineState engineStateFromString(std::string e);

enum class performType	{ init, run, abort, saveImg, editImg};
std::string performTypeToString(performType p);
performType performTypeFromString(std::string p);

enum class analysisResultStatus  { error, exception, imageSaved, imageEdited, complete, inited, running, changed, waiting};
std::string analysisResultStatusToString(analysisResultStatus p);
analysisResultStatus analysisResultStatusFromString(std::string p);


#endif // ENGINEDEFINITIONS_H
