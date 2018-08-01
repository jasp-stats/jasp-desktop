#ifndef ENGINEDEFINITIONS_H
#define ENGINEDEFINITIONS_H

#include "enumutilities.h"

DECLARE_ENUM(engineState,			idle, analysis, filter, rCode, computeColumn, moduleRequest);
DECLARE_ENUM(performType,			init, run, abort, saveImg, editImg);
DECLARE_ENUM(analysisResultStatus,	error, exception, imageSaved, imageEdited, complete, inited, running, changed, waiting);
DECLARE_ENUM(moduleStatus,			installNeeded, loadingNeeded, readyForUse, error);

#endif // ENGINEDEFINITIONS_H
