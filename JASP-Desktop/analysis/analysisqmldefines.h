#ifndef ANALYSISQMLDEFINES_H
#define ANALYSISQMLDEFINES_H

#include "enumutilities.h"

DECLARE_ENUM(qmlControlType, JASPControl, CheckBox, Switch, TextField, ButtonGroup, VariablesListView, ComboBox, FactorsList, Slider, TextArea);
DECLARE_ENUM(qmlListViewType, AssignedVariables, AssignedPairs, AssignedAnova, AvailableVariables, MeasuresCells);
DECLARE_ENUM(qmlDropMode, None, Replace, Insert)

#endif // ANALYSISQMLDEFINES_H
